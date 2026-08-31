import * as vscode from 'vscode';
import { spawn, ChildProcess } from 'child_process';
import * as path from 'path';
import * as fs from 'fs';

export interface TraceResult {
    traceLines: string[];
    canvasRaw: string | null;
    stderr: string;
}

export class TracerManager {
    private debounceTimer: ReturnType<typeof setTimeout> | null = null;
    private runningProcess: ChildProcess | null = null;

    dispose() {
        this.cancelPending();
    }

    cancelPending() {
        if (this.debounceTimer) {
            clearTimeout(this.debounceTimer);
            this.debounceTimer = null;
        }
        if (this.runningProcess) {
            this.runningProcess.kill();
            this.runningProcess = null;
        }
    }

    scheduleTrace(
        sourceCode: string,
        filePath: string,
        canvas: boolean,
        onResult: (result: TraceResult) => void
    ) {
        this.cancelPending();

        const config = vscode.workspace.getConfiguration('livePy');
        const debounceMs = config.get<number>('debounceMs', 300);

        this.debounceTimer = setTimeout(() => {
            this.runTrace(sourceCode, filePath, canvas)
                .then(onResult)
                .catch(err => {
                    onResult({
                        traceLines: [],
                        canvasRaw: null,
                        stderr: String(err),
                    });
                });
        }, debounceMs);
    }

    private async runTrace(
        sourceCode: string,
        filePath: string,
        canvas: boolean
    ): Promise<TraceResult> {
        const config = vscode.workspace.getConfiguration('livePy');
        const pythonPath = await this.resolvePython(config.get<string>('pythonPath', ''));
        const canvasWidth = config.get<number>('canvasWidth', 800);
        const canvasHeight = config.get<number>('canvasHeight', 600);

        let spacetracerDir = config.get<string>('spacetracerPath', '');
        if (!spacetracerDir) {
            const bundledPath = path.resolve(__dirname, '../bundled');
            const repoPath = path.resolve(__dirname, '../../plugin/PySrc');
            spacetracerDir = fs.existsSync(
                path.join(bundledPath, 'space_tracer')
            ) ? bundledPath : repoPath;
        }

        const driver = config.get<string>('driver', '');

        const args = [
            '-m', 'space_tracer',
            '--live',
            '--source_width', '0',
            '--traced_file', '/dev/stdin',
        ];

        const msLimit = config.get<number>('millisecondLimit', 10000);
        args.push('--millisecond_limit', String(msLimit));

        if (canvas) {
            args.push('--canvas');
            args.push('-x', String(canvasWidth));
            args.push('-y', String(canvasHeight));
        }

        const stdinFile = config.get<string>('stdinFile', '');
        if (stdinFile) {
            args.push('--stdin', stdinFile);
        }

        const traced = config.get<string>('traced', '');
        if (traced) {
            args.push('--traced', traced);
        }

        if (driver) {
            args.push('--', driver);
        }

        const env = { ...process.env, PYTHONPATH: spacetracerDir };
        const workingDir = path.dirname(filePath);

        return new Promise<TraceResult>((resolve, reject) => {
            const proc = spawn(pythonPath, args, {
                env,
                cwd: workingDir,
            });
            this.runningProcess = proc;

            let stdout = '';
            let stderr = '';

            proc.stdout?.on('data', (data: Buffer) => { stdout += data.toString(); });
            proc.stderr?.on('data', (data: Buffer) => { stderr += data.toString(); });

            proc.on('close', () => {
                this.runningProcess = null;
                resolve(this.parseOutput(stdout, stderr));
            });

            proc.on('error', (err) => {
                this.runningProcess = null;
                reject(err);
            });

            proc.stdin?.write(sourceCode);
            proc.stdin?.end();
        });
    }

    private async resolvePython(configured: string): Promise<string> {
        // 1. User explicitly configured livePy.pythonPath
        if (configured) return configured;

        // 2. VSCode Python extension's selected interpreter
        try {
            const pythonExt = vscode.extensions.getExtension('ms-python.python');
            if (pythonExt) {
                if (!pythonExt.isActive) await pythonExt.activate();
                const api = pythonExt.exports;

                // Modern API (2023.4+): environments.resolveEnvironment
                const activeEnv = api?.environments?.getActiveEnvironmentPath?.();
                if (activeEnv && api?.environments?.resolveEnvironment) {
                    const resolved = await api.environments.resolveEnvironment(activeEnv);
                    const p = resolved?.executable?.uri?.fsPath;
                    if (p) return p;
                }
                // activeEnv.path as fallback
                if (activeEnv?.path) return activeEnv.path;

                // Legacy API
                const cmd = api?.settings?.getExecutionDetails?.()?.execCommand?.[0];
                if (cmd) return cmd;
            }
        } catch {
            // Python extension not available
        }

        // 3. python.defaultInterpreterPath setting
        const defaultPath = vscode.workspace.getConfiguration('python')
            .get<string>('defaultInterpreterPath', '');
        if (defaultPath && defaultPath !== 'python') return defaultPath;

        // 4. Fallback
        return 'python3';
    }

    private parseOutput(stdout: string, stderr: string): TraceResult {
        let canvasRaw: string | null = null;
        let traceText = stdout;

        const canvasStartIdx = stdout.indexOf('start_canvas');
        if (canvasStartIdx !== -1) {
            const canvasEndIdx = stdout.indexOf('end_canvas');
            if (canvasEndIdx !== -1) {
                canvasRaw = stdout.substring(canvasStartIdx, canvasEndIdx + 'end_canvas'.length);

                const afterCanvas = stdout.substring(canvasEndIdx + 'end_canvas'.length);
                const dotSepIdx = afterCanvas.indexOf('.\n');
                if (dotSepIdx !== -1) {
                    traceText = afterCanvas.substring(dotSepIdx + 2);
                } else {
                    traceText = afterCanvas.replace(/^\s*\.\s*/, '');
                }
            }
        }

        const traceLines = traceText.split('\n');
        if (traceLines.length > 0 && traceLines[traceLines.length - 1] === '') {
            traceLines.pop();
        }

        return { traceLines, canvasRaw, stderr };
    }
}
