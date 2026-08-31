import * as vscode from 'vscode';
import { TracerManager } from './tracerManager';
import { parseCanvasCommands } from './canvasParser';
import { CanvasPanel } from './canvasPanel';
import { LivePyCodeLensProvider } from './codeLensProvider';

const SCHEME = 'live-py-output';

class LivePyContentProvider implements vscode.TextDocumentContentProvider {
    private content = '';
    private _onDidChange = new vscode.EventEmitter<vscode.Uri>();
    readonly onDidChange = this._onDidChange.event;

    update(text: string) {
        this.content = text;
        this._onDidChange.fire(vscode.Uri.parse(`${SCHEME}:Live Coding Output`));
    }

    provideTextDocumentContent(): string {
        return this.content;
    }

    dispose() {
        this._onDidChange.dispose();
    }
}

interface LiveSession {
    sourceEditor: vscode.TextEditor;
    outputEditor: vscode.TextEditor | null;
    changeListener: vscode.Disposable;
    scrollListener: vscode.Disposable;
}

// Scroll sync: PyCharm-style "who scrolled first leads for 300ms"
let scrollLeader: vscode.TextEditor | null = null;
let scrollLeaderTick = 0;

let tracerManager: TracerManager;
let contentProvider: LivePyContentProvider;
let activeSession: LiveSession | null = null;
let statusBarItem: vscode.StatusBarItem;
let outputChannel: vscode.OutputChannel;
let canvasPanel: CanvasPanel;

export function activate(context: vscode.ExtensionContext) {
    tracerManager = new TracerManager();
    contentProvider = new LivePyContentProvider();
    outputChannel = vscode.window.createOutputChannel('Live Py');
    canvasPanel = new CanvasPanel();

    context.subscriptions.push(
        vscode.workspace.registerTextDocumentContentProvider(SCHEME, contentProvider),
    );

    statusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 100);
    statusBarItem.command = 'livePy.stop';
    context.subscriptions.push(statusBarItem);

    context.subscriptions.push(
        vscode.commands.registerCommand('livePy.start', () => startSession()),
        vscode.commands.registerCommand('livePy.stop', stopSession),
    );

    // Follow active Python file
    context.subscriptions.push(
        vscode.window.onDidChangeActiveTextEditor((editor) => {
            if (!activeSession || !editor) return;
            if (editor.document.languageId !== 'python') return;
            if (editor.document.uri.scheme !== 'file') return;
            if (editor === activeSession.outputEditor) return;

            activeSession.sourceEditor = editor;
            triggerTrace();
        }),
    );

    context.subscriptions.push(
        tracerManager, contentProvider, outputChannel, canvasPanel,
    );

    context.subscriptions.push(
        vscode.languages.registerCodeLensProvider(
            { language: 'python' },
            new LivePyCodeLensProvider(),
        ),
    );
}

async function startSession() {
    const editor = vscode.window.activeTextEditor;
    if (!editor) {
        vscode.window.showWarningMessage('No active Python editor.');
        return;
    }
    if (editor.document.languageId !== 'python') {
        vscode.window.showWarningMessage('Live Py only works with Python files.');
        return;
    }
    if (activeSession) {
        stopSession();
    }

    const outputUri = vscode.Uri.parse(`${SCHEME}:Live Coding Output`);
    contentProvider.update('');
    const outputDoc = await vscode.workspace.openTextDocument(outputUri);
    const outputEditor = await vscode.window.showTextDocument(outputDoc, {
        viewColumn: vscode.ViewColumn.Beside,
        preserveFocus: true,
        preview: false,
    });

    const changeListener = vscode.workspace.onDidChangeTextDocument((e) => {
        if (activeSession && e.document === activeSession.sourceEditor.document) {
            triggerTrace();
        }
    });

    const scrollListener = vscode.window.onDidChangeTextEditorVisibleRanges((e) => {
        if (!activeSession || !activeSession.outputEditor) return;

        const { sourceEditor, outputEditor } = activeSession;
        const isSource = e.textEditor === sourceEditor;
        const isOutput = e.textEditor === outputEditor;
        if (!isSource && !isOutput) return;

        // Determine leader: first editor to scroll "leads" for 300ms
        // If there's already a leader, only the leader can drive sync
        if (scrollLeader && scrollLeader !== e.textEditor) return;

        // Record this editor as leader, reset after 300ms
        scrollLeader = e.textEditor;
        scrollLeaderTick++;
        const tick = scrollLeaderTick;
        setTimeout(() => {
            if (scrollLeaderTick === tick) scrollLeader = null;
        }, 300);

        // Sync: leader drives the follower
        const range = e.visibleRanges[0];
        if (!range) return;
        const follower = isSource ? outputEditor : sourceEditor;
        follower.revealRange(range, vscode.TextEditorRevealType.AtTop);
    });

    activeSession = { sourceEditor: editor, outputEditor, changeListener, scrollListener };
    vscode.commands.executeCommand('setContext', 'livePy.isRunning', true);

    statusBarItem.text = '$(debug-pause) Live Py';
    statusBarItem.tooltip = 'Click to stop live coding';
    statusBarItem.show();

    triggerTrace();
}

function stopSession() {
    if (!activeSession) return;

    tracerManager.cancelPending();
    activeSession.changeListener.dispose();
    activeSession.scrollListener.dispose();

    // Clear output content and close the output panel
    contentProvider.update('');
    // Close all editors in the beside group
    vscode.commands.executeCommand('workbench.action.closeEditorsInOtherGroups');

    vscode.commands.executeCommand('setContext', 'livePy.isRunning', false);
    activeSession = null;
    statusBarItem.hide();
    canvasPanel.hide();
}

function saveOtherDocuments() {
    if (!activeSession) return;
    const activeUri = activeSession.sourceEditor.document.uri.toString();
    const dirty = vscode.workspace.textDocuments.filter(
        doc => doc.isDirty && doc.uri.scheme === 'file' && doc.uri.toString() !== activeUri
    );
    dirty.forEach(doc => doc.save());
}

function triggerTrace() {
    if (!activeSession) return;
    saveOtherDocuments();

    const sourceCode = activeSession.sourceEditor.document.getText();
    const filePath = activeSession.sourceEditor.document.uri.fsPath;

    tracerManager.scheduleTrace(sourceCode, filePath, true, (result) => {
        if (!activeSession) return;

        if (result.stderr && result.traceLines.length === 0) {
            canvasPanel.hide();
            contentProvider.update(`Error:\n${result.stderr}`);
        } else if (result.canvasRaw) {
            contentProvider.update(result.traceLines.join('\n'));
            const commands = parseCanvasCommands(result.canvasRaw);
            const config = vscode.workspace.getConfiguration('livePy');
            canvasPanel.show(
                commands,
                config.get<number>('canvasWidth', 800),
                config.get<number>('canvasHeight', 600),
            );
        } else {
            canvasPanel.hide();
            contentProvider.update(result.traceLines.join('\n'));
        }

        if (result.stderr) {
            outputChannel.clear();
            outputChannel.appendLine(result.stderr);
            outputChannel.show(true);
        }

        // Update status bar to reflect mode
        if (result.canvasRaw) {
            statusBarItem.text = '$(debug-pause) Live Py (Canvas)';
        } else {
            statusBarItem.text = '$(debug-pause) Live Py';
        }

        // Color: green when passing, red when errors
        if (result.stderr && result.traceLines.length === 0) {
            statusBarItem.backgroundColor = new vscode.ThemeColor('statusBarItem.errorBackground');
        } else {
            statusBarItem.backgroundColor = undefined;
        }
    });
}

export function deactivate() {
    stopSession();
}
