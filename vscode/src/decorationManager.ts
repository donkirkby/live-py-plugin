import * as vscode from 'vscode';

export class DecorationManager {
    private traceDecorationType: vscode.TextEditorDecorationType;
    private errorDecorationType: vscode.TextEditorDecorationType;

    constructor() {
        this.traceDecorationType = vscode.window.createTextEditorDecorationType({});
        this.errorDecorationType = vscode.window.createTextEditorDecorationType({
            backgroundColor: 'rgba(255, 0, 0, 0.05)',
            isWholeLine: true,
        });
    }

    applyTrace(editor: vscode.TextEditor, traceLines: string[]) {
        const decorations: vscode.DecorationOptions[] = [];
        const lineCount = editor.document.lineCount;

        for (let i = 0; i < traceLines.length && i < lineCount; i++) {
            const trace = traceLines[i];
            if (!trace || !trace.trim()) continue;

            const line = editor.document.lineAt(i);
            decorations.push({
                range: new vscode.Range(i, line.text.length, i, line.text.length),
                renderOptions: {
                    after: {
                        contentText: `  # ${trace}`,
                        color: new vscode.ThemeColor('editorCodeLens.foreground'),
                        fontStyle: 'italic',
                        margin: '0 0 0 1.5em',
                    },
                },
            });
        }

        editor.setDecorations(this.traceDecorationType, decorations);
    }

    applyError(editor: vscode.TextEditor, errorMsg: string) {
        if (!errorMsg.trim()) {
            editor.setDecorations(this.errorDecorationType, []);
            return;
        }

        const lineMatch = errorMsg.match(/line (\d+)/);
        const errorLine = lineMatch ? parseInt(lineMatch[1], 10) - 1 : 0;
        const safeLine = Math.min(errorLine, editor.document.lineCount - 1);

        const line = editor.document.lineAt(safeLine);
        const decorations: vscode.DecorationOptions[] = [{
            range: new vscode.Range(safeLine, line.text.length, safeLine, line.text.length),
            renderOptions: {
                after: {
                    contentText: `  ⚠ ${errorMsg.split('\n').pop()?.trim() || 'Error'}`,
                    color: 'rgba(255, 80, 80, 0.9)',
                    fontStyle: 'italic',
                    margin: '0 0 0 1.5em',
                },
            },
        }];

        editor.setDecorations(this.errorDecorationType, decorations);
    }

    clearAll(editor: vscode.TextEditor) {
        editor.setDecorations(this.traceDecorationType, []);
        editor.setDecorations(this.errorDecorationType, []);
    }

    dispose() {
        this.traceDecorationType.dispose();
        this.errorDecorationType.dispose();
    }
}
