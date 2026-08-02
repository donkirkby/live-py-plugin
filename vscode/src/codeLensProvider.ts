import * as vscode from 'vscode';

export class LivePyCodeLensProvider implements vscode.CodeLensProvider {
    provideCodeLenses(document: vscode.TextDocument): vscode.CodeLens[] {
        if (document.languageId !== 'python') return [];

        const topOfFile = new vscode.Range(0, 0, 0, 0);
        return [
            new vscode.CodeLens(topOfFile, {
                title: '$(play) Start Live Coding',
                command: 'livePy.start',
            }),
        ];
    }
}
