import * as vscode from 'vscode';
import { CanvasCommand } from './canvasParser';

export class CanvasPanel {
    private panel: vscode.WebviewPanel | null = null;

    show(commands: CanvasCommand[], width: number, height: number, viewColumn: vscode.ViewColumn = vscode.ViewColumn.Beside) {
        if (!this.panel) {
            this.panel = vscode.window.createWebviewPanel(
                'livePyCanvas',
                'Live Py Canvas',
                { viewColumn, preserveFocus: true },
                { enableScripts: true },
            );
            this.panel.onDidDispose(() => {
                this.panel = null;
            });
        }

        this.panel.webview.html = this.getHtml(commands, width, height);
    }

    hide() {
        if (this.panel) {
            this.panel.dispose();
            this.panel = null;
        }
    }

    dispose() {
        this.hide();
    }

    private getHtml(commands: CanvasCommand[], width: number, height: number): string {
        const commandsJson = JSON.stringify(commands);
        return `<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<style>
    body {
        margin: 0;
        padding: 0;
        display: flex;
        justify-content: center;
        align-items: flex-start;
        background: var(--vscode-editor-background);
        overflow: hidden;
    }
    canvas {
        display: block;
        max-width: 100%;
        max-height: 100vh;
        background: #fff;
    }
</style>
</head>
<body>
<canvas id="canvas" width="${width}" height="${height}"></canvas>
<script>
(function() {
    const canvas = document.getElementById('canvas');
    const ctx = canvas.getContext('2d');
    const commands = ${commandsJson};

    function renderCommands() {
        ctx.clearRect(0, 0, canvas.width, canvas.height);

        for (const cmd of commands) {
            switch (cmd.name) {
                case 'bgcolor': {
                    const fill = cmd.fill || '#ffffff';
                    ctx.fillStyle = fill;
                    ctx.fillRect(0, 0, canvas.width, canvas.height);
                    break;
                }
                case 'create_line': {
                    if (cmd.coords.length < 4) break;
                    ctx.strokeStyle = cmd.fill || '#000000';
                    ctx.lineWidth = parseFloat(cmd.pensize) || 1;
                    ctx.beginPath();
                    ctx.moveTo(cmd.coords[0], cmd.coords[1]);
                    for (let i = 2; i < cmd.coords.length; i += 2) {
                        ctx.lineTo(cmd.coords[i], cmd.coords[i + 1]);
                    }
                    ctx.stroke();
                    break;
                }
                case 'create_polygon': {
                    if (cmd.coords.length < 4) break;
                    ctx.fillStyle = cmd.fill || '#000000';
                    if (cmd.outline) {
                        ctx.strokeStyle = cmd.outline;
                        ctx.lineWidth = parseFloat(cmd.pensize) || 1;
                    }
                    ctx.beginPath();
                    ctx.moveTo(cmd.coords[0], cmd.coords[1]);
                    for (let i = 2; i < cmd.coords.length; i += 2) {
                        ctx.lineTo(cmd.coords[i], cmd.coords[i + 1]);
                    }
                    ctx.closePath();
                    ctx.fill();
                    if (cmd.outline) ctx.stroke();
                    break;
                }
                case 'create_text': {
                    const x = cmd.coords[0] || 0;
                    const y = cmd.coords[1] || 0;
                    const text = cmd.text || '';
                    ctx.fillStyle = cmd.fill || '#000000';
                    ctx.font = cmd.font || 'normal 8px Arial';

                    const anchor = cmd.anchor || 'center';
                    const metrics = ctx.measureText(text);
                    let dx = 0, dy = 0;
                    const fontSize = parseInt((cmd.font || '8').toString().match(/\\d+/)?.[0] || '8');

                    if (anchor.includes('e')) dx = -metrics.width;
                    else if (!anchor.includes('w')) dx = -metrics.width / 2;

                    if (anchor.includes('s')) dy = 0;
                    else if (anchor.includes('n')) dy = fontSize;
                    else dy = fontSize / 2;

                    ctx.fillText(text, x + dx, y + dy);
                    break;
                }
                case 'create_image': {
                    const ix = cmd.coords[0] || 0;
                    const iy = cmd.coords[1] || 0;
                    const img = new Image();
                    img.src = 'data:image/png;base64,' + cmd.image;
                    img.onload = () => ctx.drawImage(img, ix, iy);
                    break;
                }
            }
        }
    }

    renderCommands();
})();
</script>
</body>
</html>`;
    }
}
