## VS Code Development

### Prerequisites

- [Node.js](https://nodejs.org/) 20+
- Python 3.9+ (with tkinter recommended for turtle support)

### Setup

1. Clone the repository and install dependencies:

        cd vscode
        npm install

2. Compile the TypeScript source:

        npm run compile

3. To watch for changes during development:

        npm run watch

### Running the Extension

#### Option A: Extension Development Host (F5)

1. Open the `vscode/` folder in VS Code.
2. Press F5 to launch the Extension Development Host.
3. Open a Python file and run "Live Py: Start Live Coding" from the
   command palette, or click the turtle icon in the editor title bar.

#### Option B: Install VSIX locally

1. Package the extension:

        npm run bundle-tracer
        npx @vscode/vsce package --allow-missing-repository

2. Install the `.vsix` file: Cmd+Shift+P → "Extensions: Install from
   VSIX..." → select `live-py-0.1.0.vsix`.

### Architecture

The extension follows the same pattern as the Sublime and Emacs plugins:

1. Launch `space_tracer` as a subprocess via stdin/stdout.
2. Parse the output (text trace + canvas commands).
3. Display results in a split editor view.

Key source files:

| File | Purpose |
|------|---------|
| `src/extension.ts` | Session management, commands, scroll sync |
| `src/tracerManager.ts` | Subprocess lifecycle, debounce, Python detection |
| `src/canvasPanel.ts` | Webview Canvas rendering (turtle/matplotlib) |
| `src/canvasParser.ts` | Canvas command parser (ported from `html/src/SampleAnalyst.js`) |
| `src/codeLensProvider.ts` | CodeLens "Start Live Coding" link |
| `src/decorationManager.ts` | Inline decoration support (unused in split view mode) |

### Running Tests

The extension uses [@vscode/test-electron](https://github.com/nicedoc/vscode-test)
for E2E testing. It automatically downloads a VS Code instance and runs
tests inside it.

    npm test

Test fixtures are in `src/test/fixtures/` — these are also useful as demo
files you can open to try the extension.

### Packaging for Distribution

    npm run bundle-tracer
    npx @vscode/vsce package --allow-missing-repository

This creates a `.vsix` file that bundles `space_tracer` from
`plugin/PySrc/`, so users don't need to install it separately.

### Configuration

The extension provides these settings under `livePy.*`:

| Setting | Default | Description |
|---------|---------|-------------|
| `pythonPath` | `python3` | Path to Python executable |
| `spacetracerPath` | (bundled) | Path to space_tracer module |
| `debounceMs` | `300` | Delay before re-running tracer |
| `canvasWidth` | `800` | Canvas width in pixels |
| `canvasHeight` | `600` | Canvas height in pixels |
| `driver` | (none) | Driver script for the traced code |
| `traced` | (none) | Module name to trace |
| `stdinFile` | (none) | File to use as stdin |
| `millisecondLimit` | `10000` | Execution time limit in ms |
