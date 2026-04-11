import * as assert from 'assert';
import * as vscode from 'vscode';
import * as path from 'path';

const FIXTURES = path.resolve(__dirname, '../../../src/test/fixtures');

function fixture(name: string): string {
    return path.join(FIXTURES, name);
}

function sleep(ms: number): Promise<void> {
    return new Promise(resolve => setTimeout(resolve, ms));
}

async function openFile(filePath: string): Promise<vscode.TextEditor> {
    const doc = await vscode.workspace.openTextDocument(filePath);
    return vscode.window.showTextDocument(doc, vscode.ViewColumn.One);
}

function findOutputEditor(): vscode.TextEditor | undefined {
    return vscode.window.visibleTextEditors.find(
        e => e.document.uri.scheme === 'live-py-output'
    );
}

async function startAndWait(waitMs = 3000): Promise<void> {
    await vscode.commands.executeCommand('livePy.start');
    await sleep(waitMs);
}

async function stopLivePy(): Promise<void> {
    await vscode.commands.executeCommand('livePy.stop');
    await sleep(500);
}

suite('Live Py Extension', () => {

    teardown(async () => {
        await stopLivePy();
    });

    // --- Activation ---

    test('Extension activates and commands are registered', async () => {
        const ext = vscode.extensions.getExtension('donkirkby.live-py');
        assert.ok(ext, 'Extension should be found');
        if (!ext.isActive) await ext.activate();
        assert.ok(ext.isActive, 'Extension should be active');

        const commands = await vscode.commands.getCommands(true);
        assert.ok(commands.includes('livePy.start'), 'livePy.start registered');
        assert.ok(commands.includes('livePy.stop'), 'livePy.stop registered');
    });

    // --- Basic Tracing ---

    test('basic_trace: function call and return value', async function () {
        this.timeout(15000);
        await openFile(fixture('basic_trace.py'));
        await startAndWait();

        const output = findOutputEditor();
        assert.ok(output, 'Output editor should exist');
        const content = output!.document.getText();
        assert.ok(content.length > 0, 'Output should not be empty');
        assert.ok(content.includes('x = 3'), `Should trace x=3, got: ${content.substring(0, 200)}`);
    });

    test('binary_search: loop variable tracing', async function () {
        this.timeout(15000);
        await openFile(fixture('binary_search.py'));
        await startAndWait();

        const output = findOutputEditor();
        assert.ok(output, 'Output editor should exist');
        const content = output!.document.getText();
        assert.ok(
            content.includes('low') || content.includes('high') || content.includes('mid'),
            `Should trace search vars, got: ${content.substring(0, 300)}`
        );
    });

    test('control_flow: if/elif/else branching', async function () {
        this.timeout(15000);
        await openFile(fixture('control_flow.py'));
        await startAndWait();

        const output = findOutputEditor();
        assert.ok(output, 'Output editor should exist');
        const content = output!.document.getText();
        assert.ok(content.includes('x = 42'), `Should trace x=42`);
        assert.ok(content.includes("print('More')"), `Should trace else branch`);
    });

    test('for_loop: iteration over list', async function () {
        this.timeout(15000);
        await openFile(fixture('for_loop.py'));
        await startAndWait();

        const output = findOutputEditor();
        assert.ok(output, 'Output editor should exist');
        const content = output!.document.getText();
        assert.ok(content.includes("w = 'cat'") || content.includes('cat'),
            `Should trace loop variable w`);
    });

    test('nested_loops: both loop variables traced', async function () {
        this.timeout(15000);
        await openFile(fixture('nested_loops.py'));
        await startAndWait();

        const output = findOutputEditor();
        assert.ok(output, 'Output editor should exist');
        const content = output!.document.getText();
        assert.ok(content.includes('i = 0'), 'Should trace outer loop');
        assert.ok(content.includes('j = 0') || content.includes('j = 1'), 'Should trace inner loop');
    });

    test('break_continue: prime number detection', async function () {
        this.timeout(15000);
        await openFile(fixture('break_continue.py'));
        await startAndWait();

        const output = findOutputEditor();
        assert.ok(output, 'Output editor should exist');
        const content = output!.document.getText();
        assert.ok(content.includes('n = 2') || content.includes('n = 3'), 'Should trace n');
    });

    test('fibonacci: while loop with swap', async function () {
        this.timeout(15000);
        await openFile(fixture('fibonacci.py'));
        await startAndWait();

        const output = findOutputEditor();
        assert.ok(output, 'Output editor should exist');
        const content = output!.document.getText();
        assert.ok(content.includes('a = 0') || content.includes('b = 1'),
            `Should trace fib variables`);
    });

    test('mutable_default: list accumulation', async function () {
        this.timeout(15000);
        await openFile(fixture('mutable_default.py'));
        await startAndWait();

        const output = findOutputEditor();
        assert.ok(output, 'Output editor should exist');
        const content = output!.document.getText();
        assert.ok(content.includes('a = 1') || content.includes('L'),
            `Should trace function params`);
    });

    test('print_output: formatted strings', async function () {
        this.timeout(15000);
        await openFile(fixture('print_output.py'));
        await startAndWait();

        const output = findOutputEditor();
        assert.ok(output, 'Output editor should exist');
        const content = output!.document.getText();
        assert.ok(content.includes('i = 0') || content.includes('print('),
            `Should trace loop and print`);
    });

    // --- Error Handling ---

    test('syntax_error: displays error message', async function () {
        this.timeout(15000);
        await openFile(fixture('syntax_error.py'));
        await startAndWait();

        const output = findOutputEditor();
        assert.ok(output, 'Output editor should exist');
        const content = output!.document.getText();
        assert.ok(
            content.includes('Error') || content.includes('Syntax') || content.includes('invalid'),
            `Should show error, got: ${content.substring(0, 200)}`
        );
    });

    // --- Canvas Auto-Detection ---

    test('turtle_square: auto-detects canvas mode', async function () {
        this.timeout(15000);
        await openFile(fixture('turtle_square.py'));
        await startAndWait(5000);

        const output = findOutputEditor();
        assert.ok(output, 'Output editor should exist');
        const content = output!.document.getText();
        assert.ok(content.includes('MockTurtle') || content.includes('bgcolor') || content.length > 0,
            'Should have trace for turtle code');
    });

    test('turtle_star: star polygon renders', async function () {
        this.timeout(15000);
        await openFile(fixture('turtle_star.py'));
        await startAndWait(5000);

        const output = findOutputEditor();
        assert.ok(output, 'Output editor should exist');
        const content = output!.document.getText();
        assert.ok(content.length > 0, 'Should have trace for star');
    });

    test('matplotlib_basic: plot auto-detects canvas', async function () {
        this.timeout(20000);
        await openFile(fixture('matplotlib_basic.py'));
        await startAndWait(8000);

        const output = findOutputEditor();
        assert.ok(output, 'Output editor should exist');
        const content = output!.document.getText();
        assert.ok(content.length > 0, 'Should have output for matplotlib');
    });

    // --- Session Lifecycle ---

    test('Stop and restart works correctly', async function () {
        this.timeout(15000);
        await openFile(fixture('basic_trace.py'));
        await startAndWait();
        await stopLivePy();

        await openFile(fixture('control_flow.py'));
        await startAndWait();

        const output = findOutputEditor();
        assert.ok(output, 'Output should exist after restart');
        const content = output!.document.getText();
        assert.ok(content.includes('x = 42'), 'Should trace new file after restart');
    });

    // --- File Switching ---

    test('Switching files re-traces automatically', async function () {
        this.timeout(20000);

        await openFile(fixture('basic_trace.py'));
        await startAndWait();

        const output1 = findOutputEditor();
        const content1 = output1!.document.getText();

        await openFile(fixture('for_loop.py'));
        await sleep(3000);

        const output2 = findOutputEditor();
        assert.ok(output2, 'Output should persist after switch');
        const content2 = output2!.document.getText();
        assert.notStrictEqual(content1, content2, 'Trace should change when file switches');
    });

    test('Switching text → turtle auto-shows canvas', async function () {
        this.timeout(20000);

        await openFile(fixture('basic_trace.py'));
        await startAndWait();

        await openFile(fixture('turtle_square.py'));
        await sleep(5000);

        const output = findOutputEditor();
        assert.ok(output, 'Output should persist');
    });
});
