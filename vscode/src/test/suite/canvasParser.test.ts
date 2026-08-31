import * as assert from 'assert';
import { parseCanvasCommands } from '../../canvasParser';

suite('parseCanvasCommands', () => {

    test('empty input returns single empty-name command', () => {
        // '' splits into [''], which the parser treats as a command with empty name
        const cmds = parseCanvasCommands('');
        assert.strictEqual(cmds.length, 1);
        assert.strictEqual(cmds[0].name, '');
    });

    test('single command with coords', () => {
        const input = [
            'create_line',
            '    100',
            '    200',
            '    300',
            '    400',
        ].join('\n');

        const cmds = parseCanvasCommands(input);
        assert.strictEqual(cmds.length, 1);
        assert.strictEqual(cmds[0].name, 'create_line');
        assert.deepStrictEqual(cmds[0].coords, [100, 200, 300, 400]);
    });

    test('command with key=value fields', () => {
        const input = [
            'create_line',
            '    10',
            '    20',
            '    30',
            '    40',
            "    fill='#ff0000'",
            '    pensize=2',
        ].join('\n');

        const cmds = parseCanvasCommands(input);
        assert.strictEqual(cmds[0].fill, '#ff0000');
        assert.strictEqual(cmds[0].pensize, '2');
    });

    test('multiple commands', () => {
        const input = [
            'create_line',
            '    0',
            '    0',
            '    100',
            '    100',
            'create_polygon',
            '    10',
            '    20',
            '    30',
            '    40',
        ].join('\n');

        const cmds = parseCanvasCommands(input);
        assert.strictEqual(cmds.length, 2);
        assert.strictEqual(cmds[0].name, 'create_line');
        assert.strictEqual(cmds[1].name, 'create_polygon');
    });

    test('strips start_canvas / end_canvas wrapper', () => {
        const input = [
            'start_canvas',
            'create_line',
            '    10',
            '    20',
            'end_canvas',
        ].join('\n');

        const cmds = parseCanvasCommands(input);
        assert.strictEqual(cmds.length, 1);
        assert.strictEqual(cmds[0].name, 'create_line');
        assert.deepStrictEqual(cmds[0].coords, [10, 20]);
    });

    test('stops parsing at end_canvas', () => {
        const input = [
            'create_line',
            '    1',
            '    2',
            'end_canvas',
            'create_line',
            '    99',
            '    99',
        ].join('\n');

        const cmds = parseCanvasCommands(input);
        assert.strictEqual(cmds.length, 1);
        assert.deepStrictEqual(cmds[0].coords, [1, 2]);
    });

    test('font field is converted to CSS format', () => {
        const input = [
            'create_text',
            '    100',
            '    200',
            "    font=('Arial', 14, 'bold')",
        ].join('\n');

        const cmds = parseCanvasCommands(input);
        assert.strictEqual(cmds[0].font, 'bold 14px Arial');
    });

    test('invalid font falls back to default', () => {
        const input = [
            'create_text',
            '    0',
            '    0',
            '    font=garbage',
        ].join('\n');

        const cmds = parseCanvasCommands(input);
        assert.strictEqual(cmds[0].font, 'normal 8px Arial');
    });

    test('unescapes single-quoted string values', () => {
        const input = [
            'create_text',
            '    0',
            '    0',
            "    text='hello world'",
        ].join('\n');

        const cmds = parseCanvasCommands(input);
        assert.strictEqual(cmds[0].text, 'hello world');
    });

    test('unescapes backslash sequences', () => {
        const input = [
            'create_text',
            '    0',
            '    0',
            "    text='line1\\nline2'",
        ].join('\n');

        const cmds = parseCanvasCommands(input);
        assert.strictEqual(cmds[0].text, 'line1\nline2');
    });

    test('unescapes hex sequences', () => {
        const input = [
            'create_text',
            '    0',
            '    0',
            "    text='\\x41\\x42'",
        ].join('\n');

        const cmds = parseCanvasCommands(input);
        assert.strictEqual(cmds[0].text, 'AB');
    });

    test('handles double-quoted strings', () => {
        const input = [
            'create_text',
            '    0',
            '    0',
            '    text="hello"',
        ].join('\n');

        const cmds = parseCanvasCommands(input);
        assert.strictEqual(cmds[0].text, 'hello');
    });

    test('numeric values pass through as strings', () => {
        const input = [
            'create_line',
            '    0',
            '    0',
            '    pensize=3.5',
        ].join('\n');

        const cmds = parseCanvasCommands(input);
        assert.strictEqual(cmds[0].pensize, '3.5');
    });

    test('bgcolor command with fill', () => {
        const input = [
            'bgcolor',
            "    fill='#336699'",
        ].join('\n');

        const cmds = parseCanvasCommands(input);
        assert.strictEqual(cmds.length, 1);
        assert.strictEqual(cmds[0].name, 'bgcolor');
        assert.strictEqual(cmds[0].fill, '#336699');
    });

    test('handles Windows-style line endings', () => {
        // Trailing \r\n produces an extra empty line, parsed as a second command
        const input = 'create_line\r\n    10\r\n    20\r\n';

        const cmds = parseCanvasCommands(input);
        assert.strictEqual(cmds.length, 2);
        assert.strictEqual(cmds[0].name, 'create_line');
        assert.deepStrictEqual(cmds[0].coords, [10, 20]);
        assert.strictEqual(cmds[1].name, '');
    });

    test('indented lines without a current command are ignored', () => {
        const input = '    orphan_line\ncreate_line\n    5\n    10';

        const cmds = parseCanvasCommands(input);
        assert.strictEqual(cmds.length, 1);
        assert.strictEqual(cmds[0].name, 'create_line');
    });
});
