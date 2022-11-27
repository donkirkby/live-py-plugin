import SampleAnalyst from "./SampleAnalyst";
import React from "react";

function prefixLines(sourceCode) {
    let lines = sourceCode.split("\n");
    let displayLines = lines.map(
        line => line === "" ? line : "> " + line);
    let outputLines = lines.map(
        line => line === "" ? line : "out " + line);
    let display = displayLines.join("\n");
    let output = outputLines.join("\n");
    return {get(i) { return [display, output][i]; }};
}

describe('SampleAnalyst', () => {
    it('stores source', () => {
        let expectedSource = `\
line 1
line 2
`;
        let analyst = new SampleAnalyst(expectedSource);

        expect(analyst.sourceCode).toBe(expectedSource);
    });

    it('displays analysis', () => {
        let expectedSource = `\
line 1
line 2
`;
        let expectedDisplay = `\
> line 1
> line 2
`;
        let expectedOutput = `\
out line 1
out line 2
`;
        let analyst = new SampleAnalyst(expectedSource, prefixLines);

        expect(analyst.sourceCode).toBe(expectedSource);
        expect(analyst.display).toBe(expectedDisplay);
        expect(analyst.output).toBe(expectedOutput);
        expect(analyst.goalOutput).toBeUndefined();
        expect(analyst.isLive).toBe(true);
    });

    it('hides analysis if marked as static', () => {
        let source = `\
### static ###
line 1
line 2
`;
        let expectedSource = `\
line 1
line 2
`;
        let analyst = new SampleAnalyst(source, prefixLines);

        expect(analyst.sourceCode).toBe(expectedSource);
        expect(analyst.display).toBeUndefined();
        expect(analyst.output).toBeUndefined();
        expect(analyst.goalOutput).toBeUndefined();
        expect(analyst.isLive).toBe(false);
    });

    it('hides analysis if known to be static', () => {
        let goalSourceCode = undefined,
            goalOutput = undefined,
            goalCanvasCommands = undefined,
            isLive = false,
            expectedSource = `\
line 1
line 2
`;
        let analyst = new SampleAnalyst(
            expectedSource,
            prefixLines,
            goalOutput,
            goalCanvasCommands,
            goalSourceCode,
            isLive);

        expect(analyst.sourceCode).toBe(expectedSource);
        expect(analyst.display).toBeUndefined();
        expect(analyst.output).toBeUndefined();
        expect(analyst.goalOutput).toBeUndefined();
        expect(analyst.isLive).toBe(false);
    });

    it('hides analysis if it contains REPL prompt', () => {
        let expectedSource = `\
>>> 1 + 2
3
`;
        let analyst = new SampleAnalyst(expectedSource, prefixLines);

        expect(analyst.sourceCode).toBe(expectedSource);
        expect(analyst.display).toBeUndefined();
        expect(analyst.output).toBeUndefined();
        expect(analyst.goalOutput).toBeUndefined();
        expect(analyst.isLive).toBe(false);
    });

    it('shows analysis if marked as live', () => {
        let source = `\
### live ###
print('Here >>> There')
`,
            expectedSource = `\
print('Here >>> There')
`,
            expectedDisplay = `\
> print('Here >>> There')
`,
            expectedOutput = `\
out print('Here >>> There')
`;
        let analyst = new SampleAnalyst(source, prefixLines);

        expect(analyst.sourceCode).toBe(expectedSource);
        expect(analyst.display).toBe(expectedDisplay);
        expect(analyst.output).toBe(expectedOutput);
        expect(analyst.goalOutput).toBeUndefined();
        expect(analyst.isLive).toBe(true);
    });

    it('displays goal', () => {
        let originalSource = `\
line 1
line B
### Goal ###
line 1
line 2
`;
        let expectedSource = `\
line 1
line B
`;
        let expectedDisplay = `\
> line 1
> line B
`;
        let expectedOutput = `\
out line 1
out line B
`;
        let expectedGoalOutput = `\
out line 1
out line 2
`;
        let analyst = new SampleAnalyst(originalSource, prefixLines);

        expect(analyst.sourceCode).toBe(expectedSource);
        expect(analyst.display).toBe(expectedDisplay);
        expect(analyst.output).toBe(expectedOutput);
        expect(analyst.goalOutput).toBe(expectedGoalOutput);
    });

    it('displays goal with minimal heading', () => {
        let originalSource = `\
line 1
line B
##  goal
line 1
line 2
`;
        let expectedOutput = `\
out line 1
out line B
`;
        let analyst = new SampleAnalyst(originalSource, prefixLines);

        expect(analyst.output).toBe(expectedOutput);
    });

    it('ignores second header in static sample', () => {
        let originalSource = `\
### static ###
### live ###
line 1
line B
### goal ###
line 1
line 2
`;
        let analyst = new SampleAnalyst(originalSource, prefixLines);

        expect(analyst.output).toBeUndefined();
    });

    it('ignores leading blank lines', () => {
        let originalSource = `\

### live ###
line 1
line B
### goal ###
line 1
line 2
`;
        let expectedOutput = `\
out line 1
out line B
`;
        let analyst = new SampleAnalyst(originalSource, prefixLines);

        expect(analyst.output).toBe(expectedOutput);
    });

    it('ignores goal without heading', () => {
        let originalSource = `\
line 1
line B
My goal is to test!
line 1
line 2
`;
        let expectedOutput = `\
out line 1
out line B
out My goal is to test!
out line 1
out line 2
`;
        let analyst = new SampleAnalyst(originalSource, prefixLines);

        expect(analyst.output).toBe(expectedOutput);
    });

    it('uses old goal', () => {
        let originalSource = `\
line 1
line B
### Goal ###
line 1
line 2
`;
        let oldGoal = `\
old line 1
old line 2
`;
        let expectedOutput = `\
out line 1
out line B
`;
        let analyst = new SampleAnalyst(originalSource, prefixLines, oldGoal);

        expect(analyst.output).toBe(expectedOutput);
        expect(analyst.goalOutput).toBe(oldGoal);
    });

    it('uses old goal source', () => {
        let source = `\
line 1
line B
### Goal ###
Totally unrelated!
`,
            oldGoalSource = `\
line 1
line 2
`,
            oldGoal = undefined,
            oldGoalCommands = undefined,
            expectedGoal = `\
out line 1
out line 2
`;
        let analyst = new SampleAnalyst(
            source,
            prefixLines,
            oldGoal,
            oldGoalCommands,
            oldGoalSource);

        expect(analyst.sourceCode).toBe(source);
        expect(analyst.goalOutput).toBe(expectedGoal);
    });

    it('calculates diffs', () => {
        let originalSource = `\
line 1
line 2
### Goal ###
First line
Line the second
`;

        let expectedGoalMarkers = [
            {
                // "out First line"
                //      ^^^^^^
                startRow: 0,
                startCol: 4,
                endRow: 0,
                endCol: 10,
                className: 'change-marker is-warning',
                type: 'text'
            },
            {
                // "out Line the second"
                //      ^
                startRow: 1,
                startCol: 4,
                endRow: 1,
                endCol: 5,
                className: 'change-marker is-warning',
                type: 'text'
            },
            {
                // "out Line the second"
                //           ^^^^^^^^^^
                startRow: 1,
                startCol: 9,
                endRow: 1,
                endCol: 19,
                className: 'change-marker is-warning',
                type: 'text'
            }
        ];
        let expectedOutputMarkers = [
            {
                // "out line 1"
                //          ^^
                startRow: 0,
                startCol: 8,
                endRow: 0,
                endCol: 10,
                className: 'change-marker is-warning',
                type: 'text'
            },
            {
                // "out line 2"
                //      ^
                startRow: 1,
                startCol: 4,
                endRow: 1,
                endCol: 5,
                className: 'change-marker is-warning',
                type: 'text'
            },
            {
                // "out line 2"
                //           ^
                startRow: 1,
                startCol: 9,
                endRow: 1,
                endCol: 10,
                className: 'change-marker is-warning',
                type: 'text'
            }
        ];
        let analyst = new SampleAnalyst(originalSource, prefixLines);

        expect(analyst.goalMarkers).toEqual(expectedGoalMarkers);
        expect(analyst.outputMarkers).toEqual(expectedOutputMarkers);
    });

    it('calculates percentage match', () => {
        let originalSource = `\
line 1
line 2
### Goal ###
line A
line B
`,
            expectedPercentage = 40/44 * 100,
            analyst = new SampleAnalyst(originalSource, prefixLines);

        expect(analyst.matchPercentage).toBeCloseTo(expectedPercentage, 4);
    });

    it('detects canvas mode', () => {
        let run = () => {
            let display = `\
start_canvas
create_line
    50
    50
    150
    50
    fill='black'
    pensize=1
create_line
    50
    150
    150
    150
    fill='black'
    pensize=1.5
end_canvas
.
x = 1
`;
            return {get(i) { return [display, ''][i]; }};
        },
            source = `\
### Canvas ###
line 1
`,
            expectedSource = `\
line 1
`,
            expectedDisplay = `\
x = 1
`,
            expectedCanvasCommands = [
                {
                    name: 'create_line',
                    fill: 'black',
                    pensize: 1,
                    coords: [50, 50, 150, 50]
                },
                {
                    name: 'create_line',
                    fill: 'black',
                    pensize: 1.5,
                    coords: [50, 150, 150, 150]
                }];
        let analyst = new SampleAnalyst(source, run);

        expect(analyst.sourceCode).toBe(expectedSource);
        expect(analyst.display).toBe(expectedDisplay);
        expect(analyst.isLive).toBe(true);
        expect(analyst.isCanvas).toBe(true);
        expect(analyst.canvasCommands).toEqual(expectedCanvasCommands);
    });

    it('parses font options', () => {
        let run = () => {
            let display = String.raw`start_canvas
create_text
    100
    0
    anchor='sw'
    fill='black'
    font=('Courier', 14, 'bold')
    text='Bob with quotes "\' and\r\n\t\x03whitespace.'
create_text
    100
    20
    anchor='sw'
    fill='black'
    font=('Courier', 14, 'bold')
    text="Bob's secret message with a \\backslash."
end_canvas
.
x = 1
`;
            return {get(i) { return [display, ''][i]; }};
        },
            source = `\
### Canvas ###
line 1
`,
            expectedCanvasCommands = [
                {
                    name: 'create_text',
                    fill: 'black',
                    anchor: 'sw',
                    font: 'bold 14px Courier',
                    text: 'Bob with quotes "\' and\r\n\t\x03whitespace.',
                    coords: [100, 0]
                },
                {
                    name: 'create_text',
                    fill: 'black',
                    anchor: 'sw',
                    font: 'bold 14px Courier',
                    text: "Bob's secret message with a \\backslash.",
                    coords: [100, 20]
                }];
        let analyst = new SampleAnalyst(source, run);

        expect(analyst.canvasCommands).toEqual(expectedCanvasCommands);
    });

    it('displays canvas goal', () => {
        let run = (sourceCode) => {
            let sourceLines = sourceCode.split("\n");
            let display = `\
start_canvas
create_text
    100
    0
    anchor='sw'
    fill='black'
    font=('Courier', 14, 'bold')
    text='${sourceLines[0]}'
end_canvas
.
${sourceCode}`;
                return {get(i) { return [display, ''][i]; }};
            };
        let originalSource = `\
### Canvas ###
line A
line 2
### Goal ###
line 1
line 2
`;
        let expectedSource = `\
line A
line 2
`;
        let expectedDisplay = `\
line A
line 2
`;
        let expectedCanvasCommands = [
                {
                    name: 'create_text',
                    fill: 'black',
                    anchor: 'sw',
                    font: 'bold 14px Courier',
                    text: 'line A',
                    coords: [100, 0]
                }];
        let expectedGoalCanvasCommands = [
                {
                    name: 'create_text',
                    fill: 'black',
                    anchor: 'sw',
                    font: 'bold 14px Courier',
                    text: 'line 1',
                    coords: [100, 0]
                }];
        let expectedOutput = '';
        let expectedGoalOutput = `\
`;
        let analyst = new SampleAnalyst(originalSource, run);

        expect(analyst.sourceCode).toBe(expectedSource);
        expect(analyst.display).toBe(expectedDisplay);
        expect(analyst.output).toBe(expectedOutput);
        expect(analyst.goalOutput).toBe(expectedGoalOutput);
        expect(analyst.canvasCommands).toEqual(expectedCanvasCommands);
        expect(analyst.goalCanvasCommands).toEqual(expectedGoalCanvasCommands);
    });
});
