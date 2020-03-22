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
    return [display, output];
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
#  goal
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
            expectedGoal = `\
out line 1
out line 2
`;
        let analyst = new SampleAnalyst(
            source,
            prefixLines,
            oldGoal,
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
                className: 'change-marker warning',
                type: 'text'
            },
            {
                // "out Line the second"
                //      ^
                startRow: 1,
                startCol: 4,
                endRow: 1,
                endCol: 5,
                className: 'change-marker warning',
                type: 'text'
            },
            {
                // "out Line the second"
                //           ^^^^^^^^^^
                startRow: 1,
                startCol: 9,
                endRow: 1,
                endCol: 19,
                className: 'change-marker warning',
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
                className: 'change-marker warning',
                type: 'text'
            },
            {
                // "out line 2"
                //      ^
                startRow: 1,
                startCol: 4,
                endRow: 1,
                endCol: 5,
                className: 'change-marker warning',
                type: 'text'
            },
            {
                // "out line 2"
                //           ^
                startRow: 1,
                startCol: 9,
                endRow: 1,
                endCol: 10,
                className: 'change-marker warning',
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
});
