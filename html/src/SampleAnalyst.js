import { diffChars } from 'diff';

export default class SampleAnalyst {

    constructor(sourceCode, run, goalOutput, goalSourceCode) {
        if (goalSourceCode !== undefined) {
            this.sourceCode = sourceCode;
            this.goalSourceCode = goalSourceCode;
        } else {
            let splitSource = sourceCode.split(/ *#[# ]*Goal[ #]*\n/i);
            this.sourceCode = splitSource[0];
            this.goalSourceCode = splitSource[1];
        }
        if (run !== undefined) {
            let result = run(this.sourceCode);
            this.display = result[0];
            this.output = result[1];

            if (goalOutput !== undefined) {
                this.goalOutput = goalOutput;
            } else if (this.goalSourceCode !== undefined) {
                let goalResult = run(this.goalSourceCode);
                this.goalOutput = goalResult[1];
            }
            if (this.goalOutput !== undefined) {
                let diffs = diffChars(this.goalOutput, this.output),
                    goalLineNumber = 0,
                    goalColumnNumber = 0,
                    outputLineNumber = 0,
                    outputColumnNumber = 0,
                    matchCount = 0,
                    mismatchCount = 0,
                    allMarkers = diffs.map(diff => {
                        let lineCount = (diff.value.match(/\n/g) || '').length,
                            lastGoalLine = goalLineNumber + lineCount,
                            lastGoalColumn = goalColumnNumber + diff.value.length,
                            lastOutputLine = outputLineNumber + lineCount,
                            lastOutputColumn = outputColumnNumber + diff.value.length;
                        if (diff.added || diff.removed) {
                            mismatchCount += diff.value.length;
                        } else {
                            matchCount += 2*diff.value.length;
                        }
                        let marker = {
                            startRow: goalLineNumber,
                            startCol: goalColumnNumber,
                            endRow: lastGoalLine,
                            endCol: lastGoalColumn,
                            className: "change-marker",
                            type: "text",
                            added: diff.added,
                            removed: diff.removed
                        };
                        if ( ! diff.removed) {
                            marker.startRow = outputLineNumber;
                            marker.startCol = outputColumnNumber;
                            marker.endRow = lastOutputLine;
                            marker.endCol = lastOutputColumn;
                        }
                        if (lineCount > 0) {
                            let lastNewLine = diff.value.lastIndexOf('\n');
                            marker.endCol = diff.value.length - lastNewLine - 1;
                            lastGoalColumn = lastOutputColumn = marker.endCol;
                        }
                        if ( ! diff.added) {
                            goalLineNumber = lastGoalLine;
                            goalColumnNumber = lastGoalColumn;
                        }
                        if ( ! diff.removed) {
                            outputLineNumber = lastOutputLine;
                            outputColumnNumber = lastOutputColumn;
                        }
                        return marker;
                    });
                this.goalMarkers = allMarkers.filter(marker => marker.removed);
                this.outputMarkers = allMarkers.filter(marker => marker.added);
                allMarkers.forEach(marker => {
                    delete marker.added;
                    delete marker.removed;
                });
                this.matchPercentage = 100 * (
                    matchCount / (matchCount + mismatchCount));
            }
        }
    }
}
