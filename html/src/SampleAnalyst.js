import { diffChars } from 'diff';

export default class SampleAnalyst {

    constructor(sourceCode, run, goalOutput, goalSourceCode, isLive, isTurtle) {
        if (goalSourceCode !== undefined) {
            this.sourceCode = sourceCode;
            this.goalSourceCode = goalSourceCode;
            this.isLive = true;
            this.isTurtle = isTurtle;
        } else if (isLive === false) {
            this.sourceCode = sourceCode;
            this.isLive = isLive;
            this.isTurtle = false;
        } else if (isTurtle !== undefined) {
            this.sourceCode = sourceCode;
            this.isLive = isLive;
            this.isTurtle = isTurtle;
        } else {
            let sourcePieces =
                /^(.*\n)?( *##+ *((static)|(live)|(turtle))[ #]*\n)(.*)$/is.exec(
                    sourceCode);
            if (sourcePieces !== null) {
                this.sourceCode = (sourcePieces[1] || "") + sourcePieces[7];
                this.isLive = sourcePieces[3].toLowerCase() !== "static";
                this.isTurtle = sourcePieces[3].toLowerCase() === "turtle";
            } else if (/>>>/.test(sourceCode)) {
                this.sourceCode = sourceCode;
                this.isLive = false;
                this.isTurtle = false;
            } else {
                this.isLive = true;
                this.isTurtle = false;
                let splitSource = sourceCode.split(/ *##+ *Goal[ #]*\n/i);
                this.sourceCode = splitSource[0];
                this.goalSourceCode = splitSource[1];
            }
        }
        if (run !== undefined && this.isLive) {
            let canvasSize = this.isTurtle ? [200, 200] : undefined,
                result = run(this.sourceCode, canvasSize);
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
                            className: "change-marker warning",
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
