import { diffChars } from 'diff';

function unescapeString(value) {
    if ((value.startsWith("'") && value.endsWith("'")) ||
        (value.startsWith('"') && value.endsWith('"'))) {
        value = value.substring(1, value.length - 1);
        let newValue = '';
        for (let i = 0; i < value.length; i++) {
            const c = value[i];
            if (c !== '\\' || i === value.length - 1) {
                newValue += c;
            } else {
                const c2 = value[++i];
                switch (c2) {
                    case '\\':
                        newValue += '\\';
                        break;
                    case '\'':
                        newValue += '\'';
                        break;
                    case 'n':
                        newValue += '\n';
                        break;
                    case 'r':
                        newValue += '\r';
                        break;
                    case 't':
                        newValue += '\t';
                        break;
                    case 'x':
                        if (i+2 < value.length)
                        {
                            const charCode = parseInt(
                                value.substring(i+1, i+3),
                                16);
                            newValue += String.fromCharCode(charCode);
                            i += 2;
                            break;
                        }
                        newValue += c;
                        newValue += c2;
                        break;
                    default:
                        newValue += c;
                        newValue += c2;
                        break;
                }
            }
        }
        value = newValue;
    } else if (value.match(/^[0-9.]+$/)) {
        value = parseFloat(value);
    }
    return value;
}

export default class SampleAnalyst {

    constructor(sourceCode,
                run,
                goalOutput,
                goalSourceCode,
                isLive,
                isCanvas,
                canvasSize) {
        if (goalSourceCode !== undefined) {
            this.sourceCode = sourceCode;
            this.goalSourceCode = goalSourceCode;
            this.isLive = true;
            this.isCanvas = isCanvas;
        } else if (isLive === false) {
            this.sourceCode = sourceCode;
            this.isLive = isLive;
            this.isCanvas = false;
        } else if (isCanvas !== undefined) {
            this.sourceCode = sourceCode;
            this.isLive = isLive;
            this.isCanvas = isCanvas;
        } else {
            let sourcePieces =
                /^(.*\n)?( *##+ *((static)|(live)|(canvas))[ #]*\n)(.*)$/is.exec(
                    sourceCode);
            if (sourcePieces !== null) {
                this.sourceCode = (sourcePieces[1] || "") + sourcePieces[7];
                this.isLive = sourcePieces[3].toLowerCase() !== "static";
                this.isCanvas = sourcePieces[3].toLowerCase() === "canvas";
            } else if (/>>>/.test(sourceCode)) {
                this.sourceCode = sourceCode;
                this.isLive = false;
                this.isCanvas = false;
            } else {
                this.isLive = true;
                this.isCanvas = false;
                let splitSource = sourceCode.split(/ *##+ *Goal[ #]*\n/i);
                this.sourceCode = splitSource[0];
                this.goalSourceCode = splitSource[1];
            }
        }
        if (run !== undefined && this.isLive) {
            if ( ! this.isCanvas) {
                canvasSize = undefined;
            } else if (canvasSize === undefined) {
                canvasSize = [300, 150];
            }
            let result = run(this.sourceCode, canvasSize);
            this.display = result.get(0);
            this.output = result.get(1);

            if (this.isCanvas) {
                const displayLines = this.display.split(
                    /\r\n|(?!\r\n)[\n-\r\x85\u2028\u2029]/);
                displayLines.shift();  // start_canvas
                this.canvasCommands = [];
                let currentCommand = undefined;
                while (displayLines.length) {
                    const nextLine = displayLines.shift();
                    if (nextLine === '.') {
                        break;
                    }
                    if ( ! nextLine.startsWith('    ')) {
                        if (currentCommand !== undefined) {
                            this.canvasCommands.push(currentCommand);
                        }
                        currentCommand = {name: nextLine, coords: []};
                    } else {
                        const position = nextLine.indexOf('=');
                        if (position === -1) {
                            currentCommand.coords.push(parseInt(nextLine));
                        } else {
                            const fieldName = nextLine.substring(4, position),
                                fieldValue = nextLine.substring(position+1);
                            if (fieldName !== 'font') {
                                currentCommand[fieldName] = unescapeString(
                                    fieldValue);
                            } else {
                                const fontMatch = fieldValue.match(
                                    /\('([^']*)', (\d+), '([^']*)'\)/);
                                let fontName = 'Arial',
                                    fontSize = 8,
                                    styleNames = 'normal';
                                if (fontMatch !== undefined) {
                                    fontName = fontMatch[1];
                                    fontSize = fontMatch[2];
                                    styleNames = fontMatch[3];
                                }
                                currentCommand[fieldName] = (
                                    `${styleNames} ${fontSize}px ${fontName}`);
                            }
                        }
                    }
                }
                this.display = displayLines.join('\n');
            }
            if (goalOutput !== undefined) {
                this.goalOutput = goalOutput;
            } else if (this.goalSourceCode !== undefined) {
                let goalResult = run(this.goalSourceCode);
                this.goalOutput = goalResult.get(1);
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
