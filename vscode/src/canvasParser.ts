export interface CanvasCommand {
    name: string;
    coords: number[];
    [key: string]: unknown;
}

export function parseCanvasCommands(canvasRaw: string): CanvasCommand[] {
    const lines = canvasRaw.split(/\r?\n/);
    const commands: CanvasCommand[] = [];
    let current: CanvasCommand | null = null;

    let i = 0;
    if (lines[0]?.trim() === 'start_canvas') i = 1;

    for (; i < lines.length; i++) {
        const line = lines[i];
        if (line.trim() === 'end_canvas') break;

        if (!line.startsWith('    ')) {
            if (current) {
                commands.push(current);
            }
            current = { name: line.trim(), coords: [] };
        } else if (current) {
            const trimmed = line.substring(4);
            const eqPos = trimmed.indexOf('=');
            if (eqPos === -1) {
                current.coords.push(parseInt(trimmed, 10));
            } else {
                const fieldName = trimmed.substring(0, eqPos);
                const fieldValue = trimmed.substring(eqPos + 1);
                if (fieldName === 'font') {
                    const fontMatch = fieldValue.match(
                        /\('([^']*)',\s*(\d+),\s*'([^']*)'\)/
                    );
                    if (fontMatch) {
                        current[fieldName] = `${fontMatch[3]} ${fontMatch[2]}px ${fontMatch[1]}`;
                    } else {
                        current[fieldName] = 'normal 8px Arial';
                    }
                } else {
                    current[fieldName] = unescapeString(fieldValue);
                }
            }
        }
    }
    if (current) {
        commands.push(current);
    }

    return commands;
}

function unescapeString(value: string): string {
    if (
        (value.startsWith("'") && value.endsWith("'")) ||
        (value.startsWith('"') && value.endsWith('"'))
    ) {
        value = value.substring(1, value.length - 1);
        let result = '';
        for (let i = 0; i < value.length; i++) {
            const c = value[i];
            if (c !== '\\' || i === value.length - 1) {
                result += c;
            } else {
                const c2 = value[++i];
                switch (c2) {
                    case '\\': result += '\\'; break;
                    case "'":  result += "'";  break;
                    case 'n':  result += '\n'; break;
                    case 'r':  result += '\r'; break;
                    case 't':  result += '\t'; break;
                    case 'x':
                        if (i + 2 < value.length) {
                            const charCode = parseInt(value.substring(i + 1, i + 3), 16);
                            result += String.fromCharCode(charCode);
                            i += 2;
                        } else {
                            result += c + c2;
                        }
                        break;
                    default:
                        result += c + c2;
                        break;
                }
            }
        }
        return result;
    }
    if (/^[\d.]+$/.test(value)) {
        return value;
    }
    return value;
}
