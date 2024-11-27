package io.github.donkirkby.livecanvas;

import java.io.BufferedReader;
import java.io.Closeable;
import java.io.IOException;
import java.io.StringWriter;
import java.util.ArrayList;

public class CanvasReader implements Closeable {
    private static final boolean DEBUG = false;
    private final BufferedReader inputReader;
    private String nextLine;

    public CanvasReader(BufferedReader inputReader) {
        this.inputReader = inputReader;
    }

    public CanvasCommand read() {
        try {
            String line = nextLine != null ? nextLine : inputReader.readLine();
            if (line == null) {
                return null;
            }
            nextLine = null;
            CanvasCommand command = new CanvasCommand();
            command.setName(line);
            while (true) {
                nextLine = inputReader.readLine();
                if (nextLine == null || !nextLine.startsWith("    ")) {
                    return command;
                }
                if (DEBUG) {
                    System.out.println("load canvas line: " + nextLine);
                }
                nextLine = nextLine.substring(4); // trim spaces
                int position = nextLine.indexOf('=');
                if (position == -1) {
                    command.addCoordinate(Integer.parseInt(nextLine));
                } else {
                    String name = nextLine.substring(0, position);
                    String value = nextLine.substring(position + 1);
                    value = unescapeString(value);
                    command.setOption(name, value);
                }
            }
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }
    }

    private String unescapeString(String value) {
        if (value.startsWith("'") && value.endsWith("'") ||
                value.startsWith("\"") && value.endsWith("\"")) {
            value = value.substring(1, value.length() - 1);
            StringWriter writer = new StringWriter();
            for (int i = 0; i < value.length(); i++) {
                char c = value.charAt(i);
                if (c != '\\' || i == value.length() - 1) {
                    writer.write(c);
                } else {
                    char c2 = value.charAt(++i);
                    switch (c2) {
                        case '\\':
                            writer.write('\\');
                            break;
                        case '\'':
                            writer.write('\'');
                            break;
                        case 'n':
                            writer.write('\n');
                            break;
                        case 'r':
                            writer.write('\r');
                            break;
                        case 't':
                            writer.write('\t');
                            break;
                        case 'x':
                            if (i + 2 < value.length()) {
                                int x = Integer.parseInt(
                                        value.substring(i + 1, i + 3),
                                        16);
                                writer.write((char) x);
                                i += 2;
                                break;
                            }
                        default:
                            writer.write(c);
                            writer.write(c2);
                            break;
                    }
                }
            }
            value = writer.toString();
        }
        return value;
    }

    public void close() {
        try {
            inputReader.close();
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }
    }

    public ArrayList<CanvasCommand> readCommands() {
        ArrayList<CanvasCommand> newCommands = new ArrayList<>();
        CanvasCommand command;
        boolean isDone;
        do {
            command = read();

            isDone = command == null || command.getName().equals("end_canvas");
            if (!isDone) {
                newCommands.add(command);
            }
        } while (!isDone);
        return newCommands;
    }
}
