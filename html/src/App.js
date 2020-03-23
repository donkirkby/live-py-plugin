import React, { Component } from 'react';
import AceEditor from 'react-ace';
import ReactMarkdown from 'react-markdown';
import SampleAnalyst from './SampleAnalyst.js';
import './App.css';
import lessons from './lessons.json';

import 'brace/mode/python';
import 'brace/mode/markdown';
import 'brace/theme/github';

const PythonContext = React.createContext('Python is loading...');

class ProgressBar extends Component {
    render() {
        let stateClass = (this.props.percentage < 50) ?
            "danger" :
            (this.props.percentage < 100) ?
                "warning" :
                "success";
        return <div className="progressbar-wrapper ">
            <div className={"progressbar-filler " + stateClass}
                 style={{width: `${this.props.percentage}%`}}/>
        </div>;
    }
}

class Editor extends Component {
    constructor(props) {
        super(props);
        this.state = {selectedLine: undefined};
        this.content = React.createRef();
        this.handleScroll = this.handleScroll.bind(this);
    }

    handleScroll() {
        this.props.onScroll(this.content.current.editor.session.getScrollTop());
    }

    componentDidUpdate() {
        this.content.current.editor.session.setScrollTop(this.props.scrollTop);
        if (this.props.selectedLine !== this.state.selectedLine) {
            this.setState({selectedLine: this.props.selectedLine});
            this.content.current.editor.gotoLine(this.props.selectedLine+1);
        }
    }

    render() {
        return <AceEditor
            ref={this.content}
            value={this.props.value}
            onChange={this.props.onChange}
            readOnly={this.props.readOnly}
            onScroll={this.handleScroll}
            onSelectionChange={this.props.onSelectionChange}
            onCursorChange={this.props.onCursorChange}
            mode={this.props.mode}
            theme="github"
            width="100%"
            height="100%"
            fontSize={18}
            showPrintMargin={true}
            markers={this.props.markers}
            showGutter={true}
            highlightActiveLine={true}
            editorProps={{
                $blockScrolling: Infinity
            }}
            setOptions={{
                showLineNumbers: true,
                tabSize: 4,
            }}/>;
    }
}

class CodeSample extends Component {
    static contextType = PythonContext;

    constructor(props) {
        super(props);
        let codeRunner = this.context === null ? window.analyze : undefined,
            analyst = new SampleAnalyst(props.value, codeRunner);
        this.state = {
            scrollTop: 0,
            selectedLine: undefined,
            isPythonLoaded: false,
            source: analyst.sourceCode,
            originalSource: analyst.sourceCode,
            goalSourceCode: analyst.goalSourceCode,
            display: analyst.display,
            goalOutput: analyst.goalOutput,
            output: analyst.output,
            goalMarkers: analyst.goalMarkers,
            outputMarkers: analyst.outputMarkers,
            matchPercentage: analyst.matchPercentage
        };

        this.handleChange = this.handleChange.bind(this);
        this.handleReset = this.handleReset.bind(this);
        this.handleScroll = this.handleScroll.bind(this);
        this.handleCursorChange = this.handleCursorChange.bind(this);
    }

    handleChange(newSource) {
        if (newSource === undefined) {
            newSource = this.state.source;
        }
        let codeRunner = this.context === null ? window.analyze : undefined,
            analyst = new SampleAnalyst(
                newSource,
                codeRunner,
                this.state.goalOutput,
                this.state.goalSourceCode
            );
        this.setState({
            source: newSource,
            display: analyst.display,
            output: analyst.output,
            goalOutput: analyst.goalOutput,
            goalMarkers: analyst.goalMarkers,
            outputMarkers: analyst.outputMarkers,
            matchPercentage: analyst.matchPercentage
        });
    }

    handleReset() {
        this.handleChange(this.state.originalSource);
    }

    handleScroll(scrollTop) {
        this.setState({scrollTop: scrollTop});
    }

    handleCursorChange(selection) {
        this.setState({selectedLine: selection.selectionLead.row});
    }

    componentDidUpdate(prevProps, prevState) {
        if (this.context !== null) {
            if (this.context !== this.state.display) {
                this.setState({display: this.context});
            }
        }
        else if ( ! this.state.isPythonLoaded) {
            this.handleChange();
            this.setState({isPythonLoaded: true});
        }
    }

    render() {
        let displayValue = this.context;
        if (displayValue === null) {
            displayValue = this.state.display;
        }
        let progressBar = null,
            outputHeaders = null,
            outputSection = null,
            resetButton = null;
        if (this.state.source !== this.state.originalSource) {
            resetButton = <div className="reset-wrapper">
                <button className="reset-code" onClick={this.handleReset}>Reset</button>
            </div>;
        }
        if (this.state.goalOutput !== undefined) {
            progressBar = <ProgressBar percentage={this.state.matchPercentage}/>;
            outputHeaders = <div className="editor-wrapper">
                <h4 className="editor-header">Goal output</h4>
                <h4 className="editor-header">Your output</h4>
            </div>;
            outputSection = <div className="editor-wrapper">
                <div className="editor-pane">
                    <Editor
                        value={this.state.goalOutput}
                        markers={this.state.goalMarkers}
                        readOnly={true}
                        mode="text"/>
                </div>
                <div className="editor-pane">
                    <Editor
                        value={this.state.output}
                        markers={this.state.outputMarkers}
                        readOnly={true}
                        mode="text"/>
                </div>
            </div>;
        }
        return (
            <div className="codeSample">
                <div className="editor-wrapper">
                    <div className="editor-pane">
                        <Editor
                            value={this.state.source}
                            scrollTop={this.state.scrollTop}
                            onChange={this.handleChange}
                            onScroll={this.handleScroll}
                            onCursorChange={this.handleCursorChange}
                            mode="python"/>
                    </div>
                    <div className="editor-pane">
                        <Editor
                            value={displayValue}
                            scrollTop={this.state.scrollTop}
                            readOnly={true}
                            selectedLine={this.state.selectedLine}
                            onChange={this.handleChange}
                            onScroll={this.handleScroll}
                            mode="text"/>
                    </div>
                </div>
                {resetButton}
                {progressBar}
                {outputHeaders}
                {outputSection}
          </div>
    );
  }
}

class App extends Component {
    constructor(props) {
        super(props);
        let app = this;
        this.state = {
            source: `\
This is a demonstration of Live Coding in Python. Type some Python code in the
editor on the left side. The right side is a live coding display that shows
what happens inside your code when it runs. It shows variable values and print()
calls, as well as a new column each time it runs through a loop or a function.

    def search(n, a):
        low = 0
        high = len(a) - 1
        while low <= high:
            mid = low + high // 2
            v = a[mid]
            if n == v:
                return mid
            if n < v:
                high=mid - 1
            else:
                low=mid + 1
        return -1
    
    i = search(1, [1, 2, 4])
    print(i)

Change the code, and see the changes inside. Try to find the bug in the example
code. (Hint: try searching for different numbers.) Paste your own code to see
how it works.
`,
            pythonMessage: 'Loading Python...'
        };

        // noinspection JSUnresolvedVariable
        if (window.languagePluginLoader === undefined) {
            this.state.pythonMessage = 'Python is not loaded!';
        } else {
            // noinspection JSUnresolvedVariable
            window.languagePluginLoader.then(function() {
                // noinspection JSUnresolvedVariable,JSUnresolvedFunction
                window.pyodide.loadPackage('space-tracer').then(() => {
                    // noinspection JSUnresolvedVariable,JSUnresolvedFunction
                    window.pyodide.runPython(
                        'from space_tracer.main import web_main; web_main()');
                    app.setState({pythonMessage: null});
                });
            });
        }
        let search = window.location.search;
        let params = new URLSearchParams(search);
        let lessonName = params.get('lesson');
        if (lessonName) {
            this.state.source = lessons[lessonName];
            if (this.state.source === undefined) {
                this.state.source = 'Lesson not found: ' + lessonName;
            }
        }
    }

    render() {
        return (
            <div className="app">
                <PythonContext.Provider value={this.state.pythonMessage}>
                    <ReactMarkdown
                        source={this.state.source}
                        renderers={{code: CodeSample}}/>
                </PythonContext.Provider>
            </div>
        );
    }
}

export default App;
