import React, {Component} from 'react';
import AceEditor from 'react-ace';
import ReactMarkdown from 'react-markdown';
import SampleAnalyst from './SampleAnalyst.js';
import './App.css';
import tutorials from './tutorials.json';

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
        this.content.current.editor.resize();
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
            highlightActiveLine={this.props.highlightActiveLine}
            editorProps={{
                $blockScrolling: Infinity
            }}
            setOptions={{
                showLineNumbers: true,
                tabSize: 4,
            }}/>;
    }
}

class FootnoteBuilder extends Component {
    render() {
        let coreProps = {href: this.props.href};
        if (this.props['data-sourcepos']) {
            coreProps['data-sourcepos'] = this.props['data-sourcepos'];
        }
        let match = /^#(footnote\d+)$/i.exec(coreProps.href);
        if (match !== null) {
            coreProps.name = match[1] + "ref";
        } else {
            match = /^#(footnote\d+)ref$/i.exec(coreProps.href);
            if (match !== null) {
                coreProps.name = match[1];
            }
        }
        return React.createElement('a', coreProps, this.props.children);
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
            matchPercentage: analyst.matchPercentage,
            isLive: analyst.isLive
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
                this.state.goalSourceCode,
                this.state.isLive
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

    countLines(text) {
        return text.split(/\r\n|\r|\n/).length;
    }

    render() {
        let displayValue = this.context;
        if (displayValue === null) {
            displayValue = this.state.display;
        }
        let displayEditor = null,
            progressBar = null,
            outputHeaders = null,
            outputSection = null,
            resetButton = null,
            sourceLineCount = 1 + this.countLines(this.state.source);
        if (this.state.isLive) {
            displayEditor = <div className="editor-pane">
                <Editor
                    value={displayValue}
                    scrollTop={this.state.scrollTop}
                    readOnly={true}
                    selectedLine={this.state.selectedLine}
                    onChange={this.handleChange}
                    onScroll={this.handleScroll}
                    highlightActiveLine={true}
                    mode="text"/>
            </div>;
        }
        if (this.state.source !== this.state.originalSource) {
            resetButton = <div className="reset-wrapper">
                <button className="reset-code" onClick={this.handleReset}>Reset</button>
            </div>;
        }
        if (this.state.goalOutput !== undefined) {
            let outputLineCount = this.countLines(this.state.output),
                goalLineCount = this.countLines(this.state.goalOutput),
                outputSize = Math.min(
                    50, 1 + Math.max(outputLineCount, goalLineCount));
            progressBar = <ProgressBar percentage={this.state.matchPercentage}/>;
            outputHeaders = <div className="editor-wrapper">
                <h4 className="editor-header">Goal output</h4>
                <h4 className="editor-header">Your output</h4>
            </div>;
            outputSection = <div className="editor-wrapper">
                <div className="editor-pane"
                     style={{height: outputSize*18 + "px"}}>
                    <Editor
                        value={this.state.goalOutput}
                        markers={this.state.goalMarkers}
                        readOnly={true}
                        highlightActiveLine={false}
                        mode="text"/>
                </div>
                <div className="editor-pane">
                    <Editor
                        value={this.state.output}
                        markers={this.state.outputMarkers}
                        readOnly={true}
                        highlightActiveLine={false}
                        mode="text"/>
                </div>
            </div>;
        }
        return (
            <div className="codeSample">
                <div className="editor-wrapper">
                    <div className="editor-pane"
                        style={{height: sourceLineCount*18 + "px"}}>
                        <Editor
                            value={this.state.source}
                            scrollTop={this.state.scrollTop}
                            onChange={this.handleChange}
                            onScroll={this.handleScroll}
                            onCursorChange={this.handleCursorChange}
                            highlightActiveLine={true}
                            mode="python"/>
                    </div>
                    {displayEditor}
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
            source: tutorials['index'],
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
        let tutorialName = params.get('tutorial');
        if (tutorialName) {
            this.state.source = tutorials[tutorialName];
            if (this.state.source === undefined) {
                this.state.source = 'Tutorial not found: ' + tutorialName;
            }
        }
    }

    render() {
        return (
            <div className="app">
                <PythonContext.Provider value={this.state.pythonMessage}>
                    <ReactMarkdown
                        source={this.state.source}
                        renderers={{
                            code: CodeSample,
                            link: FootnoteBuilder
                        }}/>
                </PythonContext.Provider>
            </div>
        );
    }
}

export default App;
