import React, { Component } from 'react';
import AceEditor from 'react-ace';
import './App.css';

import 'brace/mode/python';
import 'brace/mode/markdown';
import 'brace/theme/github';

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
            fontSize={18}
            showPrintMargin={true}
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

class App extends Component {
    constructor(props) {
        super(props);
        this.state = {
            scrollTop: 0,
            selectedLine: undefined,
            display: 'Loading...',
            source: `\
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
`};

        this.handleChange = this.handleChange.bind(this);
        this.handleScroll = this.handleScroll.bind(this);
        this.handleCursorChange = this.handleCursorChange.bind(this);
        let app = this;

        if (window.languagePluginLoader === undefined) {
            this.state.display = 'Pyodide is not loaded.';
        } else {
            window.languagePluginLoader.then(function() {
                fetch('code_tracer.py').then(function (response) {
                    return response.text();
                }).then(function (code_tracer_source) {
                    window.pyodide.runPython(code_tracer_source);
                    app.handleChange();
                });
            });
        }
    }

    handleChange(newSource) {
        if (newSource === undefined) {
            newSource = this.state.source;
        }
        let display = window.analyze(newSource);
        this.setState({source: newSource, display: display});
    }

    handleScroll(scrollTop) {
        this.setState({scrollTop: scrollTop});
    }

    handleCursorChange(selection) {
        this.setState({selectedLine: selection.selectionLead.row});
    }

    render() {
        return (
            <div className="App">
              <p><a href="..">Home</a></p>
              <table>
                  <tbody>
                  <tr>
                      <th><label htmlFor="source">Type Some Python</label></th>
                      <th><label htmlFor="display">See Inside</label></th>
                  </tr>
                  <tr>
                      <td><Editor
                          value={this.state.source}
                          scrollTop={this.state.scrollTop}
                          onChange={this.handleChange}
                          onScroll={this.handleScroll}
                          onCursorChange={this.handleCursorChange}
                          mode="python"/></td>
                      <td><Editor
                          value={this.state.display}
                          scrollTop={this.state.scrollTop}
                          readOnly={true}
                          selectedLine={this.state.selectedLine}
                          onChange={this.handleChange}
                          onScroll={this.handleScroll}
                          mode="text"/></td>
                  </tr>
                  </tbody>
              </table>
              <p>Change the code, and see the changes inside. Try to find the bug in the
                  example code. (Hint: try searching for different numbers.) Paste your
                  own code to see how it works.</p>
          </div>
    );
  }
}

export default App;
