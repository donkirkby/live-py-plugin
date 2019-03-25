import React, { Component } from 'react';
import AceEditor from 'react-ace';
import './App.css';

import 'brace/mode/python';
import 'brace/mode/markdown';
import 'brace/theme/github';

class App extends Component {
    constructor(props) {
        super(props);
        this.state = {
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
                      <td><AceEditor
                          value={this.state.source}
                          onChange={this.handleChange}
                          placeholder="# Python Code"
                          mode="python"
                          theme="github"
                          name="source"
                          id="source"
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
                          }}/></td>
                      <td><AceEditor
                          value={this.state.display}
                          readOnly={true}
                          mode="markdown"
                          theme="github"
                          name="display"
                          id="display"
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
                          }}/></td>
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
