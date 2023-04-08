const fs = require('fs');
const path = require('path');

function wrapReact(source) {
    const relativeSource = source.replaceAll(
        /"\.\/([^"]+)"/g,
        `"{{ 'demo/$1' | relative_url }}"`);
    return "{% if page.is_react %}\n" + relativeSource + "\n{% endif %}\n";
}

function copyIndex(indexSrcPath, destFolderPath) {
    let indexSource = fs.readFileSync(indexSrcPath, 'utf8');
    let includesPath = path.join(destFolderPath, '../_includes');

    let match = indexSource.match(/<script defer="defer".*" rel="stylesheet">/);
    let modulesSource = `
        {% if page.modules %}
            <script>
                window.liveCodingExtraModules = "{{ page.modules }}";
            </script>
        {% endif %}
        `;
    let filesSource = `
        {% if page.files %}
            <script>
                window.liveCodingExtraFiles = "{{ page.files }}";
            </script>
        {% endif %}
        `;
    let rawSource = match[0];
    let destFilePath = path.join(includesPath, 'head-scripts.html');
    fs.writeFileSync(
        destFilePath,
        wrapReact(modulesSource + filesSource + rawSource));

    match = indexSource.match(/<div id="root"><\/div>(.*)<\/body>/ms);
    destFilePath = path.join(includesPath, 'footer-scripts.html');
    fs.writeFileSync(destFilePath, wrapReact(match[1]));
}

function copyPyodide(srcPath, destPath) {
    const srcFiles = [
        'cycler-0.11.0-py3-none-any.whl',
        'distutils-1.0.0.zip',
        'fonttools-4.38.0-py3-none-any.whl',
        'kiwisolver-1.4.4-cp310-cp310-emscripten_3_1_27_wasm32.whl',
        'matplotlib-3.5.2-cp310-cp310-emscripten_3_1_27_wasm32.whl',
        'matplotlib_pyodide-0.1.1-py3-none-any.whl',
        'numpy-1.23.5-cp310-cp310-emscripten_3_1_27_wasm32.whl',
        'packaging-21.3-py3-none-any.whl',
        'PIL-9.1.1-cp310-cp310-emscripten_3_1_27_wasm32.whl',
        'pyodide.js',
        'pyodide.js.map',
        'pyodide.asm.wasm',
        'pyodide.asm.js',
        'pyodide.asm.data',
        'pyodide_py.tar',
        'pyparsing-3.0.9-py3-none-any.whl',
        'python_dateutil-2.8.2-py2.py3-none-any.whl',
        'pytz-2022.7-py2.py3-none-any.whl',
        'package.json',
        'repodata.json',
        'six-1.16.0-py2.py3-none-any.whl',
        'space_tracer-4.10.0-py3-none-any.whl'];
    fs.mkdirSync(destPath);
    for (const fileName of srcFiles) {
        const fileSrcPath = path.join(srcPath, fileName),
            fileDestPath = path.join(destPath, fileName);
        fs.copyFileSync(fileSrcPath, fileDestPath);
    }
}

function main() {
    const dest = path.resolve('../docs/demo'),
        src = path.resolve('build'),
        pyodideDest = path.resolve('../docs/demo/pyodide'),
        pyodideSrc = path.resolve('../../pyodide/dist'),
        pyodideExists = fs.existsSync(pyodideSrc),
        d = fs.opendirSync(dest);
    let entry;
    while ((entry = d.readSync()) !== null) {
        let destFilePath = path.join(dest, entry.name);
        if (entry.name === 'pyodide' && ! pyodideExists) {
            // No new copy to replace it, so don't delete it.
        } else if (entry.isDirectory()) {
            if (entry.name === 'static' || entry.name === 'pyodide') {
                fs.rmSync(destFilePath, {recursive: true});
            }
        }
    }
    const entries = fs.readdirSync(src);
    entries.sort();  // Fails if we hit index.html before _includes.
    for (entry of entries) {
        let srcFilePath = path.join(src, entry),
            destFilePath = path.join(dest, entry);
        if (entry === 'favicon.ico') {
            // Skip it.
        } else if (entry !== 'index.html') {
            fs.renameSync(srcFilePath, destFilePath);
        } else {
            copyIndex(srcFilePath, dest);
        }
    }

    if (pyodideExists) {
        copyPyodide(pyodideSrc, pyodideDest);
    }
}

main();
