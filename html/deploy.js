const fs = require('fs'),
    { execSync } = require('child_process'),
    path = require('path'),
    yaml = require('yaml');

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

function copyPyodide(srcPath, destPath, spaceTracerVersion) {
    const srcFiles = [
        'contourpy-1.3.1-cp313-cp313-pyodide_2025_0_wasm32.whl',
        'cycler-0.12.1-py3-none-any.whl',
        'fonttools-4.56.0-py3-none-any.whl',
        'kiwisolver-1.4.8-cp313-cp313-pyodide_2025_0_wasm32.whl',
        'matplotlib-3.8.4-cp313-cp313-pyodide_2025_0_wasm32.whl',
        'numpy-2.2.5-cp313-cp313-pyodide_2025_0_wasm32.whl',
        'packaging-24.2-py3-none-any.whl',
        'pillow-11.3.0-cp313-cp313-pyodide_2025_0_wasm32.whl',
        'pyodide.asm.js',
        'pyodide.asm.wasm',
        'pyodide.js',
        'pyodide-lock.json',
        'pyparsing-3.2.1-py3-none-any.whl',
        'python_dateutil-2.9.0.post0-py2.py3-none-any.whl',
        'python_stdlib.zip',
        'pytz-2025.2-py2.py3-none-any.whl',
        'six-1.17.0-py2.py3-none-any.whl',
        `space_tracer-${spaceTracerVersion}-py3-none-any.whl`];
    fs.mkdirSync(destPath);
    for (const fileName of srcFiles) {
        const fileSrcPath = path.join(srcPath, fileName),
            fileDestPath = path.join(destPath, fileName);
        fs.copyFileSync(fileSrcPath, fileDestPath);
    }
}

function isSpaceTracerDeployed() {
    const distPath = path.resolve('../../pyodide/dist'),
        d = fs.opendirSync(distPath);
    let entry;
    while ((entry = d.readSync()) !== null) {
        if (entry.name.startsWith('space_tracer-') &&
            entry.name.endsWith('.whl')) {
                return true;
        }
    }
    return false;
}

function rebuildPyodide(pyodideExists) {
    const metaSource = fs.readFileSync(
        'meta.yaml',
        {encoding: 'utf-8'}),
        metaEntries = yaml.parse(metaSource),
        metaVersion = metaEntries.package.version;
    const aboutSource = fs.readFileSync(
        '../plugin/PySrc/space_tracer/about.py',
        {encoding: 'utf-8'}),
        aboutMatch = /^ *__version__ *= *'(.*)' *$/m.exec(aboutSource),
        aboutVersion = aboutMatch && aboutMatch[1];
    if ( ! aboutMatch) {
        throw "No __version__ found in about.py.";
    }
    if (aboutVersion !== metaVersion) {
        throw `Found versions ${aboutVersion} in about.py and ${metaVersion} in meta.yaml`;
    }
    const deployedPython = fs.readFileSync(
                'deployed-python.txt',
                {encoding: 'utf-8'}),
        latestPython = execSync(
            'md5sum *.py',
            {cwd: '../plugin/PySrc/space_tracer', encoding: 'utf8'});
    if (pyodideExists && ! isSpaceTracerDeployed()) {
        console.log('Deploying space tracer in pyodide for the first time.');
    }
    else if (latestPython === deployedPython) {
        console.log('Space tracer in pyodide is up to date.');
        return aboutVersion;
    }
    if ( ! pyodideExists) {
        console.error(
            'Space tracer Python code has changed, but ../../pyodide was not ' +
            'found.');
        return aboutVersion;
    }
    console.log(`Rebuilding space-tracer ${aboutMatch[1]} in pyodide.`);
    execSync('tox devenv -epy310 .tox/py310', {cwd: '..', encoding: 'utf8'});
    execSync(
        '.tox/py310/bin/python -m pip install --upgrade setuptools wheel twine',
        {cwd: '..', encoding: 'utf8'});
    execSync('.tox/py310/bin/python setup.py sdist', {cwd: '..', encoding: 'utf8'});
    execSync('tox devenv -repy310 .tox/py310', {cwd: '..', encoding: 'utf8'});
    console.log('Requesting sudo to repackage pyodide.');
    execSync(
        'sudo rm -rf pyodide/packages/space-tracer',
        {cwd: '../..', encoding: 'utf8'});
    fs.mkdirSync('../../pyodide/packages/space-tracer');
    fs.cpSync('meta.yaml', '../../pyodide/packages/space-tracer/meta.yaml');
    execSync(
        `tar xzf ../../../../live-py-plugin/dist/space_tracer-${aboutVersion}.tar.gz`,
        {cwd: '../../pyodide/pyodide-recipes/packages/space-tracer', encoding: 'utf8'});
    execSync(
        'sudo ./run_docker --non-interactive ' +
        '"pip install ./pyodide-build && ' +
        'pyodide build-recipes matplotlib,space-tracer ' +
        '--recipe-dir pyodide-recipes/packages --install"',
        {cwd: '../../pyodide', encoding: 'utf8'});
    fs.writeFileSync('deployed-python.txt', latestPython);
    console.log('Rebuilt space tracer in pyodide.');
    return aboutVersion;
}

function main() {
    const dest = path.resolve('../docs/demo'),
        src = path.resolve('build'),
        pyodideDest = path.resolve('../docs/demo/pyodide'),
        pyodideSrc = path.resolve('../../pyodide/dist'),
        pyodideExists = fs.existsSync(pyodideSrc),
        d = fs.opendirSync(dest);
    let entry, spaceTracerVersion;
    while ((entry = d.readSync()) !== null) {
        let destFilePath = path.join(dest, entry.name);
        if (entry.isDirectory()) {
            if (entry.name === 'pyodide') {
                spaceTracerVersion = rebuildPyodide(pyodideExists);
                if ( ! pyodideExists) {
                    // No new copy to replace it, so don't delete it.
                    continue;
                }
            }
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
        copyPyodide(pyodideSrc, pyodideDest, spaceTracerVersion);
    }
    console.log('Files deployed to ../docs.');
}

main();
