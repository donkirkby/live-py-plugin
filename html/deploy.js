const fs = require('fs');
const path = require('path');

function wrapReact(source) {
    return "{% if page.is_react %}\n" + source + "\n{% endif %}\n";
}

function copyIndex(indexSrcPath, destFolderPath) {
    const indexMarkdown = `\
---
title: Live Python in the Browser
layout: react
is_react: True
hero_image: ../images/index_hero.jpg
---
`;
    let indexSource = fs.readFileSync(indexSrcPath, 'utf8');
    let destFilePath = path.join(destFolderPath, 'index.md');
    let includesPath = path.join(destFolderPath, '../_includes');

    fs.writeFileSync(destFilePath, indexMarkdown);

    let match = indexSource.match(/<script defer="defer".*" rel="stylesheet">/);
    destFilePath = path.join(includesPath, 'head-scripts.html');
    fs.writeFileSync(destFilePath, wrapReact(match[0]));

    match = indexSource.match(/<div id="root"><\/div>(.*)<\/body>/ms);
    destFilePath = path.join(includesPath, 'footer-scripts.html');
    fs.writeFileSync(destFilePath, wrapReact(match[1]));
}

function copyPiodide(srcPath, destPath) {
    const srcFiles = [
        'distutils.js',
        'distutils.data',
        'pyodide.js',
        'pyodide.js.map',
        'pyodide.asm.wasm',
        'pyodide.asm.js',
        'pyodide.asm.data',
        'packages.json',
        'space-tracer.data',
        'space-tracer.js'];
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
        piodideDest = path.resolve('../docs/demo/pyodide'),
        piodideSrc = path.resolve('../../pyodide/build'),
        piodideExists = fs.existsSync(piodideSrc),
        d = fs.opendirSync(dest);
    let entry;
    while ((entry = d.readSync()) !== null) {
        let destFilePath = path.join(dest, entry.name);
        if (entry === 'pyodide' && ! piodideExists) {
            // No new copy to replace it, so don't delete it.
        } else if (entry.isDirectory()) {
            fs.rmSync(destFilePath, {recursive: true});
        } else {
            fs.unlinkSync(destFilePath);
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

    if (piodideExists) {
        copyPiodide(piodideSrc, piodideDest);
    }
}

main();
