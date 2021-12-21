const fs = require('fs');
const path = require('path');

function main() {
    const dest = path.resolve('src/tutorials.json'),
        src = path.resolve('tutorials'),
        collection = {};
    findTutorials(src, src, collection);
    fs.writeFileSync(dest, JSON.stringify(collection));
}

function findTutorials(startPath, srcPath, collection) {
    const reader = fs.opendirSync(srcPath);
    let entry;
    while ((entry = reader.readSync()) !== null) {
        const entryPath = path.join(srcPath, entry.name);
        if (entry.isDirectory()) {
            findTutorials(startPath, entryPath, collection);
        } else {
            const relativePath = path.relative(startPath, entryPath),
                parsed = path.parse(relativePath),
                extensionLength = parsed.base.length - parsed.name.length,
                tutorialName = relativePath.substring(
                    0,
                    relativePath.length-extensionLength);
            if (parsed.name.toUpperCase() !== 'README') {
                collection[tutorialName] = fs.readFileSync(
                    entryPath,
                    'utf8');
            }
        }
    }
}

main();
