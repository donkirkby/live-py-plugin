#!/bin/bash
# Copy space_tracer into the extension for self-contained packaging

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
EXT_DIR="$(dirname "$SCRIPT_DIR")"
REPO_ROOT="$(dirname "$EXT_DIR")"

rm -rf "$EXT_DIR/bundled/space_tracer"
mkdir -p "$EXT_DIR/bundled"
cp -r "$REPO_ROOT/plugin/PySrc/space_tracer" "$EXT_DIR/bundled/space_tracer"

echo "Bundled space_tracer into $EXT_DIR/bundled/space_tracer"
