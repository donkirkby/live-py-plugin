""" Run the demo using the simple HTTP server.

Only useful for testing, but it sets the MIME types correctly.
"""

from http.server import SimpleHTTPRequestHandler
import os
import socketserver

PORT = 8000

SimpleHTTPRequestHandler.extensions_map.update({
    '.wasm': 'application/wasm',
})
SimpleHTTPRequestHandler.directory = os.path.abspath(
    os.path.join(__file__,
                 '..',
                 '..',
                 '..',
                 '..',
                 'docs',
                 'demo'))

httpd = socketserver.TCPServer(("", PORT), SimpleHTTPRequestHandler)

print("Serving at port", PORT, 'from', SimpleHTTPRequestHandler.directory)
httpd.serve_forever()