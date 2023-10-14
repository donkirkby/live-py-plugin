# Find new docker tags at https://hub.docker.com/r/pyodide/pyodide-env/tags
FROM pyodide/pyodide-env:20230612-chrome114-firefox114-py311@sha256:8bcb06afdf9c95599b51ac41e90d8c12eea92adac4c0b8932ab66f50d6bf1267 as pyodide
ENV PYODIDE_PACKAGES=core,matplotlib
ENV PYODIDE_VERSION=0.24.1
# Make pyodide checkout accessible to codespace user in final image.
ENV CODESPACE_USER=1000

RUN cd /opt && \
    git clone https://github.com/pyodide/pyodide.git && \
    cd pyodide && \
    git checkout tags/$PYODIDE_VERSION && \
    sed -i '/^ *"turtle\.py",$/d' pyodide-build/pyodide_build/pyzip.py && \
    make && \
    chown -R $CODESPACE_USER:$CODESPACE_USER .


FROM mcr.microsoft.com/devcontainers/universal:2-linux@sha256:3fc1428ea2feaca384a6f16e0d55d780c621d895ef7a06cb1edc4e2a77078024
COPY --from=pyodide /opt/pyodide /opt/pyodide
RUN python -m pip install tox && \
    mkdir /workspaces && \
    ln -s /opt/pyodide /workspaces/pyodide && \
    ln -s /workspaces/live-py-plugin /opt/live-py-plugin

ENTRYPOINT bash
