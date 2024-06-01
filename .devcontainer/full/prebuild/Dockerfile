# Find new docker tags at https://hub.docker.com/r/pyodide/pyodide-env/tags
FROM pyodide/pyodide-env:20240127-chrome114-firefox122-py312@sha256:44664c3dd2f33183f7f9c8fb07e4eefe77eee004725af10a40b809216ebb31bf as pyodide
ENV PYODIDE_PACKAGES=core,matplotlib
ENV PYODIDE_VERSION=0.26.0
# Make pyodide checkout accessible to codespace user in final image.
ENV CODESPACE_USER=1000

RUN cd /opt && \
    git clone https://github.com/pyodide/pyodide.git && \
    cd pyodide && \
    git checkout tags/$PYODIDE_VERSION && \
    sed -i '/^\s*turtle\.py\s*\\$/d' Makefile.envs && \
    make && \
    chown -R $CODESPACE_USER:$CODESPACE_USER .


FROM mcr.microsoft.com/devcontainers/universal:2-linux@sha256:3fc1428ea2feaca384a6f16e0d55d780c621d895ef7a06cb1edc4e2a77078024
COPY --from=pyodide /opt/pyodide /opt/pyodide
RUN python -m pip install tox && \
    mkdir /workspaces && \
    ln -s /opt/pyodide /workspaces/pyodide && \
    ln -s /workspaces/live-py-plugin /opt/live-py-plugin

ENTRYPOINT bash
