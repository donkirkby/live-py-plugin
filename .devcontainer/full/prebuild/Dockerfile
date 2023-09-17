FROM pyodide/pyodide-env:20221102-chrome107-firefox106 as pyodide
ENV PYODIDE_PACKAGES=core,matplotlib
ENV PYODIDE_VERSION=0.22.1
# Make pyodide checkout accessible to codespace user in final image.
ENV CODESPACE_USER=1000

RUN cd /opt && \
    git clone https://github.com/pyodide/pyodide.git && \
    cd pyodide && \
    git checkout tags/$PYODIDE_VERSION && \
    sed -i '/^turtle\.py$/d' cpython/remove_modules.txt && \
    make && \
    chown -R $CODESPACE_USER:$CODESPACE_USER .


FROM mcr.microsoft.com/devcontainers/universal:2-linux
COPY --from=pyodide /opt/pyodide /opt/pyodide
RUN python -m pip install tox && \
    mkdir /workspaces && \
    ln -s /opt/pyodide /workspaces/pyodide && \
    ln -s /workspaces/live-py-plugin /opt/live-py-plugin

ENTRYPOINT bash
