# Find new docker tags at https://hub.docker.com/r/pyodide/pyodide-env/tags
FROM pyodide/pyodide-env:20260117-chrome145-firefox145-py313@sha256:f9fd7ddc1ed568e86f9bfccdf10a3c1744ea75fa14daca3be9c1783c1f149ef3 AS pyodide
ENV PYODIDE_VERSION=0.29.1
ENV PYODIDE_RECIPES_VERSION=0.29-20260109
# Make pyodide checkout accessible to codespace user in final image.
ENV CODESPACE_USER=1000

RUN cd /opt && \
    git clone --recursive https://github.com/pyodide/pyodide && \
    cd pyodide && \
    git checkout tags/$PYODIDE_VERSION && \
    git submodule update && \
    sed -i '/^\s*turtle\.py\s*\\$/d' Makefile.envs && \
    git clone https://github.com/pyodide/pyodide-recipes && \
    cd pyodide-recipes && \
    git checkout tags/$PYODIDE_RECIPES_VERSION && \
    cd .. && \
    RUSTUP_HOME=/usr CARGO_HOME=/usr PYODIDE_PACKAGES=tag:core make && \
    pyodide build-recipes matplotlib --recipe-dir pyodide-recipes/packages --install && \
    chown -R $CODESPACE_USER:$CODESPACE_USER .


# Find versions and hashes at https://github.com/devcontainers/images/tree/main/src/universal/history
FROM mcr.microsoft.com/devcontainers/universal:4.1.1-linux@sha256:8e7d06fef78a7fcb5867cd6ed78abcc13743f5cc3712517d0c81a26a87d9de4e
COPY --from=pyodide /opt/pyodide /opt/pyodide
RUN python -m pip install --upgrade pip && \
    python -m pip install tox && \
    bash -lc "rvm install 3.2.3" && \
    mkdir /workspaces && \
    ln -s /opt/pyodide /workspaces/pyodide && \
    ln -s /workspaces/live-py-plugin /opt/live-py-plugin

ENTRYPOINT ["bash"]
