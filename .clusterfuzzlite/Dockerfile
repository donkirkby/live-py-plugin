FROM gcr.io/oss-fuzz-base/base-builder-python
RUN apt-get update && \
    apt-get install -y make autoconf automake libtool python3.9 python3.9-dev && \
    ln -sf /usr/bin/python3.9 /usr/local/bin/python3 && \
    apt-get install -y python3-pip && \
    python3 -m pip install -v --no-cache-dir "atheris>=2.1.1" "pyinstaller==5.0.1" "coverage==6.3.2"
COPY . $SRC/live-py-plugin
WORKDIR live-py-plugin
RUN python3 -m pip install -e $SRC/live-py-plugin && \
    mv test/PySrc/tests/*_fuzzer.py .clusterfuzzlite/build.sh $SRC/

