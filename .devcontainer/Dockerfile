FROM python:3.13

# Import GPG keys for RVM, as documented at https://rvm.io/rvm/install
RUN python -m pip install tox && \
    gpg --keyserver keyserver.ubuntu.com \
        --recv-keys 409B6B1796C275462A1703113804BB82D39DC0E3 \
                    7D2BAF1CF37B13E2069D6956105BD0E739499BDB && \
    (curl -sSL https://get.rvm.io | bash -s stable) && \
    bash -lc rvm install 3.2.3

ENTRYPOINT ["bash"]
