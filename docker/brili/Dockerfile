from ubuntu:22.04
#docker pull ubuntu:22.04
#22.04: Pulling from library/ubuntu
#bfbe77e41a78: Pull complete 
#Digest: sha256:2b7412e6465c3c7fc5bb21d3e6f1917c167358449fecac8176c6e496e5c1f05f
#Status: Downloaded newer image for ubuntu:22.04
#docker.io/library/ubuntu:22.04
# aarch64 Apple M1 mac  colima container runtime



USER root

RUN apt update
RUN apt install -y nodejs npm
RUN node -v
#v12.22.9

# alternate install for other node versions can use nvm
# https://github.com/nvm-sh/nvm#install--update-script
# curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.5/install.sh | bash

# todo condense apt installs and apt clean for size reduction

RUN apt install -y python3-pip
RUN which pip
RUN pip -V
# 22.0.2
RUN python3 -V
# 3.10.12

RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get install -y software-properties-common ca-certificates &&\
    update-ca-certificates

RUN apt-get update &&\
    apt-get upgrade -y && \
    apt-get install -y --no-install-recommends curl gcc g++ gnupg unixodbc-dev openssl git &&\
    rm -rf /var/lib/apt/lists/*

RUN apt-get update && apt-get upgrade
RUN apt-get install -y build-essential zlib1g-dev libncurses5-dev libgdbm-dev libssl-dev \
    libreadline-dev libffi-dev wget libbz2-dev libsqlite3-dev
RUN mkdir /python && cd /python
RUN wget https://www.python.org/ftp/python/3.12.0/Python-3.12.0.tgz
RUN tar -zxvf Python-3.12.0.tgz
RUN cd Python-3.12.0 && ls -lhR && ./configure --enable-optimizations && make install
RUN python3.12 -V
# Python 3.12.0
RUN which python3.12
# /usr/local/bin/python3.12

RUN apt install -y python3.10-venv
RUN mkdir -p /root/.venvs
RUN python3 -m venv /root/.venvs/base python=/usr/local/bin/python3.12
#RUN echo "source /root/.venvs/base/bin/activate" >> /root/env
RUN echo "source /root/.venvs/base/bin/activate" >> /root/.bashrc
RUN bash -c "python3 -V"
# Python 3.12.0

RUN bash -c "pip install turnt"

RUN cd / && git clone https://github.com/sampsyo/bril.git

RUN apt install -y unzip
RUN curl -fsSL https://deno.land/x/install/install.sh | sh
# RUN deno install brili.ts
# no official aarch64 support for Deno

RUN pwd
RUN wget -O deno_install.sh https://gist.githubusercontent.com/LukeChannings/09d53f5c364391042186518c8598b85e/raw/d626d162e77738316591a6835783a2edbfcc87/deno_install.sh
RUN chmod +x deno_install.sh
RUN ./deno_install.sh
RUN deno -V
# deno 1.38.0
RUN echo 'export DENO_INSTALL="/root/.deno"' >> /root/.bashrc
RUN echo 'export PATH="$DENO_INSTALL/bin:$PATH"' >> /root/.bashrc
RUN bash -c "deno -V"
# deno 1.38.0

RUN cd /bril && deno install brili.ts
# Successfully installed brili
# /root/.deno/bin/brili

RUN bash -c "pip install --upgrade pip"
RUN bash -c "pip install flit"
RUN cd /bril/bril-txt && FLIT_ROOT_INSTALL=1 flit install --symlink

RUN bash -c "cd /bril/benchmarks/ && time turnt -e brili */*.bril"
