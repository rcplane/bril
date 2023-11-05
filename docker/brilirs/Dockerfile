from rust:latest
# aarch64 Apple M1 mac  colima container runtime
# Digest: sha256:73af736ea21c14181c257bf674c7095a8bad6343a1eadd327a8bf1ce1c5209b4
# Status: Downloaded newer image for rust:latest
# docker.io/library/rust:latest

RUN rustc --version
# rustc 1.73.0 (cc66ad468 2023-10-03)

RUN cd / && git clone https://github.com/sampsyo/bril.git

RUN apt update
RUN apt install -y python3-pip
RUN which pip
RUN pip -V
RUN python3 -V
# Python 3.10.12

USER ROOT

RUN apt install -y python3.10-venv
RUN mkdir -p /root/.venvs
RUN python3 -m venv /root/.venvs/base
#RUN echo "source /root/.venvs/base/bin/activate" >> /root/env
RUN echo "source /root/.venvs/base/bin/activate" >> /root/.bashrc

RUN bash -c "pip install turnt"


# usage
# docker run bril:latest bash
#
# cd /bril/benchmarks
# turnt -e brilirs core/*.bril
# time turnt -e brilirs */*.bril
