# Docker image to test the static analyzer for the TAS course
# Build with: docker build -t tas .
#
###############################################

FROM debian:stable

ENV TERM xterm-256color

RUN \
  apt-get update && \
  apt-get install -y opam libgmp-dev libmpfr-dev git m4  pkg-config && \
  adduser --disabled-password --gecos 'TAS Project' tas

COPY . /home/tas
RUN chown -R tas:tas /home/tas

USER tas
WORKDIR /home/tas

RUN \
    opam init --disable-sandboxing -y && \
    eval $(opam env) && \
    opam update -y && \
    opam switch create 4.14.0 && \
    opam install -y -j 8 dune menhir zarith mlgmpidl apron && \
    echo && \
    echo "All done!" && \
    uname -a && \
    echo opam `opam --version` && \
    ocamlc -v
