FROM debian:12-slim

RUN apt-get update && apt-get install -y --no-install-recommends \
 gcc-12 \
 file binutils libc-dev libgcc-12-dev \
 make cmake pkg-config \
 autoconf autopoint automake gettext texinfo \
 git curl ca-certificates \
 tcl-dev bison flex re2c \
 libcurl4-openssl-dev libssl-dev libexpat1-dev zlib1g-dev libicu-dev \
 libncurses-dev libreadline-dev libpsl-dev libffi-dev libxml2-dev libsqlite3-dev \
 # build_gcc
 libgmp-dev libmpfr-dev libmpc-dev \
 # test_toxcore
 libsodium-dev \
 # test_perl:cpan/Socket/t/getaddrinfo.t
 netbase \
 # for ocaml testsuite
 parallel \
 # glib
 libpcre3-dev libmount-dev desktop-file-utils shared-mime-info \
 python3.11-minimal python3-distutils \
 # memcached
 libevent-dev \
 # libxml
 python3-dev

COPY . /work/slimcc
WORKDIR /work/slimcc

RUN ln -s platform/linux-ci.c platform.c
RUN gcc-12 -O2 -flto=auto -march=native *.c -fsanitize=address -o slimcc
RUN apt-get -y autoremove gcc-12 && apt-get clean

RUN ! command -v cc
RUN ! command -v cpp
RUN ! command -v gcc
RUN ! command -v gcc-12

ENV CC=/work/slimcc/slimcc

RUN bash scripts/linux_thirdparty.bash install_libtool

RUN useradd -m non-root -s /bin/bash && \
 su non-root -c "git config --global advice.detachedHead false" && \
 su non-root -c "git config --global init.defaultBranch init" && \
 mv scripts/linux_thirdparty.bash /home/non-root

WORKDIR /home/non-root
