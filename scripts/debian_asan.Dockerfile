FROM debian:12-slim

RUN apt-get update && apt-get install -y --no-install-recommends \
 gcc-12 \
 file binutils libc-dev libgcc-12-dev \
 make cmake pkg-config \
 autoconf autopoint automake gettext texinfo \
 git curl ca-certificates \
 python3 tcl-dev bison flex re2c \
 libcurl4-openssl-dev libssl-dev libexpat1-dev zlib1g-dev libicu-dev \
 libncurses-dev libreadline-dev libpsl-dev libffi-dev libxml2-dev libsqlite3-dev \
 # build_gcc
 libgmp-dev libmpfr-dev libmpc-dev \
 # test_toxcore
 libsodium-dev \
 # test_perl:cpan/Socket/t/getaddrinfo.t
 netbase

COPY . /work/slimcc
WORKDIR /work/slimcc

RUN gcc-12 -O2 -flto=auto -march=native *.c -fsanitize=address -o slimcc
RUN apt-get -y autoremove gcc-12 && apt-get clean

ENV CC=/work/slimcc/slimcc

RUN bash scripts/linux_thirdparty.bash install_libtool

RUN useradd -m non-root -s /bin/bash && \
 su non-root -c "git config --global advice.detachedHead false" && \
 su non-root -c "git config --global init.defaultBranch init" && \
 mv scripts/linux_thirdparty.bash /home/non-root

WORKDIR /home/non-root
