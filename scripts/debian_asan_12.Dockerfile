FROM debian:12-slim

RUN apt-get update && apt-get install -y --no-install-recommends \
 gcc-12 \
 file binutils libc-dev libgcc-12-dev \
 make cmake pkg-config \
 zip lzip xz-utils bzip2 zlib1g-dev \
 autoconf autopoint automake gettext texinfo \
 git curl ca-certificates wget locales \
 # glib
 libffi-dev \
 libpcre3-dev libmount-dev desktop-file-utils shared-mime-info \
 python3.11-minimal python3-distutils

COPY . /work/slimcc
WORKDIR /work/slimcc

RUN ln -s platform/linux-ci.c platform.c
RUN gcc-12 scripts/amalgamation.c -O2 -flto=auto -march=x86-64-v3 -mtune=znver3 -fsanitize=address -o slimcc
RUN apt-get -y autoremove gcc-12 && apt-get clean

RUN ! command -v cc
RUN ! command -v cpp
RUN ! command -v gcc
RUN ! command -v gcc-12

ENV CC=/work/slimcc/slimcc

RUN localedef -i en_US -c -f UTF-8 -A /usr/share/locale/locale.alias en_US.UTF-8
ENV LANG=en_US.UTF-8

RUN bash scripts/linux_thirdparty.bash ci_libtool

RUN useradd -m non-root -s /bin/bash && \
 su non-root -c "git config --global advice.detachedHead false" && \
 su non-root -c "git config --global init.defaultBranch init" && \
 su non-root -c "git config --global --add safe.directory '*'"

WORKDIR /home/non-root
