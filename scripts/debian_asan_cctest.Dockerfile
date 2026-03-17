FROM debian:12-slim AS install-deps

RUN apt-get update && apt-get install -y --no-install-recommends \
 gcc \
 file binutils libc-dev libgcc-12-dev \
 make cmake pkg-config \
 zip lzip xz-utils bzip2 zlib1g-dev \
 autoconf autopoint automake gettext texinfo \
 git curl ca-certificates wget \
 # build_gcc
# libgmp-dev libmpfr-dev libmpc-dev \
 python3 \
 && apt-get clean && rm -rf /var/cache/apt/*

FROM install-deps AS setup-toolchain

COPY . /work/slimcc
WORKDIR /work/slimcc

RUN ln -s platform/linux-ci.c platform.c \
 && gcc -O3 -flto=auto -fvisibility=hidden -march=x86-64-v3 -mtune=znver3 -fsanitize=address scripts/amalgamation.c -o slimcc

ENV CC=/work/slimcc/slimcc

RUN useradd -m non-root -s /bin/bash && \
 su non-root -c "git config --global advice.detachedHead false" && \
 su non-root -c "git config --global init.defaultBranch init" && \
 su non-root -c "git config --global --add safe.directory '*'"

WORKDIR /home/non-root
