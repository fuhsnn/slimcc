FROM debian:trixie-slim

RUN apt-get update && apt-get upgrade -y
RUN apt-get install -y --no-install-recommends \
 make cmake pkg-config \
 autoconf autopoint automake gettext texinfo \
 git curl ca-certificates \
 clang libclang-rt-19-dev \
 # C3
 zlib1g zlib1g-dev libllvm19 llvm llvm-dev llvm-runtime liblld-dev liblld-19 libpolly-19-dev

COPY . /work/slimcc
WORKDIR /work/slimcc

RUN ln -s platform/linux-ci-debian13.c platform.c
RUN clang -O2 -flto=auto -march=native *.c -fsanitize=address -o slimcc

ENV CC=/work/slimcc/slimcc

#RUN bash scripts/linux_thirdparty.bash install_libtool

RUN useradd -m non-root -s /bin/bash && \
 su non-root -c "git config --global advice.detachedHead false" && \
 su non-root -c "git config --global init.defaultBranch init" && \
 mv scripts/linux_thirdparty.bash /home/non-root

WORKDIR /home/non-root
