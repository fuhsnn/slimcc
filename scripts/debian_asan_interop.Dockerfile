FROM debian:trixie-slim

RUN apt-get update && apt-get upgrade -y
RUN apt-get install -y --no-install-recommends \
 make cmake pkg-config \
 autoconf autopoint automake gettext texinfo \
 git curl ca-certificates \
 clang libclang-rt-19-dev \
 # Nuklear (sync with ccpp.yml)
 glslc liballegro5-dev liballegro-image5-dev liballegro-ttf5-dev libcairo2-dev libglfw3 libglfw3-dev \
 libglew-dev libsdl2-dev libvulkan-dev libwayland-dev libx11-dev libxcb1-dev libxcb-util0-dev libxcb-keysyms1-dev libxft-dev libxkbcommon-x11-dev wayland-protocols \
 # game builds
 libopenal-dev glslang-tools libcurl4-openssl-dev \
 # C3
 zlib1g zlib1g-dev libllvm19 llvm llvm-dev llvm-runtime liblld-dev liblld-19 libpolly-19-dev \
 # redis/valkey
 tcl-dev

COPY . /work/slimcc
WORKDIR /work/slimcc

RUN ln -s platform/linux-ci-debian13.c platform.c
RUN clang -O2 -flto=auto -march=native *.c -fsanitize=address -o slimcc

ENV CC=/work/slimcc/slimcc

RUN useradd -m non-root -s /bin/bash && \
 su non-root -c "git config --global advice.detachedHead false" && \
 su non-root -c "git config --global init.defaultBranch init" && \
 mv scripts/linux_thirdparty.bash /home/non-root

WORKDIR /home/non-root
