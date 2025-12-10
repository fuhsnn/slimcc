FROM debian:trixie-slim

RUN apt-get update && apt-get upgrade -y
RUN apt-get install -y --no-install-recommends \
 make cmake pkg-config \
 autoconf autopoint automake gettext texinfo \
 git curl ca-certificates wget unzip lzip \
 bison flex \
 clang libclang-rt-19-dev \
 # Nuklear (sync with ccpp.yml)
 glslc liballegro5-dev liballegro-image5-dev liballegro-ttf5-dev libcairo2-dev libglfw3 libglfw3-dev \
 libglew-dev libsdl2-dev libvulkan-dev libwayland-dev libx11-dev libxcb1-dev libxcb-*-dev libxft-dev libxkbcommon-x11-dev wayland-protocols \
 # game builds
 libopenal-dev glslang-tools libcurl4-openssl-dev \
 # C3
 zlib1g zlib1g-dev libllvm19 llvm llvm-dev llvm-runtime liblld-dev liblld-19 libpolly-19-dev \
 # redis
 tcl-dev \
 # liballegro5
 xvfb xauth \
 # msgpack
 libgtest-dev \
 # i3
 libstartup-notification0-dev libyajl-dev libpango1.0-dev libev-dev \
 # wlroots/sway
 libjson-c-dev libevdev-dev xwayland libseat-dev libinput-dev hwdata libdisplay-info-dev libliftoff-dev liblcms2-dev libgdk-pixbuf-2.0-dev \
 # libxo/chimerautils
 libedit-dev libacl1-dev liblzma-dev \
 # wayst
 libutf8proc-dev \
 # xterm
 libxaw7-dev \
 # nanopb
 protobuf-compiler scons python3-grpcio \
 # sdl3
 libxtst-dev

COPY . /work/slimcc
WORKDIR /work/slimcc

RUN ln -s platform/linux-ci-debian13.c platform.c
RUN clang scripts/amalgamation.c -O2 -flto=auto -march=x86-64-v3 -mtune=znver3 -fsanitize=address -o slimcc

ENV CC=/work/slimcc/slimcc

RUN bash scripts/linux_thirdparty.bash ci_libtool
RUN bash scripts/linux_thirdparty.bash ci_muon

RUN useradd -m non-root -s /bin/bash && \
 su non-root -c "git config --global advice.detachedHead false" && \
 su non-root -c "git config --global init.defaultBranch init" && \
 su non-root -c "git config --global --add safe.directory '*'"

WORKDIR /home/non-root
