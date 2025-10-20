FROM debian:13-slim

RUN apt-get update && apt-get install -y --no-install-recommends \
 gcc-14 \
 file binutils libc-dev libgcc-14-dev \
 make cmake pkg-config \
 zip lzip xz-utils bzip2 zlib1g-dev \
 autoconf autopoint automake gettext texinfo \
 git curl ca-certificates wget locales \
 tcl-dev bison flex re2c libpcre2-dev \
 libcurl4-openssl-dev libssl-dev libexpat1-dev libicu-dev \
 libncurses-dev libreadline-dev libpsl-dev libffi-dev libxml2-dev libsqlite3-dev \
 # build_gcc
 libgmp-dev libmpfr-dev libmpc-dev \
 # test_toxcore
 libsodium-dev \
 # test_perl:cpan/Socket/t/getaddrinfo.t
 netbase \
 # for ocaml testsuite
 parallel \
 # memcached
 libevent-dev \
 # libxml
 python3-dev \
 # pacman
 libarchive-dev fakeroot fakechroot gawk \
 # libarchive
 libbz2-dev liblzma-dev \
 # jq
 libonig-dev \
 # pixman, freetype
 libpng-dev \
 # yash
 ed \
 # ruby
 ruby ruby-psych libyaml-dev \
 # samba
 libgnutls28-dev libparse-yapp-perl libacl1-dev libpam0g-dev python3-cryptography python3-pyasn1 python3-iso8601 \
 # libpsl
 libunistring-dev \
 # binutils
 dejagnu libzstd-dev \
 # lwan
 python3-requests libsqlite3-dev liblua5.1-0-dev libmariadb-dev \
 # rsync
 python3-commonmark \
 # got
 uuid-dev libbsd-dev libtls-dev \
 # openrc
 libcap-dev \
 # hare simple-cc
 qbe

COPY . /work/slimcc
WORKDIR /work/slimcc

RUN ln -s platform/linux-ci-debian13.c platform.c
RUN gcc-14 scripts/amalgamation.c -O2 -flto=auto -march=x86-64-v3 -mtune=znver3 -fsanitize=address -o slimcc
RUN apt-get -y autoremove gcc-14 && apt-get clean

RUN ! command -v cc
RUN ! command -v cpp
RUN ! command -v gcc
RUN ! command -v gcc-14

ENV CC=/work/slimcc/slimcc

RUN localedef -i en_US -c -f UTF-8 -A /usr/share/locale/locale.alias en_US.UTF-8
ENV LANG=en_US.UTF-8

RUN bash scripts/linux_thirdparty.bash ci_libtool
RUN bash scripts/linux_thirdparty.bash ci_muon

RUN useradd -m non-root -s /bin/bash && \
 su non-root -c "git config --global advice.detachedHead false" && \
 su non-root -c "git config --global init.defaultBranch init" && \
 su non-root -c "git config --global --add safe.directory '*'"

WORKDIR /home/non-root
