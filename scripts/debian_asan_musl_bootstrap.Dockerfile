FROM debian:13-slim

RUN apt-get update && apt-get install -y --no-install-recommends \
 gcc-14 libc6-dev binutils libgcc-14-dev make wget ca-certificates

COPY . /work/slimcc
WORKDIR /work/

RUN wget https://github.com/fuhsnn/slimcc-musl-bootstrap/archive/refs/heads/master.tar.gz
RUN tar -xf master.tar.gz --strip-components=1
RUN sed -i 's|$STAGE1_BUILD_CMD|gcc-14 -O2 -flto=auto -march=x86-64-v3 -mtune=znver3 scripts/amalgamation.c -fsanitize=address -o slimcc \&\& apt-get -y autoremove gcc-14 libc6-dev|g' slimcc-musl-bootstrap.sh
RUN sh slimcc-musl-bootstrap.sh

RUN mkdir ./rfs/dev ./rfs/tmp
RUN make -C slimcc/ clean && cp -r slimcc/ ./rfs/
RUN chroot ./rfs /bin/sh -c 'cd /slimcc && make CC=cc test-stage2'
