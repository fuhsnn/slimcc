# Compile musl and retarget platform.c to it

set -e -u

. `dirname $0`/utils.sh

ROOT="$PWD"

if [ ! -f ./include/.slimcc_incl_dir ]; then
 echo 'not in source directory'
 exit 1
fi

if [ ! -f ./slimcc-seed ]; then
 echo 'need slimcc-seed'
 exit 1
fi

if [ -f ./platform.c ]; then
 echo 'platform.c exists'
 exit 1
fi

if [ ! -d ./musl_linux_headers ]; then
 git_fetch https://github.com/sabotage-linux/kernel-headers 22bba0124dfe2728ef5b4555baa1ae0960492665 musl_linux_headers
 make ARCH=x86_64 prefix= DESTDIR="$PWD" install
 cd "$ROOT"
fi

if [ ! -d ./musl ]; then
 git_fetch https://github.com/bminor/musl c47ad25ea3b484e10326f933e927c0bc8cded3da musl
 rm -rf src/complex
 cd "$ROOT"
fi

if [ ! -d ./musl_install ]; then
 cd musl
 CC="$ROOT"/slimcc-seed CFLAGS=-fPIC AR=ar RANLIB=ranlib ./configure --target=x86_64-linux-musl --prefix="$ROOT"/musl_install --syslibdir="$ROOT"/musl_install/
 make clean && make install
 cd "$ROOT"
fi

sed 's|ROOT_DIR|'\""$PWD"\"'|g' ./platform/linux-musl-intree.c > platform.c

