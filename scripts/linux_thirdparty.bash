set -eu
set -o pipefail

test_bash() {
 url_tar https://ftpmirror.gnu.org/gnu/bash/bash-5.3.tar.gz bash
 fix_and_configure
 make test
}

test_bearssl() {
 url_tar https://bearssl.org/bearssl-0.6.tar.gz bearssl
 make CC=$CC LDDLL=$CC LD=$CC
 build/testcrypto all
 build/testx509
}

test_bfs() {
 github_tar tavianator bfs 4.1
 ./configure
 make check
}

test_binutils() {
 url_lz https://ftpmirror.gnu.org/gnu/binutils/binutils-2.45.tar.lz binutils
 sed -i 's|^# define __attribute__(x)$||g' include/ansidecl.h

 (cd ld/testsuite && find . -name '*.c' -exec sed -i 's|^#pragma weak.*|__attribute__((weak))|g' {} +)
 sed -i 's|\[at_least_gcc_version 5 1\]|1|g' ld/testsuite/ld-elf/linux-x86.exp
 sed -i 's|__builtin_abort ();|{void abort();abort();}|g' ld/testsuite/ld-x86-64/plt-main3.c
 sed -i 's|__builtin_abort ();|{void abort();abort();}|g' ld/testsuite/ld-x86-64/plt-main4.c
 # tests depend on printf() being converted to puts
 sed -i 's|printf (\"PASS\\n\")|puts(\"PASS\")|g' ld/testsuite/ld-elf/pr25617-1a.c
 # tests depend on asm name NOT being double quoted (which we always do)
 sed -i 's|(long) &size_of_bar|({long v;__asm(\"lea bar@SIZE(%%rip),%0\":\"=r\"(v)); v;})|g' ld/testsuite/ld-size/size-7a.c
 sed -i 's|(long) &size_of_bar|({long v;__asm(\"lea bar@SIZE(%%rip),%0\":\"=r\"(v)); v;})|g' ld/testsuite/ld-size/size-8a.c
 # tests depend on -fasynchronous-unwind-tables
 sed -i 's|3f|..|g' ld/testsuite/ld-x86-64/plt-main-ibt.dd
 # tests depend on GCC's instruction selection behavior
 sed -i 's|{error_output \"pr22001-1b.err\"}||g' ld/testsuite/ld-x86-64/x86-64.exp
 # tests depend on debug info
 rm ld/testsuite/ld-elf/compress.exp
 rm binutils/testsuite/binutils-all/addr2line.exp
 replace_line "test_objdump_S" "" binutils/testsuite/binutils-all/objdump.exp
 replace_line "readelf_wi_test" "" binutils/testsuite/binutils-all/readelf.exp
 sed -i 's|beginwarn.c:7:|beginwarn.o:|g' ld/testsuite/ld-elf/shared.exp

 # this one passes '-Bsymbolic' to cc, not sure if intended
 sed -i 's|symbolic \"-Bsymbolic\"|symbolic \"-Wl,-Bsymbolic\"|g' ld/testsuite/ld-shared/shared.exp

 fix_and_configure --disable-gprofng
 make && make check
}

test_bison() {
 url_lz https://ftpmirror.gnu.org/gnu/bison/bison-3.8.2.tar.lz bison
 ./configure
 make check
}

test_blake2() {
 git_fetch https://github.com/BLAKE2/BLAKE2 ed1974ea83433eba7b2d95c5dcd9ac33cb847913 blake2
 cd ref
 make CC=$CC check
}

test_busybox() {
 git_fetch https://github.com/sailfishos-mirror/busybox 142ac2d796dddea50d8926d1747523182c12d097 busybox
 sed -i 's|-Wp,-MD,|-MD -MF |g' scripts/Makefile.lib
 sed -i 's|-Wp,-MD,|-MD -MF |g' scripts/Makefile.host
 sed -i 's|LDLIBS += rt|LDLIBS += rt resolv|g' Makefile.flags
 sed -i 's|&& defined(__GNUC__)||g' libbb/hash_sha1_hwaccel_x86-64.S
 sed -i 's|&& defined(__GNUC__)||g' libbb/hash_sha256_hwaccel_x86-64.S
 sed -i 's|\tgcc |$CC |g' testsuite/testing.sh
 replace_line "# if defined(__GNUC__) && (defined(__i386__) || defined(__x86_64__))" "#if 1" libbb/hash_md5_sha.c
 make CC=$CC HOSTCC=$CC defconfig
 make CC=$CC HOSTCC=$CC
 make CC=$CC HOSTCC=$CC SKIP_KNOWN_BUGS=1 test
}

test_bzip2() {
 url_tar https://sourceware.org/pub/bzip2/bzip2-1.0.8.tar.gz bzip2
 make CC=$CC test
}

test_c23doku() {
 git_fetch https://github.com/fuhsnn/c23doku c0bd5daa91f4ffa0e06f418850111691577cd365 c23doku
 CC=$CC sh test.sh
}

test_c3() {
 github_tar c3lang c3c v0.7.4
 replace_line "#elif defined(__GNUC__)" "#elif 1" src/utils/whereami.c
 mkdir build && cd build
 cmake ../ -DCMAKE_C_COMPILER=$CC
 make VERBOSE=1
 cd ../test
 ../build/c3c compile-test unit
 ../build/c3c compile-run -O1 src/test_suite_runner.c3 -- ../build/c3c test_suite/
}

test_calc() {
 github_tar lcn2 calc v2.16.0.0
 make CC=$CC LCC=$CC MAN=true check
}

test_cello() {
 git_fetch https://github.com/orangeduck/Cello 61ee5c3d9bca98fd68af575e9704f5f02533ae26 cello
 make check
}

test_cjson() {
 git_fetch https://github.com/DaveGamble/cJSON 8f2beb57ddad1f94bed899790b00f46df893ccac cjson
 replace_line "#if (defined(__GNUC__) || defined(__SUNPRO_CC) || defined (__SUNPRO_C)) && defined(CJSON_API_VISIBILITY)" "#if 1" cJSON.h
 sed -i 's/if defined(__GNUC__) || defined(__ghs__)/if 1/g' tests/unity/src/unity_internals.h
 mkdir cmakebuild && cd cmakebuild
 cmake ../ -DCMAKE_C_COMPILER=$CC
 make check
}

test_coreutils() {
 url_xz https://ftpmirror.gnu.org/gnu/coreutils/coreutils-9.7.tar.xz coreutils
 sed -i 's|--std=gnu99||g' init.cfg
 # fail in docker
  sed -i 's|tests/tail/inotify-dir-recreate.sh||g' tests/local.mk
  sed -i 's|tests/rm/deep-2.sh||g' tests/local.mk
 ./configure
 make check
}

test_cpio() {
 url_bz https://ftpmirror.gnu.org/gnu/cpio/cpio-2.15.tar.bz2 cpio
 ./configure
 make check
}

test_cproc() {
 git_fetch https://github.com/michaelforney/cproc 049d4d0df33dba8525d542c3627019e282c1ebed cproc
 ./configure --host=x86_64-linux-gnu
 make CFLAGS=-std=c99 check
}

test_curl() {
 url_xz https://github.com/curl/curl/releases/download/curl-8_15_0/curl-8.15.0.tar.xz curl
 fix_and_configure --with-openssl
 make && make test
}

test_doom() {
 git_fetch https://github.com/Daivuk/PureDOOM 48376ddd6bbdb70085dab91feb1c6ceef80fa9b7 puredoom
 mkdir -p examples/Tests/build && cd "$_"
 replace_line "project(pd_tests)" "project(pd_tests C)" ../CMakeLists.txt
 cmake ../ && make
 cd ../../../ && examples/Tests/build/pd_tests
}

test_elk() {
 git_fetch https://github.com/cesanta/elk a9bb85619c5cddf49dfa8bdf529770fc9943a7fd elk
 CFLAGS='-O -ffunction-sections -fdata-sections' make -C test test elk
}

test_espruino() {
 # build process needs .git/
 git_fetch https://github.com/espruino/Espruino 08502f84a2706a72c6ad0a525d93ec3c74f09565 espruino
 rm -r tests/manual/ tests/*_FAIL.js tests/test_tensorflow.js
 sed -i 's|-lstdc++|-lc|g' make/family/LINUX.make
 BOARD=LINUX PYTHON=python3 USE_TENSORFLOW=0 make CC=$CC LD=$CC V=1 -j1
 ./bin/espruino --test-all
}

test_file() {
 github_tar file file FILE5_46
 libtoolize
 autoreconf -fi
 fix_and_configure
 make check
}

test_flex() {
 url_tar https://github.com/westes/flex/files/981163/flex-2.6.4.tar.gz flex
 fix_configure
 CC_FOR_BUILD=$CC ./configure
 make check
}

test_fossil() {
 github_tar drhsqlite fossil-mirror version-2.26
 CC_FOR_BUILD=$CC ./configure --json
 make test
}

test_gawk() {
 url_lz https://ftpmirror.gnu.org/gnu/gawk/gawk-5.3.2.tar.lz gawk
 fix_and_configure --disable-pma # pma segfault in docker
 make check
}

test_git() {
 github_tar git git v2.50.1
 make CC="$CC" test -j2
}

test_glib() {
 github_clone fuhsnn glib main
 libtoolize
 sh autogen.sh
 fix_and_configure
 replace_line "#if  __GNUC__ > 2 || (__GNUC__ == 2 && __GNUC_MINOR__ >= 7)" "#if 1" glib/gconstructor.h
 replace_line "#ifdef __GNUC__" "#if 1" glib/gmacros.h
 replace_line "#elif defined(__GNUC__) && (__GNUC__ >= 4)" "#elif 1" gio/tests/modules/symbol-visibility.h
 make check
}

test_gmake() {
 url_lz https://ftpmirror.gnu.org/gnu/make/make-4.4.1.tar.lz gmake
 fix_and_configure
 make check
}

test_go() {
 git_fetch https://github.com/golang/go ceb95ea6aef52c1fb472d3539c6ef68670778b5b go
 sed -i 's|\"-ggdb\",||g' src/cmd/dist/build.c
 sed -i 's|\"-pipe\",||g' src/cmd/dist/build.c
 sed -i 's|vadd(&gccargs, \"-fmessage-length=0\");||g' src/cmd/dist/build.c
 rm src/runtime/pprof/pprof_test.go # flaky (cpu load sensitive)
 cd src/
 GO14TESTS=1 ./all.bash
}

test_gsed() {
 url_xz https://ftpmirror.gnu.org/gnu/sed/sed-4.9.tar.xz gsed
 ./configure
 make check
}

test_gtar() {
 url_xz https://ftpmirror.gnu.org/gnu/tar/tar-1.35.tar.xz gtar
 ./configure
 make check
}

test_gzip() {
 url_xz https://ftpmirror.gnu.org/gnu/gzip/gzip-1.14.tar.xz gzip
 fix_and_configure
 make check
}

test_imagemagick() {
 github_tar ImageMagick ImageMagick 7.1.2-1
 fix_and_configure
 make check V=1
}

test_janet() {
 git_fetch https://github.com/janet-lang/janet 3d3e880f52e4b40540b7722b6fc0f58aa5bd7443 janet
 # Use C11 concurrency features
 sed -i "s|/\* #define JANET_THREAD_LOCAL _Thread_local \*/|#define JANET_THREAD_LOCAL _Thread_local|g" src/conf/janetconf.h
 sed -i "s|/\* #define JANET_USE_STDATOMIC \*/|#define JANET_USE_STDATOMIC|g" src/conf/janetconf.h
 # Enable computed goto
 replace_line "#if defined(__GNUC__) && !defined(__EMSCRIPTEN__)" "#if 1" src/core/vm.c
 make test
}

test_jemalloc() {
 git_fetch https://github.com/jemalloc/jemalloc 1972241cd204c60fb5b66f23c48a117879636161 jemalloc
 sed -i 's|ATOMIC_VAR_INIT||g' include/jemalloc/internal/atomic_c11.h
 autoconf
 ./configure --disable-cxx
 make check
}

test_lame() {
 url_tar https://sourceforge.net/projects/lame/files/lame/3.100/lame-3.100.tar.gz/download lame
 fix_and_configure
 make test
}

test_liballegro5() {
 git_fetch https://github.com/liballeg/allegro5 c870f466638b345639b46628e4d421443aacff71 liballegro5
 mkdir cmakebuild && cd cmakebuild
 cmake ../ -DCMAKE_C_COMPILER=$CC -DCMAKE_C_FLAGS=-fPIC
 make VERBOSE=1
 make run_standalone_tests
 . ../tests/grab_bitmap_suites.sh
 find ../tests -name '*.ini' | grep -v 'compressed' | xargs xvfb-run tests/test_driver --save_on_failure --xvfb | tee /tmp/test_out || true
 grep -q 'failed tests: 0' /tmp/test_out
}

test_libarchive() {
 url_tar https://github.com/libarchive/libarchive/releases/download/v3.8.1/libarchive-3.8.1.tar.gz libarchive
 replace_line "#elif defined(__GNUC__)" "#elif 1" libarchive/archive_blake2.h
 replace_line "#if defined(__GNUC__)" "#if 1" libarchive/archive_write_set_format_cpio_binary.c
 fix_and_configure
 make check
}

test_libevent() {
 git_fetch https://github.com/libevent/libevent 112421c8fa4840acd73502f2ab6a674fc025de37 libevent
 libtoolize
 sh autogen.sh
 fix_and_configure
 make check -j2
}

test_libexpat() {
 url_tar https://github.com/libexpat/libexpat/releases/download/R_2_7_1/expat-2.7.1.tar.gz libexpat
 fix_and_configure
 make check
}

test_libgc() {
 git_fetch https://github.com/bdwgc/bdwgc 9d83c00b5c146863397c31dbac0892c762e7530f libgc
 sed -i 's|__atomic_compare_exchange_n(p, &ov, nv, 0,|atomic_compare_exchange_strong_explicit(p, \&ov, nv,|g'  include/private/gc_atomic_ops.h
 use_stdatomic 'typedef size_t AO_t' include/private/gc_atomic_ops.h
 sed -i 's/(defined(__GNUC__)/1 || (defined(__GNUC__)/g' cord/cordxtra.c
 use_stdatomic '#include <stdarg.h>' cord/cordxtra.c
 sed -i 's|defined(__GNUC__)|1|g' cord/cordprnt.c
 libtoolize
 sh autogen.sh
 fix_and_configure --disable-dynamic-loading --enable-threads=posix --with-libatomic-ops=none
 make check
}

test_libgmp() {
 url_xz https://ftpmirror.gnu.org/gnu/gmp/gmp-6.3.0.tar.xz gmp
 fix_and_configure
 make && make check
}

test_libjansson() {
 github_tar akheron jansson v2.14.1
 replace_line "#if defined(__GNUC__) || defined(__clang__)" "#if 1" src/jansson.h
 convert_atomic_x_fetch src/jansson.h
 use_stdatomic "#include <stdio.h>" src/hashtable_seed.c
 cmake . -DCMAKE_C_COMPILER=$CC -DJANSSON_BUILD_DOCS=OFF -DCMAKE_C_FLAGS=-lm -DHAVE_ATOMIC_BUILTINS=1
 make && make test
}

test_libjpeg() {
 url_tar https://www.ijg.org/files/jpegsrc.v9f.tar.gz libjpeg
 fix_and_configure
 make check
}

test_libjsonc() {
 git_fetch https://github.com/json-c/json-c 7cee5237dc6c0831e3f9dc490394eaea44636861 json-c
 sed -i 's|json_object_new_double(NAN)|json_object_new_double(nan(\"\"))|g' json_tokener.c
 mkdir cmakebuild && cd cmakebuild
 cmake ../ -DCMAKE_C_COMPILER=$CC -DCMAKE_C_FLAGS=-fPIC -DHAVE_VASPRINTF=no
 make && make test
}

test_liblz4() {
 git_fetch https://github.com/lz4/lz4 2bc386d57cd9c36780366acead0054fd49dcd36b lz4
 make test
}

test_libmpc() {
 url_tar https://ftpmirror.gnu.org/gnu/mpc/mpc-1.3.1.tar.gz mpc
 fix_and_configure
 make check
}

test_libmpfr() {
 url_xz https://ftpmirror.gnu.org/gnu/mpfr/mpfr-4.2.2.tar.xz mpfr
 fix_and_configure
 make check
}

test_libpcre2() {
 github_tar PCRE2Project pcre2 pcre2-10.45
 fix_and_configure
 make check
}

test_libpng() {
 github_tar pnggroup libpng v1.6.50
 fix_and_configure
 make test
}

test_libpsl() {
 url_tar https://github.com/rockdaboot/libpsl/releases/download/0.21.5/libpsl-0.21.5.tar.gz libpsl
 fix_and_configure
 make check
}

test_libressl() {
 url_tar https://github.com/libressl/portable/releases/download/v4.1.0/libressl-4.1.0.tar.gz libressl
 replace_line "#if defined(__GNUC__)" "#if 1" crypto/bn/arch/amd64/bn_arch.h
 fix_and_configure
 make check
}

test_libsamplerate() {
 git_fetch https://github.com/libsndfile/libsamplerate 15c392d47e71b9395a759544b3818a1235fe1a1d libsamplerate
 replace_line "#   define HAVE_SSE2_INTRINSICS" "" src/common.h
 libtoolize
 sh autogen.sh
 fix_and_configure
 make test
}

test_libsodium() {
 url_tar https://github.com/jedisct1/libsodium/releases/download/1.0.20-RELEASE/libsodium-1.0.20.tar.gz libsodium
 fix_and_configure
 replace_line "#if !defined(__clang__) && !defined(__GNUC__)" "#if 0" src/libsodium/include/sodium/private/common.h
 replace_line "#if !defined(__clang__) && !defined(__GNUC__)" "#if 0" src/libsodium/include/sodium/export.h
 replace_line "#elif defined(HAVE_C11_MEMORY_FENCES)" "#elif defined(HAVE_C11_MEMORY_FENCES)\n#include <stdatomic.h>" src/libsodium/include/sodium/private/common.h
 make check
}

test_libuev() {
 github_tar troglobit libuev v2.4.1
 libtoolize
 autoreconf -fi
 fix_and_configure
 make check
}

test_libuv() {
 url_tar https://github.com/libuv/libuv/archive/refs/tags/v1.51.0.tar.gz libuv
 replace_line "#elif __GNUC__ >= 4" "#elif 1" include/uv.h
 # https://github.com/libuv/libuv/issues/2635#issuecomment-575109464
 sed -i 's|TEST_DECLARE   (udp_multicast_join)||g' test/test-list.h
 sed -i 's|TEST_DECLARE   (udp_multicast_join6)||g' test/test-list.h
 sed -i 's|TEST_ENTRY  (udp_multicast_join)||g' test/test-list.h
 sed -i 's|TEST_ENTRY  (udp_multicast_join6)||g' test/test-list.h
 libtoolize
 sh autogen.sh
 fix_and_configure
 make check
}

test_libxml() {
 github_tar GNOME libxml2 v2.14.5
 libtoolize
 sh autogen.sh
 fix_configure
 CFLAGS=-D_FILE_OFFSET_BITS=64 ./configure
 make check
}

test_libyaml() {
 git_fetch https://github.com/yaml/libyaml 840b65c40675e2d06bf40405ad3f12dec7f35923 libyaml
 mkdir cmakebuild && cd cmakebuild
 cmake ../ -DCMAKE_C_COMPILER=$CC
 make && make test
}

test_lua() {
 url_tar https://lua.org/ftp/lua-5.4.8.tar.gz lua
 cd src && make CC="$CC" linux-readline
 url_tar https://www.lua.org/tests/lua-5.4.8-tests.tar.gz luatests
 cd libs && make CC="$CC" && cd ../
 ../lua -e"_port=true" all.lua # assertion at files.lua:84 fail in CI
}

test_mawk() {
 github_tar ThomasDickey mawk-snapshots t20250131
 ./configure
 make check
}

test_mbedtls() {
 url_bz https://github.com/Mbed-TLS/mbedtls/releases/download/mbedtls-4.0.0-beta/mbedtls-4.0.0-beta.tar.bz2 mbedtls
 replace_line "    (defined(__GNUC__) || defined(__clang__)) && defined(MBEDTLS_ARCH_IS_X64)" "1" tf-psa-crypto/drivers/builtin/src/aesni.h
 cmake . -DCMAKE_C_COMPILER=$CC
 make && ctest -j2
}

test_memcached() {
 github_tar memcached memcached 1.6.39
 sed -i "s/defined(__has_builtin)/0/g" crc32c.c
 sh autogen.sh
 CFLAGS=-D_GNU_SOURCE ./configure
 make && make test
}

test_metalang99() {
 github_tar hirrolot datatype99 v1.6.5
 sh scripts/test-all.sh
 github_tar hirrolot interface99 v1.0.2
 sh scripts/test-all.sh
 github_tar hirrolot metalang99 v1.13.5
 sh scripts/test-all.sh
}

test_micropython() {
 github_clone micropython micropython v1.25.0
 use_stdbit "#include <stdbool.h>" py/misc.h
 sed -i 's|inline MP_ALWAYSINLINE const|static inline const|g' py/misc.h
 replace_line "#if defined(__clang__) || (defined(__GNUC__) && __GNUC__ >= 8)" "#if 1" py/nlrx64.c
 sed -i 's|defined(LFS2_NO_INTRINSICS)|1|g' lib/littlefs/lfs2_util.h
 make -C ports/unix/ CC=$CC VARIANT=standard V=1 test_full
 cd tests
 MICROPY_CPYTHON3=python3 MICROPY_MICROPYTHON=../ports/unix/build-standard/micropython ./run-multitests.py multi_net/*.py
}

test_mimalloc() {
 github_tar microsoft mimalloc v3.1.5
 replace_line "project(libmimalloc C CXX)" "project(libmimalloc C)" CMakeLists.txt
 replace_line "set(CMAKE_CXX_STANDARD 17)" "" CMakeLists.txt
 replace_line "#include <immintrin.h>" "" include/mimalloc/bits.h
 replace_line "#if defined(__GNUC__) || defined(__clang__)" "#if 1" src/prim/prim.c
 mkdir build && cd build
 cmake ../ -DCMAKE_C_COMPILER=$CC -DCMAKE_C_FLAGS=-fPIC
 make && make test
}

test_mruby() {
 github_tar mruby mruby 3.4.0
 sed -i 's|conf.gem :core => \"mruby-cmath\"||g' mrbgems/math.gembox
 rake test
}

test_msgpack() {
 github_tar msgpack msgpack-c c-6.1.0
 convert_atomic_x_fetch cmake/sysdep.h.in
 mkdir cmakebuild && cd cmakebuild
 cmake ../ -DCMAKE_C_COMPILER=$CC -DMSGPACK_32BIT=OFF -DBUILD_SHARED_LIBS=ON \
  -DMSGPACK_CHAR_SIGN=signed -DMSGPACK_BUILD_EXAMPLES=ON -DMSGPACK_BUILD_TESTS=ON
 make
 make test
}

test_muon() {
 git_fetch https://github.com/muon-build/muon e6038ff33267ebf035d4aa839d7815fb34c427b0 muon
 sh ./bootstrap.sh build
 build/muon-bootstrap setup build
 sed -i 's/posix.default_linker = linker_posix/posix.default_linker = linker_ld/g' src/compilers.c
 sed -i "s|\['common/13|#|g" subprojects/meson-tests/meson.build # we don't do pch
 sed -i "s|\['common/251|#|g" subprojects/meson-tests/meson.build # https://github.com/muon-build/muon/issues/149
 sed -i "s|\['frameworks/7 gnome|#|g" subprojects/meson-tests/meson.build
 build/muon-bootstrap -C build samu
 build/muon-bootstrap -C build test
}

test_nginx() {
 github_tar nginx nginx release-1.29.1
 auto/configure
 make
 cd ../
 git_fetch https://github.com/nginx/nginx-tests 06a36245e134eac985cdfc5fac982cb149f61412 nginx-tests
 prove .
}

test_nob() {
 git_fetch https://github.com/fuhsnn/nob.h aa89f4588ef4e16d4df354671fe6eb700a978a0b nob
 cd run_tests
 CC="$CC -fdefer-ts" bash ./run.sh
}

test_noplate() {
 git_fetch https://github.com/fuhsnn/noplate 77e7ab9710749c2ab03389db670633b327e5471a noplate
 make test examples
}

test_ocaml() {
 github_tar ocaml ocaml 5.3.0
 fix_and_configure --enable-ocamltest --disable-ocamldoc --disable-debugger --disable-native-compiler
 make -j2  && make -C testsuite parallel -j2
}

test_oniguruma() {
 git_fetch https://github.com/kkos/oniguruma f95747b462de672b6f8dbdeb478245ddf061ca53 oniguruma
 libtoolize
 autoreconf -fi
 fix_and_configure
 make check
}

test_jq() {
 url_tar https://github.com/jqlang/jq/releases/download/jq-1.8.1/jq-1.8.1.tar.gz jq
 fix_and_configure
 make check
}

test_openssh() {
 github_tar openssh openssh-portable V_9_8_P1
 ./configure
 make
 make file-tests
 # make t-exec ## "regress/agent-subprocess.sh" fail in CI
 make interop-tests
 make extra-tests
 make unit
}

test_openssl() {
 github_tar openssl openssl openssl-3.5.2
 replace_line "#if !defined(__DJGPP__)" "#if 0" test/rsa_complex.c
 ./Configure
 make -j2 && make test HARNESS_JOBS=2
}

test_pacman() {
 url_tar https://sources.archlinux.org/other/pacman/pacman-5.2.2.tar.gz pacman
 fix_and_configure
 make check
}

test_pdpmake() {
 git_fetch https://github.com/rmyorston/pdpmake 7adf57a0171bf188ae14576223715215a06d5768 pdpmake
 make test
}

test_perl() {
 github_tar perl perl5 v5.42.0
 # https://github.com/Perl/perl5/blob/80f266d3fc15255d56d2bebebceba52614f04943/.github/workflows/testsuite.yml#L810
 export NO_NETWORK_TESTING=1
 ./Configure -des -Dcc="$CC" -Accflags=-fPIC -Alibs="-lpthread -ldl -lm -lcrypt -lutil -lc" \
   -Alibpth="/usr/local/lib /lib /usr/lib /lib64 /usr/lib64 /lib/x86_64-linux-gnu /usr/lib/x86_64-linux-gnu"
 make -j3 test_prep && HARNESS_OPTIONS=j3 make test_harness
}

test_pixman() {
 url_tar https://cairographics.org/releases/pixman-0.42.2.tar.gz pixman
 # enable constructor for coverage
 replace_line "#if defined(__GNUC__) && (__GNUC__ > 2 || (__GNUC__ == 2 && __GNUC_MINOR__ >= 7))" "#if 1" configure
 fix_and_configure
 make check
}

test_php() {
 github_tar php php-src php-8.5.0beta1
 replace_line "#elif (defined(__i386__) || defined(__x86_64__)) && defined(__GNUC__)" "#elif 1" Zend/zend_multiply.h
 replace_line "#elif defined(__GNUC__) && defined(__x86_64__)" "#elif 1" Zend/zend_multiply.h
 sed -i 's/defined(__SUNPRO_CC)$/defined(__SUNPRO_CC) || 1/g' ext/pcre/pcre2lib/sljit/sljitNativeX86_common.c
 sed -i 's|#if __has_feature(c_atomic) && defined(__clang__)|#if 1\n#include <stdatomic.h>|g' Zend/zend_atomic.h
 sed -i 's|__c11_atomic_init(|atomic_store(|g' Zend/zend_atomic.h
 sed -i 's|__c11_atomic_|atomic_|g' Zend/zend_atomic.h
 sed -i 's|, __ATOMIC_SEQ_CST||g' Zend/zend_atomic.h

 # don't work in CI https://github.com/php/php-src/blob/17187c4646f3293e1de8df3f26d56978d45186d6/.github/actions/test-linux/action.yml#L40
 export SKIP_IO_CAPTURE_TESTS=1

 ./buildconf --force
 fix_configure
 coverage=(--enable-pcntl --enable-zend-test --with-bz2 --with-curl --with-ffi --with-gettext --with-gmp --with-openssl --with-readline --with-sodium --with-zlib)
 CFLAGS=-fdisable-visibility ./configure "${coverage[@]}"
 make test NO_INTERACTION=1
}

test_postgres() {
 github_tar postgres postgres REL_18_BETA3
 replace_line "#if defined(__GNUC__) || defined(__INTEL_COMPILER)" "#if 1" src/include/storage/s_lock.h
 replace_line "#if (defined(__x86_64__) || defined(_M_AMD64))" "#if 0" src/include/port/simd.h
 replace_line "#if defined(__GNUC__) || defined(__INTEL_COMPILER)" "#if 1" src/include/port/atomics.h
 replace_line "#if defined(__GNUC__) || defined(__INTEL_COMPILER)" "#if 1" src/include/port/atomics/arch-x86.h
 ./configure && make && make check
}

test_python() {
 github_tar python cpython v3.13.7
 replace_line "#if defined(__GNUC__) || defined(__clang__)" "#if 1" Include/pyport.h
 replace_line "#if defined(__linux__) && (defined(__GNUC__) || defined(__clang__))" "#if 1" Python/pylifecycle.c
 replace_line "#elif defined(__GNUC__) || defined(__clang__)" "#elif 1" Objects/mimalloc/init.c
 skip_tests=(
  test_asyncio test_socket # Fail in CI
  test_os # https://github.com/python/cpython/issues/126112
 )
 ./configure
 make -j3 && ./python -m test -j3 --exclude "${skip_tests[@]}"
}

test_qbe_hare() {
 git_fetch git://c9x.me/qbe.git 120f316162879b6165deba77815cd4193fb2fb59 qbe
 make CC="$CC" check
 url_tar https://git.sr.ht/~sircmpwn/harec/archive/0.25.2.tar.gz harec
 mv configs/linux.mk config.mk
 make CC="$CC" QBE=../qbe check
}

test_quickjs() {
 git_fetch https://github.com/bellard/quickjs 1fdc768fdc8571300755cdd3e4654ce99c0255ce quickjs
 use_stdbit "#include <stdlib.h>" cutils.h
 make CC=$CC test
}

test_redis() {
 github_tar redis redis 8.2.0
 replace_line "#    if defined(__GNUC__) && !(defined(__clang__) && defined(__cplusplus))" "#if 1" src/redismodule.h
 sed -i 's|asm volatile|__asm volatile|g' deps/hdr_histogram/hdr_atomic.h
 convert_atomic_x_fetch deps/hdr_histogram/hdr_atomic.h
 use_stdbit "#include <stdint.h>" deps/hdr_histogram/hdr_histogram.c
 use_stdbit "#include <stdint.h>" src/util.h
 use_stdbit "#include <stdint.h>" src/dict.c
 use_stdbit "#include <stdint.h>" src/hyperloglog.c
 use_stdbit "#include <stdarg.h>" src/server.c
 use_stdbit '#include "server.h"' src/bitops.c
 rm tests/integration/logging.tcl
 make V=1 CC=$CC CXX=clang++ OPTIMIZATION=-O MALLOC=libc test
}

test_valkey() {
 github_tar valkey-io valkey 8.1.2
 replace_line "#if defined(__GNUC__) && !(defined(__clang__) && defined(__cplusplus))" "#if 1" src/valkeymodule.h
 sed -i 's|asm volatile|__asm volatile|g' deps/hdr_histogram/hdr_atomic.h
 convert_atomic_x_fetch deps/hdr_histogram/hdr_atomic.h
 use_stdbit "#include <stdint.h>" deps/hdr_histogram/hdr_histogram.c
 use_stdbit "#include <stdint.h>" src/dict.c
 sed -i 's|builtin_ctzll|__builtin_ctzll|g' src/hyperloglog.c
 use_stdbit "#include <stdint.h>" src/hyperloglog.c
 use_stdbit "#include <stdarg.h>" src/server.c
 sed -i 's|v = __builtin_bswap64(v)|__asm(\"bswap %0\":\"+r\"(v))|g' src/hashtable.c
 use_stdbit "#include <stdint.h>" src/hashtable.c
 make V=1 CC=$CC OPTIMIZATION=-O MALLOC=libc test
}

test_ruby() {
 git_fetch https://github.com/ruby/ruby d025bc230cd1f31d0b03c9f6490d8db066fc01b1 ruby
 sh autogen.sh
 cflags=-fPIC cxxflags=-fPIC ./configure
 make check -j4
}

test_samba() {
 github_tar samba-team samba samba-4.22.3
 sed -i 's|conf.fatal|print|g' buildtools/wafsamba/generic_cc.py
 sed -i 's|from waflib.Tools import |from waflib.Tools import gcc, |g' buildtools/wafsamba/generic_cc.py
 sed -i 's|conf.generic_cc_common_flags|conf.gcc_common_flags|g' buildtools/wafsamba/generic_cc.py
 use_stdatomic '#include <stdarg.h>' third_party/socket_wrapper/socket_wrapper.c
 sed -i 's|elif x.startswith(('\''-m'\'', '\''-f'\''|elif x != '\''-fstack-protector-strong'\'' and x.startswith(('\''-m'\'', '\''-f'\''|g' third_party/waf/waflib/Tools/c_config.py
 replace_line "#if defined(__clang__) || defined(__GNUC__) || defined(__SUNPRO_C)" "#if 1" third_party/heimdal/include/heim_threads.h
 ./configure --without-json --without-ad-dc --enable-selftest --without-ldap --without-ldb-lmdb --without-ads --with-shared-modules='!vfs_snapper,!vfs_nfs4acl_xattr'
 LD_LIBRARY_PATH=$PWD/bin/default/lib/util:$PWD/bin/default/libcli/util:$PWD/bin/default/librpc:$PWD/bin/default/nsswitch/libwbclient:$PWD/bin/default/source3:$PWD/bin/default/source3/libsmb make quicktest -j2
}

test_scrapscript() {
 git_fetch https://github.com/tekknolagi/scrapscript 986e0fbbcb497bea22875006d4c60dd3809a0cff scrapscript
 curl -LsSf https://astral.sh/uv/install.sh | sh
 ~/.local/bin/uv python install 3.10
 ~/.local/bin/uv python pin 3.010
 ~/.local/bin/uv run python compiler_tests.py
}

test_sokol() {
 git_fetch https://github.com/floooh/sokol 4012bd599827c9721502a90eaa661249b156d09e sokol
 sed -i 's|floooh/dcimgui|floooh/dcimgui --branch v1.92.0|g' tests/ext/CMakeLists.txt

 mkdir cmakebuild && cd cmakebuild
 cmake ../tests/ -DCMAKE_C_COMPILER=$CC -DSOKOL_BACKEND=SOKOL_GLCORE
 make VERBOSE=1 && ./sokol-test
}

test_sqlite() {
 github_tar sqlite sqlite version-3.50.4
 CC_FOR_BUILD="$CC" CFLAGS=-D_GNU_SOURCE ./configure
 make test
}

test_tcl() {
 github_tar tcltk tcl core-9-0-2
 ./unix/configure
 rm ./tests/socket.test # fails under su
 make test | tee __testlog
 grep -P -q '^all.tcl.*Failed\t0$' __testlog
}

test_tinycc() {
 git_fetch https://github.com/Tiny-C-Compiler/tinycc-mirror-repository 19fdef46f960dd72fb5883514df25501db5b2d4e tinycc
 ./configure && make && cd tests/tests2/ && make
}

test_toxcore() {
 github_clone TokTok c-toxcore v0.2.21
 libtoolize
 autoreconf -fi
 fix_and_configure
 make check
}

test_toybox() {
 github_tar landley toybox 0.8.12
 replace_line "#define QUIET" "#define QUIET = 0" lib/portability.h
 replace_line "  default n" "  default y" toys/pending/awk.c
 replace_line "  default n" "  default y" toys/pending/expr.c
 replace_line "  default n" "  default y" toys/pending/diff.c
 replace_line "  default n" "  default y" toys/pending/tr.c
 make CC="$CC" HOSTCC="$CC" defconfig
 make CC="$CC" HOSTCC="$CC"
 make CC="$CC" HOSTCC="$CC" tests
}

test_vim() {
 github_tar vim vim v9.1.1484
 ./configure
 make && make testtiny
}

test_wasm3() {
 git_fetch https://github.com/wasm3/wasm3 79d412ea5fcf92f0efe658d52827a0e0a96ff442 wasm3
 sed -i 's|#  ifdef __linux__|#if 1\n#include <stdint.h>\n|g' source/wasm3_defs.h
 use_stdbit2 "#include <limits.h>" source/m3_exec.h
 mkdir build
 $CC -O3 -g0 -s -Isource -Dd_m3HasWASI source/*.c platforms/app/main.c -lm -o build/wasm3
 cd test
 python3 run-wasi-test.py
 python3 run-spec-test.py
}

test_wget() {
 url_lz https://ftpmirror.gnu.org/gnu/wget/wget2-2.2.0.tar.lz wget
 fix_and_configure
 make check
}

test_wuffs() {
 git_fetch https://github.com/google/wuffs 67e9078eabcbda867ccc9fa912d81cba4fb0aa89 wuffs
 sed -i 's|Building (C)|Build CC:$CC|g' ./build-example.sh
 ./build-example.sh convert-to-nia
 ./build-example.sh gifplayer
 ./build-example.sh mzcat
 ./build-example.sh stb-imagedumper
 ./build-example.sh zcat
 CXX=clang++ ./build-example.sh crc32 #required for tests below

 # taken from build-all.sh
 script/print-mzcat-checksums.sh | diff --unified test/mzcat-checksums-of-data.txt /dev/stdin
 script/print-nia-checksums.sh | diff --unified test/nia-checksums-of-data.txt /dev/stdin
}

test_xxhash() {
 git_fetch https://github.com/Cyan4973/xxHash 38d555879fddae7300ece2c0820b3332c1d5748f xxhash
 make CC=$CC check
}

test_xz() {
 url_tar https://github.com/tukaani-project/xz/releases/download/v5.8.1/xz-5.8.1.tar.gz xz
 mkdir cmakebuild && cd cmakebuild
 cmake ../ -DCMAKE_C_COMPILER=$CC
 make && make test
}

test_yash() {
 github_tar magicant yash 2.59
 ./configure
 sed -i 's| docs$||g' Makefile
 sed -i 's| sigquit[1-8]-p.tst||g' tests/Makefile # extremely slow in CI
 make test
}

test_zlib() {
 github_tar madler zlib v1.3.1
 CFLAGS=-fPIC ./configure
 make test
}

test_zsh() {
 github_tar zsh-users zsh zsh-5.9.0.2-test
 libtoolize
 autoreconf -fi
 ./configure
 sed -i 's/stat.mdd link=no/stat.mdd link=static/g' config.modules # Required to pass D07multibyte.ztst
 rm Test/A08time.ztst # Fail in CI
 make && make check
}

test_zstd() {
 github_tar facebook zstd v1.5.7
 replace_line "#if defined(__ELF__) && defined(__GNUC__)" "#if 1" lib/decompress/huf_decompress_amd64.S
 make check
}

build_dash() {
 git_fetch https://git.kernel.org/pub/scm/utils/dash/dash.git b4ef25d7466c2f85247c6271a47c6ccc064b4625 dash
 sh autogen.sh
 ./configure
 make
}

build_erlang() {
 github_tar erlang otp OTP-27.3.4
 replace_line "#  if defined(__GNUC__)" "#if 1" erts/include/internal/ethread.h
 replace_line "#if defined(__GNUC__)" "#if 1" erts/include/internal/ethread_inline.h
 sed -i 's|-funroll-loops||g' lib/megaco/src/flex/Makefile.in
 CFLAGS='-O -fPIC' ./configure --enable-bootstrap-only
 OTP_TINY_BUILD=true make
}

build_freetype() {
 url_tar https://gitlab.freedesktop.org/freetype/freetype/-/archive/VER-2-13-3/freetype-VER-2-13-3.tar.gz freetype
 mkdir cmakebuild && cd cmakebuild
 cmake ../ -DCMAKE_C_COMPILER=$CC -DCMAKE_PREFIX_PATH=$(dirname $(find /usr/ 2>/dev/null | grep libbz2.so$))
 make
}

build_gcc() {
 url_bz https://ftpmirror.gnu.org/gnu/gcc/gcc-4.7.4/gcc-4.7.4.tar.bz2 gcc47
 fix_configure
 sed -i 's/^\s*struct ucontext/ucontext_t/g' ./libgcc/config/i386/linux-unwind.h
 mkdir buildonly && cd "$_"
 export MAKEINFO=missing
 ../configure --enable-languages=c,c++ --disable-multilib --disable-bootstrap
 make
}

build_glfw() {
 git_fetch https://github.com/glfw/glfw 768e81a0eb3ae411d108168fdff7cd3335f2a34a glfw
 cmake ./ -DCMAKE_C_COMPILER=$CC -DCMAKE_PREFIX_PATH=/usr/lib/x86_64-linux-gnu -DGLFW_BUILD_WAYLAND=ON -DGLFW_BUILD_X11=ON
 make
}

build_libev() {
 url_tar https://dist.schmorp.de/libev/libev-4.33.tar.gz libev
 fix_and_configure
 make
}

build_luajit() {
 git_fetch https://github.com/LuaJIT/LuaJIT f9140a622a0c44a99efb391cc1c2358bc8098ab7 luajit
 sed -i 's|-O2 -fomit-frame-pointer|-O2 -DLUAJIT_NO_UNWIND|g' src/Makefile
 replace_line "#if defined(__GNUC__) || defined(__clang__) || defined(__psp2__)" "#if 1" src/lj_def.h
 use_stdbit "#include <stdlib.h>" src/lj_def.h
 make CC=$CC
}

build_lynx() {
 github_tar ThomasDickey lynx-snapshots v2-9-2p
 ./configure
 make
}

build_nano() {
 url_tar https://www.nano-editor.org/dist/v8/nano-8.5.tar.gz nano
 ./configure && make
}

build_ncurses() {
 github_tar ThomasDickey ncurses-snapshots v6_5_20250809
 ./configure
 make V=1
}

build_nuklear() {
 github_tar Immediate-Mode-UI Nuklear 4.12.7
 rm -r demo/sfml_opengl*
 find demo/sdl_*/Makefile -exec sed -i 's|std=c89 |std=c89 -DSDL_INLINE=__inline |g' {} +
 find demo/*/sdl/Makefile -exec sed -i 's|std=c89 |std=c89 -DSDL_INLINE=__inline |g' {} +
 sed -i 's|CFLAGS+=|CFLAGS+=-DSDL_DISABLE_IMMINTRIN_H |g' demo/rawfb/sdl/Makefile
 use_stdbit "#include <X11/Xlib.h>" demo/rawfb/x11/nuklear_xlib.h
 sed -i 's|-std=c99|-DSTBI_NO_SIMD -std=c99|g' demo/glfw_opengl2/Makefile
 sed -i 's|-fsanitize=address||g' demo/sdl_vulkan/Makefile
 make CC=$CC demos
}

build_q2rtx() {
 github_clone NVIDIA Q2RTX v1.8.0
 replace_line "#if (defined __GNUC__)" "#if 1" inc/common/intreadwrite.h
 replace_line "#define inline __inline" "" inc/shared/config.h
 sed -i 's|-msse2 -mfpmath=sse||g' CMakeLists.txt
 mkdir build && cd build
 cmake ../ -DUSE_SYSTEM_CURL=on -DUSE_SYSTEM_OPENAL=on -DUSE_SYSTEM_SDL2=on -DUSE_SYSTEM_ZLIB=on -DCONFIG_BUILD_GLSLANG=no \
  -DCMAKE_C_COMPILER=$CC -DCMAKE_C_FLAGS='-fPIC -DSTBI_NO_SIMD -DSTBIR_NO_SIMD -DSDL_DISABLE_IMMINTRIN_H -fms-anon-struct'
 make VERBOSE=1
}

build_quake3e() {
 git_fetch https://github.com/ec-/Quake3e 814f2b5c8b6ff9a6f0976b880963a86f0b2afdb6 quake3e
 sed -i 's|-pipe||g' Makefile
 sed -i 's|Snd_Memset|Snd_Memset_unused|g' code/unix/linux_snd.c
 make CC=$CC V=1 CFLAGS=-DSDL_DISABLE_IMMINTRIN_H
}

build_raylib_raygui() {
 git_fetch https://github.com/raysan5/raylib 4bc8d3761c48f4dcf56f126640da8f3567dc516b raylib
 use_stdbit "#include <string.h>" src/external/sinfl.h
 sed -i 's|#define DR_MP3_ONLY_SIMD||g' src/external/dr_mp3.h

 make CC=$CC CUSTOM_CFLAGS='-DSTBI_NO_SIMD -DSTBIR_NO_SIMD' -C src/ PLATFORM=PLATFORM_DESKTOP RAYLIB_LIBTYPE=STATIC -B
 make CC=$CC CUSTOM_CFLAGS='-DSTBI_NO_SIMD -DSTBIR_NO_SIMD' -C examples/ PLATFORM=PLATFORM_DESKTOP -B
 make CC=$CC CUSTOM_CFLAGS='-DSTBI_NO_SIMD -DSTBIR_NO_SIMD' -C src/ PLATFORM=PLATFORM_DESKTOP RAYLIB_LIBTYPE=SHARED -B

 cd ../
 git_fetch https://github.com/raysan5/raygui 99b37e4d4fdc19e7bf73844d6b9f177cbb27ce24 raygui
 make CC=$CC -C examples RAYLIB_PREFIX=../../raylib/ -B
}

build_sdl3() {
 github_tar libsdl-org SDL release-3.2.20
 replace_line "#elif defined(__GNUC__) && (defined(__i386__) || defined(__x86_64__))" "#elif 1" src/atomic/SDL_spinlock.c
 mkdir cmakebuild && cd cmakebuild
 cmake ../ -DCMAKE_C_FLAGS='-fPIC -DSTBI_NO_SIMD' -DCMAKE_PREFIX_PATH=/usr/lib/x86_64-linux-gnu
 make VERBOSE=1
}

build_stb() {
 git_fetch https://github.com/nothings/stb f58f558c120e9b32c217290b80bad1a0729fbb2c stb
 sed -i 's|-DSTB_DIVIDE_TEST|-DSTB_DIVIDE_TEST -DSTBI_NO_SIMD -DSTBIR_NO_SIMD|g' tests/Makefile
 sed -i 's|$(CC) $(INCLUDES) $(CPPFLAGS) -std=c++0x test_cpp_compilation.cpp -lm -lstdc++||g' tests/Makefile
 make -C tests
}

build_tin() {
 github_tar ThomasDickey tin-beta-snapshots v2_6_5-20250707
 ./configure
 make build
}

build_yquake2() {
 github_tar yquake2 yquake2 QUAKE2_8_51
 sed -i 's|-pipe -fomit-frame-pointer||g' Makefile
 sed -i 's|__VERSION__|\"\"|g' src/backends/unix/signalhandler.c
 make CC=$CC VERBOSE=1 CFLAGS='-DSTBI_NO_SIMD -DSDL_DISABLE_IMMINTRIN_H'
}

build_zig() {
 github_clone fuhsnn zig wasm2c-stack
 sed -i 's/stack-size=0x1000000/stack-size=0x4000000/g' bootstrap.c
 cp lib/zig.h stage1/
 "$CC" bootstrap.c -o _bootstrap
 ./_bootstrap
 ./zig2 test --show-builtin
}

# utilities

fix_configure() {
 find . -name 'configure' -exec sed -i 's|^\s*lt_prog_compiler_wl=$|lt_prog_compiler_wl=-Wl,|g' {} +
 find . -name 'configure' -exec sed -i 's|^\s*lt_prog_compiler_pic=$|lt_prog_compiler_pic=-fPIC|g' {} +
 find . -name 'configure' -exec sed -i 's|^\s*lt_prog_compiler_static=$|lt_prog_compiler_static=-static|g' {} +
}

fix_and_configure() {
 fix_configure
 ./configure $@
}

replace_line() {
 sed -i s/^"$1"$/"$2"/g "$3"
}

wget_timeout_noretry() {
 wget -c -T30 -t2 $@
}

wget_loop() {
 local URL="$1"

 while ! wget_timeout_noretry $URL -O "$2"; do
  URL=`echo $URL | sed -e 's|ftpmirror.gnu|ftp.gnu|g'`
 done
}

get_tar() {
  mkdir "$2"
  local F="$2".tar"$1"

  if ! [ -f $F ]; then
   wget_loop "$3" $F
  fi
  tar -xf $F -C "$2" --strip-components=1
  cd "$2"
}

github_clone() {
  git clone --depth 1 --recurse-submodules --shallow-submodules --branch "$3" https://github.com/"$1"/"$2"
  cd "$2"
}

git_fetch() {
 mkdir -p "$3" && cd "$3"
 git init
 git remote add origin "$1"
 git fetch --depth 1 origin "$2"
 git checkout FETCH_HEAD
}

github_tar() {
 get_tar .gz "$2" https://github.com/"$1"/"$2"/archive/refs/tags/"$3".tar.gz
}

url_tar() {
 get_tar .gz "$2" "$1"
}

url_bz() {
 get_tar .bz "$2" "$1"
}

url_lz() {
 get_tar .lz "$2" "$1"
}

url_xz() {
 get_tar .xz "$2" "$1"
}

install_libtool() {
 url_xz https://ftpmirror.gnu.org/gnu/libtool/libtool-2.5.4.tar.xz __libtool
 fix_and_configure
 make  install
 cd ../ && rm -rf __libtool
}

use_stdatomic() {
 sed -i 's|^'"$1"'|#include <stdatomic.h>\n'"$1"'|g' "$2"

 sed -i 's|__ATOMIC_RELAXED|memory_order_relaxed|g' "$2"
 sed -i 's|__ATOMIC_CONSUME|memory_order_consume|g' "$2"
 sed -i 's|__ATOMIC_ACQUIRE|memory_order_acquire|g' "$2"
 sed -i 's|__ATOMIC_RELEASE|memory_order_release|g' "$2"
 sed -i 's|__ATOMIC_ACQ_REL|memory_order_acq_rel|g' "$2"
 sed -i 's|__ATOMIC_SEQ_CST|memory_order_seq_cst|g' "$2"

 sed -i 's|__atomic_load_n|atomic_load_explicit|g' "$2"
 sed -i 's|__atomic_store_n|atomic_store_explicit|g' "$2"

 sed -i 's|__atomic_clear|atomic_flag_clear_explicit|g' "$2"
 sed -i 's|__atomic_test_and_set|atomic_flag_test_and_set_explicit|g' "$2"
 sed -i 's|__atomic_signal_fence|atomic_signal_fence|g' "$2"
 sed -i 's|__atomic_thread_fence|atomic_thread_fence|g' "$2"

 sed -i 's|__atomic_fetch_add|atomic_fetch_add_explicit|g' "$2"
 sed -i 's|__atomic_fetch_sub|atomic_fetch_sub_explicit|g' "$2"
 sed -i 's|__atomic_fetch_or|atomic_fetch_or_explicit|g' "$2"
 sed -i 's|__atomic_fetch_xor|atomic_fetch_xor_explicit|g' "$2"
 sed -i 's|__atomic_fetch_and|atomic_fetch_and_explicit|g' "$2"

 sed -i 's|__sync_synchronize()|atomic_thread_fence(memory_order_seq_cst)|g' "$2"

 convert_atomic_x_fetch "$2"
}

convert_atomic_x_fetch() {
 sed -i 's|__atomic_add_fetch|__builtin_atomic_arith_add|g' "$1"
 sed -i 's|__atomic_sub_fetch|__builtin_atomic_arith_sub|g' "$1"
 sed -i 's|__atomic_or_fetch|__builtin_atomic_arith_or|g' "$1"
 sed -i 's|__atomic_xor_fetch|__builtin_atomic_arith_xor|g' "$1"
 sed -i 's|__atomic_and_fetch|__builtin_atomic_arith_and|g' "$1"

 sed -i 's|__sync_add_and_fetch|__builtin_atomic_arith_add|g' "$1"
 sed -i 's|__sync_sub_and_fetch|__builtin_atomic_arith_sub|g' "$1"
 sed -i 's|__sync_or_and_fetch|__builtin_atomic_arith_or|g' "$1"
 sed -i 's|__sync_and_and_fetch|__builtin_atomic_arith_xor|g' "$1"
 sed -i 's|__sync_xor_and_fetch|__builtin_atomic_arith_and|g' "$1"
}

use_stdbit() {
 sed -i 's|^'"$1"'|#include <stdbit.h>\n'"$1"'|g' "$2"

 sed -i 's|__builtin_ctzll(|(int)stdc_trailing_zeros_ull(|g' "$2"
 sed -i 's|__builtin_ctzl(|(int)stdc_trailing_zeros_ul(|g' "$2"
 sed -i 's|__builtin_ctz(|(int)stdc_trailing_zeros_ui(|g' "$2"
 sed -i 's|__builtin_clzll(|(int)stdc_leading_zeros_ull(|g' "$2"
 sed -i 's|__builtin_clzl(|(int)stdc_leading_zeros_ul(|g' "$2"
 sed -i 's|__builtin_clz(|(int)stdc_leading_zeros_ui(|g' "$2"
 sed -i 's|__builtin_popcountll(|(int)stdc_count_ones_ull(|g' "$2"
 sed -i 's|__builtin_popcountl(|(int)stdc_count_ones_ul(|g' "$2"
 sed -i 's|__builtin_popcount(|(int)stdc_count_ones_ui(|g' "$2"
}

use_stdbit2() {
 sed -i 's|^'"$1"'|#include <stdbit.h>\n'"$1"'|g' "$2"

 sed -i 's|__builtin_ctzll|(int)stdc_trailing_zeros_ull|g' "$2"
 sed -i 's|__builtin_ctzl|(int)stdc_trailing_zeros_ul|g' "$2"
 sed -i 's|__builtin_ctz|(int)stdc_trailing_zeros_ui|g' "$2"
 sed -i 's|__builtin_clzll|(int)stdc_leading_zeros_ull|g' "$2"
 sed -i 's|__builtin_clzl|(int)stdc_leading_zeros_ul|g' "$2"
 sed -i 's|__builtin_clz|(int)stdc_leading_zeros_ui|g' "$2"
 sed -i 's|__builtin_popcountll|(int)stdc_count_ones_ull|g' "$2"
 sed -i 's|__builtin_popcountl|(int)stdc_count_ones_ul|g' "$2"
 sed -i 's|__builtin_popcount|(int)stdc_count_ones_ui|g' "$2"
}

# run a test

if [[ $(type -t "$1") != function ]]; then
  echo 'expected a test name'
  exit 1
fi

$1
