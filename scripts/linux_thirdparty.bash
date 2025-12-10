set -eu
set -o pipefail

if [ "$CC" = /work/slimcc/slimcc ]; then
 is_CI=
 SRC_DIR=`dirname $CC`
 MUON=$SRC_DIR/muon/build/muon
fi

test_ag() {
 git_fetch https://github.com/aswild/the_silver_searcher 7b571a8a94d0e22a06e3313cb0d9672b416fb2c1 ag
 sh autogen.sh
 ./configure && make test
}

test_apk() {
 github_tar alpinelinux apk-tools v3.0.0_rc8
 $MUON setup -Dpython=disabled muonbuild
 $MUON -C muonbuild test -j1
}

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
 url_lz https://ftpmirror.gnu.org/gnu/binutils/binutils-2.45.1.tar.lz binutils
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

test_box2d() {
 github_tar erincatto box2d v3.1.1
 use_stdatomic '#include <stdint.h>' src/atomic.h
 use_stdbit '#include <stdint.h>' src/ctz.h
 replace_line "#elif defined( __GNUC__ ) || defined( __clang__ )" "#elif 1" src/atomic.h
 sed -i 's|__atomic_compare_exchange_n( &a->value, &expected, desired, false,|atomic_compare_exchange_strong_explicit(\&a->value,\&expected,desired,|g' src/atomic.h
 cmake_init -DBOX2D_DISABLE_SIMD=ON
 make
 ./bin/test
}

test_busybox() {
 git_fetch https://github.com/sailfishos-mirror/busybox a98b95b715359a8b002d1cb8e1f998a4afa2c73e busybox
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

test_bzip3() {
 git_fetch https://github.com/iczelia/bzip3 fe3b43d83d8388734c67480b4191a118e506dc45 bzip3
 libtoolize
 sh ./bootstrap.sh
 fix_and_configure --disable-arch-native
 sed -i 's|#include <intrin.h>||g' include/common.h
 sed -i 's|_mm_prefetch((const void \*)(address), _MM_HINT_NTA)||g' include/common.h
 sed -i 's|_m_prefetchw((const void \*)(address))||g' include/common.h
 make roundtrip test
}

test_c2() {
 git_fetch https://github.com/c2lang/c2compiler 7b17e77e8bfbfd20f5d6040dde7f851ebe0d94b7 c2compiler
 sed -i 's|-pipe ||g' bootstrap/Makefile
 sed -i 's|-pipe ||g' bootstrap/bootstrap.c
 sed -i 's|-pipe ||g' generator/c/c_generator_special.c2
 export C2_LIBDIR=$PWD/libs
 export C2_PLUGINDIR=$PWD/output/plugins
 make CC=$CC test
}

test_c23doku() {
 git_fetch https://github.com/fuhsnn/c23doku 9c84d5229e16af9e58deca7322dc79751d7474b0 c23doku
 sh test.sh
 sh test_c2y.sh
}

test_c3() {
 github_tar c3lang c3c v0.7.8
 cmake_init
 make VERBOSE=1
 cd ../test
 ../cmakebuild/c3c compile-test unit
 ../cmakebuild/c3c compile-run -O1 src/test_suite_runner.c3 -- ../cmakebuild/c3c test_suite/
}

test_calc() {
 github_tar lcn2 calc v2.16.1.1
 make CC=$CC LCC=$CC MAN=true check
}

test_cello() {
 git_fetch https://github.com/orangeduck/Cello 61ee5c3d9bca98fd68af575e9704f5f02533ae26 cello
 make check
}

test_cgit() {
 git_fetch https://git.zx2c4.com/cgit 09d24d7cd0b7e85633f2f43808b12871bb209d69 cgit
 make get-git
 make CC=$CC test
}

test_cjson() {
 github_tar DaveGamble cJSON v1.7.19
 replace_line "#if (defined(__GNUC__) || defined(__SUNPRO_CC) || defined (__SUNPRO_C)) && defined(CJSON_API_VISIBILITY)" "#if 1" cJSON.h
 sed -i 's/if defined(__GNUC__) || defined(__ghs__)/if 1/g' tests/unity/src/unity_internals.h
 cmake_init
 make check
}

test_cmocka() {
 gitlab_tar gitlab.com/cmocka cmocka cmocka-2.0.1
 sed -i 's|${DEFAULT_LINK_FLAGS}$|& -lm|g' example/CMakeLists.txt
 cmake_init
 make && ctest
}

test_coreutils() {
 url_xz https://ftpmirror.gnu.org/gnu/coreutils/coreutils-9.9.tar.xz coreutils
 ${is_CI+ replace_line "skip_if_root_" "skip_" tests/rm/deep-2.sh }
 ./configure
 make check
}

test_cpio() {
 url_bz https://ftpmirror.gnu.org/gnu/cpio/cpio-2.15.tar.bz2 cpio
 ./configure
 make check
}

test_cproc() {
 local CCTESTSCRIPT=$(dirname $(realpath $0))/cctest_cproc.bash

 git_fetch https://github.com/michaelforney/cproc 70511143110525030fb68aee4e60ed1581d1d2c9 cproc
 ./configure --host=x86_64-linux-gnu
 make CFLAGS=-std=c99 check

 bash $CCTESTSCRIPT
}

test_curl() {
 github_tar curl curl curl-8_17_0
 libtoolize
 autoreconf -fi
 fix_and_configure --with-openssl
 make && make test
}

test_darkhttpd() {
 github_tar emikulic darkhttpd v1.17
 sed -i 's|-fsanitize=address | |g' devel/run-tests
 sed -i 's|-fsanitize=undefined | |g' devel/run-tests
 sed -i 's|-fprofile-arcs -ftest-coverage | |g' devel/run-tests
 make CC=$CC test
}

test_diffutils() {
 url_xz https://ftpmirror.gnu.org/gnu/diffutils/diffutils-3.12.tar.xz diffutils
 ./configure
 make && make check SUBDIRS=./tests
}

test_doom() {
 git_fetch https://github.com/Daivuk/PureDOOM c169e1148789a9bcecf4d69bdd249fcc7b6e4f80 puredoom
 mkdir -p examples/Tests/build && cd "$_"
 replace_line "project(pd_tests)" "project(pd_tests C)" ../CMakeLists.txt
 cmake ../ && make
 cd ../../../ && examples/Tests/build/pd_tests
}

test_elk() {
 git_fetch https://github.com/cesanta/elk a9bb85619c5cddf49dfa8bdf529770fc9943a7fd elk
 CFLAGS='-O -ffunction-sections -fdata-sections' make -C test test elk
}

test_emacs() {
 url_xz https://ftpmirror.gnu.org/gnu/emacs/emacs-30.2.tar.xz emacs
 ./configure
 make check -j2
}

test_espruino() {
 # build process needs .git/
 git_fetch https://github.com/espruino/Espruino 37bd2082786535c052928e5e7728115950ac8096 espruino
 rm -r tests/manual/ tests/*_FAIL.js tests/test_tensorflow.js
 sed -i 's|-lstdc++|-lc|g' make/family/LINUX.make
 BOARD=LINUX PYTHON=python3 USE_TENSORFLOW=0 make CC=$CC LD=$CC V=1 -j1
 ./bin/espruino --test-all
}

test_fftw() {
 url_tar https://fftw.org/fftw-3.3.10.tar.gz fftw
 fix_and_configure
 make check
}

test_file() {
 github_tar file file FILE5_46
 libtoolize
 autoreconf -fi
 fix_and_configure
 make check
}

test_findutils() {
 url_xz https://ftpmirror.gnu.org/gnu/findutils/findutils-4.10.0.tar.xz findutils
 ./configure
 make && make check-TESTS
}

test_flatcc() {
 git_fetch https://github.com/dvidelabs/flatcc e3e44836c5f625b5532586ddce895f8b5e36a212 flatcc
 cmake_init -DCMAKE_C_FLAGS=-DPORTABLE_USE_ISINF
 make && ctest
}

test_flex() {
 url_tar https://github.com/westes/flex/files/981163/flex-2.6.4.tar.gz flex
 fix_configure
 CC_FOR_BUILD=$CC ./configure
 make check
}

test_fossil() {
 git_fetch https://github.com/drhsqlite/fossil-mirror 4ff79ef91d4d945413b4f409fee047b8ae3cc6a2 fossil
 use_stdatomic '# define SQLITE_ATOMIC_INTRINSICS 1' extsrc/sqlite3.c
 sed -i 's|# define SQLITE_ATOMIC_INTRINSICS|# define SQLITE_MEMORY_BARRIER atomic_thread_fence(memory_order_seq_cst)\n# define SQLITE_ATOMIC_INTRINSICS|' extsrc/sqlite3.c
 CC_FOR_BUILD=$CC ./configure
 make test
}

test_gawk() {
 url_lz https://ftpmirror.gnu.org/gnu/gawk/gawk-5.3.2.tar.lz gawk
 fix_and_configure ${is_CI+ --disable-pma } # pma segfault in docker
 make check
}

test_git() {
 github_tar git git v2.52.0
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

test_gnutls() {
 url_xz https://www.gnupg.org/ftp/gcrypt/gnutls/v3.8/gnutls-3.8.11.tar.xz gnutls
 fix_configure
 CFLAGS=-DCRAU_MAYBE_UNUSED= ./configure --disable-hardware-acceleration
 make -j2
 make -C tests check -j2
 make -C fuzz check -j2
}

test_go() {
 git_fetch https://github.com/golang/go ceb95ea6aef52c1fb472d3539c6ef68670778b5b go
 sed -i 's|\"-ggdb\",||g' src/cmd/dist/build.c
 sed -i 's|\"-pipe\",||g' src/cmd/dist/build.c
 sed -i 's|vadd(&gccargs, \"-fmessage-length=0\");||g' src/cmd/dist/build.c
 ${is_CI+ rm src/runtime/pprof/pprof_test.go } # flaky (cpu load sensitive)
 cd src/
 GO14TESTS=1 ./all.bash
}

test_got() {
 github_tar gameoftrees got-portable 0.120
 sh autogen.sh

 local GOT=$PWD/got_install
 ./configure --prefix=$GOT
 make PREFIX=$GOT install
 export PATH="$GOT/bin:$PATH"
 # decomposed from `make tests`
 subtests=(
  compat regress-delta regress-deltify regress-fetch regress-idset
  regress-path regress-tog
  # regress-cmdline # permission/ssh issues
 )
 make "${subtests[@]}"
}

test_gsed() {
 url_xz https://ftpmirror.gnu.org/gnu/sed/sed-4.9.tar.xz gsed
 ./configure
 make && make check-TESTS
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

test_handmademath() {
 git_fetch https://github.com/HandmadeMath/HandmadeMath 142ba3cd9da700e3e599d301bbd27d415ad14626 HandmadeMath
 make -C test CC=$CC c99 c11
}

test_hare() {
 url_tar https://git.sr.ht/~sircmpwn/harec/archive/0.25.2.tar.gz harec
 mv configs/linux.mk config.mk
 make CC="$CC" check
}

test_imagemagick() {
 github_tar ImageMagick ImageMagick 7.1.2-12
 fix_and_configure
 make check V=1
}

test_inih() {
 github_tar benhoyt inih r62
 $MUON setup muonbuild
 $MUON -C muonbuild samu -v -j1
 $MUON -C muonbuild test -v -j1
}

test_janet() {
 github_tar janet-lang janet v1.40.1
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

test_jerryscript() {
 github_tar jerryscript-project jerryscript v3.0.0
 sed -i 's|if(NOT (${CMAKE_C_COMPILER_ID} STREQUAL MSVC))|if(FALSE)|g' tests/unit-doc/CMakeLists.txt
 replace_line "#ifdef __GNUC__" "#if 1" jerry-ext/include/jerryscript-ext/autorelease.impl.h
 replace_line "#elif defined(__GNUC__)" "#elif 1" jerry-ext/include/jerryscript-ext/module.h
 python3 tools/run-tests.py --unittest
 python3 tools/run-tests.py --jerry-tests
 python3 tools/run-tests.py --test262
 python3 tools/build.py
}

test_jq() {
 github_tar jqlang jq jq-1.8.1
 libtoolize
 autoreconf -fi
 fix_and_configure
 make check
}

test_kefir() {
 codeberg_tar jprotopopov kefir v0.5.0
 mkdir bin && CC=gcc scripts/detect-host-env.sh --header > bin/config.h
 make CC="$CC"
 LC_ALL=C.UTF-8 make CC=gcc test
}

test_ksh93() {
 git_fetch https://github.com/ksh93/ksh 0f6866b6bebc26d003acb6f467d0694a8aea5177 ksh93
 replace_line 'occ=cc' 'occ=$CC' src/cmd/INIT/iffe.sh
 # probe depends on -Wincompatible-pointer-types
 sed -i 's|$i (\*Sig_handler_t)($j)|void (*Sig_handler_t)(int)|g' src/lib/libast/features/sig.sh
 ${is_CI+ rm src/cmd/ksh93/tests/basic.sh src/cmd/ksh93/tests/io.sh src/cmd/ksh93/tests/sigchld.sh src/cmd/ksh93/tests/variables.sh }
 bin/package make
 bin/package test
}

test_lame() {
 url_tar https://sourceforge.net/projects/lame/files/lame/3.100/lame-3.100.tar.gz/download lame
 fix_and_configure
 make test
}

test_lexbor() {
 github_tar lexbor lexbor v2.6.0
 sed -i 's| -pipe | |g' source/lexbor/ports/posix/config.cmake
 cmake_init -DLEXBOR_BUILD_TESTS=ON
 make && ctest
}

test_liballegro5() {
 github_tar liballeg allegro5 5.2.11.1
 cmake_init
 make VERBOSE=1
 make run_standalone_tests
 . ../tests/grab_bitmap_suites.sh
 find ../tests -name '*.ini' | grep -v 'compressed' | xargs xvfb-run tests/test_driver --save_on_failure --xvfb | tee /tmp/test_out || true
 grep -q 'failed tests: 0' /tmp/test_out
}

test_libarchive() {
 github_tar libarchive libarchive v3.8.4
 replace_line "#elif defined(__GNUC__)" "#elif 1" libarchive/archive_blake2.h
 replace_line "#if defined(__GNUC__)" "#if 1" libarchive/archive_write_set_format_cpio_binary.c
 libtoolize
 autoreconf -fi
 fix_and_configure
 make check
}

test_libevent() {
 git_fetch https://github.com/libevent/libevent a994a52d5373d6284b27576efa617aff2baa7bd3 libevent
 libtoolize
 sh autogen.sh
 fix_and_configure
 make check -j2
}

test_libexpat() {
 github_tar libexpat libexpat R_2_7_3
 cd expat
 cmake_init
 make && ctest
}

test_libgc() {
 git_fetch https://github.com/bdwgc/bdwgc beffb57e89902d080eace96a576458a99d09f938 libgc
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

test_libgit2(){
 github_tar libgit2 libgit2 v1.9.2
 use_stdatomic '#ifdef GIT_THREADS' src/util/thread.h
 sed -i 's|defined(GIT_BUILTIN_ATOMIC)|1|g' src/util/thread.h
 sed -i 's|__atomic_exchange(ptr, &newval, &foundval,|return atomic_exchange_explicit(ptr, newval,|g' src/util/thread.h
 sed -i 's|__atomic_compare_exchange(ptr, &foundval, &newval, false,|atomic_compare_exchange_strong_explicit(ptr, \&foundval, newval,|g' src/util/thread.h
 replace_line "#elif defined(__clang__) || defined(__GNUC__)" "#elif 1" deps/ntlmclient/utf8.h
 sed -i 's|__has_builtin(__builtin_add_overflow)|0|g' src/util/integer.h
 cmake_init
 make && ctest --verbose
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
 cmake_init -DCMAKE_C_FLAGS=-lm -DJANSSON_BUILD_DOCS=OFF -DHAVE_ATOMIC_BUILTINS=1
 make && make test
}

test_libjpeg() {
 url_tar https://www.ijg.org/files/jpegsrc.v9f.tar.gz libjpeg
 fix_and_configure
 make check
}

test_libjsonc() {
 git_fetch https://github.com/json-c/json-c 14c8e2e5eeafe91a4f499cd33cb93fb893594eb0 json-c
 cmake_init
 make && make test
}

test_liblz4() {
 git_fetch https://github.com/lz4/lz4 a018abc07a3018371625a265f195c4fafbf1f99d lz4
 make test
}

test_libmicrohttpd() {
 url_tar https://ftp.gnu.org/gnu/libmicrohttpd/libmicrohttpd-1.0.2.tar.gz libmicrohttpd
 sed -i 's|defined(__GNUC__)|1|g' src/include/autoinit_funcs.h
 fix_and_configure
 make check
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

test_libopus() {
 url_tar https://downloads.xiph.org/releases/opus/opus-1.6.tar.gz opus
 fix_configure
 CFLAGS=-fdisable-visibility ./configure
 make check
}

test_libpcre2() {
 github_tar PCRE2Project pcre2 pcre2-10.47
 fix_and_configure
 make check
}

test_libpng() {
 github_tar pnggroup libpng v1.6.53
 fix_and_configure
 make test
}

test_libpsl() {
 url_lz https://github.com/rockdaboot/libpsl/releases/download/0.21.5/libpsl-0.21.5.tar.lz libpsl
 fix_and_configure
 make check
}

test_libressl() {
 github_tar libressl portable v4.2.1
 libtoolize
 sh autogen.sh
 fix_and_configure
 replace_line "#if defined(__GNUC__)" "#if 1" crypto/bn/arch/amd64/bn_arch.h
 make check
}

test_libsamplerate() {
 git_fetch https://github.com/libsndfile/libsamplerate 2ccde9568cca73c7b32c97fefca2e418c16ae5e3 libsamplerate
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
 github_tar libuv libuv v1.51.0
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

test_libwebp() {
 github_tar webmproject libwebp v1.6.0
 cmake_init
 make
 ./dwebp ../examples/test.webp -ppm -o test.ppm
 md5sum test.ppm | grep ebdd46e0760b2a4891e6550b37c00660
}

test_libxml() {
 github_tar GNOME libxml2 v2.15.1
 libtoolize
 sh autogen.sh
 fix_configure
 CFLAGS=-D_FILE_OFFSET_BITS=64 ./configure
 make check
}

test_libxo_chimerautils() {
 local LIBXO=$PWD/libxo_install
 github_tar chimera-linux chimerautils v15.0.3

 github_tar Juniper libxo 1.7.5
 sed -i 's|__int128_t|_BitInt(128)|g' libxo/xo_humanize.h
 libtoolize
 autoreconf -fi
 fix_and_configure --prefix=$LIBXO
 make && make test && make install
 cd ../

 export PKG_CONFIG_PATH="$LIBXO/lib/pkgconfig:${PKG_CONFIG_PATH:-}"
 sed -i "s|type: 'boolean', value: 'true'|type: 'boolean', value: true|g" meson_options.txt
 $MUON setup muonbuild
 $MUON -C muonbuild samu -v -j1

 ./muonbuild/src.freebsd/coreutils/echo/echo 'echo hello_world' > hello.sh
 ./muonbuild/src.freebsd/sh/sh hello.sh | ./muonbuild/src.freebsd/grep/grep ^hello_world$
}

test_libyaml() {
 git_fetch https://github.com/yaml/libyaml 840b65c40675e2d06bf40405ad3f12dec7f35923 libyaml
 cmake_init
 make && make test
}

test_lighthttpd() {
 url_xz https://download.lighttpd.net/lighttpd/releases-1.4.x/lighttpd-1.4.82.tar.xz lighthttpd
 libtoolize
 sh autogen.sh
 fix_configure
 CC_FOR_BUILD=$CC ./configure
 make check
}

test_lua() {
 url_tar https://lua.org/ftp/lua-5.5.0.tar.gz lua
 cd src && make CC="$CC"
 url_tar https://lua.org/tests/lua-5.5.0-tests.tar.gz luatests
 cd libs && make CC="$CC" && cd ../
 ../lua ${is_CI+ -e"_port=true" } all.lua # assertion at files.lua:84 in CI
}

test_lwan(){
 git_fetch https://github.com/lpereira/lwan e7ac7f13afd1e060bfef61d2ec5d2a60f356b0eb lwan
 use_stdbit '#include <assert.h>' src/bin/tools/mimegen.c
 use_stdbit '#include <assert.h>' src/lib/timeout.c
 use_stdbit '#include <assert.h>' src/samples/techempower/json.c
 use_stdbit '#include <stdlib.h>' src/lib/lwan-private.h
 sed -i 's|__sync_##O##_and_fetch|__builtin_atomic_arith_##O|g' src/lib/lwan.h
 sed -i 's|__uint128_t|unsigned _BitInt(128)|g' src/lib/lwan-thread.c
 sed -i 's|__builtin_inf()|HUGE_VAL|g' src/samples/forthsalon/forth.c
 replace_line "#if defined(__x86_64__)" "#if 0" src/lib/lwan-websocket.c
 replace_line "#if __x86_64__" "#if 0" src/lib/lwan-template.c
 cmake_init
 make testsuite
}

test_mawk() {
 github_tar ThomasDickey mawk-snapshots t20250131
 ./configure
 make check
}

test_mbedtls() {
 url_bz https://github.com/Mbed-TLS/mbedtls/releases/download/mbedtls-4.0.0/mbedtls-4.0.0.tar.bz2 mbedtls
 replace_line "    (defined(__GNUC__) || defined(__clang__)) && defined(MBEDTLS_ARCH_IS_X64)" "1" tf-psa-crypto/drivers/builtin/src/aesni.h
 cmake_init
 make && ctest -j2
}

test_memcached() {
 github_tar memcached memcached 1.6.40
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
 github_clone micropython micropython v1.26.1
 use_stdbit "#include <stdbool.h>" py/misc.h
 # sed -i 's|inline MP_ALWAYSINLINE const|static inline const|g' py/misc.h ## if without CFLAGS_EXTRA=-ffake-always-inline
 replace_line "#if defined(__clang__) || (defined(__GNUC__) && __GNUC__ >= 8)" "#if 1" py/nlrx64.c
 sed -i 's|defined(LFS2_NO_INTRINSICS)|1|g' lib/littlefs/lfs2_util.h
 make -C ports/unix/ CC=$CC CFLAGS_EXTRA=-ffake-always-inline V=1 VARIANT=standard MICROPY_PY_THREAD_GIL=1 test_full
 cd tests
 MICROPY_CPYTHON3=python3 MICROPY_MICROPYTHON=../ports/unix/build-standard/micropython ./run-multitests.py multi_net/*.py
}

test_mimalloc() {
 github_tar microsoft mimalloc v3.1.5
 replace_line "project(libmimalloc C CXX)" "project(libmimalloc C)" CMakeLists.txt
 replace_line "set(CMAKE_CXX_STANDARD 17)" "" CMakeLists.txt
 replace_line "#include <immintrin.h>" "" include/mimalloc/bits.h
 replace_line "#if defined(__GNUC__) || defined(__clang__)" "#if 1" src/prim/prim.c
 cmake_init
 make && make test
}

test_mquickjs() {
 git_fetch https://github.com/bellard/mquickjs 767e5db54241f0b53193a04aea193c823a9e3121 mquickjs
 use_stdbit "#include <stdlib.h>" cutils.h
 sed -i 's|-fno-math-errno -fno-trapping-math||g' Makefile
 make CC=$CC HOST_CC=$CC test
}

test_mruby() {
 github_tar mruby mruby 3.4.0
 sed -i 's|conf.gem :core => \"mruby-cmath\"||g' mrbgems/math.gembox
 rake test
}

test_msgpack() {
 github_tar msgpack msgpack-c c-6.1.0
 convert_atomic_x_fetch cmake/sysdep.h.in
 cmake_init -DMSGPACK_32BIT=OFF -DBUILD_SHARED_LIBS=ON \
  -DMSGPACK_CHAR_SIGN=signed -DMSGPACK_BUILD_EXAMPLES=ON -DMSGPACK_BUILD_TESTS=ON
 make
 make test
}

test_muon() {
 shared_muon
 sed -i "s|\['common/13|#|g" subprojects/meson-tests/meson.build # we don't do pch
 sed -i "s|\['frameworks/7 gnome|#|g" subprojects/meson-tests/meson.build
 sed -i "s|'clang'|'clang', 'slimcc'|g" 'subprojects/meson-tests/common/44 pkgconfig-gen/meson.build'
 build/muon -C build test
}

test_nanopb() {
 git_fetch https://github.com/nanopb/nanopb c716db13070bfb7de03b33f5a6558528cbf8a249 nanopb
 cd tests
 scons CC=$CC
}

test_neovim() {
 github_tar neovim neovim v0.10.4
 cmake_init
 make unittest
}

test_nginx() {
 github_tar nginx nginx release-1.29.4
 auto/configure
 make
 cd ../
 git_fetch https://github.com/nginx/nginx-tests 0fccfcef1278263416043e0bbb3e0116b84026e4 nginx-tests
 prove .
}

test_njs() {
 github_tar nginx njs 0.9.4
 NJS_CC_NAME=$CC ./configure
 make test
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

test_nqp() {
 github_clone MoarVM MoarVM 2025.12
 use_stdbit '#include "moar.h"' src/core/coerce.c
 perl Configure.pl --cc=$CC --no-mimalloc
 make install
 cd ../

 github_clone Raku nqp 2025.12
 perl Configure.pl --with-moar=$PWD/../MoarVM/install/bin/moar
 make test
}

test_ocaml() {
 github_tar ocaml ocaml 5.4.0
 sed -i 's|arguments = "mainarith|arguments = "-lm mainarith|g' testsuite/tests/asmgen/arith.cmm
 fix_and_configure --enable-ocamltest
 make -j4  && make -C testsuite parallel -j4
}

test_oniguruma() {
 git_fetch https://github.com/kkos/oniguruma f95747b462de672b6f8dbdeb478245ddf061ca53 oniguruma
 libtoolize
 autoreconf -fi
 fix_and_configure
 make check
}

test_openrc() {
 github_tar OpenRC openrc 0.63
 $MUON setup muonbuild
 $MUON -C muonbuild samu -j1
 $MUON -C muonbuild test -j1
}

test_openssh() {
 github_tar openssh openssh-portable V_10_2_P1
 ./configure
 make
 make file-tests
 ${is_CI- make t-exec } # "regress/agent-subprocess.sh" fail in CI
 make interop-tests
 make extra-tests
 make unit
}

test_openssl() {
 github_tar openssl openssl openssl-3.6.0
 replace_line "#if !defined(__DJGPP__)" "#if 0" test/rsa_complex.c
 ./Configure
 make -j2 && make test HARNESS_JOBS=2
}

test_orangeduck_mpc() {
 git_fetch https://github.com/orangeduck/mpc 1049534fc56b1971345c7aaa792dea55d6f9b7bc mpc
 make CC=$CC check
}

test_pacman() {
 gitlab_tar gitlab.archlinux.org/pacman pacman v7.1.0
 sed -i "s| '',|& '\\\\n',|g" meson.build
 $MUON setup muonbuild
 $MUON -C muonbuild samu -j1
 $MUON -C muonbuild test -j1
}

test_parrot() {
 git_fetch https://github.com/parrot/parrot 472c0538637dedfe2fd0f29567d0c26bf9f38edd parrot
 perl Configure.pl --cc=$CC --ld=$CC --link=$CC --linkflags=
 sed -i 's| -fstack-protector-strong||g' Makefile
 make test
}

test_pdpmake() {
 git_fetch https://github.com/rmyorston/pdpmake 0faaf258c9b3eaa9f5e4f6842e2484cbea6f23f8 pdpmake
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
 gitlab_tar gitlab.freedesktop.org/pixman pixman pixman-0.46.4
 $MUON setup -Dmmx=disabled -Dsse2=disabled build
 $MUON -C build test -j1 -t 20
}

test_php() {
 github_tar php php-src php-8.5.1
 replace_line "#elif defined(__GNUC__) || defined(__INTEL_COMPILER) || defined(__SUNPRO_C) || defined(__TINYC__)" "#elif 1" ext/pcre/pcre2lib/sljit/sljitNativeX86_common.c
 replace_line "#elif (defined(__i386__) || defined(__x86_64__)) && defined(__GNUC__)" "#elif 1" Zend/zend_multiply.h
 replace_line "#elif defined(__GNUC__) && defined(__x86_64__)" "#elif 1" Zend/zend_multiply.h
 sed -i 's|#if __has_feature(c_atomic) && defined(__clang__)|#if 1\n#include <stdatomic.h>|g' Zend/zend_atomic.h
 sed -i 's|__c11_atomic_init(|atomic_store(|g' Zend/zend_atomic.h
 sed -i 's|__c11_atomic_|atomic_|g' Zend/zend_atomic.h
 sed -i 's|, __ATOMIC_SEQ_CST||g' Zend/zend_atomic.h

 # don't work in CI https://github.com/php/php-src/blob/17187c4646f3293e1de8df3f26d56978d45186d6/.github/actions/test-linux/action.yml#L40
 ${is_CI+ export SKIP_IO_CAPTURE_TESTS=1 }

 ./buildconf --force
 fix_configure
 coverage=(--enable-pcntl --enable-zend-test --with-bz2 --with-curl --with-ffi --with-gettext --with-gmp --with-openssl --with-readline --with-sodium --with-zlib)
 CFLAGS=-fdisable-visibility ./configure "${coverage[@]}" --enable-huge-code-pages=no
 make test NO_INTERACTION=1
}

test_postgres() {
 github_tar postgres postgres REL_18_1
 replace_line "#if defined(__GNUC__) || defined(__INTEL_COMPILER)" "#if 1" src/include/storage/s_lock.h
 replace_line "#if (defined(__x86_64__) || defined(_M_AMD64))" "#if 0" src/include/port/simd.h
 replace_line "#if defined(__GNUC__) || defined(__INTEL_COMPILER)" "#if 1" src/include/port/atomics.h
 replace_line "#if defined(__GNUC__) || defined(__INTEL_COMPILER)" "#if 1" src/include/port/atomics/arch-x86.h
 ./configure && make && make check
}

test_python() {
 github_tar python cpython v3.14.2
 replace_line "#if defined(__GNUC__) || defined(__clang__)" "#if 1" Include/pyport.h
 replace_line "#if defined(__linux__) && (defined(__GNUC__) || defined(__clang__))" "#if 1" Include/internal/pycore_debug_offsets.h
 replace_line "#elif defined(__GNUC__) || defined(__clang__)" "#elif 1" Objects/mimalloc/init.c
 skip_tests=(
  ${is_CI+ test_asyncio test_socket }
  test_os # https://github.com/python/cpython/issues/126112
 )
 ./configure
 make -j3 && ./python -m test -j3 --exclude "${skip_tests[@]}"
}

test_qbe() {
 git_fetch git://c9x.me/qbe.git 120f316162879b6165deba77815cd4193fb2fb59 qbe
 make CC="$CC" check
}

test_quickjs() {
 git_fetch https://github.com/bellard/quickjs f1139494d18a2053630c5ed3384a42bb70db3c53 quickjs
 use_stdbit "#include <stdlib.h>" cutils.h
 make CC=$CC test
}

test_redis() {
 github_tar redis redis 8.4.0
 replace_line "#    if defined(__GNUC__) && !(defined(__clang__) && defined(__cplusplus))" "#if 1" src/redismodule.h
 sed -i 's|asm volatile|__asm volatile|g' deps/hdr_histogram/hdr_atomic.h
 convert_atomic_x_fetch deps/hdr_histogram/hdr_atomic.h
 use_stdbit "#include <stdint.h>" deps/hdr_histogram/hdr_histogram.c
 use_stdbit "#include <stdint.h>" src/util.h
 use_stdbit "#include <stdint.h>" src/dict.c
 use_stdbit "#include <stdint.h>" src/hyperloglog.c
 use_stdbit "#include <stdarg.h>" src/server.c
 use_stdbit '#include "server.h"' src/bitops.c
 sed -i 's|system_backtrace_supported {} {|& return 0|g' tests/support/util.tcl
 sed -i 's|CC = gcc||g' tests/modules/Makefile
 sed -i 's|LD = gcc||g' tests/modules/Makefile
 make V=1 CC=$CC CXX=clang++ OPTIMIZATION=-O MALLOC=libc test
}

test_valkey() {
 github_tar valkey-io valkey 9.0.1
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
 use_stdbit "#include <math.h>" src/networking.c
 use_stdbit "#include <math.h>" src/cluster_legacy.c
 use_stdbit "#include <math.h>" src/t_zset.c
 make V=1 CC=$CC OPTIMIZATION=-O MALLOC=libc test
}

test_rpmalloc() {
 git_fetch https://github.com/mjansson/rpmalloc feb43aee0d4dcca9fd91b3dd54311c34c6cc6187 rpmalloc
 replace_line "#if defined(__clang__) || defined(__GNUC__)" "#if 1" rpmalloc/rpmalloc.h
 use_stdbit '#include <stdint.h' rpmalloc/rpmalloc.c
 python3 configure.py
 sed -i 's|-fstrict-aliasing||g' build.ninja
 sed -i 's|-fno-math-errno||g' build.ninja
 sed -i 's|-ffinite-math-only||g' build.ninja
 sed -i 's|-funsafe-math-optimizations||g' build.ninja
 sed -i 's|-fno-trapping-math||g' build.ninja
 sed -i 's|-ffast-math||g' build.ninja
 sed -i 's|-fomit-frame-pointer||g' build.ninja
 sed -i 's|-funroll-loops||g' build.ninja
 $MUON samu -v -j1
 ./bin/linux/release/x86-64/rpmalloc-test
}

test_rsync() {
 github_tar RsyncProject rsync v3.4.1
 ./configure
 make test
}

test_ruby() {
 github_tar ruby ruby v4.0.0
 rm tool/test/test_commit_email.rb
 sh autogen.sh
 cflags=-fPIC cxxflags=-fPIC ./configure
 make check -j4
}

test_rvvm() {
 git_fetch https://github.com/LekKit/RVVM 1a99624d041ae411839262b51c7f5e19a123aef7 rvvm
 sed -i 's|defined(__SSE2__) && defined(__SSE2_MATH__)|1|g' src/fpu_lib.c
 make test CC=$CC CFLAGS='-std=c23 -DSDL_DISABLE_IMMINTRIN_H' USE_SDL=2
}

test_samba() {
 github_tar samba-team samba samba-4.23.4
 sed -i 's|from waflib.Tools import |from waflib.Tools import gcc, |g' buildtools/wafsamba/generic_cc.py
 sed -i 's|conf.generic_cc_common_flags|conf.gcc_common_flags|g' buildtools/wafsamba/generic_cc.py
 use_stdatomic '#include <stdarg.h>' third_party/socket_wrapper/socket_wrapper.c
 use_stdatomic '#include <stdarg.h>' third_party/quic_ko_wrapper/quic_ko_wrapper.c
 sed -i 's|elif x.startswith(('\''-m'\'', '\''-f'\''|elif x != '\''-fstack-protector-strong'\'' and x.startswith(('\''-m'\'', '\''-f'\''|g' third_party/waf/waflib/Tools/c_config.py
 replace_line "#if defined(__clang__) || defined(__GNUC__) || defined(__SUNPRO_C)" "#if 1" third_party/heimdal/include/heim_threads.h
 sed -i 's/defined(_MSC_VER) && !defined(__clang__) &&/1||/g' third_party/ngtcp2/lib/ngtcp2_ringbuf.c
 ./configure --without-json --without-ad-dc --enable-selftest --without-ldap --without-ldb-lmdb --without-ads --with-shared-modules='!vfs_snapper,!vfs_nfs4acl_xattr'
 LD_LIBRARY_PATH=$PWD/bin/default/lib/util:$PWD/bin/default/libcli/util:$PWD/bin/default/librpc:$PWD/bin/default/nsswitch/libwbclient:$PWD/bin/default/source3:$PWD/bin/default/source3/libsmb make quicktest -j4
}

test_scrapscript() {
 git_fetch https://github.com/tekknolagi/scrapscript 986e0fbbcb497bea22875006d4c60dd3809a0cff scrapscript
 curl -LsSf https://astral.sh/uv/install.sh | sh
 ~/.local/bin/uv python install 3.10
 ~/.local/bin/uv python pin 3.010
 ~/.local/bin/uv run python compiler_tests.py
}

test_sdl3() {
 github_tar libsdl-org SDL release-3.4.0
 replace_line "void \*alloca(size_t);" "#include <alloca.h>" include/SDL3/SDL_stdinc.h
 replace_line "#if defined(HAVE_GCC_ATOMICS) || defined(HAVE_GCC_SYNC_LOCK_TEST_AND_SET)" "#if 1" src/atomic/SDL_spinlock.c
 use_stdatomic '#include "SDL_internal.h"' src/atomic/SDL_spinlock.c
 sed -i 's|__sync_lock_test_and_set|atomic_exchange|g' src/atomic/SDL_spinlock.c
 sed -i 's|__sync_lock_release(lock)|atomic_store(lock, 0)|g' src/atomic/SDL_spinlock.c
 cmake_init -DCMAKE_C_FLAGS=-DSTBI_NO_SIMD
 make VERBOSE=1 && ctest
}

test_sokol() {
 git_fetch https://github.com/floooh/sokol f38e0b520f99a501b71172c3a3181c9ab6ebdd79 sokol
 sed -i 's|floooh/dcimgui|floooh/dcimgui --branch v1.92.5|g' tests/ext/CMakeLists.txt
 cd tests
 cmake_init -DSOKOL_BACKEND=SOKOL_GLCORE
 make VERBOSE=1 && ./sokol-test
}

test_sqlite() {
 github_tar sqlite sqlite version-3.51.1
 use_stdatomic '# define SQLITE_ATOMIC_INTRINSICS 1' src/sqliteInt.h
 CC_FOR_BUILD="$CC" CFLAGS=-D_GNU_SOURCE ./configure
 make test
}

test_tcl() {
 github_tar tcltk tcl core-9-0-3
 ./unix/configure
 rm ./tests/socket.test # fails under su
 make test | tee __testlog
 grep -P -q '^all.tcl.*Failed\t0$' __testlog
}

test_tinycc() {
 local CCTESTSCRIPT=$(dirname $(realpath $0))/cctest_tinycc.bash

 git_fetch https://github.com/Tiny-C-Compiler/tinycc-mirror-repository 9a7edb20d3920b73f06a856fa1c82262d165bc6d tinycc
 ./configure && make
 if gcc --version; then
  make CC=gcc test
 else
  make -C tests/tests2
 fi

 bash $CCTESTSCRIPT
}

test_tomlc17() {
 github_tar cktan tomlc17 R251225
 wget -O - https://github.com/toml-lang/toml-test/releases/download/v2.1.0/toml-test-v2.1.0-linux-amd64.gz | gunzip -c > test/stdtest/toml-test
 chmod +x test/stdtest/toml-test
 sed -i 's|^go install.*$||g' test/stdtest/run.sh
 sed -i 's|^toml-test|./&|g' test/stdtest/run.sh
 make CC=$CC test
}

test_toxcore() {
 github_clone TokTok c-toxcore v0.2.21
 libtoolize
 autoreconf -fi
 fix_and_configure
 make check
}

test_toybox() {
 github_tar landley toybox 0.8.13
 replace_line "#define QUIET" "#define QUIET = 0" lib/portability.h
 replace_line "  default n" "  default y" toys/pending/awk.c
 replace_line "  default n" "  default y" toys/pending/expr.c
 replace_line "  default n" "  default y" toys/pending/diff.c
 replace_line "  default n" "  default y" toys/pending/tr.c
 make CC="$CC" HOSTCC="$CC" defconfig
 make CC="$CC" HOSTCC="$CC"
 make CC="$CC" HOSTCC="$CC" tests
}

test_umka() {
 github_tar vtereshkov umka-lang v1.5.5
 sed -i "s|^gcc |$CC |g" tests/lib/build_lib_linux.sh
 sed -i 's|__attribute__((always_inline)) inline||g' src/umka_vm.c
 sed -i 's|/bin/sh|&\nset -e|g' test_linux.sh
 make CC=$CC
 mv build umka_linux
 sh test_linux.sh
}

test_utf8h() {
 git_fetch https://github.com/sheredom/utf8.h 505f9bb638d023468d4f5db279ffbfe0b955949b utf8h
 replace_line "#elif defined(__clang__) || defined(__GNUC__) || defined(__TINYC__)" "#elif 1" test/utest.h
 replace_line "#elif defined(__clang__) || defined(__GNUC__)" "#elif 1" utf8.h
 "$CC" test/main.c -I./ -o run_tests
 ./run_tests
}

test_utillinux() {
 github_tar util-linux util-linux v2.41.3
 replace_line "# define __attribute__(_arg_)" "" include/c.h
 use_stdbit '#include <stdlib.h>' libblkid/src/superblocks/btrfs.c
 sh autogen.sh
 fix_and_configure
 make check TESTS_OPTIONS="--exclude='fadvise/drop fincore/count'"
}

test_vim() {
 github_tar vim vim v9.1.1825
 ./configure
 make && make testtiny
}

test_vlc() {
 github_tar videolan vlc 3.0.23-2
 libtoolize
 autoreconf -fi
 fix_configure
 CFLAGS=-fdisable-visibility ./configure --disable-lua --disable-avcodec --disable-swscale
 touch src/revision.txt
 replace_line "# ifdef __GNUC__" "#if 1" src/modules/bank.c
 make check
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
 url_lz https://ftpmirror.gnu.org/gnu/wget/wget2-2.2.1.tar.lz wget
 fix_and_configure
 make check
}

test_wolfssl() {
 github_tar wolfSSL wolfssl v5.8.4-stable
 cmake_init
 make && ctest
}

test_wuffs() {
 git_fetch https://github.com/google/wuffs 072595ae7df55544874caf5096cd9ef4ecf1bdbb wuffs
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

test_xterm() {
 github_tar ThomasDickey xterm-snapshots xterm-406
 ./configure
 make
 make check
}

test_xxhash() {
 git_fetch https://github.com/Cyan4973/xxHash 66979328cf3f15cecdc61ea58c9f81e6071f8983 xxhash
 make CC=$CC DISPATCH=0 check
}

test_xz() {
 github_tar tukaani-project xz v5.8.2
 cmake_init
 make && make test
}

test_yash() {
 github_tar magicant yash 2.60
 ./configure
 sed -i 's| docs$||g' Makefile
 ${is_CI+ sed -i 's| sigquit[1-8]-p.tst||g' tests/Makefile } # extremely slow in CI
 make test
}

test_yyjson() {
 github_tar ibireme yyjson 0.12.0
 cmake_init -DYYJSON_BUILD_TESTS=ON
 make && ctest
}

test_zlib() {
 github_tar madler zlib v1.3.1.2
 CFLAGS=-fPIC ./configure
 replace_line 'LDSHARED=cc -shared' 'LDSHARED=$(CC) -shared' Makefile
 make test
}

test_zlibng() {
 github_tar zlib-ng zlib-ng 2.3.2
 ./configure --without-optimizations
 make && make test
}

test_zsh() {
 url_xz https://sourceforge.net/projects/zsh/files/zsh-test/5.9.0.3-test/zsh-5.9.0.3-test.tar.xz zsh
 libtoolize
 autoreconf -fi
 ./configure
 sed -i 's/stat.mdd link=no/stat.mdd link=static/g' config.modules # Required to pass D07multibyte.ztst
 ${is_CI+ rm Test/A08time.ztst }
 make && make check
}

test_zstd() {
 github_tar facebook zstd v1.5.7
 replace_line "#if defined(__ELF__) && defined(__GNUC__)" "#if 1" lib/decompress/huf_decompress_amd64.S
 make check
}

build_cairo() {
 gitlab_tar gitlab.freedesktop.org/cairo cairo 1.18.4
 $MUON setup -Dtests=disabled muonbuild
 $MUON -C muonbuild samu -v -j1
}

build_dash() {
 git_fetch https://kernel.googlesource.com/pub/scm/utils/dash/dash 5139d6a27763fc0be386e15634db21e45598b299 dash
 sh autogen.sh
 ./configure
 make

 echo 'echo hello_world' > hello.sh
 src/dash hello.sh | grep ^hello_world$
}

build_ellipsis() {
 git_fetch https://codeberg.org/gustedt/ellipsis b596e4f166c1db0320208498ae30b5cb4a51576b ellipsis
 cd sources
 sed -i "s|'-std=gnu2x'|-std=gnu23 -ffake-always-inline|g" Makefile-options
 make distclean && make

 echo 'Rev(1,2,3)' | ./ellipsis -xc - -D'Rev(X,...)=__VA_TAIL__()__VA_OPT__(,)X' | grep '^3,2,1$'
}

build_erlang() {
 github_tar erlang otp OTP-28.3
 replace_line "#  if defined(__GNUC__)" "#if 1" erts/include/internal/ethread.h
 replace_line "#if defined(__GNUC__)" "#if 1" erts/include/internal/ethread_inline.h
 sed -i 's|-funroll-loops||g' lib/megaco/src/flex/Makefile.in
 CFLAGS='-O -fPIC' ./configure --enable-bootstrap-only
 OTP_TINY_BUILD=true make

cat << EOF > hello.erl
-module(hello).
-export([fn/0]).
fn() -> io:fwrite("hello_world\n").
EOF
 ./bootstrap/bin/erlc  hello.erl
 ./bootstrap/bin/erl -noshell -s hello fn -s init stop | grep ^hello_world$
}

build_foot() {
 codeberg_tar dnkl foot 1.25.0
 $MUON setup muonbuild
 use_stdbit '#include <stdlib.h>' composed.c
 use_stdbit '#include <stdlib.h>' key-binding.c
 use_stdbit '#include <string.h>' sixel.c
 use_stdbit '#include <string.h>' render.c
 use_stdbit '#include <stdlib.h>' subprojects/fcft/fcft.c
 $MUON -C muonbuild samu -j1
}

build_freetype() {
 gitlab_tar gitlab.freedesktop.org/freetype freetype VER-2-14-1
 cmake_init
 make
}

build_gcc() {
 url_bz https://ftpmirror.gnu.org/gnu/gcc/gcc-4.7.4/gcc-4.7.4.tar.bz2 gcc47
 fix_configure
 sed -i 's/^\s*struct ucontext/ucontext_t/g' ./libgcc/config/i386/linux-unwind.h
 mkdir buildonly && cd "$_"
 MAKEINFO=true ../configure --enable-languages=c,c++ --disable-multilib --disable-bootstrap
 make

cat << EOF > hello.cpp
#include <stdio.h>
class H { public: H(){printf("hello");} ~H(){puts("_world");} };
int main() { H h; }
EOF

 ./gcc/xgcc -B./gcc/ -fno-exceptions hello.cpp -o hello
 ./hello | grep ^hello_world$
}

build_glfw() {
 git_fetch https://github.com/glfw/glfw dbadda26835ec5089ef922e6c290bcf58cf12056 glfw
 cmake_init -DGLFW_BUILD_WAYLAND=ON -DGLFW_BUILD_X11=ON
 make
}

build_htop() {
 github_tar htop-dev htop 3.4.1
 sh autogen.sh
 ./configure
 make
}

build_i3() {
 github_tar i3 i3 4.25
 $MUON setup -Dbuildtype=release muonbuild
 $MUON -C muonbuild samu -v -j1
}

build_lacc() {
 local CCTESTSCRIPT=$(dirname $(realpath $0))/cctest_lacc.bash

 git_fetch https://github.com/larmel/lacc 30839843daaff9d87574b5854854c9ee4610cdcd lacc
 ./configure --prefix=$PWD/install
 make && make install

 echo 'int puts(const char*); int main(){ puts("hello_world"); return 0; }' > hello.c
 ./install/bin/lacc hello.c -o hello
 ./hello | grep ^hello_world$

 bash $CCTESTSCRIPT
}

build_libev() {
 url_tar https://dist.schmorp.de/libev/libev-4.33.tar.gz libev
 fix_and_configure
 make
}

build_luajit() {
 git_fetch https://github.com/LuaJIT/LuaJIT 7152e15489d2077cd299ee23e3d51a4c599ab14f luajit
 sed -i 's|-O2 -fomit-frame-pointer|-O2 -DLUAJIT_NO_UNWIND|g' src/Makefile
 replace_line "#if defined(__GNUC__) || defined(__clang__) || defined(__psp2__)" "#if 1" src/lj_def.h
 use_stdbit "#include <stdlib.h>" src/lj_def.h
 make CC=$CC

 echo 'print("hello_world")' > hello.lua
 src/luajit hello.lua | grep ^hello_world$
}

build_lynx() {
 github_tar ThomasDickey lynx-snapshots v2-9-2p
 ./configure
 make
}

build_mg() {
 github_tar troglobit mg v3.7
 sh ./autogen.sh
 fix_and_configure
 make
}

build_mksh() {
 git_fetch https://github.com/MirBSD/mksh 1de4a45108a1a37a8317324ca28096f3f48d49fc mksh
 sh Build.sh

 echo 'echo hello_world' > hello.sh
 ./mksh hello.sh | grep ^hello_world$
}

build_nano() {
 url_xz https://www.nano-editor.org/dist/v8/nano-8.7.tar.xz nano
 ./configure && make
}

build_ncurses() {
 github_tar ThomasDickey ncurses-snapshots v6_6_20260103
 ./configure
 make V=1
}

build_nuklear() {
 github_tar Immediate-Mode-UI Nuklear 4.12.8
 rm -r demo/sfml_opengl*
 find demo/sdl_*/Makefile -exec sed -i 's|std=c89 |std=c89 -DSDL_INLINE=__inline |g' {} +
 find demo/*/sdl/Makefile -exec sed -i 's|std=c89 |std=c89 -DSDL_INLINE=__inline |g' {} +
 sed -i 's|CFLAGS+=|CFLAGS+=-DSDL_DISABLE_IMMINTRIN_H |g' demo/rawfb/sdl/Makefile
 use_stdbit "#include <X11/Xlib.h>" demo/rawfb/x11/nuklear_xlib.h
 sed -i 's|-std=c99|-DSTBI_NO_SIMD -std=c99|g' demo/glfw_opengl2/Makefile
 sed -i 's|-fsanitize=address||g' demo/sdl_vulkan/Makefile
 make CC=$CC demos
}

build_oksh() {
 github_tar ibara oksh oksh-7.8
 sh ./configure --cc=$CC
 make

 echo 'echo hello_world' > hello.sh
 ./oksh hello.sh | grep ^hello_world$
}

build_pcc() {
 local DIR=$PWD/pcc_install
 git_fetch https://github.com/PortableCC/pcc a8781a47e8a4d6ab6fea4b2af285e0786e0bb090 pcc
 ./configure --prefix=$DIR
 make && make install
 cd ../

 git_fetch https://github.com/PortableCC/pcc-libs 832b4519d65226cbfb5be3a2d2a784e7d6085c8b pcc-libs
 ./configure --prefix=$DIR
 make && make install

cat << EOF > hello.c
#include <stdio.h>
int main() { puts("hello_world"); return 0; }
EOF
 $DIR/bin/pcc hello.c -o hello
 ./hello | grep ^hello_world$
}

build_q2rtx() {
 github_clone NVIDIA Q2RTX v1.8.0
 replace_line "#if (defined __GNUC__)" "#if 1" inc/common/intreadwrite.h
 replace_line "#define inline __inline" "" inc/shared/config.h
 sed -i 's|-msse2 -mfpmath=sse||g' CMakeLists.txt
 cmake_init -DUSE_SYSTEM_CURL=on -DUSE_SYSTEM_OPENAL=on -DUSE_SYSTEM_SDL2=on -DUSE_SYSTEM_ZLIB=on -DCONFIG_BUILD_GLSLANG=no \
  -DCMAKE_C_FLAGS='-DSTBI_NO_SIMD -DSTBIR_NO_SIMD -DSDL_DISABLE_IMMINTRIN_H -fms-anon-struct'
 make VERBOSE=1
}

build_quake3e() {
 git_fetch https://github.com/ec-/Quake3e fbf1fff52de31013b7363b1fd22b0d6faf1e8e3d quake3e
 sed -i 's|-pipe||g' Makefile
 sed -i 's|Snd_Memset|Snd_Memset_unused|g' code/unix/linux_snd.c
 make CC=$CC V=1 CFLAGS=-DSDL_DISABLE_IMMINTRIN_H
}

build_raylib_raygui() {
 git_fetch https://github.com/raysan5/raylib 2b48cf67936eace2a4aa58f7e22c34170883f0a8 raylib
 use_stdbit "#include <string.h>" src/external/sinfl.h
 sed -i 's|#define DR_MP3_ONLY_SIMD||g' src/external/dr_mp3.h

 make CC=$CC CUSTOM_CFLAGS='-DSTBI_NO_SIMD -DSTBIR_NO_SIMD' -C src/ PLATFORM=PLATFORM_DESKTOP RAYLIB_LIBTYPE=STATIC -B
 make CC=$CC CUSTOM_CFLAGS='-DSTBI_NO_SIMD -DSTBIR_NO_SIMD' -C examples/ PLATFORM=PLATFORM_DESKTOP -B
 make CC=$CC CUSTOM_CFLAGS='-DSTBI_NO_SIMD -DSTBIR_NO_SIMD' -C src/ PLATFORM=PLATFORM_DESKTOP RAYLIB_LIBTYPE=SHARED -B

 cd ../
 git_fetch https://github.com/raysan5/raygui 0b94b80c92c34c79f8d902f51f68dfc1edacf51b raygui
 make CC=$CC -C examples RAYLIB_PREFIX=../../raylib/ -B
}

build_simplecc() {
 local CCTESTSCRIPT=$(dirname $(realpath $0))/cctest_simplecc.bash

 git_fetch git://git.simple-cc.org/scc 84128eaf37e33fed023f271018b157b7398ad91c simplecc
 make CC="$CC" HOSTCC="$CC" NOCARET=
 echo 'int puts(const char*); int main(){ puts("hello_world"); return 0; }' > hello.c
 ./bin/scc hello.c -o hello
 ./hello | grep ^hello_world$

 bash $CCTESTSCRIPT
}

build_stb() {
 git_fetch https://github.com/nothings/stb f1c79c02822848a9bed4315b12c8c8f3761e1296 stb
 sed -i 's|-DSTB_DIVIDE_TEST|-DSTB_DIVIDE_TEST -DSTBI_NO_SIMD -DSTBIR_NO_SIMD|g' tests/Makefile
 sed -i 's|$(CC) $(INCLUDES) $(CPPFLAGS) -std=c++0x test_cpp_compilation.cpp -lm -lstdc++||g' tests/Makefile
 make -C tests
}

build_tin() {
 github_tar ThomasDickey tin-beta-snapshots v2_6_5-20250919
 ./configure
 make build
}

build_wayst() {
 git_fetch https://github.com/91861/wayst 8868cea5eb0a870aae8374c89092165b4482e3ef wayst
 sed -i 's|-fshort-enums|& -DSTBI_NO_SIMD -DSTBIR_NO_SIMD|g' Makefile
 make mode=quick
}

build_wlroots_sway() {
 github_tar swaywm sway 1.11
 sed -i 's|__PRETTY_FUNCTION__|__FUNCTION__|g' include/log.h

 mkdir subprojects && cd "$_"
 gitlab_tar gitlab.freedesktop.org/wlroots wlroots 0.19.2
 use_stdbit '#include <assert.h>' types/data_device/wlr_data_offer.c
 cd ../../

 $MUON setup muonbuild
 $MUON -C muonbuild samu -v -j1
}

build_yquake2() {
 github_tar yquake2 yquake2 QUAKE2_8_60
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

bootstrap_musl() {
 local ROOT_DIR=$PWD/musl_build

 git_fetch https://github.com/bminor/musl 1b76ff0767d01df72f692806ee5adee13c67ef88 musl
 rm -r src/complex/ include/complex.h
 AR=ar RANLIB=ranlib sh ./configure --target=x86_64-linux-musl --prefix=$ROOT_DIR --includedir=$ROOT_DIR/usr/include --syslibdir=/dev/null
 make install
 cd ../

 git clone $SRC_DIR slimcc-musl && cd slimcc-musl
 mkdir -p $ROOT_DIR/lib/slimcc
 cp -r ./slimcc_headers/include $ROOT_DIR/lib/slimcc/
 sed 's|ROOT_DIR|'\"$ROOT_DIR\"'|g' ./platform/linux-musl-bootstrap.c > platform.c
 make test-all
 cd ../

 git_fetch https://github.com/morr0ne/libc-test f2bac7711bec93467b73bec1465579ea0b8d5071 libc-test
 sed -e 's|-pipe||g' -e 's|-frounding-math||g' config.mak.def > config.mak
 make CC=$PWD/../slimcc-musl/slimcc-stage2
 grep '^FAIL ' src/REPORT | wc -l | grep '^7$'
}

bootstrap_uclibcng() {
 local ROOT_DIR=$PWD/uclibcng_build

 github_tar sabotage-linux kernel-headers v4.19.88-2
 make ARCH=x86_64 prefix= DESTDIR=$PWD/../linuxhdr/ install
 cd ../

 git_fetch https://github.com/wbx-github/uclibc-ng 3dcc84c74ece048b62c992edceab9cce54446f57 uclibcng
 sed -i 's|dN|dM|g' extra/scripts/gen_bits_syscall_h.sh
 sed -i 's|:$(DEVEL_PREFIX)|:$(PREFIX)$(DEVEL_PREFIX)|g' Makefile.in
 sed -i 's|:$(RUNTIME_PREFIX)|:$(PREFIX)$(RUNTIME_PREFIX)|g' Makefile.in
 replace_line '#ifdef\t__GNUC__' '#if 1' include/alloca.h
 replace_line '#if defined __GNUC__ && __GNUC__ >= 2' '#if 1' include/byteswap.h
 replace_line '# if defined __GNUC__ && __GNUC__ >= 2' '#if 1' libc/sysdeps/linux/common/bits/sigset.h
 replace_line '#if __GNUC_PREREQ (3,1)' '#if 1' include/sys/cdefs.h
 replace_line '#if __GNUC__' '#if 1' libc/misc/ftw/ftw.c
 sed -i 's|defined __ICC|1|g' include/libc-symbols.h
 sed -i 's|# define __inline||g' include/sys/cdefs.h
 sed -i 's|#if defined __GNUC__|#ifndef __USE_ISOCXX11\n#include<stdint.h>\ntypedef uint_least16_t char16_t;\ntypedef uint_least32_t char32_t;\n#elif 0|g' include/uchar.h
 sed -i 's|__VERSION__||g' libc/misc/internals/version.c
 perl -i -p0e 's|int rename\(const char \*oldpath, const char \*newpath\)\n\{\n\t_syscall5|static inline _syscall5|g' libc/sysdeps/linux/common/rename.c
 perl -i -p0e 's|\treturn renameat2|int rename\(const char \*oldpath, const char \*newpath\)\n\{\n\treturn renameat2|g' libc/sysdeps/linux/common/rename.c
 sed -i 's|int \*status|void*status|g' libc/sysdeps/linux/common/wait4.c
 perl -i -p0e 's|__asm__ \("mov %rax, %rcx\\n\\t"\s+"neg %rcx"|__asm("neg %%eax;mov %%eax,%0":"=r"(err_no)|g' libc/sysdeps/linux/x86_64/__syscall_error.c

 config=(
  HAS_GLIBC_CUSTOM_STREAMS HAS_HEXADECIMAL_FLOATS HAS_WCHAR # for slimcc build-test
  # for test suite
  USE_NETLINK SUPPORT_AI_ADDRCONFIG HAS_PROGRAM_INVOCATION_NAME HAS_FTW HAS_NFTW HAS_UTMPX HAS_UTMP HAS_ARGP HAS_SHA256_CRYPT_IMPL HAS_SHA512_CRYPT_IMPL SUSV2_LEGACY
  # HAS_LIBICONV HAS_RESOLVER_SUPPORT HAS_LOCALE BUILD_ALL_LOCALE # for disabled tests
  SUSV4_LEGACY # utime.h
 )
 for i in ${config[@]}; do echo UCLIBC_$i=y >> extra/Configs/defconfigs/x86_64/defconfig; done

 make CC=$CC HOSTCC=$CC CC_IPREFIX=$SRC_DIR/slimcc_headers/include KERNEL_HEADERS=$PWD/../linuxhdr/include/ PREFIX=$ROOT_DIR ARCH=x86_64 defconfig
 make CC=$CC HOSTCC=$CC CC_IPREFIX=$SRC_DIR/slimcc_headers/include KERNEL_HEADERS=$PWD/../linuxhdr/include/ PREFIX=$ROOT_DIR ARCH=x86_64 install
 cd ../
 cp -r ./linuxhdr/include/* $ROOT_DIR/usr/x86_64-linux-uclibc/usr/include/

 git clone $SRC_DIR slimcc-uclibcng && cd slimcc-uclibcng
 mkdir -p $ROOT_DIR/usr/x86_64-linux-uclibc/lib/slimcc
 cp -r ./slimcc_headers/include $ROOT_DIR/usr/x86_64-linux-uclibc/lib/slimcc/
 sed 's|ROOT_DIR|'\"$ROOT_DIR/usr/x86_64-linux-uclibc/\"'|g' ./platform/linux-uclibcng-bootstrap.c > platform.c
 rm test/atomic*.c test/tls*.c
 make test-all
 cd ../

 git_fetch https://git.uclibc-ng.org/git/uclibc-ng-test.git a0ccc413f3b5bcb0de429e4bfbedd9a1b32db24e uclibc-ng-test
 sed -i 's| strdupa |strdup|g' test/string/tester.c
 cd test/
 rm -r dlopen nptl pthread tls math # pthread/tls/math not built
 rm -r iconv regex/tst-regexloc.c # skipped in upstream https://downloads.uclibc-ng.org/reports/
 rm -r locale locale-mbwc # not in upstream test log?
 rm inet/tst-ethers*.c mmap/mmap2.c stat/stat-loop256.c # nonexistent /dev/*
 rm misc/tst-msgctl.c misc/tst-semctl.c misc/tst-shmctl.c # permission issues
 rm string/test-ffs.c # nested function

 rm inet/tst-getni2.c inet/tst-res.c # fail with unknwon reason
 cd ../

 make CC=$PWD/../slimcc-uclibcng/slimcc-stage2 test
}

cctest_lcc() {
 local SCRIPT=$(dirname $(realpath $0))/cctest_lcc.bash
 git_fetch https://github.com/drh/lcc 2b5cf358d9aa6759923dd7461f2df7f7f2a28471 lcc
 bash $SCRIPT
}

cctest_pcctests() {
 local SCRIPT=$(dirname $(realpath $0))/cctest_pcc.bash
 git_fetch https://github.com/PortableCC/pcc-tests bce11cbb345b0cf9f702ed57ba09704ea3d153f0 pcc-tests
 bash $SCRIPT
}

cctest_wacc() {
 local SCRIPT=$(dirname $(realpath $0))/cctest_wacc.bash
 git_fetch https://github.com/nlsandler/writing-a-c-compiler-tests 7a5b3589c81c123579659bf52a6743c7d6b9f96e wacc_tests
 bash $SCRIPT
}

# utilities

fix_configure() {
 find . -name 'configure' -exec sed -i 's|^\s*lt_prog_compiler_wl=$|lt_prog_compiler_wl=-Wl,|g' {} +
 find . -name 'configure' -exec sed -i 's|^\s*lt_prog_compiler_pic=$|lt_prog_compiler_pic=-fPIC|g' {} +
 find . -name 'configure' -exec sed -i 's|^\s*lt_prog_compiler_static=$|lt_prog_compiler_static=-static|g' {} +
}

fix_and_configure() {
 fix_configure
 ./configure "$@"
}

cmake_init() {
 mkdir cmakebuild && cd cmakebuild
 cmake ../ -DCMAKE_C_COMPILER=$CC -DCMAKE_PREFIX_PATH=/usr/lib/x86_64-linux-gnu \
  -DCMAKE_C_COMPILE_OPTIONS_PIC=-fPIC -DCMAKE_C_COMPILE_OPTIONS_PIE=-fPIE "$@"
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
  URL=`echo $URL | sed -e 's|mirrors.edge.kernel.org|ftp.gnu.org|g'`
  URL=`echo $URL | sed -e 's|ftpmirror.gnu.org|mirrors.edge.kernel.org|g'`
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
 if git remote add origin "$1"; then
   git fetch --depth 1 origin "$2"
   git checkout FETCH_HEAD
 else
   git checkout -f "$2"
 fi
}

codeberg_tar() {
 get_tar .gz "$2" https://codeberg.org/"$1"/"$2"/archive/"$3".tar.gz
}

github_tar() {
 get_tar .gz "$2" https://github.com/"$1"/"$2"/archive/refs/tags/"$3".tar.gz
}

gitlab_tar() {
 get_tar .bz "$2" https://"$1"/"$2"/-/archive/"$3"/"$2"-"$3".tar.bz2
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

shared_muon() {
 git_fetch https://github.com/muon-build/muon 302626b33b897c0aeaab9129ed266016be0daf2f muon
 cat << EOF >> src/script/runtime/toolchains.meson
toolchain.register_compiler(
    'slimcc',
    inherit: 'posix',
    linker: 'ld',
    detect: func(out str) -> int
        return 'slimcc' in out ? 100 : 0
    endfunc,
    handlers: {
        'print_search_dirs': ['-print-search-dirs'],
    },
)
EOF
 sh ./bootstrap.sh build
 build/muon-bootstrap setup -Dlibpkgconf=disabled ${1:-} build
 build/muon-bootstrap -C build samu
}

ci_muon() {
 shared_muon -Dmeson-docs=disabled -Dmeson-tests=disabled
}

ci_libtool() {
 url_xz https://ftpmirror.gnu.org/gnu/libtool/libtool-2.5.4.tar.xz __libtool
 fix_and_configure
 make install -j2
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

 sed -i 's|__atomic_exchange_n|atomic_exchange_explicit|g' "$2"

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
