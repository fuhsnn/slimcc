set -eu
set -o pipefail

# utilities

fix_configure() {
 sed -i 's/^\s*lt_prog_compiler_wl=$/lt_prog_compiler_wl=-Wl,/g' "$1"
 sed -i 's/^\s*lt_prog_compiler_pic=$/lt_prog_compiler_pic=-fPIC/g' "$1"
 sed -i 's/^\s*lt_prog_compiler_static=$/lt_prog_compiler_static=-static/g' "$1"
}

replace_line() {
 sed -i s/^"$1"$/"$2"/g "$3"
}

github_tar() {
  mkdir -p "$2"
  curl -fL https://github.com/"$1"/"$2"/archive/refs/tags/"$3".tar.gz | tar xz -C "$2" --strip-components=1
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

url_tar() {
  mkdir -p "$2"
  curl -fL "$1" | tar xz -C "$2" --strip-components=1
  cd "$2"
}

install_libtool() {
 url_tar https://ftp.gnu.org/gnu/libtool/libtool-2.5.4.tar.gz __libtool
 fix_configure ./configure
 fix_configure libltdl/configure
 ./configure
 make -j2 install
 cd ../ && rm -rf __libtool
}

# tests

test_cello() {
 git_fetch https://github.com/orangeduck/Cello 61ee5c3d9bca98fd68af575e9704f5f02533ae26 cello
 make check
}

test_curl() {
 github_tar curl curl curl-8_11_1
 sed -i 's/^if(MSVC OR CMAKE_COMPILER_IS_GNUCC OR CMAKE_C_COMPILER_ID MATCHES "Clang")$/if (TRUE)/g' tests/CMakeLists.txt
 mkdir build && cd "$_"
 cmake ../ -DCMAKE_C_FLAGS=-fPIC
 make && make test-quiet
}

test_doom() {
 git_fetch https://github.com/Daivuk/PureDOOM 48376ddd6bbdb70085dab91feb1c6ceef80fa9b7 puredoom
 mkdir -p examples/Tests/build && cd "$_"
 replace_line "project(pd_tests)" "project(pd_tests C)" ../CMakeLists.txt
 cmake ../ && make
 cd ../../../ && examples/Tests/build/pd_tests
}

test_git() {
 github_tar git git v2.49.0
 make CC="$CC" test -j2
}

test_libpng() {
 github_tar pnggroup libpng v1.6.47
 fix_configure ./configure
 ./configure
 make test
}

test_libuev() {
 github_tar troglobit libuev v2.4.1
 libtoolize
 autoreconf -fi
 fix_configure ./configure
 ./configure
 make check
}

test_lua() {
 url_tar https://lua.org/ftp/lua-5.4.7.tar.gz lua
 cd src && make CC="$CC" linux-readline
 url_tar https://www.lua.org/tests/lua-5.4.7-tests.tar.gz luatests
 cd libs && make CC="$CC" && cd ../
 ../lua -e"_port=true" all.lua # assertion at files.lua:84 fail on CI
}

test_metalang99() {
 github_tar hirrolot datatype99 v1.6.5
 sh scripts/test-all.sh
 github_tar hirrolot interface99 v1.0.2
 sh scripts/test-all.sh
 github_tar hirrolot metalang99 v1.13.5
 sh scripts/test-all.sh
}

test_oniguruma_jq() {
 git_fetch https://github.com/jqlang/jq 96e8d893c10ed2f7656ccb8cfa39a9a291663a7e jq
 cd modules/

 git_fetch https://github.com/kkos/oniguruma e0a8f04615ecc5bbae0bec8b007ecb4816966ae5 oniguruma
 libtoolize
 autoreconf -fi
 fix_configure ./configure
 CFLAGS=-fenable-universal-char ./configure
 make check

 cd ../../
 libtoolize
 autoreconf -fi
 fix_configure ./configure
 ./configure
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
 github_tar openssl openssl openssl-3.4.0
 replace_line "#if !defined(__DJGPP__)" "#if 0" test/rsa_complex.c
 ./Configure
 make test -j2
}

test_perl() {
 github_tar perl perl5 v5.40.0
 sed -i "187i push(@file, '/usr/include/asm-generic/errno-base.h');" ext/Errno/Errno_pm.PL
 sed -i "188i push(@file, '/usr/include/asm-generic/errno.h');" ext/Errno/Errno_pm.PL
 # https://github.com/Perl/perl5/blob/80f266d3fc15255d56d2bebebceba52614f04943/.github/workflows/testsuite.yml#L810
 export NO_NETWORK_TESTING=1
 ./Configure -des -Dcc="$CC" -Accflags=-fPIC -Alibs="-lpthread -ldl -lm -lcrypt -lutil -lc" \
   -Alibpth="/usr/local/lib /lib /usr/lib /lib64 /usr/lib64 /lib/x86_64-linux-gnu /usr/lib/x86_64-linux-gnu"
 make -j4 test_prep && HARNESS_OPTIONS=j4 make test_harness
}

test_php() {
 github_tar php php-src php-8.1.31
 replace_line "#elif (defined(__i386__) || defined(__x86_64__)) && defined(__GNUC__)" "#elif 1" Zend/zend_multiply.h
 replace_line "#elif defined(__GNUC__) || defined(__INTEL_COMPILER) || defined(__SUNPRO_C)" "#elif 1" ext/pcre/pcre2lib/sljit/sljitNativeX86_common.c

 # don't work in CI https://github.com/php/php-src/blob/17187c4646f3293e1de8df3f26d56978d45186d6/.github/actions/test-linux/action.yml#L40
 export SKIP_IO_CAPTURE_TESTS=1

 ./buildconf --force
 fix_configure ./configure
 ./configure --disable-opcache
 make test NO_INTERACTION=1
}

test_postgres() {
 github_tar postgres postgres REL_17_4
 replace_line "#if defined(__GNUC__) || defined(__INTEL_COMPILER)" "#if 1" src/include/storage/s_lock.h
 replace_line "#if (defined(__x86_64__) || defined(_M_AMD64))" "#if 0" src/include/port/simd.h
 ./configure && make && make check
}

test_python() {
 github_tar python cpython v3.13.2
 replace_line "#if defined(__GNUC__) || defined(__clang__)" "#if 1" Include/pyport.h
 replace_line "#if defined(__linux__) && (defined(__GNUC__) || defined(__clang__))" "#if 1" Python/pylifecycle.c
 ./configure && make

 rm Lib/test/test_ctypes/test_dlerror.py #https://github.com/python/cpython/issues/127626
 skip_tests=(
  # don't work in CI https://github.com/python/cpython/blob/6d3b5206cfaf5a85c128b671b1d9527ed553c930/.github/workflows/build.yml#L408
  test_asyncio test_socket
 )
 ./python -m test -j4 --exclude "${skip_tests[@]}"
}

test_qbe_hare() {
 git_fetch git://c9x.me/qbe.git 90050202f57b22243f5d3dd434a81df2f89de9ed qbe
 make CC="$CC" check
 url_tar https://git.sr.ht/~sircmpwn/harec/archive/0.24.2.tar.gz harec
 mv configs/linux.mk config.mk
 make CC="$CC" QBE=../qbe check
}

test_sqlite() {
 github_tar sqlite sqlite version-3.49.1
 CC_FOR_BUILD="$CC" CFLAGS=-D_GNU_SOURCE ./configure
 make test
}

test_tcl() {
 github_tar tcltk tcl core-9-0-1
 ./unix/configure
 rm ./tests/socket.test # fails under su
 make test | tee __testlog
 grep -P -q '^all.tcl.*Failed\t0$' __testlog
}

test_tinycc() {
 git_fetch https://github.com/TinyCC/tinycc 8c4e67380e54296a6a1f9d242b7fc4bf9f16fddb tinycc
 ./configure && make && cd tests/tests2/ && make
}

test_toxcore() {
 github_clone TokTok c-toxcore v0.2.20
 libtoolize
 autoreconf -fi
 fix_configure ./configure
 ./configure
 make check
}

test_toybox() {
 github_tar landley toybox 0.8.12
 replace_line "#define QUIET" "#define QUIET = 0" lib/portability.h
 make CC="$CC" HOSTCC="$CC" defconfig
 make CC="$CC" HOSTCC="$CC"
 make CC="$CC" HOSTCC="$CC" tests
}

test_vim() {
 github_tar vim vim v9.1.1200
 ./configure
 make && make testtiny
}

test_zlib() {
 github_tar madler zlib v1.3.1
 ./configure
 make test
}

test_zstd() {
 github_tar facebook zstd v1.5.7
 replace_line "#if defined(__ELF__) && defined(__GNUC__)" "#if 1" lib/decompress/huf_decompress_amd64.S
 make check
}

build_gcc() {
 url_tar https://ftp.gnu.org/gnu/gcc/gcc-4.7.4/gcc-4.7.4.tar.gz gcc47
 export -f fix_configure
 find . -name 'configure' -exec bash -c 'fix_configure "$0"' {} \;
 sed -i 's/^\s*struct ucontext/ucontext_t/g' ./libgcc/config/i386/linux-unwind.h
 mkdir buildonly && cd "$_"
 export MAKEINFO=missing
 ../configure --enable-languages=c,c++ --disable-multilib --disable-bootstrap
 make
}

build_musl() {
 github_tar bminor musl v1.2.5
 rm -rf src/complex/
 AR=ar RANLIB=ranlib ../musl/configure --target=x86_64-linux-musl
 make
}

build_nano() {
 url_tar https://www.nano-editor.org/dist/v8/nano-8.3.tar.gz nano
 ./configure && make
}

build_sdl() {
 github_tar libsdl-org SDL release-2.30.9
 fix_configure ./configure
 replace_line "#elif defined(__GNUC__) && (defined(__i386__) || defined(__x86_64__))" "#elif 1" src/atomic/SDL_spinlock.c
 ./configure
 make
}

build_zig() {
 github_clone fuhsnn zig wasm2c-stack
 sed -i 's/stack-size=0x1000000/stack-size=0x4000000/g' bootstrap.c
 cp lib/zig.h stage1/
 "$CC" bootstrap.c -o _bootstrap
 ./_bootstrap
 ./zig2 test --show-builtin
}

# run a test

if [[ $(type -t "$1") != function ]]; then
  echo 'expected a test name'
  exit 1
fi

$1
