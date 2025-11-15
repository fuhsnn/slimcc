This is a fork of [Rui Ueyama's chibicc](https://github.com/rui314/chibicc) with [fixes](https://github.com/fuhsnn/slimcc/issues?q=is%3Aissue+is%3Aclosed+label%3Aupstream-chibicc) and enhancements, including:
 - C99 features: VLA parameters, VLA de-allocation, K&R old-style functions.
 - C11 features: `_Static_assert()`, over-aligned locals, `_Generic` with qualifiers.
 - C23 features: `constexpr`, `enum:T`, `#embed`, `auto`, `_BitInt`, `nullptr`, `={}`
 - C2Y features: named loops, `if` declarations, `_Countof`.
 - `defer/_Defer`: enable non-unglified form with `-fdefer-ts` option or `#include <stddefer.h>`.
 - WG14 proposals:  `__VA_TAIL__`, `#def #enddef`.
 - GNU features: inline assembly, commonly used `__attribute__`'s.
 - MSVC features: `#pragma pack`, anonymous struct.
 - Basic codegen optimizations (close to GCC/Clang at `-O0`).

## What can it build?
Compiling real C projects with slimcc has been a major focus, the [CI workflow](https://github.com/fuhsnn/slimcc/blob/main/.github/workflows/linux_thirdparty.yml) now regularly build-test a healthy collection of familiar names, for bragging rights:
 - Core system commands (GNU utils, busybox, toybox, bash, zsh)
 - Command-line tools (curl, git, jq, wget, rsync, apk, pacman)
 - Programming languages (Python, Go, Lua, Perl, Ruby, PHP, Ocaml, Erlang, C3, Zig)
 - Gamedev/graphics libraries (SDL3, glfw, stb, raylib, sokol)
 - Critical infrastructure softwares (Redis, Valkey, Nginx, PostgreSQL)

[slimcc-musl-bootstrap](https://github.com/fuhsnn/slimcc-musl-bootstrap) is a minimal statically-linked chroot environment with `musl`, `binutils`, `toybox` entirely compiled with slimcc.

## Building and Using
 - Recent GNU binutils is required.
 - Find a relevant preset in `platform/`, copy/symlink to `platform.c` or create one, hit `make`.
 - On recent glibc Linux, this should just work:
 ```
 ln -s platform/linux-glibc-generic.c platform.c
 make
 ```
 - For simplicity, built-in headers are searched relative to the binary, it's intended to be executed in-tree like:
 ```
 ~/slimcc/slimcc *.c
 CC=~/slimcc/slimcc ./configure
 cmake -DCMAKE_C_COMPILER=/home/username/slimcc/slimcc
 ```
 - For installing/symlinking/packaging, please edit `platform.c` with absolute paths.
 - `cc scripts/amalgamation.c` builds the project as a single translation unit.
 - `make slimcc-asan/test-asan` build and run tests with address sanitizer, recommended if you are hacking the source.
 - `make slimcc-filc/test-filc` build and run tests with [Fil-C](https://github.com/pizlonator/fil-c/releases), env `FILC` is used.

## Compatibility
 - Autoconf based projects might fail to build due to `-fPIC` or `-Wl` not being passed. If there is a block in `./configure` that looks like below, append `-Wl,`, `-fPIC`, `-static` respectively and re-configure: 
```
  lt_prog_compiler_wl=
lt_prog_compiler_pic=
lt_prog_compiler_static=
```
 - CMake: these extra flags might be helpful:
 - - `-DCMAKE_PREFIX_PATH`: help `find_package()` find installed libraries.
 - - `-DCMAKE_C_COMPILE_OPTIONS_PIC=-fPIC`
 - - `-DCMAKE_C_COMPILE_OPTIONS_PIE=-fPIE`
 - Meson: see https://github.com/mesonbuild/meson/issues/5406. Try [Muon](https://github.com/muon-build/muon).
 - [Muon](https://github.com/muon-build/muon): slimcc works ok under Muon's posix cc mode, for better compatibility, register slimcc in Muon's `src/script/runtime/toolchains.meson` with:
```
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
```
 - Projects with deep GCC assumptions might actually work if you carefully opt in/out of `#if __GNUC__` blocks, see the [CI test scripts](https://github.com/fuhsnn/slimcc/blob/main/scripts/linux_thirdparty.bash) for a taste of hacks that can be done.

## Compatibility options
 - `-fms-anon-struct`: Enable declaring anonymous struct member with tags like GCC's `-fms-extensions` or `-fplan9-extensions`.
 - `-fdisable-visibility`: Some projects adopt an idiom that globally set `-fvisibility=hidden` then selectively export public API with `__attribute__((visibility("default")))`. slimcc support both, yet a problem arises when the former is probed by build system while the latter hardcoded in `#if __GNUC__` blocks regardless of the build system's decision. The option simply disable both features to prevent the conflict.
 - `-ffake-always-inline`: slimcc currently don't inline functions, but some projects' usage of GCC `always_inline` on non-static inline functions would cause missing definition at link time. This option turns these definitions into `static inline`'s to work around compile error.

## How optimized is the compiler?
The coding style is highly influenced by `chibicc`, which means in the eyes of C veterans there might be all kinds of inefficiencies in the name of readability. While micro-optimizing is fun, I only commit changes that noticeably improve fully-optimized builds.

As such, optimization flags and custom allocators have great impact on its performance, `make slimcc-(lto/lto-mi/lto-je)` is provided for optimized builds. It's usually faster than Clang/GCC for common tasks, but only by doing less internally.

## How portable is the compiler?
 - It's written in C99 with several POSIX 2008 functions.
 - Little endian is assumed.
 - `tcc(TinyCC)`, `kefir` and `pcc` should be able to compile it, `chibicc` needs [a few patches](https://github.com/fuhsnn/chibicc/tree/minimum-fix).
 - `cproc`(QBE), `cparser`(libFirm) have incomplete support for `long double`, adding `-DNO_LONG_DOUBLE` to the flags allow these otherwise capable compilers to bootstrap slimcc. `long double` constants would be broken so don't use the build for anything other than self-hosting slimcc.
