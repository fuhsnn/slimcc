This is a fork of [Rui Ueyama's chibicc](https://github.com/rui314/chibicc) with [fixes](https://github.com/fuhsnn/slimcc/issues?q=is%3Aissue+is%3Aclosed+label%3Aupstream-chibicc) and enhancements, including:
 - C99 features: VLA parameters, VLA de-allocation, K&R old-style functions.
 - C11 features: `_Static_assert()`, over-aligned locals, `_Generic` with qualifiers.
 - C23 features: `constexpr`, `enum:T`, `#embed`, `auto`, `_BitInt`, `nullptr`, `={}`
 - C2Y features: named loops, `if` declarations, `_Countof`.
 - `defer`: enable with `-fdefer-ts`, or use `_Defer`.
 - WG14 proposals:  `__VA_TAIL__`, `#def #enddef`.
 - GNU features: inline assembly, commonly used `__attribute__`'s.
 - MSVC features: `#pragma pack`, anonymous struct.
 - Basic codegen optimizations (close to GCC/Clang at `-O0`).

## What can it build?
Compiling real C projects with slimcc has been a major focus, the [CI workflow](https://github.com/fuhsnn/slimcc/blob/main/.github/workflows/linux_thirdparty.yml) now regularly build-test a healthy collection of familiar names, for bragging rights:
 - Core system commands (GNU coreutils, busybox, toybox)
 - Developer tools (binutils, Bash, Curl, Git, jq, wget, Zsh)
 - Programming languages (Python, Go, Lua, Perl, Ruby, PHP, Ocaml, Zig)
 - Game dev libraries (SDL3, stb, raylib, sokol)
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
 - `make slimcc-asan/test-asan` build and run tests with address sanitizer, recommended if you are hacking the source.

## Compatibility
 - Autoconf based projects might fail to build due to `-fPIC` or `-Wl` not being passed. If there is a block in `./configure` that looks like below, append `-Wl,`, `-fPIC`, `-static` respectively and re-configure: 
```
  lt_prog_compiler_wl=
lt_prog_compiler_pic=
lt_prog_compiler_static=
```
 - It might be necessary to configure with `CFLAGS=-fPIC`, for CMake, `-DCMAKE_C_FLAGS=-fPIC`.
 - CMake `find_package()` might fail to find installed libraries, help it with `-DCMAKE_PREFIX_PATH`.
 - Meson: see https://github.com/mesonbuild/meson/issues/5406. Try [Muon](https://github.com/muon-build/muon) or hope for slimcc to join the club.
 - Projects with deep GCC assumptions might actually work if you carefully opt in/out of `#if __GNUC__` blocks, see the [CI test scripts](https://github.com/fuhsnn/slimcc/blob/main/scripts/linux_thirdparty.bash) for a taste of hacks that can be done.
 - Some projects adopt an idiom that globally set `-fvisibility=hidden` then selectively export public API with `__attribute__((visibility("default")))`. slimcc support both, yet a problem arises when the former is probed by build system while the latter hardcoded in `#if __GNUC__` blocks regardless of the build system's decision. An option `-fdisable-visibility` is added to simply disable both features to prevent the conflict.

## How optimized is the compiler?
The coding style is highly influenced by `chibicc`, which means in the eyes of C veterans there might be all kinds of inefficiencies in the name of readability. While micro-optimizing is fun, I only commit changes that noticeably improve fully-optimized builds.

As such, optimization flags and custom allocators have great impact on its performance, `make lto/lto-mi/lto-je` is provided for optimized builds. It's usually faster than Clang/GCC for common tasks, but only by doing way less internally.

## How portable is the compiler?
 - It's written in C99 with several POSIX 2008 functions.
 - `TinyCC`(mob branch), `kefir`, `pcc`, and vanilla `chibicc` should be able to compile it.
 - `cproc` is blocked by the lack of `long double` support in its backend, `QBE`.
