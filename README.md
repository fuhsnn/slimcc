This is a fork of [Rui Ueyama's chibicc](https://github.com/rui314/chibicc) with [fixes](https://github.com/fuhsnn/slimcc/issues?q=is%3Aissue+is%3Aclosed+label%3Aupstream-chibicc) and improvements, including:
 - C99 features: VLA parameters, VLA de-allocation, K&R old-style functions.
 - C11 features: `_Static_assert()`, over-aligned locals, `_Generic` with qualifiers.
 - C23 features: `constexpr`, `enum:T{}`, `#embed`, `auto` type-inference, etc.
 - C2y features: labeled loop/switch, if/switch declaration
 - TS features: `defer`(enable with `-fdefer-ts`), `__VA_TAIL__`
 - GNU features: inline assembly, symbol attributes, `cleanup`, `cons/destructor`
 - Basic codegen optimizations: const folding, reg-alloc for temporaries, instruction selection.

## Project goals

 - Compile correct code correctly.
 - Code readability.
 - Build real world projects, test with their suites.
 - Implement GNU extensions and features from newer C standards if doable.
 - Match GCC/Clang's behavior even if non-standard for better compatibility with existing code in the wild.

## Building and Using
 - Recent GNU binutils is required.
 - Find a relevant preset in `platform/`, copy/symlink to `platform.c` or write one yourself, hit `make`.
 - For recent glibc-based Linux, this should just work:
 ```
 ln -s platform/linux-glibc-generic.c platform.c
 make
 ```
 - Default build finds builtin headers by relative path and is expected to be executed in source directory, so use it like:
 - - `~/slimcc/slimcc *.c`, or `CC=~/slimcc/slimcc ./configure`
 - For installing/symlinking/packaging, please update `platform.c` with absolute paths.
 - `make slimcc-asan/test-asan` build and run tests with address sanitizer, recommended if you are hacking the source.
 - Optimization flags and custom allocators are quite impactful on the compiler's performance; try `make lto/lto-mi/lto-je` for a quick taste.
 - For a fully bootstrapped musl environment, checkout [slimcc-musl-bootstrap](https://github.com/fuhsnn/slimcc-musl-bootstrap).

## What can it build?

`slimcc` is compatible with posix `c99` and common GCC/Clang options, most C89 to C11 projects not requiring optional or compiler-specific features should work.

For testing, a CI workflow is set up to not only build real C codebases in a container with `slimcc` as the sole C compiler (no `cc`/`gcc`/`cpp`), but also expect 100% pass rate from their test-suites. The lineup includes the likes of CPython, Curl, Git, jq, Ocaml, PHP, Perl, PostgreSQL, Sqlite, Toybox, and Zstd; please have a look at the [workflow](https://github.com/fuhsnn/slimcc/blob/main/.github/workflows/linux_thirdparty.yml), [dockerfile](https://github.com/fuhsnn/slimcc/blob/main/scripts/debian_asan.Dockerfile), and [build instructions](https://github.com/fuhsnn/slimcc/blob/main/scripts/linux_thirdparty.bash) to verify this claim.

## Compatibility with build systems
 - `autoconf` based builds sometimes miss out `-fPIC` or `-Wl,` options, causing failures, if there is a block in `./configure` that looks like below, append `-Wl,`, `-fPIC`, `-static` respectively and re-configure: 
```
  lt_prog_compiler_wl=
lt_prog_compiler_pic=
lt_prog_compiler_static=
```
 - If it's just `-fPIC`, configure with `CFLAGS=-fPIC`, if cmake, `-DCMAKE_C_FLAGS=-fPIC`.
 - `cmake` `find_package()` might fail to find installed libraries for slimcc, help it with `-DCMAKE_PREFIX_PATH`.
 - `meson`: see https://github.com/mesonbuild/meson/issues/5406. Try `muon` or hope for slimcc to join the club.

## How optimized are the generated binaries?

Binary size is roughly among TinyCC's and GCC/Clang at `-O0`, no claims about speed.

## How optimized is the compiler itself?

The coding style is greatly influenced by `chibicc`, which means in the eyes of C veterans there might be all kinds of inefficiencies in the name of readability. While micro-optimizing is fun, I only commit changes that noticeably improve `-flto=auto -march=native -lmimalloc` builds.

Previously all allocations were leaked, that led to pretty bad RAM usage for large translation units (Zig bootstrap) or macro expansion tricks (Metalang99). This was remedied with arena allocation and mark-and-sweep of linked lists.

## Can it be built with another small C compiler?

 - TinyCC, kefir, vanilla chibicc: can build with `cc *.c`, `make test` might fail at ABI tests.
 - pcc: has a bug parsing escaped sequence in a comment, otherwise can build.
 - cproc, simple-cc: lack long double support (it's a QBE limitation).
 - mir, xcc: missing header.
