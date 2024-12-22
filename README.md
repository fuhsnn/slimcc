This is a fork of [Rui Ueyama's chibicc](https://github.com/rui314/chibicc) with [fixes](https://github.com/fuhsnn/slimcc/issues?q=is%3Aissue+is%3Aclosed+label%3Aupstream-chibicc) and improvements, including:
 - C99 features: VLA parameters, VLA de-allocation, K&R old-style functions.
 - C11 features: `_Static_assert()`, over-aligned locals, `_Generic` with qualifiers.
 - C23 features: `constexpr`, `enum:T{}`, `#embed`, `auto` type-inference, etc.
 - C2Y/TS features: `defer`(as `_Defer`), `__VA_TAIL__`
 - GNU features: inline asm, `cleanup`
 - Basic codegen optimizations: const folding, reg-alloc for temporaries, instruction selection.

## Project goal

 - Compile correct code correctly.
 - Code readability.
 - Build real world projects, test with their suites.
 - Implement GNU extensions and C23 features if doable.
 - Match GCC / Clang's behavior to improve compatibility with existing code in the wild.

## Building & Running

 - Should just work on recent glibc-based (2.28+) x86-64 Linux. Also see [porting](#porting)
 - Test script needs `bash`; depending on the distro, `file` and `glibc-static` packages may be required.
```
git clone --depth 1 https://github.com/fuhsnn/slimcc
cd slimcc
make test-stage2 -j4
```
Run it in base directory like `CC=~/slimcc/slimcc`, since internal header path is hardcoded for simplicity.

## What can it build?

`slimcc` should be able to compile most C89 to C11 projects not requiring optional or compiler-specific features.

For testing, a CI workflow is set up to not only build real C codebases in a container with `slimcc` as the sole C compiler (no `cc`/`gcc`/`cpp`), but also expect 100% pass rate from their test-suites. The lineup includes the likes of CPython, Curl, Git, jq, OpenSSH, OpenSSL, PHP, Perl, PostgreSQL, Sqlite, Toybox, and Zstd; please have a look at the [workflow](https://github.com/fuhsnn/slimcc/blob/main/.github/workflows/linux_thirdparty.yml), [dockerfile](https://github.com/fuhsnn/slimcc/blob/main/scripts/debian_asan.Dockerfile), and [build instructions](https://github.com/fuhsnn/slimcc/blob/main/scripts/linux_thirdparty.bash) to verify this claim.

`musl` is also buildable sans complex numbers, it's a goal to set up a self-hosted `slimcc-musl` environment.

## Can it pass `csmith`?

1M tests were run with `--no-packed-struct` flag, all issues found were fixed.

## How optimized are the generated binaries?

Binary size is roughly among TinyCC's and GCC/Clang at `-O0`, no claims about speed.

## How optimized is the compiler itself?

The coding style is greatly influenced by `chibicc`, which means in the eyes of C veterans there might be all kinds of inefficiencies in the name of readability. While micro-optimizing is fun, I only commit changes that noticeably improve `-flto=auto -march=native -lmimalloc` builds.

Previously all allocations were leaked, that led to pretty bad RAM usage for large translation units (Zig bootstrap) or macro expansion tricks (Metalang99). This was remedied with arena allocation and tracing linked lists, peak consumption brought down from over 16 GB to about 3 GB.

## Can it be built with another small C compiler?

 - TinyCC, kefir, vanilla chibicc: can build with `cc *.c`, `make test` might fail at ABI tests.
 - pcc: has a bug parsing escaped sequence in a comment, otherwise can build.
 - cproc: lack long double support.
 - mir, xcc: missing header.

## Porting

musl Linux and BSDs should be doable. Check hard-coded paths in `main.c`, and pre-defined macros in `preprocessor.c`.

The [`widcc`](https://github.com/fuhsnn/widcc) branch is a less noisy codebase than `slimcc` that should be more appealing for developers to hack on.
