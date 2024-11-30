This is a fork of [Rui Ueyama's chibicc](https://github.com/rui314/chibicc) with [fixes](https://github.com/fuhsnn/slimcc/issues?q=is%3Aissue+is%3Aclosed+label%3Aupstream-chibicc) and improvements, including:
 - C99 features: VLA parameters, VLA de-allocation, K&R old-style functions.
 - C11 features: `_Static_assert()`, over-aligned locals, `_Generic` with qualifiers.
 - C23 features: `constexpr`, `enum:T{}`, `#embed`, `auto` type-inference, etc.
 - C2Y/TS features: `defer`(as `_Defer`), `__VA_TAIL__`
 - GNU features: inline asm, `cleanup`
 - Basic optimizations: const folding, reg-alloc for temporaries, instruction selection.

## Project goal

 - Compile correct code correctly.
 - Code readability.
 - Build real world projects, test with their suites.
 - Implement GNU extensions and C23 features if doable.
 - Match GCC / Clang's behavior to improve compatibility with existing code in the wild.

## Building & Running

 - Should just work on recent glibc-based (2.28+) x86-64 Linux. Also see [porting](#porting)
 - Test script needs `bash`; depending on the distro, `file` and/or `glibc-static` packages may be required.
```
git clone --depth 1 https://github.com/fuhsnn/slimcc
cd slimcc
make test-stage2 -j4
```
Run it in base directory like `CC=~/slimcc/slimcc`, since internal header path is hardcoded for simplicity.

## What can it build?
`slimcc` should be able to compile most C89 to C11 projects not requiring optional or compiler-specific features.

A CI workflow is set up to `make test` real-world projects in a Debian container with `cc` `gcc` `cpp` uninstalled. The lineup includes Curl, Git, OpenSSL, PHP, Perl, Python, PostgreSQL, and continues to grow. Please have a look a the [workflow](https://github.com/fuhsnn/slimcc/blob/main/.github/workflows/linux_thirdparty.yml), [dockerfile](https://github.com/fuhsnn/slimcc/blob/main/scripts/debian_asan.Dockerfile), and [build instructions](https://github.com/fuhsnn/slimcc/blob/main/scripts/linux_thirdparty.bash) to verify this claim.

`musl` is also buildable sans complex numbers, it's a goal to set up a self-hosted `slimcc-musl` environment.

## Can it pass `csmith`?

1M tests were run with `--no-packed-struct` flag, all issues found were fixed.

## How optimized are the generated binaries?

Binary size is roughly among TinyCC's and GCC/Clang at `-O0`, no claims about speed.

## How optimized is the compiler itself?

The compiler leaks all allocations, it gets bad for large translation units, I'm looking for a clean way to remedy this.

## Can it be built with another small C compiler?

Here's some I've tried:
 - TinyCC, kefir, vanilla chibicc: can build with `cc *.c`, `make test` will fail but not big deal.
 - pcc: has a bug parsing escaped sequence in a comment, otherwise can build.
 - cproc: can't build, lacks long double support.
 - mir, xcc: missing header.

## Porting

musl Linux and BSDs should be doable. Check hard-coded paths in `main.c`, and pre-defined macros in `preprocessor.c`.

The [`widcc`](https://github.com/fuhsnn/widcc) branch is a less noisy codebase than `slimcc` that should be more appealing for developers to hack on.
