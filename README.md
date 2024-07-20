This is a fork of [Rui Ueyama's chibicc](https://github.com/rui314/chibicc) with [fixes](https://github.com/fuhsnn/slimcc/issues?q=is%3Aissue+is%3Aclosed+label%3Aupstream) and [improvements](#changes-over-chibicc).

# Project goal
 - Compile correct code correctly.
 - Code readability.
 - Build real world projects, test with their suites.
 - Implement GNU extensions and C23 features if doable.
 - Match GCC / Clang's behavior when possible.

# Building & Running
 - Should just work on recent glibc-based (2.28+) x86-64 Linux. Also see [porting](#porting)
 - Test script needs `bash`; depending on the distro, `file` and/or `glibc-static` packages may be required.
```
git clone --depth 1 https://github.com/fuhsnn/slimcc
cd slimcc
make test-stage2 -j
```
Run it in base directory like `CC=~/slimcc/slimcc`.

# What can it build?
I regularly attempt to keep up with latest versions of Curl, Git, Python, PostgreSQL, Sqlite, Vim etc.

Check out [widcc's list](https://github.com/fuhsnn/widcc?tab=readme-ov-file#building-real-world-projects) for detailed build scripts.

# Can it pass `csmith`?
1M tests were run with `--no-packed-struct` flag, all issues found were fixed.

# How optimized are the generated binaries?
The codegen strategy is matching AST nodes to hard-coded assembly snippets and glue them with data movements.

Some optimizations can be made with this by pattern-matching to smarter alternatives, I apply them when effective and don't mess up readability.

Compare size of chibicc binary built with several compilers:
```
      text       data        bss      total filename
    112363      43987        616     156966 gcc_O0_build
    113867      43156        504     157527 clang_O0_build
    138747      31913        456     170972 slimcc_build
    144896      29536        440     174872 tcc_build
    263659      41163        456     305278 chibicc_build
```
In general, code size is on par with TinyCC but execute 30% slower. Much work to be done!

# Changes over chibicc
 - Written in C99
 - New algorithm for macro expansion, better support for [deep recursion tricks](https://stackoverflow.com/a/70342272)
 - `setjmp.h` compatibility
 - Support `_Static_assert()`, `__has_include`
 - Support C23 `constexpr` and `auto` type inference
 - Support variably-modified types in function prototype (aka VLA parameters)
 - Support VLA auto-deallocation
 - Basic stack reuse optimization
 - Basic register allocation of temporaries
 - Constant folding and basic strength reduction
 - More involved x86-64 instruction selection

# Porting
musl linux and BSDs should be doable. Check hard-coded paths in `main.c`, and pre-defined macros in `preprocessor.c`.
The biggest obstacle would be GNU inline assembly in some headers. Which is not supported yet.

The [`widcc`](https://github.com/fuhsnn/widcc) branch is a less noisy codebase than `slimcc` that should be more appealing for developers to hack on.
