# slimcc: A C11 compiler
This is a fork of [Rui Ueyama's chibicc](https://github.com/rui314/chibicc) that aims to fill in some missing features and fixes. 

Although not adopting chibicc's educational commit style, the goal is to be consistent with its readability-first attitude and minimalistic approach; hopefully readers and fork-mates will find something useful.

# Changes over chibicc
 - written in C99
 - bug & conformance fixes (see [closed issues](https://github.com/fuhsnn/slimcc/issues?q=is%3Aissue+is%3Aclosed))
 - less RAM consumption during preprocessing
 - more conforming `__VA_OPT__`
 - new algorithm for macro expansion, support [stuff like this](https://stackoverflow.com/a/70342272) better
 - `setjmp.h` compatibility
 - support _Static_assert()
 - support variably-modified types in function prototype (aka VLA parameters)
 - support VLA auto-deallocation

# Platform support
Should just work on glibc-based x86-64 Linux.

