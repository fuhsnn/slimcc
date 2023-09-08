# slimcc: A C11 compiler
This is a fork of [Rui Ueyama's chibicc](https://github.com/rui314/chibicc) that aims to fill in some missing features, compatibilities and fixes. 

Although not adopting chibicc's educational commit style, the goal is to be consistent with its readability-first attitude and minimalistic approach; hopefully readers and fork-mates will find something useful.

# Changes over chibicc
 - compile with C99
 - less RAM consumption during preprocessor
 - more conforming `__VA_OPT__`
 - new algorithm for macro expansion, support [stuff like this](https://stackoverflow.com/a/70342272) better

# Platform support
Should just work with glibc-based x86-64 Linux.
