# slimcc: A C11 compiler
This is a fork of [Rui Ueyama's chibicc](https://github.com/rui314/chibicc) that aims to fill in some missing features and fixes. 

# Changes over chibicc
 - written in C99
 - bug & conformance fixes (see [closed issues](https://github.com/fuhsnn/slimcc/issues?q=is%3Aissue+is%3Aclosed+label%3Aupstream))
 - new algorithm for macro expansion, less RAM usage and support [deep recursion tricks](https://stackoverflow.com/a/70342272) better
 - `setjmp.h` compatibility
 - support `_Static_assert()`, `__has_include`
 - support C23 `constexpr`, `__VA_OPT__` and `[[atttributes]]` (though mostly ignored)
 - support variably-modified types in function prototype (aka VLA parameters)
 - support VLA auto-deallocation
 - basic stack reuse optimization
 - basic register allocation of temporaries
 - constant folding and basic strength reduction

# Platform support
Should just work on recent (2.28+) glibc-based x86-64 Linux. Musl support is almost there.

# Known issues
 - libtool doesn't generate the nessasary `-Wl,` `-fPIC` option, if you see `-soname` and PIC linker errors, try filling `*_wl=` `*_pic=` in configure scripts with `*_wl='-Wl,'` `*_pic='-fPIC'`
