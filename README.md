slimcc started as fork of [Rui Ueyama's chibicc](https://github.com/rui314/chibicc), after three years about 85% of the code is new. It [fixed](https://github.com/fuhsnn/slimcc/issues?q=is%3Aissue+is%3Aclosed+label%3Aupstream-chibicc) issues compiling real projects, added basic codegen optimizations, reduced RAM footprint, mostly-completed support for older (C89) to newer (C23) standards, and implemented commonly used GNU extensions.

It is mature enough to serve as a drop-in C compiler for most portably-written programs in glibc environment, and able to [bootstrap a chroot](https://github.com/fuhsnn/slimcc-musl-bootstrap) with `musl`/`binutils`/userland all compiled by itself. Please have a look at the [test suite](https://github.com/fuhsnn/slimcc/blob/main/.github/workflows/linux_thirdparty.yml) to count how many big-name C projects you know are in it, bet it's plenty!

Many C2Y features are already implemented, the bigger ones being `if` declarations, labeled `break`/`continue`, `_Countof`, `_Generic(typename,...)`, `enum:_BitInt()`, and implicit integer `constexpr`.

`_Defer` is enabled in all modes, to use non-uglified `defer`, pass `-fdefer-ts` or `#include <stddefer.h>`.

Also enabled are some WG14 proposals I like, these are N3307(`__VA_TAIL__`), N3531(`#def`/`#enddef`) and N3658 (allow duplicated loop labels).

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
 - `make slimcc-san/test-san` build and run tests with address and undefined sanitizer enabled, recommended if you are hacking the source.
 - `make slimcc-filc/test-filc` build and run tests with [Fil-C](https://github.com/pizlonator/fil-c/releases), env `FILC` is used.

## Build systems compatibility
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

## Compatibility options
 - `-fms-anon-struct`: Enable declaring anonymous struct member with tags like GCC's `-fms-extensions` or `-fplan9-extensions`.
 - `-fdisable-visibility`: Some projects adopt an idiom that globally set `-fvisibility=hidden` then selectively export public API with `__attribute__((visibility("default")))`. slimcc support both, yet a problem arises when the former is probed by build system while the latter hardcoded in `#if __GNUC__` blocks regardless of the build system's decision. The option simply disable both features to prevent the conflict.
 - `-ffake-always-inline`: slimcc currently don't inline functions, but some projects' usage of GCC `always_inline` on non-static inline functions would cause missing definition at link time. This option turns these definitions into `static inline`'s to work around compile error.

## How portable is the compiler?
 - It's written in C99 with several POSIX 2008 functions.
 - Host is assumed to be little-endian.
 - For compilers with more restricted feature set, preprocessor flags are provided to enable bootstrapping stage1 with unneeded features crippled, please don't use them outside of self-hosting.
 - - `BOOTSTRAP_NO_LDOUBLE` aliases `long double` to `double`.
 - - `BOOTSTRAP_NO_VLA` makes VLA-using functions no-op (only `_BitInt` evaluation affected).
 - As proof of portability and reproducibility, the following compilers are known to bootstrap identical stage2 of slimcc:
 - - `gcc`, `clang`, `tcc`, [`kefir`](https://sr.ht/~jprotopopov/kefir), [`fil-c`](https://github.com/pizlonator/fil-c)
 - - `chibicc` with [backported patches](https://github.com/fuhsnn/chibicc/tree/minimum-fix), [`widcc`](https://github.com/fuhsnn/widcc)
 - - [`pcc`](https://github.com/portablecc/pcc) with `-std=c99`
 - - [`antcc`](https://codeberg.org/lsof/antcc) with `-DBOOTSTRAP_NO_VLA`
 - - [`cproc`](https://sr.ht/~mcf/cproc) with `-DBOOTSTRAP_NO_LDOUBLE -U__has_builtin`
 - - [`cparser`](https://github.com/libfirm/cparser) with `-DBOOTSTRAP_NO_LDOUBLE`
 - - [`compcert`](https://github.com/AbsInt/CompCert) with `-DBOOTSTRAP_NO_LDOUBLE -DBOOTSTRAP_NO_VLA -U__has_builtin -fstruct-passing`
 - - [`dmd`](https://dlang.org/) with `-P=-DBOOTSTRAP_NO_VLA -P=-U__has_builtin`, without `-O`, after patching out file scope compound literals with `perl -i -pe 's|^([\w ]+) \*(\S+) = &\(\S+\)({.*};)$|\1 _\2 = \3\n\1 *\2 = &_\2;|g' *.c`
 - - If using `platform/linux-glibc-generic.c`, remember to pass `-fno-pie -no-pie` when building stage2 to avoid host deterministic behavior.
 - - In case you're wondering — no, "Claude's C compiler" doesn't work, worse still, the cli option that could help bypass the miscompilation also wasn't implemented properly.

## Why reinvent the wheel?

Because the wheel has an open standard, each independent re-implementation of it solidifies the standard and helps uncover which cars silently depend on quirks of specific wheel vendor.

Despite being a self-taught amateur, I had found bugs in big projects like [Python](https://github.com/python/cpython/issues/145792), [jemalloc](https://github.com/facebook/jemalloc/issues/59) and [valkey](https://github.com/valkey-io/valkey/issues/2284), simply by attempting to compile them with slimcc, as well as compliance/compatibility issues in [Clang](https://github.com/llvm/llvm-project/issues?q=is%3Aissue%20author%3Afuhsnn%20label%3Aconfirmed) and [GCC](https://gcc.gnu.org/pipermail/gcc-patches/2026-May/715456.html), by triple-checking my implementation against theirs. This is not implying slimcc is more "correct" as a C compiler, but to prove it already contributed to the ecosystem by performing this act of reinvent the wheel.

## About jserv(Jim Huang)

I was so thrilled when this hero in local developer community showed up in my repo, yet soon I found three of his PRs were code taken from another chibicc fork without any credit or attribution (I cannot comprehend how someone famed for open source work could do that without blinking an eye). Even after those commits were removed he is still listed on Github contributor list. Contributor indeed, for my distrust in humanity.

And then there was the student he brought along, who later published a research with slimcc as topic based on idea I gave him, it went nowhere because he didn't even know passing `-lm` to link libm is a thing, even crazier, in the report they blamed the failure on slimcc's C23 support being fraudulent, unbelievable. The research somehow got government grant so my tax dollars literally went into defamation of my own project, refund me, asshole.
