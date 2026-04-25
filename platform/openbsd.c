// Follows src/gnu/llvm/clang/lib/Driver/ToolChains/OpenBSD.cpp

#include "slimcc.h"

void platform_init_cc1(void) {
  define_macro("__ELF__", "1");

  define_macro("__OpenBSD__", "1");

  init_ty_lp64();
}

void platform_init_driver(void) {
  dumpmachine_str = "amd64-unknown-openbsd7.7";

  default_as = "gas";

  opt_femulated_tls = true;
  opt_fpie = 1;
}

void platform_stdinc_paths(StringArray *paths) {
  // Replace this block with absolute path if you intend to
  // execute the compiler outside of source directory.
  // If you are thinking of just removing the error while keeping
  // the relative search path, please read:
  // https://github.com/rui314/chibicc/issues/162
  {
    char *hdr_dir = format("%s/slimcc_headers", dirname(strdup(argv0)));
    if (!file_exists(hdr_dir))
      error("can't find built-in headers");

    add_include_path(paths, format("%s/platform_fix/openbsd", hdr_dir));
    add_include_path(paths, format("%s/include", hdr_dir));
  }

  add_include_path(paths, "/usr/include");
}

void platform_search_dirs(StringArray *paths) {
  strarray_push(paths, "/usr/lib");
}

void run_assembler(StringArray *as_args, const char *input, const char *output) {
  run_assembler_gnustyle(as_args, input, output);
}

void run_linker(StringArray *paths, StringArray *args, const char *output) {
  StringArray arr = {0};
  const bool opt_pg = false;

  ldarg_gnu_base(&arr, output);

  if (!opt_nostartfiles && !opt_shared && !opt_r) {
    strarray_push(&arr, "-e");
    strarray_push(&arr, "__start");
  }

  strarray_push(&arr, "--eh-frame-hdr");

  if (opt_static) {
    strarray_push(&arr, "-Bstatic");
    strarray_push(&arr, "-static");
  } else {
    if (opt_rdynamic)
      strarray_push(&arr, "-export-dynamic");

    strarray_push(&arr, "-Bdynamic");
    if (opt_shared) {
      strarray_push(&arr, "-shared");
    } else if (!opt_r) {
      strarray_push(&arr, "-dynamic-linker");
      strarray_push(&arr, "/usr/libexec/ld.so");
    }
  }

  if (opt_pie)
    strarray_push(&arr, "-pie");
  if (opt_nopie || opt_pg)
    strarray_push(&arr, "-nopie");

  if (!opt_nostartfiles && !opt_r) {
    if (!opt_shared) {
      if (opt_pg)
        strarray_push(&arr, format("/usr/lib/gcrt0.o"));
      else if (opt_static && !opt_nopie)
        strarray_push(&arr, format("/usr/lib/rcrt0.o"));
      else
        strarray_push(&arr, format("/usr/lib/crt0.o"));

      strarray_push(&arr, format("/usr/lib/crtbegin.o"));
    } else {
      strarray_push(&arr, format("/usr/lib/crtbeginS.o"));
    }
  }

  ldarg_gnu_inputs(&arr, paths, args);

  if (opt_r)
    strarray_push(&arr, "-r");

  if (!(opt_nodefaultlibs || opt_r)) {
    strarray_push(&arr, "-lcompiler_rt");

    if (opt_pthread) {
      if (!opt_shared && opt_pg)
        strarray_push(&arr, "-lpthread_p");
      else
        strarray_push(&arr, "-lpthread");
    }

    if (!opt_shared && !opt_nolibc) {
      if (opt_pg)
        strarray_push(&arr, "-lc_p");
      else
        strarray_push(&arr, "-lc");
    }
    strarray_push(&arr, "-lcompiler_rt");
  }

  if (!opt_nostartfiles && !opt_r) {
    if (!opt_shared)
      strarray_push(&arr, format("/usr/lib/crtend.o"));
    else
      strarray_push(&arr, format("/usr/lib/crtendS.o"));
  }

  strarray_push(&arr, NULL);

  run_subprocess(arr.data);
}
