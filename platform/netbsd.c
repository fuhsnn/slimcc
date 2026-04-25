#include "slimcc.h"

void platform_init_cc1(void) {
  define_macro("__ELF__", "1");

  define_macro("__NetBSD__", "1");

  init_ty_lp64();
}

void platform_init_driver(void) {
  dumpmachine_str = "x86_64--netbsd";
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

    add_include_path(paths, format("%s/platform_fix/netbsd", hdr_dir));
    add_include_path(paths, format("%s/include", hdr_dir));
  }

  add_include_path(paths, "/usr/local/include");
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

  ldarg_gnu_base(&arr, output);

  LinkType lt = link_type_gnustyle();
  if (opt_static && opt_pie)
    lt = LT_STATIC_PIE;
  else if (lt == LT_STATIC_PIE)
    lt = LT_DYNAMIC;

  ldarg_gnu_linktype(&arr, lt, "/usr/libexec/ld.elf_so");

  if (!opt_nostartfiles && lt != LT_RELO) {
    if (lt != LT_SHARED)
      strarray_push(&arr, "/usr/lib/crt0.o");
    strarray_push(&arr, "/usr/lib/crti.o");

    ldarg_gnu_crtbegin(&arr, lt, "/usr/lib");
  }

  ldarg_gnu_inputs(&arr, paths, args);

  if (!opt_nostartfiles && lt != LT_RELO) {
    ldarg_gnu_lc_lgcc(&arr, lt, true);

    ldarg_gnu_crtend(&arr, lt, "/usr/lib");

    strarray_push(&arr, "/usr/lib/crtn.o");
  }

  strarray_push(&arr, NULL);

  run_subprocess(arr.data);
}
