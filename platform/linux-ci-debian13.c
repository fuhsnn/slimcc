// for scripts/debian_asan.Dockerfile

#include "slimcc.h"

void platform_init(void) {
  define_macro("__ELF__", "1");

  define_macro("linux", "1");
  define_macro("__linux", "1");
  define_macro("__linux__", "1");
  define_macro("__gnu_linux__", "1");

  init_ty_lp64();

  dumpmachine_str = "x86_64-linux-gnu";

  set_fpie("2");
  opt_pie = true;
}

void platform_stdinc_paths(StringArray *paths) {
  add_include_path(paths, "/work/slimcc/slimcc_headers/platform_fix/linux_glibc");
  add_include_path(paths, "/work/slimcc/slimcc_headers/include");
  add_include_path(paths, "/usr/include/x86_64-linux-gnu");
  add_include_path(paths, "/usr/include");
}

void platform_search_dirs(StringArray *paths) {
  strarray_push(paths, "/usr/lib/gcc/x86_64-linux-gnu/14");
  strarray_push(paths, "/usr/lib/x86_64-linux-gnu");
}

void run_assembler(StringArray *as_args, char *input, char *output) {
  run_assembler_gnustyle(as_args, input, output);
}

void run_linker(StringArray *paths, StringArray *inputs, char *output) {
  run_linker_gnustyle(paths, inputs, output,
    "/usr/lib64/ld-linux-x86-64.so.2",
    "/usr/lib/x86_64-linux-gnu",
    "/usr/lib/gcc/x86_64-linux-gnu/14");
}
