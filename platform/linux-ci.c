// for scripts/debian_asan.Dockerfile

#include "slimcc.h"

void platform_init(void) {
  define_macro("__ELF__", "1");

  define_macro("linux", "1");
  define_macro("__linux", "1");
  define_macro("__linux__", "1");
  define_macro("__gnu_linux__", "1");

  init_ty_lp64();
}

void platform_stdinc_paths(StringArray *paths) {
  add_include_path(paths, "/work/slimcc/include");
  add_include_path(paths, "/usr/include/x86_64-linux-gnu");
  add_include_path(paths, "/usr/include");
}

void run_assembler(StringArray *as_args, char *input, char *output) {
  run_assembler_gnustyle(as_args, input, output);
}

void run_linker(StringArray *paths, StringArray *inputs, char *output) {
  strarray_push(paths, "-L/usr/lib/gcc/x86_64-linux-gnu/12");
  strarray_push(paths, "-L/usr/lib/x86_64-linux-gnu");

  run_linker_gnustyle(paths, inputs, output,
    "/usr/lib64/ld-linux-x86-64.so.2",
    "/usr/lib/x86_64-linux-gnu",
    "/usr/lib/gcc/x86_64-linux-gnu/12");
}
