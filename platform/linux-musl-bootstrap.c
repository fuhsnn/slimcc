
#include "slimcc.h"

void platform_init(void) {
  define_macro("__ELF__", "1");

  define_macro("linux", "1");
  define_macro("__linux", "1");
  define_macro("__linux__", "1");

  init_ty_lp64();

  opt_func_sections = opt_data_sections = true;
}

void platform_stdinc_paths(StringArray *paths) {
  add_include_path(paths, ROOT_DIR"/lib/slimcc/include");
  add_include_path(paths, ROOT_DIR"/usr/include");
}

void run_assembler(StringArray *as_args, char *input, char *output) {
  run_assembler_gnustyle(as_args, input, output);
}

void run_linker(StringArray *paths, StringArray *inputs, char *output) {
  strarray_push(paths, "-L"ROOT_DIR"/lib");

  strarray_push(inputs, "--gc-sections");

  run_linker_gnustyle(paths, inputs, output,
    ROOT_DIR"/lib/libc.so",
    ROOT_DIR"/lib",
    NULL);
}
