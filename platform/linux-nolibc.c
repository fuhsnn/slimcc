#include "slimcc.h"

void platform_init_cc1(void) {
  define_macro("__ELF__", "1");

  define_macro("__linux", "1");
  define_macro("__linux__", "1");

  init_ty_lp64();
}

void platform_init_driver(void) {
  dumpmachine_str = "x86_64-linux-gnu";

  opt_nostartfiles = opt_nodefaultlibs = true;
}

void platform_stdinc_paths(StringArray *paths) {
  add_include_path(paths, LINUX_SRC "/usr/lib/slimcc/include");
  add_include_path(paths, LINUX_SRC "/tools/include/nolibc");
  add_include_path(paths, LINUX_SRC "/usr/include");
}

void platform_search_dirs(StringArray *paths) {
}

void run_assembler(StringArray *as_args, const char *input, const char *output) {
  run_assembler_gnustyle(as_args, input, output);
}

void run_linker(StringArray *paths, StringArray *inputs, const char *output) {
  run_linker_gnustyle(paths, inputs, output, NULL, NULL, NULL);
}
