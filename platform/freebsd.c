#include "slimcc.h"

void platform_init(void) {
  define_macro("__FreeBSD__", "15");

  init_ty_lp64();
}

void platform_stdinc_paths(StringArray *paths) {
  add_include_path(paths, format("%s/plat_incl/freebsd", dir_of_exe()));
  add_include_path(paths, format("%s/include", dir_of_exe()));

  add_include_path(paths, "/usr/local/include");
  add_include_path(paths, "/usr/include");
}

void run_assembler(StringArray *as_args, char *input, char *output) {
  run_assembler_gnustyle("as", as_args, input, output);
}

void run_linker(StringArray *paths, StringArray *inputs, char *output) {
  strarray_push(paths, "-L/usr/lib");
  strarray_push(paths, "-L/lib");

  run_linker_gnustyle(paths, inputs, output,
    "/libexec/ld-elf.so.1",
    "/usr/lib",
    "/usr/lib");
}
