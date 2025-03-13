#include "slimcc.h"

void platform_init(void) {
  define_macro("__FreeBSD__", "15");
  define_macro("__CC_SUPPORTS_SYMVER", "1");

  init_ty(ty_ulong, ty_long, ty_long);
}

void platform_stdinc_paths(StringArray *paths) {
  add_include_path(paths, in_tree_hdr());

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
