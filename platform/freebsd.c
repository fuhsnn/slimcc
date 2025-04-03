#include "slimcc.h"

void platform_init(void) {
  define_macro("__ELF__", "1");

  define_macro("__FreeBSD__", "15");
  define_macro("__extension__", "");

  init_ty_lp64();
}

void platform_stdinc_paths(StringArray *paths) {
  // Replace this block with absolute path if you intend to
  // execute the compiler outside of source directory.
  {
    char *src_dir = source_dir();
    if (!file_exists(format("%s/include/.slimcc_incl_dir", src_dir)))
      error("can't find built-in headers");

    add_include_path(paths, format("%s/platform_include/freebsd", src_dir));
    add_include_path(paths, format("%s/include", src_dir));
  }

  add_include_path(paths, "/usr/local/include");
  add_include_path(paths, "/usr/include");
}

void run_assembler(StringArray *as_args, char *input, char *output) {
  run_assembler_gnustyle(as_args, input, output);
}

void run_linker(StringArray *paths, StringArray *inputs, char *output) {
  strarray_push(paths, "-L/usr/lib");
  strarray_push(paths, "-L/lib");

  run_linker_gnustyle(paths, inputs, output,
    "/libexec/ld-elf.so.1",
    "/usr/lib",
    "/usr/lib");
}
