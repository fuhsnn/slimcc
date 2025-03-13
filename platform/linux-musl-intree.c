// For use with scripts/musl-bootstrap.sh

#include "slimcc.h"

void platform_init(void) {
  define_macro("linux", "1");
  define_macro("__linux", "1");
  define_macro("__linux__", "1");

  init_ty_lp64();
}

void platform_stdinc_paths(StringArray *paths) {
  add_include_path(paths, ROOT_DIR"/include");
  add_include_path(paths, ROOT_DIR"/musl_install/include");
  add_include_path(paths, ROOT_DIR"/musl_linux_headers/include");
}

void run_assembler(StringArray *as_args, char *input, char *output) {
  run_assembler_gnustyle("as", as_args, input, output);
}

void run_linker(StringArray *paths, StringArray *inputs, char *output) {
  strarray_push(paths, "-L"ROOT_DIR"/musl_install/lib");

  run_linker_gnustyle(paths, inputs, output,
    ROOT_DIR"/musl_install/ld-musl-x86_64.so.1",
    ROOT_DIR"/musl_install/lib",
    NULL);
}
