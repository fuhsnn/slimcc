#include "slimcc.h"

void platform_init(void) {
  define_macro("linux", "1");
  define_macro("__linux", "1");
  define_macro("__linux__", "1");

  init_ty(ty_ulong, ty_long, ty_long);

  set_pic("2", true);
  opt_pie = true;
}

void platform_stdinc_paths(StringArray *paths) {
  incpath_push(paths, in_tree_hdr());

  incpath_push(paths, "/usr/include");
}

void run_assembler(StringArray *as_args, char *input, char *output) {
  run_assembler_gnustyle("as", as_args, input, output);
}

void run_linker(StringArray *paths, StringArray *inputs, char *output) {
  char *libgccpath = find_dir_w_file("/usr/lib/gcc/x86_64-alpine-linux-musl/*/crtbegin.o");

  if (libgccpath)
    strarray_push(paths, format("-L%s", libgccpath));
  strarray_push(paths, "-L/usr/lib");
  strarray_push(paths, "-L/lib");

  run_linker_gnustyle(paths, inputs, output,
    "/lib/ld-musl-x86_64.so.1",
    "/usr/lib", // ubuntu musl: /usr/lib/x86_64-linux-musl/
    libgccpath);
}
