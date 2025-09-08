#include "slimcc.h"

void platform_init(void) {
  define_macro("__ELF__", "1");

  define_macro("linux", "1");
  define_macro("__linux", "1");
  define_macro("__linux__", "1");

  init_ty_lp64();

  dumpmachine_str = "x86_64-alpine-linux-musl";

  set_fpie("2");
  opt_pie = true;
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

    add_include_path(paths, format("%s/include", hdr_dir));
  }

  add_include_path(paths, "/usr/include");
}

void run_assembler(StringArray *as_args, char *input, char *output) {
  run_assembler_gnustyle(as_args, input, output);
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
