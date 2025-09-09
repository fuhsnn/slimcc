#include "slimcc.h"

static char *gcclibpath;

void platform_init(void) {
  define_macro("__ELF__", "1");

  define_macro("linux", "1");
  define_macro("__linux", "1");
  define_macro("__linux__", "1");
  define_macro("__gnu_linux__", "1");

  init_ty_lp64();

  dumpmachine_str = "x86_64-linux-gnu";

  // Follow build compiler's PIE on/off status
#ifdef __pie__
  set_fpie("2");
  opt_pie = true;
#endif
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

    add_include_path(paths, format("%s/platform_fix/linux_glibc", hdr_dir));
    add_include_path(paths, format("%s/include", hdr_dir));
  }

  add_include_path(paths, "/usr/local/include");
  add_include_path(paths, "/usr/include/x86_64-linux-gnu");
  add_include_path(paths, "/usr/include");
}

void platform_search_dirs(StringArray *paths) {
  gcclibpath = find_dir_w_file("/usr/lib*/gcc/x86_64*-linux*/*/crtbegin.o");
  if (!gcclibpath)
    error("gcc library path not found");

  strarray_push(paths, gcclibpath);
  strarray_push(paths, "/usr/lib/x86_64-linux-gnu");
  strarray_push(paths, "/usr/lib/x86_64-pc-linux-gnu");
  strarray_push(paths, "/usr/lib/x86_64-redhat-linux");
  strarray_push(paths, "/usr/lib64");
  strarray_push(paths, "/lib64");
  strarray_push(paths, "/usr/lib");
  strarray_push(paths, "/lib");
}

void run_assembler(StringArray *as_args, char *input, char *output) {
  run_assembler_gnustyle(as_args, input, output);
}

static char *libpath(void) {
  static char *path;
  if (!path) {
    if (file_exists("/usr/lib/x86_64-linux-gnu/crti.o"))
      path = "/usr/lib/x86_64-linux-gnu";
    else if (file_exists("/usr/lib64/crti.o"))
      path = "/usr/lib64";

    if (!path)
      error("library path is not found");
  }
  return path;
}

void run_linker(StringArray *paths, StringArray *inputs, char *output) {
  run_linker_gnustyle(paths, inputs, output,
    "/lib64/ld-linux-x86-64.so.2",
    libpath(),
    gcclibpath);
}
