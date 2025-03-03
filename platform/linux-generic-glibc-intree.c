#include "slimcc.h"


void platform_init(void) {
  define_macro("linux", "1");
  define_macro("__linux", "1");
  define_macro("__linux__", "1");
  define_macro("__gnu_linux__", "1");
/*
  define_macro("__inline", "inline");
  define_macro("__GNUC__", "2");
  define_macro("__GNUC_MINOR__", "7");
  define_macro("__PRETTY_FUNCTION__", "__func__");
*/
  init_ty(ty_ulong, ty_long, ty_long);

#if 0 // default PIE
  set_pic("2", true);
  opt_pie = true;
#endif
}

void platform_stdinc_paths(StringArray *paths) {
  incpath_push(paths, in_tree_hdr());

  // Add standard include paths.
  incpath_push(paths, "/usr/local/include");
  incpath_push(paths, "/usr/include/x86_64-linux-gnu");
  incpath_push(paths, "/usr/include");
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

void run_assembler(StringArray *as_args, char *input, char *output) {
  run_assembler_gnustyle("as", as_args, input, output);
}

void run_linker(StringArray *paths, StringArray *inputs, char *output) {
  char *gcclibpath = find_dir_w_file("/usr/lib*/gcc/x86_64*-linux*/*/crtbegin.o");
  if (!gcclibpath)
    error("gcc library path not found");

  strarray_push(paths, format("-L%s", gcclibpath));
  strarray_push(paths, "-L/usr/lib/x86_64-linux-gnu");
  strarray_push(paths, "-L/usr/lib64");
  strarray_push(paths, "-L/lib64");
  strarray_push(paths, "-L/usr/lib/x86_64-linux-gnu");
  strarray_push(paths, "-L/usr/lib/x86_64-pc-linux-gnu");
  strarray_push(paths, "-L/usr/lib/x86_64-redhat-linux");
  strarray_push(paths, "-L/usr/lib");
  strarray_push(paths, "-L/lib");

  run_linker_gnustyle(paths, inputs, output,
    "/lib64/ld-linux-x86-64.so.2",
    libpath(),
    gcclibpath);
}
