#include "slimcc.h"

#include <glob.h>

void platform_init(void) {
  define_macro("linux", "1");
  define_macro("__linux", "1");
  define_macro("__linux__", "1");
  define_macro("__gnu_linux__", "1");

  init_ty(ty_ulong, ty_long, ty_long);

  // set_pic("2", true);
  // opt_pie = true;
}

static char *find_file(char *pattern) {
  char *path = NULL;
  glob_t buf = {0};
  glob(pattern, 0, NULL, &buf);
  if (buf.gl_pathc > 0)
    path = strdup(buf.gl_pathv[buf.gl_pathc - 1]);
  globfree(&buf);
  return path;
}

void add_default_include_paths(StringArray *paths, char *argv0) {
  // We expect that compiler-provided include files are installed
  // to ./include relative to argv[0].
  strarray_push(paths, format("%s/include", dirname(strdup(argv0))));

  // Add standard include paths.
  strarray_push(paths, "/usr/local/include");
  strarray_push(paths, "/usr/include/x86_64-linux-gnu");
  strarray_push(paths, "/usr/include");
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

static char *gcc_libpath(void) {
  static char *path;
  if (!path) {
    path = find_file("/usr/lib*/gcc/x86_64*-linux*/*/crtbegin.o");
    if (!path)
      error("gcc library path is not found");
    path = dirname(path);
  }
  return path;
}

void run_assembler(StringArray *as_args, char *input, char *output) {
  StringArray arr = {0};

  strarray_push(&arr, "as");
  strarray_push(&arr, input);
  strarray_push(&arr, "-o");
  strarray_push(&arr, output);
  strarray_push(&arr, "--fatal-warnings");

  for (int i = 0; i < as_args->len; i++)
    strarray_push(&arr, as_args->data[i]);

  strarray_push(&arr, NULL);

  run_subprocess(arr.data);
}

void link_linux_libc_begin(StringArray *arr, LinkType lt, char * path) {
  if (!opt_nostartfiles && lt != LT_RELO) {
    switch (lt) {
    case LT_STATIC_PIE:
      strarray_push(arr, format("%s/rcrt1.o", path));
      break;
    case LT_PIE:
      strarray_push(arr, format("%s/Scrt1.o", path));
      break;
    case LT_STATIC:
    case LT_DYNAMIC:
      strarray_push(arr, format("%s/crt1.o", path));
      break;
    }
    strarray_push(arr, format("%s/crti.o", path));
  }
}

void link_linux_libc_end(StringArray *arr, LinkType lt, char * path) {
  if (!opt_nostartfiles && lt != LT_RELO)
    strarray_push(arr, format("%s/crtn.o", path));
}

void link_gnu_crt_begin(StringArray *arr, LinkType lt, char * path) {
  if (!opt_nostartfiles && lt != LT_RELO) {
    switch (lt) {
    case LT_STATIC_PIE:
    case LT_SHARED:
    case LT_PIE:
      strarray_push(arr, format("%s/crtbeginS.o", path));
      break;
    case LT_STATIC:
      strarray_push(arr, format("%s/crtbeginT.o", path));
      break;
    case LT_DYNAMIC:
      strarray_push(arr, format("%s/crtbegin.o", path));
      break;
    }
  }
}

void link_gnu_crt_end(StringArray *arr, LinkType lt, char * path) {
  if (!opt_nostartfiles && lt != LT_RELO) {
    switch (lt) {
    case LT_STATIC_PIE:
    case LT_SHARED:
    case LT_PIE:
      strarray_push(arr, format("%s/crtendS.o", path));
      break;
    case LT_STATIC:
    case LT_DYNAMIC:
      strarray_push(arr, format("%s/crtend.o", path));
    }
  }
}

void link_libgcc(StringArray *arr, LinkType type, bool is_static) {
  strarray_push(arr, "-lgcc");

  if (is_static) {
    strarray_push(arr, "-lgcc_eh");
  } else {
    strarray_push(arr, "--push-state");
    strarray_push(arr, "--as-needed");
    strarray_push(arr, "-lgcc_s");
    strarray_push(arr, "--pop-state");
  }
}

void link_defaultlibs_libgcc(StringArray *arr, LinkType type) {
  if (!opt_nodefaultlibs && type != LT_RELO) {
    if (type == LT_STATIC_PIE || type == LT_STATIC) {
      strarray_push(arr, "--start-group");
      link_libgcc(arr, type, true);

      if (opt_pthread)
        strarray_push(arr, "-lpthread");
      if (!opt_nolibc)
        strarray_push(arr, "-lc");

      strarray_push(arr, "--end-group");
    } else {
      link_libgcc(arr, type, opt_static_libgcc);

      if (opt_pthread)
        strarray_push(arr, "-lpthread");
      if (!opt_nolibc)
        strarray_push(arr, "-lc");

      link_libgcc(arr, type, opt_static_libgcc);
    }
  }
}

void link_type(StringArray *arr, LinkType type, char *ldso_path) {
  switch (type) {
  case LT_RELO:
    strarray_push(arr, "-r");
    break;
  case LT_SHARED:
    strarray_push(arr, "-shared");
    break;
  case LT_STATIC_PIE:
    strarray_push(arr, "-static");
    strarray_push(arr, "-pie");
    break;
  case LT_STATIC:
    strarray_push(arr, "-static");
    break;
  case LT_PIE:
    strarray_push(arr, "-pie");
    break;
  }

  switch (type) {
  case LT_STATIC_PIE:
    strarray_push(arr, "-no-dynamic-linker");
    break;
  case LT_DYNAMIC:
  case LT_SHARED:
  case LT_PIE:
    if (opt_rdynamic)
      strarray_push(arr, "--export-dynamic");

    strarray_push(arr, "-dynamic-linker");
    strarray_push(arr, ldso_path);
  }
}

void run_linker(StringArray *paths, StringArray *inputs, char *output) {
  StringArray arr = {0};

  strarray_push(&arr, "ld");
  strarray_push(&arr, "-o");
  strarray_push(&arr, output);
  strarray_push(&arr, "-m");
  strarray_push(&arr, "elf_x86_64");
  strarray_push(&arr, "--eh-frame-hdr");

  LinkType type = get_link_type();
  link_type(&arr, type, "/lib64/ld-linux-x86-64.so.2");

  link_linux_libc_begin(&arr, type, libpath());
  link_gnu_crt_begin(&arr, type, gcc_libpath());

  for (int i = 0; i < paths->len; i++)
    strarray_push(&arr, paths->data[i]);

  if (!opt_nodefaultlibs) {
    strarray_push(&arr, format("-L%s", gcc_libpath()));
    strarray_push(&arr, "-L/usr/lib/x86_64-linux-gnu");
    strarray_push(&arr, "-L/usr/lib64");
    strarray_push(&arr, "-L/lib64");
    strarray_push(&arr, "-L/usr/lib/x86_64-linux-gnu");
    strarray_push(&arr, "-L/usr/lib/x86_64-pc-linux-gnu");
    strarray_push(&arr, "-L/usr/lib/x86_64-redhat-linux");
    strarray_push(&arr, "-L/usr/lib");
    strarray_push(&arr, "-L/lib");
  }

  for (int i = 0; i < inputs->len; i++)
    strarray_push(&arr, inputs->data[i]);

  link_defaultlibs_libgcc(&arr, type);

  link_gnu_crt_end(&arr, type, gcc_libpath());
  link_linux_libc_end(&arr, type, libpath());

  strarray_push(&arr, NULL);

  run_subprocess(arr.data);
}
