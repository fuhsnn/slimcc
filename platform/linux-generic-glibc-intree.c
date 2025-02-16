#include "slimcc.h"

#include <glob.h>

void platform_init(void) {
  define_macro("linux", "1");
  define_macro("__linux", "1");
  define_macro("__linux__", "1");
  define_macro("__gnu_linux__", "1");

  init_ty(ty_ulong, ty_long, ty_long);

  set_pic("2", true);
  opt_pie = true;
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
  if (opt_nostdinc)
    return;

  // We expect that compiler-provided include files are installed
  // to ./include relative to argv[0].
  strarray_push(paths, format("%s/include", dirname(strdup(argv0))));

  // Add standard include paths.
  strarray_push(paths, "/usr/local/include");
  strarray_push(paths, "/usr/include/x86_64-linux-gnu");
  strarray_push(paths, "/usr/include");

  for (int i = 0; i < paths->len; i++)
    strarray_push(&include_paths, paths->data[i]);
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

void run_linker(StringArray *extra_args, StringArray *inputs, char *output) {
  StringArray arr = {0};

  strarray_push(&arr, "ld");
  strarray_push(&arr, "-o");
  strarray_push(&arr, output);
  strarray_push(&arr, "-m");
  strarray_push(&arr, "elf_x86_64");

  strarray_push(&arr, "--eh-frame-hdr");

  if (opt_r) {
    strarray_push(&arr, "-r");
  } else if (opt_shared) {
    strarray_push(&arr, "-shared");
  } else if (opt_static_pie) {
    strarray_push(&arr, "-static");
    strarray_push(&arr, "-pie");
  } else if (opt_static) {
    strarray_push(&arr, "-static");
  } else if (opt_pie) {
    strarray_push(&arr, "-pie");
  }

  if (!(opt_nostartfiles || opt_r)) {
    if (opt_shared) {
      strarray_push(&arr, format("%s/crti.o", libpath()));
      strarray_push(&arr, format("%s/crtbeginS.o", gcc_libpath()));
    } else if (opt_static_pie) {
      strarray_push(&arr, format("%s/rcrt1.o", libpath()));
      strarray_push(&arr, format("%s/crti.o", libpath()));
      strarray_push(&arr, format("%s/crtbeginS.o", gcc_libpath()));
    } else if (opt_static) {
      strarray_push(&arr, format("%s/crt1.o", libpath()));
      strarray_push(&arr, format("%s/crti.o", libpath()));
      strarray_push(&arr, format("%s/crtbeginT.o", gcc_libpath()));
    } else if (opt_pie) {
      strarray_push(&arr, format("%s/Scrt1.o", libpath()));
      strarray_push(&arr, format("%s/crti.o", libpath()));
      strarray_push(&arr, format("%s/crtbeginS.o", gcc_libpath()));
    } else {
      strarray_push(&arr, format("%s/crt1.o", libpath()));
      strarray_push(&arr, format("%s/crti.o", libpath()));
      strarray_push(&arr, format("%s/crtbegin.o", gcc_libpath()));
    }
  }

  for (int i = 0; i < extra_args->len; i++)
    strarray_push(&arr, extra_args->data[i]);

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

  if (opt_static || opt_static_pie || opt_r) {
    strarray_push(&arr, "-no-dynamic-linker");
  } else {
    if (opt_rdynamic)
      strarray_push(&arr, "-export-dynamic");

    strarray_push(&arr, "-dynamic-linker");
    strarray_push(&arr, "/lib64/ld-linux-x86-64.so.2");
  }

  for (int i = 0; i < inputs->len; i++)
    strarray_push(&arr, inputs->data[i]);

  if (!(opt_nodefaultlibs || opt_r)) {
    if (opt_pthread)
      strarray_push(&arr, "-lpthread");

    if (opt_static || opt_static_pie) {
      strarray_push(&arr, "--start-group");
      strarray_push(&arr, "-lgcc");
      strarray_push(&arr, "-lgcc_eh");
      if (!opt_nolibc)
        strarray_push(&arr, "-lc");
      strarray_push(&arr, "--end-group");
    } else {
      if (!opt_nolibc)
        strarray_push(&arr, "-lc");
      strarray_push(&arr, "-lgcc");
      strarray_push(&arr, "--as-needed");
      strarray_push(&arr, "-lgcc_s");
      strarray_push(&arr, "--no-as-needed");
    }
  }

  if (!(opt_nostartfiles || opt_r)) {
    if (opt_shared || opt_pie || opt_static_pie)
      strarray_push(&arr, format("%s/crtendS.o", gcc_libpath()));
    else
      strarray_push(&arr, format("%s/crtend.o", gcc_libpath()));

    strarray_push(&arr, format("%s/crtn.o", libpath()));
  }

  strarray_push(&arr, NULL);

  run_subprocess(arr.data);
}
