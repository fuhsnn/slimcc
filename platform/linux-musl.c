#include "slimcc.h"
#define MUSLPATH
#if !defined(MUSLPATH)
#error
#endif

void platform_init(void) {
  define_macro("linux", "1");
  define_macro("__linux", "1");
  define_macro("__linux__", "1");

  init_ty(ty_ulong, ty_long, ty_long);

  set_pic("2", true);
  opt_pie = true;
}

void platform_stdinc_paths(StringArray *paths, char *argv0) {
  // We expect that compiler-provided include files are installed
  // to ./include relative to argv[0].
  incpath_push(paths, format("%s/include", dirname(strdup(argv0))));

  // Add standard include paths.
  incpath_push(paths, MUSLPATH"/include");
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

  LinkType type = get_link_type();

  switch (type) {
  case LT_RELO:
    strarray_push(&arr, "-r");
    break;
  case LT_SHARED:
    strarray_push(&arr, "-shared");
    break;
  case LT_STATIC_PIE:
    strarray_push(&arr, "-static");
    strarray_push(&arr, "-pie");
    break;
  case LT_STATIC:
    strarray_push(&arr, "-static");
    break;
  case LT_PIE:
    strarray_push(&arr, "-pie");
    break;
  }

  switch (type) {
  case LT_STATIC_PIE:
    strarray_push(&arr, "-no-dynamic-linker");
    break;
  case LT_DYNAMIC:
  case LT_SHARED:
  case LT_PIE:
    if (opt_rdynamic)
      strarray_push(&arr, "-export-dynamic");

    strarray_push(&arr, "-dynamic-linker");
    strarray_push(&arr, MUSLPATH"/lib/libc.so");
  }

  if (!opt_nostartfiles && type != LT_RELO) {
    if (type == LT_STATIC_PIE)
      strarray_push(&arr, MUSLPATH"/lib/rcrt1.o");
    else if (type == LT_PIE)
      strarray_push(&arr, MUSLPATH"/lib/Scrt1.o");
    else
      strarray_push(&arr, MUSLPATH"/lib/crt1.o");

    strarray_push(&arr, MUSLPATH"/lib/crti.o");
  }

  for (int i = 0; i < extra_args->len; i++)
    strarray_push(&arr, extra_args->data[i]);

  if (!opt_nodefaultlibs)
    strarray_push(&arr, ("-L"MUSLPATH"/lib"));

  for (int i = 0; i < inputs->len; i++)
    strarray_push(&arr, inputs->data[i]);

  if (!opt_nodefaultlibs && type != LT_RELO) {
    if (opt_pthread)
      strarray_push(&arr, "-lpthread");
    if (!opt_nolibc)
      strarray_push(&arr, "-lc");
  }

  if (!opt_nostartfiles && type != LT_RELO)
    strarray_push(&arr, MUSLPATH"/lib/crtn.o");

  strarray_push(&arr, NULL);

  run_subprocess(arr.data);
}
