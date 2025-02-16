#include "slimcc.h"

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

void add_default_include_paths(StringArray *paths, char *argv0) {
  if (opt_nostdinc)
    return;

  // We expect that compiler-provided include files are installed
  // to ./include relative to argv[0].
  strarray_push(paths, format("%s/include", dirname(strdup(argv0))));

  // Add standard include paths.
  strarray_push(paths, MUSLPATH"/include");

  for (int i = 0; i < paths->len; i++)
    strarray_push(&include_paths, paths->data[i]);
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
    if (opt_static_pie)
      strarray_push(&arr, MUSLPATH"/lib/rcrt1.o");
    else if (opt_pie)
      strarray_push(&arr, MUSLPATH"/lib/Scrt1.o");
    else
      strarray_push(&arr, MUSLPATH"/lib/crt1.o");

    strarray_push(&arr, MUSLPATH"/lib/crti.o");
  }

  for (int i = 0; i < extra_args->len; i++)
    strarray_push(&arr, extra_args->data[i]);

  if (!opt_nodefaultlibs) {
    strarray_push(&arr, ("-L"MUSLPATH"/lib"));
  }

  if (opt_static || opt_static_pie || opt_r) {
    strarray_push(&arr, "-no-dynamic-linker");
  } else {
    if (opt_rdynamic)
      strarray_push(&arr, "-export-dynamic");

    strarray_push(&arr, "-dynamic-linker");
    strarray_push(&arr, MUSLPATH"/lib/libc.so");
  }

  for (int i = 0; i < inputs->len; i++)
    strarray_push(&arr, inputs->data[i]);

  if (!(opt_nodefaultlibs || opt_r)) {
    if (opt_pthread)
      strarray_push(&arr, "-lpthread");
    if (!opt_nolibc)
      strarray_push(&arr, "-lc");
  }

  if (!(opt_nostartfiles || opt_r))
    strarray_push(&arr, MUSLPATH"/lib/crtn.o");

  strarray_push(&arr, NULL);

  run_subprocess(arr.data);
}
