#include "slimcc.h"

typedef enum {
  FILE_NONE, FILE_C, FILE_ASM, FILE_OBJ, FILE_AR, FILE_DSO, FILE_PP_ASM
} FileType;

typedef enum {
  LT_RELO,
  LT_SHARED,
  LT_DYNAMIC,
  LT_STATIC_PIE,
  LT_STATIC,
  LT_PIE,
} LinkType;

StringArray include_paths;
bool opt_fcommon;
bool opt_fpic;
bool opt_fpie;
bool opt_femulated_tls;
bool opt_optimize = true;
bool opt_reuse_stack = true;
bool opt_g;
bool opt_func_sections;
bool opt_data_sections;
bool opt_werror;
bool opt_cc1_asm_pp;
char *opt_visibility;
StdVer opt_std;

static StringArray opt_include;
bool opt_E;
bool opt_enable_universal_char;
static bool opt_P;
static bool opt_M;
static bool opt_MD;
static bool opt_MM;
static bool opt_MP;
static bool opt_S;
static bool opt_c;
static bool opt_hash_hash_hash;
bool opt_pie;
bool opt_nopie;
bool opt_pthread;
bool opt_r;
bool opt_rdynamic;
bool opt_static;
bool opt_static_pie;
bool opt_static_libgcc;
bool opt_shared;
bool opt_nostartfiles;
bool opt_nodefaultlibs;
bool opt_nolibc;
char *opt_use_ld = "ld";
char *opt_use_as = "as";
static char *opt_MF;
static char *opt_MT;
static char *opt_o;

static StringArray ld_paths;
static StringArray ld_extra_args;
static StringArray sysincl_paths;
static StringArray input_paths;
static StringArray tmpfiles;
static StringArray as_args;

static char *argv0;

static void cc1(char *input_file, char *output, bool is_asm_pp);

static void usage(int status) {
  fprintf(stderr, "slimcc [ -o <path> ] <file>\n");
  exit(status);
}

static bool startswith(char *arg, char **p, char *str) {
  size_t len = strlen(str);
  if (!strncmp(arg, str, len)) {
    *p = arg + len;
    return true;
  }
  return false;
}

static bool take_arg(char **argv, int *i, char **arg, char *opt) {
  if (strcmp(argv[*i], opt))
    return false;
  *i += 1;
  if (argv[*i]) {
    *arg = argv[*i];
    return true;
  }
  fprintf(stderr, "missing argument to %s\n", opt);
  exit(1);
}

static bool take_arg_s(char **argv, int *i, char **p, char *str) {
  return take_arg(argv, i, p, str) || startswith(argv[*i], p, str);
}

static bool comma_arg(char *arg, StringArray *arr, char *str) {
  if (startswith(arg, &arg, str)) {
    arg = strtok(strdup(arg), ",");
    while (arg) {
      strarray_push(arr, arg);
      arg = strtok(NULL, ",");
    }
    return true;
  }
  return false;
}

void add_include_path(StringArray *arr, char *path) {
  size_t orig_len = strlen(path);
  size_t len = orig_len;

  while (len > 1 && path[len - 1] == '/')
    len--;

  for (int i = 0; i < arr->len; i++) {
    char *s2 = arr->data[i];
    if (!strncmp(s2, path, len) && s2[len] == '\0')
      return;
  }

  if (len != orig_len)
    path = strndup(path, len);

  strarray_push(arr, path);
}

static FileType parse_opt_x(char *s) {
  if (!strcmp(s, "c"))
    return FILE_C;
  if (!strcmp(s, "assembler"))
    return FILE_ASM;
  if (!strcmp(s, "assembler-with-cpp"))
    return FILE_PP_ASM;
  if (!strcmp(s, "none"))
    return FILE_NONE;
  error("<command line>: unknown argument for -x: %s", s);
}

static bool set_bool(char *p, bool val, char *str, bool *opt) {
  if (!strcmp(p, str)) {
    *opt = val;
    return true;
  }
  return false;
}

static bool set_true(char *p, char *str, bool *opt) {
  return set_bool(p, true, str, opt);
}

static void set_std(int val) {
  if (val == 89 || val == 90)
    opt_std = STD_C89;
  else if (val == 99)
    opt_std = STD_C99;
  else if (val == 11)
    opt_std = STD_C11;
  else if (val == 17 || val == 18)
    opt_std = STD_C17;
  else if (val == 23)
    opt_std = STD_C23;
  else
    error("unknown c standard");
}

void set_fpic(char *lvl) {
  opt_fpic = true;
  opt_fpie = false;
  define_macro("__pic__", lvl);
  define_macro("__PIC__", lvl);
  undef_macro("__pie__");
  undef_macro("__PIE__");
}

void set_fpie(char *lvl) {
  opt_fpic = false;
  opt_fpie = true;
  define_macro("__pic__", lvl);
  define_macro("__PIC__", lvl);
  define_macro("__pie__", lvl);
  define_macro("__PIE__", lvl);
}

static char *quote_makefile(char *s) {
  char *buf = calloc(1, strlen(s) * 2 + 1);

  for (int i = 0, j = 0; s[i]; i++) {
    switch (s[i]) {
    case '$':
      buf[j++] = '$';
      buf[j++] = '$';
      break;
    case '#':
      buf[j++] = '\\';
      buf[j++] = '#';
      break;
    case ' ':
    case '\t':
      for (int k = i - 1; k >= 0 && s[k] == '\\'; k--)
        buf[j++] = '\\';
      buf[j++] = '\\';
      buf[j++] = s[i];
      break;
    default:
      buf[j++] = s[i];
      break;
    }
  }
  return buf;
}

static int parse_args(int argc, char **argv) {
  char *arg;
  StringArray idirafter = {0};
  int input_cnt = 0;
  bool opt_nostdinc = false;

  for (int i = 1; i < argc; i++) {
    if (*argv[i] == '\0')
      continue;

    if (!strcmp(argv[i], "-###")) {
      opt_hash_hash_hash = true;
      continue;
    }

    if (!strcmp(argv[i], "--help"))
      usage(0);

    if (take_arg_s(argv, &i, &arg, "-o")) {
      opt_o = arg;
      continue;
    }

    if (!strcmp(argv[i], "-S")) {
      opt_S = true;
      continue;
    }

    if (!strcmp(argv[i], "-c")) {
      opt_c = true;
      continue;
    }

    if (!strcmp(argv[i], "-E")) {
      opt_E = true;
      continue;
    }

    if (!strcmp(argv[i], "-P")) {
      opt_P = true;
      continue;
    }

    if (take_arg_s(argv, &i, &arg, "-I")) {
      add_include_path(&include_paths, arg);
      continue;
    }

    if (take_arg_s(argv, &i, &arg, "-isystem")) {
      add_include_path(&sysincl_paths, arg);
      continue;
    }

    if (take_arg_s(argv, &i, &arg, "-idirafter")) {
      add_include_path(&idirafter, arg);
      continue;
    }

    if (take_arg_s(argv, &i, &arg, "-D")) {
      define_macro_cli(arg);
      continue;
    }

    if (take_arg_s(argv, &i, &arg, "-U")) {
      undef_macro(arg);
      continue;
    }

    if (take_arg_s(argv, &i, &arg, "-include")) {
      strarray_push(&opt_include, arg);
      continue;
    }

    if (take_arg_s(argv, &i, &arg, "-x")) {
      strarray_push(&input_paths, "-x");
      strarray_push(&input_paths, arg);
      continue;
    }

    if (take_arg_s(argv, &i, &arg, "-L")) {
      strarray_push(&ld_paths, "-L");
      strarray_push(&ld_paths, arg);
      continue;
    }

    if (comma_arg(argv[i], &as_args, "-Wa,") ||
      comma_arg(argv[i], &ld_extra_args, "-Wl,"))
      continue;

    if (take_arg_s(argv, &i, &arg, "-l")) {
      strarray_push(&ld_extra_args, "-l");
      strarray_push(&ld_extra_args, arg);
      continue;
    }

    if (take_arg(argv, &i, &arg, "-Xlinker")) {
      strarray_push(&ld_extra_args, arg);
      continue;
    }

    if (!strcmp(argv[i], "-s")) {
      strarray_push(&ld_extra_args, "-s");
      continue;
    }

    if (!strcmp(argv[i], "-M")) {
      opt_M = true;
      continue;
    }

    if (!strcmp(argv[i], "-MM")) {
      opt_M = opt_MM = true;
      continue;
    }

    if (!strcmp(argv[i], "-MD")) {
      opt_MD = true;
      continue;
    }

    if (!strcmp(argv[i], "-MMD")) {
      opt_MD = opt_MM = true;
      continue;
    }

    if (take_arg(argv, &i, &arg, "-MF")) {
      opt_MF = arg;
      continue;
    }

    if (!strcmp(argv[i], "-MP")) {
      opt_MP = true;
      continue;
    }

    if (take_arg(argv, &i, &arg, "-MT")) {
      if (opt_MT == NULL)
        opt_MT = arg;
      else
        opt_MT = format("%s %s", opt_MT, arg);
      continue;
    }

    if (take_arg(argv, &i, &arg, "-MQ")) {
      if (opt_MT == NULL)
        opt_MT = quote_makefile(arg);
      else
        opt_MT = format("%s %s", opt_MT, quote_makefile(arg));
      continue;
    }

    if (!strcmp(argv[i], "-fpic")) { set_fpic("1"); continue; }
    if (!strcmp(argv[i], "-fPIC")) { set_fpic("2"); continue; }
    if (!strcmp(argv[i], "-fpie")) { set_fpie("1"); continue; }
    if (!strcmp(argv[i], "-fPIE")) { set_fpie("2"); continue; }

    if (!strcmp(argv[i], "-fno-pic") || !strcmp(argv[i], "-fno-PIC") ||
      !strcmp(argv[i], "-fno-pie") || !strcmp(argv[i], "-fno-PIE")) {
      opt_fpic = opt_fpie = false;
      undef_macro("__pic__");
      undef_macro("__PIC__");
      undef_macro("__pie__");
      undef_macro("__PIE__");
      continue;
    }

    if (!strcmp(argv[i], "-hashmap-test")) {
      hashmap_test();
      exit(0);
    }

    if (startswith(argv[i], &arg, "-g")) {
      opt_g = strcmp(arg, "0");
      continue;
    }

    if (startswith(argv[i], &arg, "-O")) {
      opt_optimize = strcmp(arg, "0");
      continue;
    }

    if (!strcmp(argv[i], "-ansi")) {
      set_std(89);
      define_macro_cli("__STRICT_ANSI__");
      continue;
    } else if (startswith(argv[i], &arg, "-std=c") ||
      startswith(argv[i], &arg, "--std=c")) {
      set_std(strtoul(arg, NULL, 10));
      continue;
    } else if (!strcmp(argv[i], "--std")) {
      if (startswith(argv[++i], &arg, "c")) {
        set_std(strtoul(arg, NULL, 10));
        continue;
      }
      error("unknown c standard");
    }

    {
      bool bval;
      char *argp = NULL;
      if (startswith(argv[i], &argp, "-fno-"))
        bval = false;
      else if (startswith(argv[i], &argp, "-f"))
        bval = true;

      if (argp &&
        (set_bool(argp, bval, "common", &opt_fcommon) ||
        set_bool(argp, bval, "function-sections", &opt_func_sections) ||
        set_bool(argp, bval, "data-sections", &opt_data_sections) ||
        set_bool(argp, bval, "emulated-tls", &opt_femulated_tls) ||
        set_bool(argp, bval, "enable-universal-char", &opt_enable_universal_char)))
        continue;
    }

    if (set_bool(argv[i], false, "-fsigned-char", &ty_pchar->is_unsigned) ||
      set_bool(argv[i], true, "-funsigned-char", &ty_pchar->is_unsigned))
      continue;

    if (startswith(argv[i], &arg, "-fstack-reuse=")) {
      opt_reuse_stack = !strcmp(arg, "all");
      continue;
    }

    if (startswith(argv[i], &opt_visibility, "-fvisibility=") ||
      startswith(argv[i], &opt_use_as, "-fuse-as="))
      continue;

    if (startswith(argv[i], &arg, "-fuse-ld=")) {
      if (!strcmp(arg, "lld")) {
        opt_use_ld = "ld.lld";
        continue;
      }
      opt_use_ld = arg;
      continue;
    }

    if (argv[i][0] == '-') {
      arg = (argv[i][1] == '-') ? &argv[i][2] : &argv[i][1];

      if (set_true(arg, "r", &opt_r) ||
        set_true(arg, "rdynamic", &opt_rdynamic) ||
        set_true(arg, "static", &opt_static) ||
        set_true(arg, "static-pie", &opt_static_pie) ||
        set_true(arg, "static-libgcc", &opt_static_libgcc) ||
        set_true(arg, "shared", &opt_shared) ||
        set_true(arg, "pie", &opt_pie) ||
        set_true(arg, "nopie", &opt_nopie))
        continue;

      if (!strcmp(arg, "no-pie")) {
        opt_pie = false;
        opt_nopie = true;
        continue;
      }
      if (!strcmp(arg, "pthread")) {
        opt_pthread = true;
        define_macro_cli("_REENTRANT");
        continue;
      }
    }

    if (!strcmp(argv[i], "-nostdinc")) {
      opt_nostdinc = true;
      continue;
    }

    if (set_true(argv[i], "-nostartfiles", &opt_nostartfiles) ||
      set_true(argv[i], "-nodefaultlibs", &opt_nodefaultlibs) ||
      set_true(argv[i], "-nolibc", &opt_nolibc))
      continue;

    if (!strcmp(argv[i], "-nostdlib")) {
      opt_nostartfiles = opt_nodefaultlibs = true;
      continue;
    }

    if (set_bool(argv[i], true, "-Werror", &opt_werror) ||
      set_bool(argv[i], false, "-Wno-error", &opt_werror))
      continue;

    // These options are ignored for now.
    if (startswith(argv[i], &arg, "-W") ||
        startswith(argv[i], &arg, "-std=") ||
        startswith(argv[i], &arg, "-march=") ||
        !strcmp(argv[i], "-ffreestanding") ||
        !strcmp(argv[i], "-fno-builtin") ||
        !strcmp(argv[i], "-fno-lto") ||
        !strcmp(argv[i], "-fno-asynchronous-unwind-tables") ||
        !strcmp(argv[i], "-fno-delete-null-pointer-checks") ||
        !strcmp(argv[i], "-fno-omit-frame-pointer") ||
        !strcmp(argv[i], "-fno-stack-protector") ||
        !strcmp(argv[i], "-fno-strict-aliasing") ||
        !strcmp(argv[i], "-fno-strict-overflow") ||
        !strcmp(argv[i], "-fwrapv") ||
        !strcmp(argv[i], "-m64") ||
        !strcmp(argv[i], "-mfpmath=sse") ||
        !strcmp(argv[i], "-mno-red-zone") ||
        !strcmp(argv[i], "-pedantic") ||
        !strcmp(argv[i], "-w"))
      continue;

    if (argv[i][0] == '-' && argv[i][1] != '\0')
      error("unknown argument: %s", argv[i]);

    strarray_push(&input_paths, argv[i]);
    input_cnt++;
  }

  if (!opt_nostdinc)
    platform_stdinc_paths(&sysincl_paths);

  for (int i = 0; i < idirafter.len; i++)
    add_include_path(&sysincl_paths, idirafter.data[i]);

  // Filter system directories passed as -I
  int incl_cnt = 0;
  for (int i = 0; i < include_paths.len; i++) {
    bool match = false;
    for (int j = 0; j < sysincl_paths.len; j++)
      if ((match = !strcmp(sysincl_paths.data[j], include_paths.data[i])))
        break;
    if (!match)
      include_paths.data[incl_cnt++] = include_paths.data[i];
  }
  include_paths.len = incl_cnt;

  for (int i = 0; i < sysincl_paths.len; i++)
    strarray_push(&include_paths, sysincl_paths.data[i]);

  return input_cnt;
}

static FILE *open_file(char *path) {
  if (!path || strcmp(path, "-") == 0)
    return stdout;

  FILE *out = fopen(path, "w");
  if (!out)
    error("cannot open output file: %s: %s", path, strerror(errno));
  return out;
}

static bool endswith(char *p, char *q) {
  int len1 = strlen(p);
  int len2 = strlen(q);
  return (len1 >= len2) && !strcmp(p + len1 - len2, q);
}

// Replace file extension
static char *replace_extn(char *tmpl, char *extn) {
  char *filename = basename(strdup(tmpl));
  char *dot = strrchr(filename, '.');
  if (dot)
    *dot = '\0';
  return format("%s%s", filename, extn);
}

static void cleanup(void) {
  for (int i = 0; i < tmpfiles.len; i++)
    unlink(tmpfiles.data[i]);
}

static char *create_tmpfile(void) {
  char *path = strdup("/tmp/slimcc-XXXXXX");
  int fd = mkstemp(path);
  if (fd == -1)
    error("mkstemp failed: %s", strerror(errno));
  close(fd);

  strarray_push(&tmpfiles, path);
  return path;
}

void run_subprocess(char **argv) {
  // If -### is given, dump the subprocess's command line.
  if (opt_hash_hash_hash) {
    fprintf(stderr, "\"%s\"", argv[0]);
    for (int i = 1; argv[i]; i++)
      fprintf(stderr, " \"%s\"", argv[i]);
    fprintf(stderr, "\n");
    return;
  }

  if (fork() == 0) {
    // Child process. Run a new command.
    execvp(argv[0], argv);
    fprintf(stderr, "exec failed: %s: %s\n", argv[0], strerror(errno));
    _exit(1);
  }

  // Wait for the child process to finish.
  int status;
  if (wait(&status) <= 0 || status != 0)
    exit(1);
}

static void run_cc1(char *input, char *output, bool no_fork, bool is_asm_pp) {
  if (opt_hash_hash_hash)
    return;

  if (no_fork) {
    cc1(input, output, is_asm_pp);
    return;
  }

  if (fork() == 0) {
    cc1(input, output, is_asm_pp);
    _exit(0);
  }

  // Wait for the child process to finish.
  int status;
  if (wait(&status) <= 0 || status != 0)
    exit(1);
}

// Print tokens to stdout. Used for -E.
static void print_tokens(Token *tok, char *path) {
  FILE *out = open_file(path);

  int line = 1;
  File *markerfile = NULL;

  for (; tok->kind != TK_EOF; tok = tok->next) {
    Token *orig = tok->origin ? tok->origin : tok;
    if (!opt_P && markerfile != orig->file) {
      markerfile = orig->file;
      char *name = orig->file->name;
      if (!strcmp(name, "-"))
        name = "<stdin>";
      fprintf(out, "\n# %d \"%s\"\n", orig->line_no, name);
    }

    if (line > 1 && tok->at_bol)
      fprintf(out, "\n");
    if (tok->has_space && !tok->at_bol)
      fprintf(out, " ");
    fprintf(out, "%.*s", tok->len, tok->loc);
    line++;
  }
  fprintf(out, "\n");

  if (out == stdout)
    fflush(out);
  else
    fclose(out);
}

bool in_sysincl_path(char *path) {
  for (int i = 0; i < sysincl_paths.len; i++) {
    char *dir = sysincl_paths.data[i];
    int len = strlen(dir);
    if (strncmp(dir, path, len) == 0 && path[len] == '/')
      return true;
  }
  return false;
}

// If -M options is given, the compiler write a list of input files to
// stdout in a format that "make" command can read. This feature is
// used to automate file dependency management.
static void print_dependencies(char *input) {
  char *path;
  if (opt_MF)
    path = opt_MF;
  else if (opt_MD)
    path = replace_extn(opt_o ? opt_o : input, ".d");
  else if (opt_o)
    path = opt_o;
  else
    path = "-";

  FILE *out = open_file(path);
  if (opt_MT)
    fprintf(out, "%s:", opt_MT);
  else
    fprintf(out, "%s:", quote_makefile(replace_extn(input, ".o")));

  File **files = get_input_files();

  for (int i = 0; files[i]; i++) {
    char *name = files[i]->name;
    if ((opt_MM && files[i]->is_syshdr) || !files[i]->is_input)
      continue;
    fprintf(out, " \\\n  %s", name);
  }

  fprintf(out, "\n\n");

  if (opt_MP) {
    for (int i = 1; files[i]; i++) {
      char *name = files[i]->name;
      if ((opt_MM && files[i]->is_syshdr) || !files[i]->is_input)
        continue;
      fprintf(out, "%s:\n\n", quote_makefile(name));
    }
  }
  if (out == stdout)
    fflush(out);
  else
    fclose(out);
}

static Token *must_tokenize_file(char *path, Token **end) {
  int incl_no = -1;
  Token *tok = tokenize_file(path, end, &incl_no);
  if (!tok)
    error("%s: %s", path, strerror(errno));
  return tok;
}

static void cc1(char *input_file, char *output_file, bool is_asm_pp) {
  Token head = {0};
  Token *cur = &head;

  if (is_asm_pp)
    opt_E = opt_cc1_asm_pp = true;

  if (!opt_E) {
    head.next = tokenize(add_input_file("slimcc_builtins",
    "typedef struct {"
    "  unsigned int gp_offset;"
    "  unsigned int fp_offset;"
    "  void *overflow_arg_area;"
    "  void *reg_save_area;"
    "} __builtin_va_list[1];", NULL), &cur);
  }

  // Process -include option
  for (int i = 0; i < opt_include.len; i++) {
    char *incl = opt_include.data[i];

    char *path;
    if (file_exists(incl)) {
      path = incl;
    } else {
      path = search_include_paths(incl);
      if (!path)
        error("-include: %s: %s", incl, strerror(errno));
    }

    Token *end = NULL;
    cur->next = must_tokenize_file(path, &end);
    if (end)
      cur = end;
  }

  // Tokenize and parse.
  cur->next = must_tokenize_file(input_file, NULL);
  Token *tok = preprocess(head.next, input_file);

  // If -M or -MD are given, print file dependencies.
  if (opt_M || opt_MD) {
    print_dependencies(input_file);
    if (opt_M)
      return;
  }

  // If -E is given, print out preprocessed C code as a result.
  if (opt_E) {
    print_tokens(tok, output_file);
    return;
  }

  Obj *prog = parse(tok);

  // Write the asembly text to a file.
  FILE *out = open_file(output_file);
  codegen(prog, out);

  if (out == stdout)
    fflush(out);
  else
    fclose(out);
}

void run_assembler_gnustyle( StringArray *args, char *input, char *output) {
  StringArray arr = {0};

  strarray_push(&arr, opt_use_as);
  strarray_push(&arr, input);
  strarray_push(&arr, "-o");
  strarray_push(&arr, output);
  strarray_push(&arr, "--fatal-warnings");

  for (int i = 0; i < args->len; i++)
    strarray_push(&arr, args->data[i]);

  strarray_push(&arr, NULL);

  run_subprocess(arr.data);
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

char *find_dir_w_file(char *pattern) {
  static char *path;
  if (!path) {
    path = find_file(pattern);
    if (!path)
      return NULL;
    path = dirname(path);
  }
  return path;
}

// Returns true if a given file exists.
bool file_exists(char *path) {
  struct stat st;
  return !stat(path, &st);
}

char *source_dir(void) {
  static char *path;
  if (!path)
    path = format("%s", dirname(strdup(argv0)));
  return path;
}

static LinkType get_link_type(void) {
  if (opt_r)
    return LT_RELO;
  if (opt_shared)
    return LT_SHARED;
  if (opt_static_pie)
    return LT_STATIC_PIE;
  if (opt_static)
    return LT_STATIC;
  if (opt_pie)
    return LT_PIE;
  return LT_DYNAMIC;
}

static LinkType link_type(StringArray *arr, char *ldso_path) {
  LinkType type = get_link_type();

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
  return type;
}

static void link_libgcc(StringArray *arr, bool link_libgcc, bool is_static) {
  if (!link_libgcc)
    return;
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

static void link_libc(StringArray *arr) {
  if (opt_pthread)
    strarray_push(arr, "-lpthread");
  if (!opt_nolibc)
    strarray_push(arr, "-lc");
}

void run_linker_gnustyle(StringArray *paths, StringArray *args, char *output,
  char *ldso_path, char *libpath, char *gcc_libpath) {
  StringArray arr = {0};

  strarray_push(&arr, opt_use_ld);
  strarray_push(&arr, "-o");
  strarray_push(&arr, output);
  strarray_push(&arr, "-m");
  strarray_push(&arr, "elf_x86_64");
  strarray_push(&arr, "--eh-frame-hdr");

  LinkType lt = link_type(&arr, ldso_path);

  if (!opt_nostartfiles && lt != LT_RELO) {
    switch (lt) {
    case LT_STATIC_PIE:
      strarray_push(&arr, format("%s/rcrt1.o", libpath));
      break;
    case LT_PIE:
      strarray_push(&arr, format("%s/Scrt1.o", libpath));
      break;
    case LT_STATIC:
    case LT_DYNAMIC:
      strarray_push(&arr, format("%s/crt1.o", libpath));
      break;
    }
    strarray_push(&arr, format("%s/crti.o", libpath));

    if (gcc_libpath) {
      switch (lt) {
      case LT_STATIC_PIE:
      case LT_SHARED:
      case LT_PIE:
        strarray_push(&arr, format("%s/crtbeginS.o", gcc_libpath));
        break;
      case LT_STATIC:
        strarray_push(&arr, format("%s/crtbeginT.o", gcc_libpath));
        break;
      case LT_DYNAMIC:
        strarray_push(&arr, format("%s/crtbegin.o", gcc_libpath));
        break;
      }
    }
  }

  for (int i = 0; i < paths->len; i++)
    strarray_push(&arr, paths->data[i]);

  for (int i = 0; i < args->len; i++)
    strarray_push(&arr, args->data[i]);

  if (!opt_nodefaultlibs && lt != LT_RELO) {
    if (lt == LT_STATIC_PIE || lt == LT_STATIC) {
      strarray_push(&arr, "--start-group");
      link_libgcc(&arr, gcc_libpath, true);
      link_libc(&arr);
      strarray_push(&arr, "--end-group");
    } else {
      link_libgcc(&arr, gcc_libpath, opt_static_libgcc);
      link_libc(&arr);
      link_libgcc(&arr, gcc_libpath, opt_static_libgcc);
    }
  }

  if (!opt_nostartfiles && lt != LT_RELO) {
    if (gcc_libpath) {
      switch (lt) {
      case LT_STATIC_PIE:
      case LT_SHARED:
      case LT_PIE:
        strarray_push(&arr, format("%s/crtendS.o", gcc_libpath));
        break;
      case LT_STATIC:
      case LT_DYNAMIC:
        strarray_push(&arr, format("%s/crtend.o", gcc_libpath));
      }
    }
    strarray_push(&arr, format("%s/crtn.o", libpath));
  }
  strarray_push(&arr, NULL);

  run_subprocess(arr.data);
}

static FileType get_file_type(char *filename) {
  if (endswith(filename, ".a"))
    return FILE_AR;
  if (endswith(filename, ".so"))
    return FILE_DSO;
  if (endswith(filename, ".o") || endswith(filename, ".lo"))
    return FILE_OBJ;
  if (endswith(filename, ".c"))
    return FILE_C;
  if (endswith(filename, ".s"))
    return FILE_ASM;
  if (endswith(filename, ".S"))
    return FILE_PP_ASM;

  if (opt_E && (!strcmp(filename, "-") || endswith(filename, ".h")))
    return FILE_C;

  char *p = strstr(filename, ".so.");
  if (p) {
    p += 3;
    while (Isdigit(*p) || (*p == '.' && Isdigit(p[1])))
      p++;
    if (!*p)
      return FILE_DSO;
  }

  error("<command line>: unknown file extension: %s", filename);
}

int main(int argc, char **argv) {
  argv0 = argv[0];
  atexit(cleanup);
  init_macros();
  platform_init();

  int input_cnt = parse_args(argc, argv);
  if (input_cnt < 1)
    error("no input files");
  else if (input_cnt > 1 && opt_o && (opt_c || opt_S || opt_E))
    error("cannot specify '-o' with '-c,' '-S' or '-E' with multiple files");

  bool no_fork = (input_cnt == 1);
  StringArray ld_args = {0};
  FileType opt_x = FILE_NONE;

  for (int i = 0; i < input_paths.len; i++) {
    if (!strcmp(input_paths.data[i], "-x")) {
      opt_x = parse_opt_x(input_paths.data[++i]);
      continue;
    }
    char *input = input_paths.data[i];

    char *output;
    if (opt_o)
      output = opt_o;
    else if (opt_S)
      output = replace_extn(input, ".s");
    else
      output = replace_extn(input, ".o");

    FileType type;
    if (opt_x != FILE_NONE)
      type = opt_x;
    else
      type = get_file_type(input);

    // Handle .o or .a
    if (type == FILE_OBJ || type == FILE_AR || type == FILE_DSO) {
      strarray_push(&ld_args, input);
      continue;
    }

    // Handle .s
    if (type == FILE_ASM) {
      if (opt_S || opt_E || opt_M)
        continue;

      if (opt_c) {
        run_assembler(&as_args, input, output);
        continue;
      }

      char *tmp = create_tmpfile();
      run_assembler(&as_args, input, tmp);
      strarray_push(&ld_args, tmp);
      continue;
    }

    // Handle .S
    if (type == FILE_PP_ASM) {
      if (opt_S || opt_E || opt_M) {
        run_cc1(input, (opt_o ? opt_o : "-"), no_fork, true);
        continue;
      }
      if (opt_c) {
        char *tmp = create_tmpfile();
        run_cc1(input, tmp, no_fork, true);
        run_assembler(&as_args, tmp, output);
        continue;
      }
      char *tmp1 = create_tmpfile();
      char *tmp2 = create_tmpfile();
      run_cc1(input, tmp1, no_fork, true);
      run_assembler(&as_args, tmp1, tmp2);
      strarray_push(&ld_args, tmp2);
      continue;
    }

    assert(type == FILE_C);

    // Just preprocess
    if (opt_E || opt_M) {
      run_cc1(input, (opt_o ? opt_o : "-"), no_fork, false);
      continue;
    }

    // Compile
    if (opt_S) {
      run_cc1(input, output, no_fork, false);
      continue;
    }

    // Compile and assemble
    if (opt_c) {
      char *tmp = create_tmpfile();
      run_cc1(input, tmp, no_fork, false);
      run_assembler(&as_args, tmp, output);
      continue;
    }

    // Compile, assemble and link
    char *tmp1 = create_tmpfile();
    char *tmp2 = create_tmpfile();
    run_cc1(input, tmp1, no_fork, false);
    run_assembler(&as_args, tmp1, tmp2);
    strarray_push(&ld_args, tmp2);
    continue;
  }

  if (ld_args.len) {
    for (int i = 0; i < ld_extra_args.len; i++)
      strarray_push(&ld_args, ld_extra_args.data[i]);

    run_linker(&ld_paths, &ld_args, opt_o ? opt_o : "a.out");
  }
  return 0;
}
