#include "slimcc.h"

typedef enum {
  FILE_NONE, FILE_C, FILE_ASM, FILE_PP_ASM, FILE_LDARG,
} FileType;

typedef enum {
  LT_RELO,
  LT_SHARED,
  LT_DYNAMIC,
  LT_STATIC_PIE,
  LT_STATIC,
  LT_PIE,
} LinkType;

typedef struct {
  char *arg;
  bool is_def;
} MacroChange;

typedef struct {
  MacroChange *data;
  int capacity;
  int len;
} MacroChangeArr;

StringArray include_paths;
StringArray iquote_paths;
bool opt_fcommon;
bool opt_fpic;
bool opt_fpie;
bool opt_femulated_tls;
bool opt_use_plt = true;
bool opt_optimize = true;
bool opt_reuse_stack = true;
bool opt_g;
bool opt_func_sections;
bool opt_data_sections;
bool opt_werror;
bool opt_cc1_asm_pp;
char *opt_visibility;
StdVer opt_std = STD_C17;
bool is_iso_std;
bool opt_fdefer_ts;
bool opt_short_enums;
bool opt_gnu89_inline;
bool opt_ms_anon_struct;
bool opt_disable_visibility;

static StringArray opt_imacros;
static StringArray opt_include;
bool opt_E;
bool opt_dM;
static bool opt_P;
static bool opt_M;
static bool opt_MD;
static bool opt_MM;
static bool opt_MP;
static bool opt_S;
static bool opt_c;
static bool opt_verbose;
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
bool opt_s;
bool opt_nostartfiles;
bool opt_nodefaultlibs;
bool opt_nolibc;
char *default_ld = "ld";
char *default_as = "as";
char *dumpmachine_str;
static char *opt_use_ld;
static char *opt_use_as;
static char *opt_MF;
static char *opt_MT;
static char *opt_o;

static StringArray ld_paths;
static StringArray input_args;
static StringArray sysincl_paths;
static StringArray tmpfiles;
static StringArray as_args;
static MacroChangeArr macrodefs;

char *argv0;

static void cc1(char *input_file, char *output, bool is_asm_pp);

static void version(void) {
  puts("slimcc version 0.0");
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

static void set_std(bool is_iso, char *arg) {
  char *end;
  int val = strtoul(arg, &end, 10);

  if (end - arg == 2) {
    is_iso_std = is_iso;

    switch (val) {
    case 89:
    case 90: opt_std = STD_C89; return;
    case 99: opt_std = STD_C99; return;
    case 11: opt_std = STD_C11; return;
    case 17:
    case 18: opt_std = STD_C17; return;
    case 23: opt_std = STD_C23; return;
    }
  }
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

static void macrochange_push(MacroChangeArr *arr, char *arg, bool is_def) {
  if (arr->len == arr->capacity) {
    arr->capacity += 4;
    arr->data = realloc(arr->data, sizeof(MacroChange) * arr->capacity);
  }
  MacroChange *m = &arr->data[arr->len++];
  m->arg = arg;
  m->is_def = is_def;
}

static void build_macros(MacroChangeArr *arr, bool is_asm_pp) {
  if (is_asm_pp) {
    define_macro("__ASSEMBLER__", "1");
  } else {
    if (is_iso_std)
      define_macro("__STRICT_ANSI__", "1");

    switch (opt_std) {
    case STD_C99: define_macro("__STDC_VERSION__", "199901L"); break;
    case STD_C11: define_macro("__STDC_VERSION__", "201112L"); break;
    case STD_C17: define_macro("__STDC_VERSION__", "201710L"); break;
    case STD_C23: define_macro("__STDC_VERSION__", "202311L"); break;
    }
  }

  for (int i = 0; i < arr->len; i++) {
    MacroChange *m = &arr->data[i];
    if (m->is_def)
      define_macro_cli(m->arg);
    else
      undef_macro(m->arg);
  }
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

static void build_incl_paths(char *opt_B, bool opt_nostdinc, StringArray *isystem, StringArray *idirafter) {
  if (opt_B)
    add_include_path(&sysincl_paths, opt_B);

  for (int i = 0; i < isystem->len; i++)
    add_include_path(&sysincl_paths, isystem->data[i]);

  if (!opt_nostdinc)
    platform_stdinc_paths(&sysincl_paths);

  for (int i = 0; i < idirafter->len; i++)
    add_include_path(&sysincl_paths, idirafter->data[i]);

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
}

static void build_ld_paths(char *opt_B, StringArray *paths) {
  if (opt_B)
    strarray_push(&ld_paths, opt_B);

  for (int i = 0; i < paths->len; i++)
    strarray_push(&ld_paths, paths->data[i]);

  platform_search_dirs(&ld_paths);
}

static void parse_args(int argc, char **argv, bool *run_ld, bool *no_fork) {
  char *arg;
  int input_cnt = 0;
  char *opt_B = NULL;
  bool has_wl = false;
  bool opt_nostdinc = false;
  StringArray libpaths = {0};
  StringArray isystem = {0};
  StringArray idirafter = {0};

  for (int i = 1; i < argc; i++) {
    if (*argv[i] == '\0')
      continue;

    if (!strcmp(argv[i], "-###")) {
      opt_hash_hash_hash = true;
      continue;
    }

    if (!strcmp(argv[i], "-v") || !strcmp(argv[i], "--verbose")) {
      opt_verbose = true;
      continue;
    }

    if (!strcmp(argv[i], "--help")) {
      puts("slimcc [ -o <path> ] <file>");
      exit(0);
    }

    if (!strcmp(argv[i], "--version")) {
      version();
      exit(0);
    }

    if (!strcmp(argv[i], "-dumpmachine")) {
      if (!dumpmachine_str)
        error("'-dumpmachine' not configured");
      puts(dumpmachine_str);
      exit(0);
    }

    if (!strcmp(argv[i], "-print-search-dirs") || !strcmp(argv[i], "--print-search-dirs")) {
      StringArray dirs = {0};
      platform_search_dirs(&dirs);
      printf("libraries: =");
      for (int i = 0; i < dirs.len; i++)
        printf("%s%s", dirs.data[i], (i + 1 != dirs.len) ? ":" : "\n");
      exit(0);
    }

    if (!strcmp(argv[i], "-hashmap-test")) {
      hashmap_test();
      exit(0);
    }

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

    if (!strcmp(argv[i], "-dM")) {
      opt_dM = true;
      continue;
    }

    if (take_arg_s(argv, &i, &opt_B, "-B"))
      continue;

    if (take_arg_s(argv, &i, &arg, "-I")) {
      add_include_path(&include_paths, arg);
      continue;
    }

    if (take_arg_s(argv, &i, &arg, "-isystem")) {
      strarray_push(&isystem, arg);
      continue;
    }

    if (take_arg_s(argv, &i, &arg, "-idirafter")) {
      strarray_push(&idirafter, arg);
      continue;
    }

    if (take_arg_s(argv, &i, &arg, "-iquote")) {
      add_include_path(&iquote_paths, arg);
      continue;
    }

    if (take_arg_s(argv, &i, &arg, "-D")) {
      macrochange_push(&macrodefs, arg, true);
      continue;
    }

    if (take_arg_s(argv, &i, &arg, "-U")) {
      macrochange_push(&macrodefs, arg, false);
      continue;
    }

    if (take_arg_s(argv, &i, &arg, "-imacros")) {
      strarray_push(&opt_imacros, arg);
      continue;
    }

    if (take_arg_s(argv, &i, &arg, "-include")) {
      strarray_push(&opt_include, arg);
      continue;
    }

    if (take_arg_s(argv, &i, &arg, "-x")) {
      strarray_push(&input_args, "-x");
      strarray_push(&input_args, arg);
      continue;
    }

    if (take_arg_s(argv, &i, &arg, "-L")) {
      strarray_push(&libpaths, arg);
      continue;
    }

    if (comma_arg(argv[i], &as_args, "-Wa,"))
      continue;

    if (startswith(argv[i], &arg, "-Wl,")) {
      strarray_push(&input_args, argv[i]);
      has_wl = true;
      continue;
    }

    if (take_arg_s(argv, &i, &arg, "-l")) {
      strarray_push(&input_args, format("-Wl,-l%s", arg));
      has_wl = true;
      continue;
    }

    if (take_arg(argv, &i, &arg, "-Xlinker")) {
      strarray_push(&input_args, format("-Wl,%s", arg));
      has_wl = true;
      continue;
    }

    if (take_arg_s(argv, &i, &arg, "-z")) {
      strarray_push(&input_args, format("-Wl,-z,%s", arg));
      has_wl = true;
      continue;
    }

    if (!strcmp(argv[i], "-s")) {
      opt_s = true;
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

    if (startswith(argv[i], &arg, "-g")) {
      opt_g = strcmp(arg, "0");
      continue;
    }

    if (startswith(argv[i], &arg, "-O")) {
      opt_optimize = strcmp(arg, "0");
      continue;
    }

    if (!strcmp(argv[i], "-ansi")) {
      set_std(true, "89");
      continue;
    }

    if (startswith(argv[i], &arg, "-std=") ||
      startswith(argv[i], &arg, "--std=") ||
      take_arg(argv, &i, &arg, "--std")) {
      if (startswith(arg, &arg, "c"))
        set_std(true, arg);
      else if (startswith(arg, &arg, "gnu"))
        set_std(false, arg);
      else
        error("unknown c standard");
      continue;
    }

    if (startswith(argv[i], &arg, "-f")) {
      bool b = !startswith(arg, &arg, "no-");

      if (set_bool(arg, b, "common", &opt_fcommon) ||
        set_bool(arg, b, "plt", &opt_use_plt) ||
        set_bool(arg, b, "function-sections", &opt_func_sections) ||
        set_bool(arg, b, "data-sections", &opt_data_sections) ||
        set_bool(arg, b, "emulated-tls", &opt_femulated_tls) ||
        set_bool(arg, b, "short-enums", &opt_short_enums) ||
        set_bool(arg, b, "gnu89-inline", &opt_gnu89_inline))
        continue;

      if (set_bool(arg, b, "ms-anon-struct", &opt_ms_anon_struct))
        continue;

      if (b) {
        if (!strcmp(arg, "pic")) { set_fpic("1"); continue; }
        if (!strcmp(arg, "PIC")) { set_fpic("2"); continue; }
        if (!strcmp(arg, "pie")) { set_fpie("1"); continue; }
        if (!strcmp(arg, "PIE")) { set_fpie("2"); continue; }
      } else {
        if (!strcmp(arg, "pic") || !strcmp(arg, "PIC") ||
          !strcmp(arg, "pie") || !strcmp(arg, "PIE")) {
          opt_fpic = opt_fpie = false;
          undef_macro("__pic__");
          undef_macro("__PIC__");
          undef_macro("__pie__");
          undef_macro("__PIE__");
          continue;
        }
      }

      // -f only options
      if (b) {
        if (set_true(arg, "defer-ts", &opt_fdefer_ts)) {
          define_macro("__STDC_DEFER_TS25755__", "1");
          continue;
        }
        if (set_bool(arg, false, "signed-char", &ty_pchar->is_unsigned) ||
          set_bool(arg, true, "unsigned-char", &ty_pchar->is_unsigned))
          continue;

        if (startswith(arg, &arg, "stack-reuse=")) {
          opt_reuse_stack = !strcmp(arg, "all");
          continue;
        }

        if (set_true(arg, "disable-visibility", &opt_disable_visibility))
          continue;

        if (startswith(arg, &opt_visibility, "visibility=") ||
          startswith(arg, &opt_use_as, "use-as="))
          continue;

        if (startswith(arg, &arg, "use-ld=")) {
          if (!strcmp(arg, "lld")) {
            opt_use_ld = "ld.lld";
            continue;
          }
          opt_use_ld = arg;
          continue;
        }
      }
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
        startswith(argv[i], &arg, "-march=") ||
        !strcmp(argv[i], "-ffreestanding") ||
        !strcmp(argv[i], "-fno-builtin") ||
        !strcmp(argv[i], "-fno-lto") ||
        !strcmp(argv[i], "-fno-asynchronous-unwind-tables") ||
        !strcmp(argv[i], "-fno-delete-null-pointer-checks") ||
        !strcmp(argv[i], "-fno-exceptions") ||
        !strcmp(argv[i], "-fno-omit-frame-pointer") ||
        !strcmp(argv[i], "-fno-stack-protector") ||
        !strcmp(argv[i], "-fno-strict-aliasing") ||
        !strcmp(argv[i], "-fno-strict-overflow") ||
        !strcmp(argv[i], "-fwrapv") ||
        !strcmp(argv[i], "-m64") ||
        !strcmp(argv[i], "-mfpmath=sse") ||
        !strcmp(argv[i], "-mno-red-zone") ||
        !strcmp(argv[i], "-pedantic") ||
        !strcmp(argv[i], "-pedantic-errors") ||
        !strcmp(argv[i], "-w"))
      continue;

    if (argv[i][0] == '-' && argv[i][1] != '\0')
      error("unknown argument: %s", argv[i]);

    strarray_push(&input_args, argv[i]);
    input_cnt++;
  }

  if (!opt_E && opt_dM)
    error("option -dM without -E not supported");

  if (opt_disable_visibility && opt_visibility)
    error("-fvisibility disabled with -fdisable-visibility");

  if (opt_B) {
    char *as_b = format("%s/%s", opt_B, default_as);
    char *ld_b = format("%s/%s", opt_B, default_ld);
    if (file_exists(as_b))
      default_as = as_b;
    if (file_exists(ld_b))
      default_ld = ld_b;
  }

  build_incl_paths(opt_B, opt_nostdinc, &isystem, &idirafter);
  build_ld_paths(opt_B, &libpaths);

  bool no_input = !input_cnt && !has_wl;
  if (opt_hash_hash_hash || opt_verbose) {
    version();
    if (no_input)
      exit(0);
  }
  if (no_input)
    error("no input files");
  else if (input_cnt > 1 && opt_o && (opt_c || opt_S || opt_E))
    error("cannot specify '-o' with '-c,' '-S' or '-E' with multiple files");

  *no_fork = (input_cnt == 1);
  *run_ld = has_wl && !(opt_c || opt_S || opt_E);
}

static FILE *open_file(char *path) {
  if (!path || strcmp(path, "-") == 0)
    return stdout;

  FILE *out = fopen(path, "w");
  if (!out)
    error("cannot open output file: %s: %s", path, strerror(errno));
  return out;
}

static void close_file(FILE *file) {
  if (file == stdout)
    fflush(file);
  else
    fclose(file);
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
  if (opt_hash_hash_hash || opt_verbose) {
    fprintf(stderr, "\"%s\"", argv[0]);
    for (int i = 1; argv[i]; i++)
      fprintf(stderr, " \"%s\"", argv[i]);
    fprintf(stderr, "\n");
    if (opt_hash_hash_hash)
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
  if (wait(&status) <= 0 || status != 0) {
    fprintf(stderr, "exec failed: %s\n", argv[0]);
    exit(1);
  }
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

static void print_linemarker(FILE *out, Token *tok) {
  char *name = NULL;
  File **files = get_input_files();
  for (int i = 0; files[i]; i++) {
    if (files[i]->file_no == tok->display_file_no) {
      name = files[i]->name;
      break;
    }
  }
  if (!name)
    internal_error();
  if (!strcmp(name, "-"))
    name = "<stdin>";
  fprintf(out, "\n# %d \"%s\"\n", tok->display_line_no, name);
}

// Print tokens to stdout. Used for -E.
static void print_tokens(Token *tok, FILE *out) {
  int line = 0;
  int markerfile = 0;
  tok->at_bol = false;

  for (; tok->kind != TK_EOF; tok = tok->next) {
    if (tok->at_bol) {
      fprintf(out, "\n");
      line++;
    }
    if (!opt_P) {
      if (markerfile != tok->display_file_no) {
        markerfile = tok->display_file_no;
        print_linemarker(out, tok);
      } else {
        int diff = tok->display_line_no - line;
        if (diff > 0 && diff <= 8)
          while (line++ < tok->display_line_no)
            fprintf(out, "\n");
        else if (diff)
          print_linemarker(out, tok);
      }
      line = tok->display_line_no;
    }
    if (tok->has_space)
      fprintf(out, " ");

    fprintf(out, "%.*s", tok->len, tok->loc);
  }
  fprintf(out, "\n");
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

static char *get_path(char *mode, char *incl) {
  if (file_exists(incl)) {
    return incl;
  }
  char *path = search_include_paths(incl);
  if (!path)
    error("%s: %s: %s", mode, incl, strerror(errno));
  return path;
}

static void cc1(char *input_file, char *output_file, bool is_asm_pp) {
  if (is_asm_pp)
    opt_E = opt_cc1_asm_pp = true;

  build_macros(&macrodefs, is_asm_pp);

  // Process -imacros option
  Token imacros_head = {0};
  Token *imacros_cur = &imacros_head;
  for (int i = 0; i < opt_imacros.len; i++) {
    char *path = get_path("-imacros", opt_imacros.data[i]);
    Token *end = NULL;
    imacros_cur->next = must_tokenize_file(path, &end);
    if (end)
      imacros_cur = end;
  }

  // Process -include option
  Token head = {0};
  Token *cur = &head;
  for (int i = 0; i < opt_include.len; i++) {
    char *path = get_path("-include", opt_include.data[i]);
    Token *end = NULL;
    cur->next = must_tokenize_file(path, &end);
    if (end)
      cur = end;
  }

  // Tokenize and parse.
  cur->next = must_tokenize_file(input_file, NULL);
  Token *tok = preprocess(head.next, imacros_head.next, input_file);

  // If -M or -MD are given, print file dependencies.
  if (opt_M || opt_MD) {
    print_dependencies(input_file);
    if (opt_M)
      return;
  }

  FILE *out = open_file(output_file);

  if (opt_E) {
    if (opt_dM)
      dump_defines(out);
    else
      print_tokens(tok, out);

    close_file(out);
    return;
  }

  Obj *prog = parse(tok);

  codegen(prog, out);

  close_file(out);
}

void run_assembler_gnustyle( StringArray *args, char *input, char *output) {
  StringArray arr = {0};

  strarray_push(&arr, opt_use_as ? opt_use_as : default_as);
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

  strarray_push(&arr, opt_use_ld ? opt_use_ld : default_ld);
  strarray_push(&arr, "-o");
  strarray_push(&arr, output);
  strarray_push(&arr, "-m");
  strarray_push(&arr, "elf_x86_64");
  strarray_push(&arr, "--eh-frame-hdr");

  if (opt_s)
    strarray_push(&arr, "-s");

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

  for (int i = 0; i < paths->len; i++) {
    strarray_push(&arr, "-L");
    strarray_push(&arr, paths->data[i]);
  }

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
  if (endswith(filename, ".c"))
    return FILE_C;
  if (endswith(filename, ".s"))
    return FILE_ASM;
  if (endswith(filename, ".S"))
    return FILE_PP_ASM;
  if (!strcmp(filename, "-")) {
    if (!opt_E)
      error("-E or -x required when input is from standard input");
    return FILE_C;
  }
  if (endswith(filename, ".h")) {
    if (!opt_E)
      error("pch not supported");
    return FILE_C;
  }
  return FILE_LDARG;
}

int main(int argc, char **argv) {
  argv0 = argv[0];
  atexit(cleanup);
  init_macros();
  platform_init();

  bool run_ld, no_fork;
  parse_args(argc, argv, &run_ld, &no_fork);

  StringArray ld_args = {0};
  FileType opt_x = FILE_NONE;

  for (int i = 0; i < input_args.len; i++) {
    if (!strcmp(input_args.data[i], "-x")) {
      opt_x = parse_opt_x(input_args.data[++i]);
      continue;
    }
    if (comma_arg(input_args.data[i], &ld_args, "-Wl,"))
      continue;

    char *input = input_args.data[i];

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

    if (type == FILE_LDARG) {
      strarray_push(&ld_args, input);
      run_ld = true;
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
      run_ld = true;
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
      run_ld = true;
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
    run_ld = true;
    continue;
  }

  if (run_ld)
    run_linker(&ld_paths, &ld_args, opt_o ? opt_o : "a.out");

  return 0;
}
