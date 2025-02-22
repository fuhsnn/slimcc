#include "slimcc.h"

typedef enum {
  FILE_NONE, FILE_C, FILE_ASM, FILE_OBJ, FILE_AR, FILE_DSO, FILE_PP_ASM
} FileType;

StringArray incpaths;
bool opt_fcommon = true;
bool opt_fpic;
bool opt_optimize = true;
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
static bool opt_MMD;
static bool opt_MP;
static bool opt_S;
static bool opt_c;
static bool opt_cc1;
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
bool opt_nostdinc;
bool opt_nostartfiles;
bool opt_nodefaultlibs;
bool opt_nolibc;
static char *opt_MF;
static char *opt_MT;
static char *opt_o;

static StringArray ld_paths;
static StringArray ld_extra_args;
static StringArray sys_incpaths;

char *base_file;

static StringArray input_paths;
static StringArray tmpfiles;
static StringArray as_args;

static void usage(int status) {
  fprintf(stderr, "slimcc [ -o <path> ] <file>\n");
  exit(status);
}

static char *take_arg(char **argv, int *i, char *opt) {
  if (strcmp(argv[*i], opt))
    return NULL;

  if (!argv[++(*i)]) {
    fprintf(stderr, "missing argument to %s\n", opt);
    exit(1);
  }
  return argv[*i];
}

static bool take_arg2(char **argv, int *i, char *opt, char **arg) {
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

static bool startswith(char *s, char *n, char **p) {
  size_t len = strlen(n);
  if (!strncmp(s, n, len)) {
    *p = s + len;
    return true;
  }
  return false;
}

void incpath_push(StringArray *arr, char *s) {
  size_t orig_len = strlen(s);
  size_t len = orig_len;

  for (int i = len - 1; i > 0 && s[i] == '/'; i--)
    len--;

  for (int i = 0; i < arr->len; i++) {
    char *s2 = arr->data[i];
    if ((strlen(s2) == len) && !memcmp(s2, s, len))
      return;
  }

  if (len != orig_len)
    s = strndup(s, len);

  strarray_push(arr, s);
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

static bool std_arg(char **argv, int *i) {
  if (!strcmp(argv[*i], "-ansi")) {
    set_std(89);
    define_macro_cli("__STRICT_ANSI__");
    return true;
  }
  if (!strncmp(argv[*i], "-std=c", 6)) {
    set_std(strtoul(argv[*i] + 6, NULL, 10));
    return true;
  }
  if (!strncmp(argv[*i], "--std=c", 7)) {
    set_std(strtoul(argv[*i] + 7, NULL, 10));
    return true;
  }
  if (!strcmp(argv[*i], "--std")) {
    *i += 1;
    if (*argv[*i] == 'c') {
      set_std(strtoul(argv[*i] + 1, NULL, 10));
      return true;
    }
    error("unknown c standard");
  }
  return false;
}

void set_pic(char *lvl, bool is_pie) {
  if (is_pie) {
    opt_fpic = true;
    // opt_fpie = true;
    define_macro("__pic__", lvl);
    define_macro("__PIC__", lvl);
    define_macro("__pie__", lvl);
    define_macro("__PIE__", lvl);
  } else {
    opt_fpic = true;
    // opt_fpie = false;
    define_macro("__pic__", lvl);
    define_macro("__PIC__", lvl);
    undef_macro("__pie__");
    undef_macro("__PIE__");
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

static bool bool_opt(char *p, char *str, bool *opt, bool val) {
  if (!strcmp(p, str)) {
    *opt = val;
    return true;
  }
  return false;
}

static bool set_opt(char *p, char *str, bool *opt) {
  return bool_opt(p, str, opt, true);
}

static bool comma_arg(StringArray *arr, char *p, char *hdr) {
  if (startswith(p, hdr, &p)) {
    char *arg = strtok(strdup(p), ",");
    while (arg) {
      strarray_push(arr, arg);
      arg = strtok(NULL, ",");
    }
    return true;
  }
  return false;
}

static void parse_args(int argc, char **argv) {
  char *arg;
  StringArray idirafter_arr = {0};

  for (int i = 1; i < argc; i++) {
    if (*argv[i] == '\0')
      continue;

    if (!strcmp(argv[i], "--help"))
      usage(0);

    if (!strcmp(argv[i], "-hashmap-test")) {
      hashmap_test();
      exit(0);
    }

    if (set_opt(argv[i], "-S", &opt_S) ||
      set_opt(argv[i], "-c", &opt_c) ||
      set_opt(argv[i], "-###", &opt_hash_hash_hash))
      continue;

    if (take_arg2(argv, &i, "-o", &opt_o) ||
      startswith(argv[i], "-o", &opt_o))
      continue;

    if (take_arg2(argv, &i, "-x", &arg) ||
      startswith(argv[i], "-x", &arg)) {
      strarray_push(&input_paths, "-x");
      strarray_push(&input_paths, arg);
      continue;
    }

    if (take_arg2(argv, &i, "-L", &arg) ||
      startswith(argv[i], "-L", &arg)) {
      strarray_push(&ld_paths, "-L");
      strarray_push(&ld_paths, arg);
      continue;
    }

    if (comma_arg(&as_args, argv[i], "-Wa,"))
      continue;

    if (comma_arg(&ld_extra_args, argv[i], "-Wl,"))
      continue;

    if ((arg = take_arg(argv, &i, "-Xlinker"))) {
      strarray_push(&ld_extra_args, arg);
      continue;
    }

    if (!strncmp(argv[i], "-l", 2)) {
      strarray_push(&ld_extra_args, argv[i]);
      continue;
    }

    if (!strcmp(argv[i], "-s")) {
      strarray_push(&ld_extra_args, "-s");
      continue;
    }

    if (set_opt(argv[i], "-pie", &opt_pie) ||
      set_opt(argv[i], "-nopie", &opt_nopie) ||
      set_opt(argv[i], "-r", &opt_r) ||
      set_opt(argv[i], "-rdynamic", &opt_rdynamic) ||
      set_opt(argv[i], "-static", &opt_static) ||
      set_opt(argv[i], "-static-pie", &opt_static_pie) ||
      set_opt(argv[i], "-static-libgcc", &opt_static_libgcc) ||
      set_opt(argv[i], "-shared", &opt_shared) ||
      set_opt(argv[i], "-nostdinc", &opt_nostdinc) ||
      set_opt(argv[i], "-nostartfiles", &opt_nostartfiles) ||
      set_opt(argv[i], "-nodefaultlibs", &opt_nodefaultlibs) ||
      set_opt(argv[i], "-nolibc", &opt_nolibc))
      continue;

    if (!strcmp(argv[i], "-nostdlib")) {
      opt_nostartfiles = opt_nodefaultlibs = true;
      continue;
    }

    if (!strcmp(argv[i], "-no-pie")) {
      opt_pie = false;
      opt_nopie = true;
      continue;
    }

    if (set_opt(argv[i], "-E", &opt_E) ||
      set_opt(argv[i], "-P", &opt_P) ||
      set_opt(argv[i], "-Werror", &opt_werror))
      continue;

    if (!strncmp(argv[i], "-g", 2)) {
      if (argv[i][2] == '0')
        opt_g = false;
      else
        opt_g = true;
      continue;
    }

    if (!strncmp(argv[i], "-O", 2)) {
      if (argv[i][2] == '0')
        opt_optimize = false;
      else
        opt_optimize = true;
      continue;
    }

    if (take_arg2(argv, &i, "-I", &arg) ||
      startswith(argv[i], "-I", &arg)) {
      incpath_push(&incpaths, arg);
      continue;
    }

    if (take_arg2(argv, &i, "-isystem", &arg)) {
      incpath_push(&sys_incpaths, arg);
      continue;
    }

    if (take_arg2(argv, &i, "-idirafter", &arg)) {
      incpath_push(&idirafter_arr, arg);
      continue;
    }

    if ((arg = take_arg(argv, &i, "-D"))) {
      define_macro_cli(arg);
      continue;
    }

    if (!strncmp(argv[i], "-D", 2)) {
      define_macro_cli(argv[i] + 2);
      continue;
    }

    if ((arg = take_arg(argv, &i, "-U"))) {
      undef_macro(arg);
      continue;
    }

    if (!strncmp(argv[i], "-U", 2)) {
      undef_macro(argv[i] + 2);
      continue;
    }

    if ((arg = take_arg(argv, &i, "-include"))) {
      strarray_push(&opt_include, arg);
      continue;
    }

    if (!strcmp(argv[i], "-fpic")) { set_pic("1", false); continue; }
    if (!strcmp(argv[i], "-fPIC")) { set_pic("2", false); continue; }
    if (!strcmp(argv[i], "-fpie")) { set_pic("1", true); continue; }
    if (!strcmp(argv[i], "-fPIE")) { set_pic("2", true); continue; }

    if (!strcmp(argv[i], "-fno-pic") || !strcmp(argv[i], "-fno-PIC") ||
      !strcmp(argv[i], "-fno-pie") || !strcmp(argv[i], "-fno-PIE")) {
      opt_fpic = false;
      undef_macro("__pic__");
      undef_macro("__PIC__");
      undef_macro("__pie__");
      undef_macro("__PIE__");
      continue;
    }

    if (startswith(argv[i], "-fstack-reuse=", &arg)) {
      if (strncmp(arg, "all\0", 4))
        dont_reuse_stack = true;
      continue;
    }

    if (bool_opt(argv[i], "-fsigned-char", &ty_pchar->is_unsigned, false) ||
      bool_opt(argv[i], "-funsigned-char", &ty_pchar->is_unsigned, true))
      continue;

    if (startswith(argv[i], "-fvisibility=", &arg)) {
      opt_visibility = arg;
      continue;
    }

    bool bval;
    char *argp = NULL;
    if (startswith(argv[i], "-fno-", &argp))
      bval = false;
    else if (startswith(argv[i], "-f", &argp))
      bval = true;

    if (argp && (bool_opt(argp, "common", &opt_fcommon, bval) ||
      bool_opt(argp, "function-sections", &opt_func_sections, bval) ||
      bool_opt(argp, "data-sections", &opt_data_sections, bval) ||
      bool_opt(argp, "enable-universal-char", &opt_enable_universal_char, bval)))
      continue;

    if (std_arg(argv, &i))
      continue;

    if (set_opt(argv[i], "-M", &opt_M) ||
      set_opt(argv[i], "-MP", &opt_MP) ||
      set_opt(argv[i], "-MD", &opt_MD))
      continue;

    if (!strcmp(argv[i], "-MMD")) {
      opt_MD = opt_MMD = true;
      continue;
    }

    if (take_arg2(argv, &i, "-MF", &opt_MF))
      continue;

    if ((arg = take_arg(argv, &i, "-MT"))) {
      if (opt_MT == NULL)
        opt_MT = arg;
      else
        opt_MT = format("%s %s", opt_MT, arg);
      continue;
    }

    if ((arg = take_arg(argv, &i, "-MQ"))) {
      if (opt_MT == NULL)
        opt_MT = quote_makefile(arg);
      else
        opt_MT = format("%s %s", opt_MT, quote_makefile(arg));
      continue;
    }

    if (!strcmp(argv[i], "-pthread")) {
      opt_pthread = true;
      define_macro_cli("_REENTRANT");
      continue;
    }

    // These options are ignored for now.
    if (!strncmp(argv[i], "-W", 2) ||
        !strncmp(argv[i], "-std=gnu", 8) ||
        !strncmp(argv[i], "-march=", 7) ||
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
    continue;
  }

  if (input_paths.len == 0)
    error("no input files");

  if (!opt_nostdinc)
    add_default_include_paths(&sys_incpaths, argv[0]);

  for (int i = 0; i < idirafter_arr.len; i++)
    incpath_push(&sys_incpaths, idirafter_arr.data[i]);

  HashMap sys_incs = {0};
  for (int i = 0; i < sys_incpaths.len; i++)
    hashmap_put(&sys_incs, sys_incpaths.data[i], (void *)1);

  int j = 0;
  for (int i = 0; i < incpaths.len; i++) {
    if (hashmap_get(&sys_incs, incpaths.data[i]))
      continue;
    incpaths.data[j++] = incpaths.data[i];
  }

  size_t new_len = j + sys_incpaths.len;
  if (new_len > incpaths.len)
    incpaths.data = realloc(incpaths.data, sizeof(char *) * new_len);

  memcpy(&incpaths.data[j], &sys_incpaths.data[0], sizeof(char *) * sys_incpaths.len);
  incpaths.len = new_len;
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

static int jobs = 0;

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

static void cc1(char *input, char *output);
static void run_cc1(char *input, char *output, bool is_asm_pp) {
  if (opt_hash_hash_hash)
    return;

  if (fork() == 0) {
    if (is_asm_pp)
      opt_E = opt_cc1_asm_pp = true;

    cc1(input, output);
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
  fclose(out);
}

static bool in_std_include_path(char *path) {
  for (int i = 0; i < sys_incpaths.len; i++) {
    char *dir = sys_incpaths.data[i];
    int len = strlen(dir);
    if (strncmp(dir, path, len) == 0 && path[len] == '/')
      return true;
  }
  return false;
}

// If -M options is given, the compiler write a list of input files to
// stdout in a format that "make" command can read. This feature is
// used to automate file dependency management.
static void print_dependencies(void) {
  char *path;
  if (opt_MF)
    path = opt_MF;
  else if (opt_MD)
    path = replace_extn(opt_o ? opt_o : base_file, ".d");
  else if (opt_o)
    path = opt_o;
  else
    path = "-";

  FILE *out = open_file(path);
  if (opt_MT)
    fprintf(out, "%s:", opt_MT);
  else
    fprintf(out, "%s:", quote_makefile(replace_extn(base_file, ".o")));

  File **files = get_input_files();

  for (int i = 0; files[i]; i++) {
    char *name = files[i]->name;
    if ((opt_MMD && in_std_include_path(name)) || !files[i]->is_input)
      continue;
    fprintf(out, " \\\n  %s", name);
  }

  fprintf(out, "\n\n");

  if (opt_MP) {
    for (int i = 1; files[i]; i++) {
      char *name = files[i]->name;
      if ((opt_MMD && in_std_include_path(name)) || !files[i]->is_input)
        continue;
      fprintf(out, "%s:\n\n", quote_makefile(name));
    }
  }
  fclose(out);
}

static Token *must_tokenize_file(char *path, Token **end) {
  int incl_no = -1;
  Token *tok = tokenize_file(path, end, &incl_no);
  if (!tok)
    error("%s: %s", path, strerror(errno));
  return tok;
}

static void cc1(char *input, char *output_file) {
  Token head = {0};
  Token *cur = &head;
  base_file = input;

  if (!opt_E) {
    Token *end;
    head.next = tokenize(add_input_file("slimcc_builtins",
    "typedef struct {"
    "  unsigned int gp_offset;"
    "  unsigned int fp_offset;"
    "  void *overflow_arg_area;"
    "  void *reg_save_area;"
    "} __builtin_va_list[1];", NULL), &end);
    cur = end;
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
  cur->next = must_tokenize_file(base_file, NULL);
  Token *tok = preprocess(head.next);

  // If -M or -MD are given, print file dependencies.
  if (opt_M || opt_MD) {
    print_dependencies();
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
  int failed = codegen(prog, out);

  fclose(out);
  if (failed)
    unlink(output_file);
}

// Returns true if a given file exists.
bool file_exists(char *path) {
  struct stat st;
  return !stat(path, &st);
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

LinkType get_link_type(void) {
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

int main(int argc, char **argv) {
  atexit(cleanup);
  init_macros();
  platform_init();
  parse_args(argc, argv);

  StringArray ld_args = {0};
  int file_count = 0;
  FileType opt_x = FILE_NONE;
  bool run_ld = false;

  for (int i = 0; i < input_paths.len; i++) {
    if (!strcmp(input_paths.data[i], "-x")) {
      opt_x = parse_opt_x(input_paths.data[++i]);
      continue;
    }

    char *input = input_paths.data[i];

    if (opt_o && (opt_c || opt_S || opt_E))
      if (++file_count > 1)
        error("cannot specify '-o' with '-c,' '-S' or '-E' with multiple files");

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
        run_cc1(input, (opt_o ? opt_o : "-"), true);
        continue;
      }
      if (opt_c) {
        char *tmp = create_tmpfile();
        run_cc1(input, tmp, true);
        run_assembler(&as_args, tmp, output);
        continue;
      }
      char *tmp1 = create_tmpfile();
      char *tmp2 = create_tmpfile();
      run_cc1(input, tmp1, true);
      run_assembler(&as_args, tmp1, tmp2);
      strarray_push(&ld_args, tmp2);
      run_ld = true;
      continue;
    }

    assert(type == FILE_C);

    // Just preprocess
    if (opt_E || opt_M) {
      run_cc1(input, (opt_o ? opt_o : "-"), false);
      continue;
    }

    // Compile
    if (opt_S) {
      run_cc1(input, output, false);
      continue;
    }

    // Compile and assemble
    if (opt_c) {
      char *tmp = create_tmpfile();
      run_cc1(input, tmp, false);
      run_assembler(&as_args, tmp, output);
      continue;
    }

    // Compile, assemble and link
    char *tmp1 = create_tmpfile();
    char *tmp2 = create_tmpfile();
    run_cc1(input, tmp1, false);
    run_assembler(&as_args, tmp1, tmp2);
    strarray_push(&ld_args, tmp2);
    run_ld = true;
    continue;
  }

  if (run_ld) {
    for (int i = 0; i < ld_extra_args.len; i++)
      strarray_push(&ld_args, ld_extra_args.data[i]);

    run_linker(&ld_paths, &ld_args, opt_o ? opt_o : "a.out");
  }
  return 0;
}
