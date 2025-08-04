#include "slimcc.h"

// Input file
static File *current_file;

// A list of all input files.
static File **input_files;

// True if the current position is at the beginning of a line
static bool at_bol;

// True if the current position follows a space character
static bool has_space;

#define startswith2(p, x, y) ((*(p) == x) && ((p)[1] == y))
#define startswith3(p, x, y, z) ((*(p) == x) && ((p)[1] == y) && ((p)[2] == z))

// Reports an error and exit.
void error(char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  fprintf(stderr, "\n");
  exit(1);
}

void error_ice(char *file, int32_t line) {
  error("internal error at %s:%"PRIi32, file, line);
}

// Reports an error message in the following format.
//
// foo.c:10: x = y + 1;
//               ^ <error message here>
static void verror_at(char *filename, char *input, int line_no,
               char *loc, char *fmt, va_list ap) {
  // Find a line containing `loc`.
  char *line = loc;
  while (input < line && line[-1] != '\n')
    line--;

  char *end = loc;
  while (*end && *end != '\n')
    end++;

  // Print out the line.
  fprintf(stderr, "%s:%d: \n", filename, line_no);
  fprintf(stderr, "%.*s\n", (int)(end - line), line);

  // Show the error message.
  int pos = display_width(line, loc - line);

  fprintf(stderr, "%*s", pos, ""); // print pos spaces.
  fprintf(stderr, "^ ");
  vfprintf(stderr, fmt, ap);
  fprintf(stderr, "\n");
}

void verror_at_tok(Token *tok, char *fmt, va_list ap) {
  if (tok->is_generated) {
    vfprintf(stderr, fmt, ap);
    return;
  }
  if (tok->file->file_no != tok->display_file_no) {
    File **files = get_input_files();
    if (files)
      for (int i = 0; files[i]; i++)
        if (tok->display_file_no == files[i]->file_no)
          fprintf(stderr, "#line %d \"%s\"\n", tok->display_line_no, files[i]->name);
  }
  verror_at(tok->file->name, tok->file->contents, tok->line_no, tok->loc, fmt, ap);

  if (tok->origin)
    notice_tok(tok->origin, "in expansion of macro");
}

void error_at(char *loc, char *fmt, ...) {
  int line_no = 1;
  for (char *p = current_file->contents; p < loc; p++)
    if (*p == '\n')
      line_no++;

  va_list ap;
  va_start(ap, fmt);
  verror_at(current_file->name, current_file->contents, line_no, loc, fmt, ap);
  va_end(ap);
  exit(1);
}

void error_tok(Token *tok, char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  verror_at_tok(tok, fmt, ap);
  va_end(ap);
  exit(1);
}

void warn_tok(Token *tok, char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  verror_at_tok(tok, fmt, ap);
  va_end(ap);
  if (opt_werror)
    exit(1);
}

void notice_tok(Token *tok, char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  verror_at_tok(tok, fmt, ap);
  va_end(ap);
}

// Consumes the current token if it matches `op`.
bool equal(Token *tok, char *op) {
  return strlen(op) == tok->len && !memcmp(tok->loc, op, tok->len);
}

bool equal_ext(Token *tok, char *op) {
  char buf[64];
  snprintf(buf, 64, "__%s__", op);
  return equal(tok, op) || equal(tok, buf);
}

// Ensure that the current token is `op`.
Token *skip(Token *tok, char *op) {
  if (!equal(tok, op))
    error_tok(tok, "expected '%s'", op);
  return tok->next;
}

bool consume(Token **rest, Token *tok, char *str) {
  if (equal(tok, str)) {
    *rest = tok->next;
    return true;
  }
  return false;
}

// Create a new token.
static Token *new_token(TokenKind kind, char *start, char *end) {
  Token *tok;
  if ((tok = tok_freelist)) {
    tok_freelist = tok->next;
    memset(tok, 0, sizeof(Token));
  } else {
    tok = calloc(1, sizeof(Token));
  }
  tok->kind = kind;
  tok->loc = start;
  tok->len = end - start;
  tok->file = current_file;

  tok->at_bol = at_bol;
  tok->has_space = has_space;
  at_bol = has_space = false;

  tok->alloc_next = last_alloc_tok;
  last_alloc_tok = tok;
  return tok;
}

static Token *read_ident(char *p) {
  char *start = p;
  bool has_ucn = false;

  for (;;) {
    if (*p == '$') {
      if (opt_cc1_asm_pp)
        break;
      p++;
      continue;
    }
    if (Isalnum(*p) || *p == '_') {
      p++;
      continue;
    }
    if (*p == '\\') {
      has_ucn = true;
      p++;
      continue;
    }
    if ((unsigned char)*p >= 128) {
      char *pos;
      uint32_t c = decode_utf8(&pos, p);
      if (p == start ? is_ident1(c) : is_ident2(c)) {
        p = pos;
        continue;
      }
    }
    break;
  }

  if (p == start)
    return NULL;

  Token *tok = new_token(TK_IDENT, start, p);
  tok->has_ucn = has_ucn;
  return tok;
}

static int from_hex(char c) {
  if (Inrange(c, '0', '9'))
    return c - '0';
  if (Inrange(c, 'a', 'f'))
    return c - 'a' + 10;
  return c - 'A' + 10;
}

// Read a punctuator token from p and returns its length.
static int read_punct(char *p) {
  bool is_repeat = p[1] == *p;
  bool is_assign = p[1] == '=';

  switch (*p) {
  case '-':
    if (p[1] == '>')
      return 2;
  case '&':
  case '+':
  case '=':
  case '|':
    return (is_repeat | is_assign) + 1;
  case '<':
  case '>':
    if (is_repeat)
      return (p[2] == '=') + 2;
  case '!':
  case '%':
  case '*':
  case '/':
  case '^':
    return is_assign + 1;
  case '#':
    return is_repeat + 1;
  case '.':
    return (is_repeat && p[2] == *p) ? 3 : 1;
  case '$':
    return opt_cc1_asm_pp;
  case '(':
  case ')':
  case ',':
  case ':':
  case ';':
  case '?':
  case '@':
  case '[':
  case ']':
  case '`':
  case '{':
  case '}':
  case '~':
    return 1;
  }
  return 0;
}

TokenKind ident_keyword(Token *tok) {
  static HashMap map;

  if (map.capacity == 0) {
    static char *kw[] = {
      "return", "if", "else", "for", "while", "do", "goto", "break", "continue",
      "switch", "case", "default", "_Alignof", "sizeof", "__asm", "__asm__",
      "_Static_assert", "_Countof",
      "__label__",
      "_Defer",
    };
    for (int i = 0; i < sizeof(kw) / sizeof(*kw); i++)
      hashmap_put(&map, kw[i], (void *)TK_KEYWORD);

    static char *ty_kw[] = {
      "void", "_Bool", "char", "short", "int", "long", "struct", "union",
      "typedef", "enum", "static", "extern", "_Alignas", "signed", "unsigned",
      "const", "auto", "register", "restrict", "__restrict", "__restrict__",
      "_Noreturn", "float", "double", "inline", "__auto_type",
      "_Thread_local", "__thread", "_Atomic", "__typeof", "__typeof__",
      "volatile", "__volatile", "__volatile__",
      "_BitInt"
    };
    for (int i = 0; i < sizeof(ty_kw) / sizeof(*ty_kw); i++)
      hashmap_put(&map, ty_kw[i], (void *)TK_TYPEKW);

    if (!is_iso_std)
      hashmap_put(&map, "asm", (void *)TK_KEYWORD);
    if (!is_iso_std || opt_std >= STD_C23)
      hashmap_put(&map, "typeof", (void *)TK_TYPEKW);
    if (opt_std >= STD_C23) {
      hashmap_put(&map, "alignof", (void *)TK_KEYWORD);
      hashmap_put(&map, "false", (void *)TK_KEYWORD);
      hashmap_put(&map, "true", (void *)TK_KEYWORD);
      hashmap_put(&map, "nullptr", (void *)TK_KEYWORD);
      hashmap_put(&map, "static_assert", (void *)TK_KEYWORD);

      hashmap_put(&map, "alignas", (void *)TK_TYPEKW);
      hashmap_put(&map, "bool", (void *)TK_TYPEKW);
      hashmap_put(&map, "constexpr", (void *)TK_TYPEKW);
      hashmap_put(&map, "thread_local", (void *)TK_TYPEKW);
      hashmap_put(&map, "typeof_unqual", (void *)TK_TYPEKW);
    }
    if (opt_fdefer_ts)
      hashmap_put(&map, "defer", (void *)TK_KEYWORD);
  }
  void *val = hashmap_get2(&map, tok->loc, tok->len);
  return val ? (TokenKind)(intptr_t)val : TK_IDENT;
}

static bool read_ucn(uint32_t *val, char **new_pos, char *p) {
  int len = (*p++ == 'u') ? 4 : 8;

  uint32_t c = 0;
  for (int i = 0; i < len; i++) {
    if (!Isxdigit(p[i]))
      return false;
    c = (c << 4) | from_hex(p[i]);
  }
  *val = c;
  *new_pos = p + len;
  return true;
}

static uint32_t read_escape_seq(char **new_pos, char *p) {
  if (Inrange(*p, '0', '7')) {
    // Read an octal number.
    uint32_t c = *p++ - '0';
    if (Inrange(*p, '0', '7')) {
      c = (c << 3) + (*p++ - '0');
      if (Inrange(*p, '0', '7'))
        c = (c << 3) + (*p++ - '0');
    }
    *new_pos = p;
    return c;
  }

  if (*p == 'x') {
    // Read a hexadecimal number.
    p++;
    if (!Isxdigit(*p))
      error_at(p, "invalid hex escape sequence");

    uint32_t c = 0;
    for (; Isxdigit(*p); p++)
      c = (c << 4) + from_hex(*p);
    *new_pos = p;
    return c;
  }

  if (Casecmp(*p, 'u')) {
    uint32_t c;
    if (!read_ucn(&c, new_pos, p))
      error_at(p, "incomplete universal character name");
    return c;
  }

  *new_pos = p + 1;

  // Escape sequences are defined using themselves here. E.g.
  // '\n' is implemented using '\n'. This tautological definition
  // works because the compiler that compiles our compiler knows
  // what '\n' actually is. In other words, we "inherit" the ASCII
  // code of '\n' from the compiler that compiles our compiler,
  // so we don't have to teach the actual code here.
  //
  // This fact has huge implications not only for the correctness
  // of the compiler but also for the security of the generated code.
  // For more info, read "Reflections on Trusting Trust" by Ken Thompson.
  // https://github.com/rui314/chibicc/wiki/thompson1984.pdf
  switch (*p) {
  case 'a': return '\a';
  case 'b': return '\b';
  case 't': return '\t';
  case 'n': return '\n';
  case 'v': return '\v';
  case 'f': return '\f';
  case 'r': return '\r';
  // [GNU] \e for the ASCII escape character is a GNU C extension.
  case 'e': return 27;
  default: return *p;
  }
}

// Find a closing double-quote.
static char *string_literal_end(char *p) {
  char *start = p;
  for (; *p != '"'; p++) {
    if (*p == '\n' || *p == '\0')
      error_at(start, "unclosed string literal");
    if (*p == '\\')
      p++;
  }
  return p;
}

static Token *read_string_literal(char *start, char *quote, Type *ty) {
  char *end = string_literal_end(quote + 1);
  char *buf = calloc(1, end - quote);
  int len = 0;

  for (char *p = quote + 1; p < end;) {
    if (*p == '\\') {
      if (Casecmp(p[1], 'u')) {
        len += encode_utf8(&buf[len], read_escape_seq(&p, p + 1));
        continue;
      }
      buf[len++] = read_escape_seq(&p, p + 1);
      continue;
    }
    buf[len++] = *p++;
  }

  Token *tok = new_token(TK_STR, start, end + 1);
  tok->ty = array_of(ty, len + 1);
  tok->str = buf;
  return tok;
}

// Read a UTF-8-encoded string literal and transcode it in UTF-16.
//
// UTF-16 is yet another variable-width encoding for Unicode. Code
// points smaller than U+10000 are encoded in 2 bytes. Code points
// equal to or larger than that are encoded in 4 bytes. Each 2 bytes
// in the 4 byte sequence is called "surrogate", and a 4 byte sequence
// is called a "surrogate pair".
static Token *read_utf16_string_literal(char *start, char *quote) {
  char *end = string_literal_end(quote + 1);
  uint16_t *buf = calloc(2, end - start);
  int len = 0;

  for (char *p = quote + 1; p < end;) {
    uint32_t c;
    if (*p == '\\')
      c = read_escape_seq(&p, p + 1);
    else
      c = decode_utf8(&p, p);

    if (c < 0x10000) {
      // Encode a code point in 2 bytes.
      buf[len++] = c;
    } else {
      // Encode a code point in 4 bytes.
      c -= 0x10000;
      buf[len++] = 0xd800 + ((c >> 10) & 0x3ff);
      buf[len++] = 0xdc00 + (c & 0x3ff);
    }
  }

  Token *tok = new_token(TK_STR, start, end + 1);
  tok->ty = array_of(ty_char16_t, len + 1);
  tok->str = (char *)buf;
  return tok;
}

// Read a UTF-8-encoded string literal and transcode it in UTF-32.
//
// UTF-32 is a fixed-width encoding for Unicode. Each code point is
// encoded in 4 bytes.
static Token *read_utf32_string_literal(char *start, char *quote, Type *ty) {
  char *end = string_literal_end(quote + 1);
  uint32_t *buf = calloc(4, end - quote);
  int len = 0;

  for (char *p = quote + 1; p < end;) {
    if (*p == '\\')
      buf[len++] = read_escape_seq(&p, p + 1);
    else
      buf[len++] = decode_utf8(&p, p);
  }

  Token *tok = new_token(TK_STR, start, end + 1);
  tok->ty = array_of(ty, len + 1);
  tok->str = (char *)buf;
  return tok;
}

static Token *read_char_literal(char *start, char *quote, Type *ty) {
  char *p = quote + 1;
  if (*p == '\0')
    error_at(start, "unclosed char literal");

  uint32_t c;
  if (*p == '\\')
    c = read_escape_seq(&p, p + 1);
  else
    c = decode_utf8(&p, p);

  char *end = strchr(p, '\'');
  if (!end)
    error_at(p, "unclosed char literal");

  Token *tok = new_token(TK_INT_NUM, start, end + 1);
  tok->ival = c;
  tok->ty = ty;
  return tok;
}

// The definition of the numeric literal at the preprocessing stage
// is more relaxed than the definition of that at the later stages.
// In order to handle that, a numeric literal is tokenized as a
// "pp-number" token first and then converted to a regular number
// token after preprocessing.
static Token *new_pp_number(char *start, char *p) {
  for (;;) {
    if (*p == '.') {
      p++;
      continue;
    } else if (*p == '\'' && Isalnum(p[1]) && opt_std >= STD_C23) {
      p += 2;
      continue;
    } else if ((*p == '+' || *p == '-') && (Casecmp(p[-1], 'e') || Casecmp(p[-1], 'p'))) {
      p++;
      continue;
    }
    if (*p == '$') {
      if (opt_cc1_asm_pp)
        break;
      p++;
      continue;
    }
    if (Isalnum(*p) || *p == '_') {
      p++;
      continue;
    }
    if ((unsigned char)*p >= 128) {
      char *pos;
      if (is_ident2(decode_utf8(&pos, p))) {
        p = pos;
        continue;
      }
    }
    break;
  }
  return new_token(TK_PP_NUM, start, p);
}

static void push_digit(uint32_t **data, size_t *limb_cnt, int base, int digit) {
  uint32_t *buf = *data;
  size_t cnt = *limb_cnt;

  uint64_t accum = 0;
  for (size_t i = 0; i < cnt; i++) {
    uint64_t prod = (uint64_t)base * buf[i];
    accum += prod;

    buf[i] = (uint32_t)accum;
    accum >>= 32;
  }
  if (accum) {
    buf = realloc(buf, (++cnt) * sizeof(uint32_t));
    buf[cnt - 1] = (uint32_t)accum;
  }
  buf[0] += digit;

  *data = buf;
  *limb_cnt = cnt;
}

static bool convert_pp_bitint(char *begin, char *end, Node *node, int base, bool is_unsigned) {
  uint32_t *data = calloc(2, sizeof(uint32_t));
  size_t limb32 = 1;

  for (char *p = begin; p != end; p++) {
    int digit;
    if (*p >= 'a')
      digit = *p - 'a' + 10;
    else if (*p >= 'A')
      digit = *p - 'A' + 10;
    else
      digit = *p - '0';

    if (digit >= base)
      return false;

    push_digit(&data, &limb32, base, digit);
  }

  int64_t bit_width = 0;
  for (int i = 0; i < 32; i++)
    if (data[limb32 - 1] & (1ULL << i))
      bit_width = i + 1;
  bit_width += (limb32 - 1) * 32;
  bit_width = MAX(bit_width, 1) + !is_unsigned;

  size_t limb64 = (bit_width + 63) / 64;
  if (limb64 * 2 != limb32) {
    data = realloc(data, limb64 * sizeof(uint64_t));
    data[limb32] = 0;
  }
  node->bitint_data = (uint64_t *)data;
  node->ty = new_bitint(bit_width, node->tok);
  node->ty->is_unsigned = is_unsigned;
  return true;
}

static bool convert_pp_int(char *loc, int len, Node *node) {
  char *p = loc;
  char *p_end = loc + len;

  // Read a binary, octal, decimal or hexadecimal number.
  int base = 10;
  if (*p == '0') {
    if (Casecmp(p[1], 'x') && Isxdigit(p[2])) {
      p += 2;
      base = 16;
    } else if (Casecmp(p[1], 'b') && (p[2] == '0' || p[2] == '1')) {
      p += 2;
      base = 2;
    } else {
      base = 8;
    }
  }

#if __SIZEOF_LONG_LONG__ != 8
#error
#endif

  char *digit_begin = p;
  int64_t val = strtoull(p, &p, base);
  char *digit_end = p;

  bool u = false;
  bool ll = false;
  bool l = false;
  bool wb = false;

  if (Casecmp(*p, 'u'))
    u = true, p++;

  if (Casecmp(*p, 'l')) {
    if (*p == p[1])
      ll = true, p += 2;
    else
      l = true, p += 1;
  } else if ((*p == 'w' && p[1] == 'b') || (*p == 'W' && p[1] == 'B')) {
    wb = true, p += 2;
  }

  if (!u && Casecmp(*p, 'u'))
    u = true, p++;

  if (p != p_end)
    return false;
  if (wb)
    return convert_pp_bitint(digit_begin, digit_end, node, base, u);

  // Infer a type.
  Type *ty;
  if (base == 10) {
    if (ll && u)
      ty = ty_ullong;
    else if (l && u)
      ty = ty_ulong;
    else if (ll)
      ty = ty_llong;
    else if (l)
      ty = ty_long;
    else if (u)
      ty = (val >> 32) ? ty_ulong : ty_uint;
    else
      ty = (val >> 31) ? ty_long : ty_int;
  } else {
    if (ll && u)
      ty = ty_ullong;
    else if (l && u)
      ty = ty_ulong;
    else if (ll)
      ty = (val >> 63) ? ty_ullong : ty_llong;
    else if (l)
      ty = (val >> 63) ? ty_ulong : ty_long;
    else if (u)
      ty = (val >> 32) ? ty_ulong : ty_uint;
    else if (val >> 63)
      ty = ty_ulong;
    else if (val >> 32)
      ty = ty_long;
    else if (val >> 31)
      ty = ty_uint;
    else
      ty = ty_int;
  }

  node->val = val;
  node->ty = ty;
  return true;
}

// Converts a pp-number token to a regular number token.
void convert_pp_number(Token *tok, Node *node) {
  if (tok->kind == TK_INT_NUM) {
    node->val = tok->ival;
    node->ty = tok->ty;
    return;
  }

  char *p;
  int len = 0;
  if (opt_std >= STD_C23) {
    // Remove digit seperators
    static size_t buflen;
    static char *buf;

    if (tok->len >= buflen) {
      buflen = tok->len + 1;
      buf = realloc(buf, buflen);
    }

    for (int i = 0; i < tok->len; i++) {
      if (tok->loc[i] == '\'')
        continue;
      buf[len++] = tok->loc[i];
    }
    buf[len] = '\0';
    p = buf;
  } else {
    len = tok->len;
    p = tok->loc;
  }

  // Try to parse as an integer constant.
  if (convert_pp_int(p, len, node))
    return;

  // If it's not an integer, it must be a floating point constant.
  char *end;
  long double val = strtold(p, &end);
  Type *ty;

  if (*end == 'f' || *end == 'F') {
    val = (float)val;
    ty = ty_float;
    end++;
  } else if (*end == 'l' || *end == 'L') {
    ty = ty_ldouble;
    end++;
  } else {
    val = (double)val;
    ty = ty_double;
  }

  if (&p[len] != end)
    error_tok(tok, "invalid numeric constant");

  node->fval = val;
  node->ty = ty;
}

// Initialize line info for all tokens.
static void add_line_numbers(Token *tok) {
  char *p = current_file->contents;
  int n = 1;

  do {
    if (p == tok->loc) {
      tok->line_no = n;
      tok = tok->next;
    }
    if (*p == '\n')
      n++;
  } while (*p++);
}

Token *tokenize_string_literal(Token *tok, Type *basety) {
  Token *t;
  if (basety->size == 2)
    t = read_utf16_string_literal(tok->loc, tok->loc);
  else
    t = read_utf32_string_literal(tok->loc, tok->loc, basety);
  t->next = tok->next;
  return t;
}

void convert_ucn_ident(Token *tok) {
  char *end = tok->loc + tok->len;
  char *p = tok->loc;
  char *q = p;

  while (p != end) {
    if (p < end) {
      if (*p != '\\') {
        *q++ = *p++;
        continue;
      }
      uint32_t c;
      if (Casecmp(p[1], 'u') && read_ucn(&c, &p, p + 1) &&
        (p == tok->loc ? is_ident1(c) : is_ident2(c))) {
        q += encode_utf8(q, c);
        continue;
      }
    }
    error_tok(tok, "invalid token");
  }

  tok->len = q - tok->loc;
}

// Tokenize a given string and returns new tokens.
Token *tokenize(File *file, Token **end) {
  current_file = file;

  char *p = file->contents;
  Token head = {0};
  Token *cur = &head;

  at_bol = true;
  has_space = false;

  while (*p) {
    // Skip newline.
    if (*p == '\n') {
      p++;
      at_bol = true;
      has_space = false;
      continue;
    }

    // Skip whitespace characters.
    if (*p == ' ' || *p == '\t' || *p =='\v' || *p == '\f') {
      for (char c = *p; *(++p) == c;);
      has_space = true;
      continue;
    }

    // Skip line comments.
    if (startswith2(p, '/', '/')) {
      p += 2;
      while (*p != '\n')
        p++;
      has_space = true;
      continue;
    }

    // Skip block comments.
    if (startswith2(p, '/', '*')) {
      char *q = p + 2;
      for (; *q; q++)
        if (startswith2(q, '*', '/'))
          break;
      if (!*q)
        error_at(p, "unclosed block comment");
      p = q + 2;
      has_space = true;
      continue;
    }

    // Numeric literal
    char *p2 = (*p == '.') ? p + 1 : p;
    if (Isdigit(*p2)) {
      cur = cur->next = new_pp_number(p, p2 + 1);
      p += cur->len;
      continue;
    }

    // Punctuators
    int punct_len = read_punct(p);
    if (punct_len) {
      cur = cur->next = new_token(TK_PUNCT, p, p + punct_len);
      p += cur->len;
      continue;
    }

    // String literal
    if (*p == '"') {
      cur = cur->next = read_string_literal(p, p, ty_pchar);
      p += cur->len;
      continue;
    }

    // UTF-8 string literal
    if (startswith3(p, 'u', '8', '\"')) {
      cur = cur->next = read_string_literal(p, p + 2, opt_std >= STD_C23 ? ty_uchar : ty_pchar);
      p += cur->len;
      continue;
    }

    // UTF-16 string literal
    if (startswith2(p, 'u', '\"')) {
      cur = cur->next = read_utf16_string_literal(p, p + 1);
      p += cur->len;
      continue;
    }

    // Wide string literal
    if (startswith2(p, 'L', '\"')) {
      cur = cur->next = read_utf32_string_literal(p, p + 1, ty_wchar_t);
      p += cur->len;
      continue;
    }

    // UTF-32 string literal
    if (startswith2(p, 'U', '\"')) {
      cur = cur->next = read_utf32_string_literal(p, p + 1, ty_char32_t);
      p += cur->len;
      continue;
    }

    // Character literal
    if (*p == '\'') {
      cur = cur->next = read_char_literal(p, p, ty_int);
      cur->ival = (char)cur->ival;
      p += cur->len;
      continue;
    }

    // UTF-8 character literal
    if (startswith3(p, 'u', '8', '\'') && opt_std >= STD_C23) {
      cur = cur->next = read_char_literal(p, p + 2, ty_uchar);
      cur->ival &= 0xff;
      p += cur->len;
      continue;
    }

    // UTF-16 character literal
    if (startswith2(p, 'u', '\'')) {
      cur = cur->next = read_char_literal(p, p + 1, ty_char16_t);
      cur->ival &= 0xffff;
      p += cur->len;
      continue;
    }

    // Wide character literal
    if (startswith2(p, 'L', '\'')) {
      cur = cur->next = read_char_literal(p, p + 1, ty_wchar_t);
      p += cur->len;
      continue;
    }

    // UTF-32 character literal
    if (startswith2(p, 'U', '\'')) {
      cur = cur->next = read_char_literal(p, p + 1, ty_char32_t);
      p += cur->len;
      continue;
    }

    // Identifier or keyword
    Token *ident = read_ident(p);
    if (ident) {
      cur = cur->next = ident;
      p += cur->len;
      continue;
    }

    error_at(p, "invalid token");
  }

  if (end && cur != &head)
    *end = cur;
  cur->next = new_token(TK_EOF, p, p);
  cur->next->at_bol = true;
  add_line_numbers(head.next);
  return head.next;
}

// Returns the contents of a given file.
char *read_file(char *path) {
  FILE *fp;

  if (strcmp(path, "-") == 0) {
    // By convention, read from stdin if a given filename is "-".
    fp = stdin;
  } else {
    fp = fopen(path, "r");
    if (!fp)
      return NULL;
  }

  char *buf;
  size_t buflen;
  FILE *out = open_memstream(&buf, &buflen);

  // Read the entire file.
  for (;;) {
    char buf2[4096];
    int n = fread(buf2, 1, sizeof(buf2), fp);
    if (n == 0)
      break;
    fwrite(buf2, 1, n, out);
  }

  if (fp != stdin)
    fclose(fp);

  // Make sure that the last line is properly terminated with '\n'.
  fflush(out);
  if (buflen == 0 || buf[buflen - 1] != '\n')
    fputc('\n', out);
  fputc('\0', out);
  fclose(out);
  return buf;
}

File **get_input_files(void) {
  return input_files;
}

File *new_file(char *name, int file_no, char *contents) {
  File *file = calloc(1, sizeof(File));
  file->name = name;
  file->display_file = file;
  file->file_no = file_no;
  file->contents = contents;
  return file;
}

// Replaces \r or \r\n with \n.
static void canonicalize_newline(char *p) {
  char *first = strchr(p, '\r');
  if (first) {
    char *q = p = first;

    while (*p) {
      if (*p == '\r') {
        *q++ = '\n';
        p += (p[1] == '\n') + 1;
        continue;
      }
      *q++ = *p++;
    }

    *q = '\0';
  }
}

// Removes backslashes followed by a newline.
static void remove_backslash_newline(char *p) {
  char *first = strchr(p, '\\');
  if (first) {
    char *q = p = first;
    int n = 0;

    while (*p) {
      if (*p == '\\' && p[1] == '\n') {
        p += 2;
        n++;
        continue;
      }
      if (*p == '\n') {
        for (; n > 0; n--)
          *q++ = '\n';
      }
      *q++ = *p++;
    }

    *q = '\0';
  }
}

File *add_input_file(char *path, char *contents, int *incl_no) {
  static HashMap input_files_map;

  File *file = hashmap_get(&input_files_map, path);
  if (file)
    return file;

  static int file_no;
  file = new_file(path, file_no + 1, contents);
  if (incl_no) {
    file->is_input = true;
    file->incl_no = *incl_no;
  }

  input_files = realloc(input_files, sizeof(File *) * (file_no + 2));
  input_files[file_no] = file;
  input_files[file_no + 1] = NULL;
  file_no++;

  hashmap_put(&input_files_map, path, file);
  return file;
}

Token *tokenize_file(char *path, Token **end, int *incl_no) {
  char *p = read_file(path);
  if (!p)
    return NULL;

  // UTF-8 texts may start with a 3-byte "BOM" marker sequence.
  // If exists, just skip them because they are useless bytes.
  // (It is actually not recommended to add BOM markers to UTF-8
  // texts, but it's not uncommon particularly on Windows.)
  if (!memcmp(p, "\xef\xbb\xbf", 3))
    p += 3;

  canonicalize_newline(p);
  remove_backslash_newline(p);

  return tokenize(add_input_file(path, p, incl_no), end);
}
