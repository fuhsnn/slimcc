#include "slimcc.h"

typedef struct {
  int pos;
  int cnt;
} SlashPos;

struct SlashDelta {
  SlashPos *sp;
  int capacity;
  int len;
};

static File *current_file;

// True if the current position is at the beginning of a line
static bool at_bol;

// True if the current position follows a space character
static bool has_space;

static bool read_ucn(const char **new_pos, const char *p, uint32_t *val, bool *invalid);
static void canonicalize_newline(char *p);
static void remove_backslash_newline(char *p, SlashDelta *dlt);

#define Startswith2(p, x, y) ((*(p) == x) && ((p)[1] == y))
#define Startswith3(p, x, y, z) ((*(p) == x) && ((p)[1] == y) && ((p)[2] == z))

#define Is_c_ident(c) (Isalnum(c) || (c) == '_' || (c) == '$')

// Reports an error and exit.
void error(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  fprintf(stderr, "\n");
  cleanup_exit(1);
}

void error_ice(const char *file, int32_t line) {
  error("internal error at %s:%" PRIi32, file, line);
}

static void verror_at(const char *filename, const char *input, int line_no,
                      const char *loc, const char *fmt, va_list ap) {
  // Find a line containing `loc`.
  const char *line = loc;
  while (input < line && line[-1] != '\n')
    line--;

  const char *end = loc;
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

void verror_at_tok(Token *tok, const char *fmt, va_list ap) {
  if (tok->file->is_placeholder) {
    verror_at(tok->file->name, tok->loc, 1, tok->loc, fmt, ap);
  } else {
    if (!tok->origin && tok->display_line_no) {
      if (tok->file->file_no != tok->display_file_no || tok->line_no != tok->display_line_no)
        fprintf(stderr, "%s:%d | ", display_files.data[tok->display_file_no],
                tok->display_line_no);
    }
    verror_at(tok->file->name, tok->file->contents, tok->line_no, tok->loc, fmt, ap);
  }

  if (tok->origin)
    notice_tok(tok->origin, "in expansion of macro");
}

void error_at(const char *loc, const char *fmt, ...) {
  int line_no = 1;
  for (const char *p = current_file->contents; p < loc; p++)
    if (*p == '\n')
      line_no++;

  va_list ap;
  va_start(ap, fmt);
  verror_at(current_file->name, current_file->contents, line_no, loc, fmt, ap);
  va_end(ap);
  cleanup_exit(1);
}

void error_tok(Token *tok, const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  verror_at_tok(tok, fmt, ap);
  va_end(ap);
  cleanup_exit(1);
}

void warn_tok(Token *tok, const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  verror_at_tok(tok, fmt, ap);
  va_end(ap);
  if (opt_werror)
    cleanup_exit(1);
}

void notice_tok(Token *tok, const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  verror_at_tok(tok, fmt, ap);
  va_end(ap);
}

bool equal(Token *tok, const char *op) {
  return strlen(op) == tok->len && !memcmp(tok->loc, op, tok->len);
}

bool equal_ext(Token *tok, const char *op) {
  char buf[64];
  snprintf(buf, 64, "__%s__", op);
  return equal(tok, op) || equal(tok, buf);
}

Token *skip(Token *tok, const char *op) {
  if (!equal(tok, op))
    error_tok(tok, "expected '%s'", op);
  return tok->next;
}

bool consume(Token **rest, Token *tok, const char *str) {
  if (equal(tok, str)) {
    *rest = tok->next;
    return true;
  }
  return false;
}

static Token *new_token(TokenKind kind, const char *start, const char *end) {
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

static Token *read_ident(const char *p) {
  const char *start = p;
  bool has_ucn = false;
  bool invalid = false;

  for (;;) {
    if (Is_c_ident(*p)) {
      p++;
      continue;
    }
    if (*p == '\\' && read_ucn(&p, p + 1, &(uint32_t){0}, &invalid)) {
      has_ucn = true;
      continue;
    }
    if ((unsigned char)*p >= 128) {
      const char *pos;
      uint32_t c = decode_utf8(&pos, p);
      if (!(p == start ? is_ident1(c) : is_ident2(c)))
        invalid = true;
      p = pos;
      continue;
    }
    break;
  }

  if (p == start)
    return NULL;

  if (invalid)
    return new_token(TK_INVALID, start, p);

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

static int read_punct(const char *p, TokenKind *k) {
  switch (*p) {
  case '(': return *k = TK_LPAREN, 1;
  case ')': return *k = TK_RPAREN, 1;
  case ',': return *k = TK_COMMA, 1;
  case ';': return *k = TK_SEMI, 1;
  case '?': return *k = TK_QMARK, 1;
  case '[': return *k = TK_LBRACK, 1;
  case ']': return *k = TK_RBRACK, 1;
  case '{': return *k = TK_LCURLY, 1;
  case '}': return *k = TK_RCURLY, 1;
  case '~': return *k = TK_BITNOT, 1;
  case '@':
  case '`': return *k = TK_PUNCT, 1;
  case '!': return (p[1] == '=') ? (*k = TK_NOT_EQ, 2) : (*k = TK_NOT, 1);
  case '*': return (p[1] == '=') ? (*k = TK_MUL_EQ, 2) : (*k = TK_MUL, 1);
  case '/': return (p[1] == '=') ? (*k = TK_DIV_EQ, 2) : (*k = TK_DIV, 1);
  case '^': return (p[1] == '=') ? (*k = TK_XOR_EQ, 2) : (*k = TK_XOR, 1);
  case '=': return (p[1] == '=') ? (*k = TK_EQ2, 2) : (*k = TK_EQ, 1);
  case '#': return (p[1] == '#') ? (*k = TK_HASH2, 2) : (*k = TK_HASH, 1);
  case '.': return Startswith2(p + 1, '.', '.') ? (*k = TK_DOT3, 3) : (*k = TK_DOT, 1);
  case '&':
    switch (p[1]) {
    case '&': return *k = TK_AND2, 2;
    case '=': return *k = TK_AND_EQ, 2;
    default:  return *k = TK_AND, 1;
    }
  case '+':
    switch (p[1]) {
    case '+': return *k = TK_ADD2, 2;
    case '=': return *k = TK_ADD_EQ, 2;
    default:  return *k = TK_ADD, 1;
    }
  case '-':
    switch (p[1]) {
    case '>': return *k = TK_ARROW, 2;
    case '-': return *k = TK_SUB2, 2;
    case '=': return *k = TK_SUB_EQ, 2;
    default:  return *k = TK_SUB, 1;
    }
  case '|':
    switch (p[1]) {
    case '|': return *k = TK_OR2, 2;
    case '=': return *k = TK_OR_EQ, 2;
    default:  return *k = TK_OR, 1;
    }
  case ':':
    if (opt_std >= STD_C94 && p[1] == '>')
      return *k = TK_RBRACK, 2;
    return (p[1] == ':') ? (*k = TK_COLON2, 2) : (*k = TK_COLON, 1);
  case '%':
    if (opt_std >= STD_C94) {
      switch (p[1]) {
      case '>': return *k = TK_RCURLY, 2;
      case ':':
        return Startswith2(p + 2, '%', ':') ? (*k = TK_HASH2, 4) : (*k = TK_HASH, 2);
      }
    }
    return (p[1] == '=') ? (*k = TK_REM_EQ, 2) : (*k = TK_REM, 1);
  case '<':
    if (opt_std >= STD_C94) {
      switch (p[1]) {
      case '%': return *k = TK_LCURLY, 2;
      case ':': return *k = TK_LBRACK, 2;
      }
    }
    switch (p[1]) {
    case '<': return (p[2] == '=') ? (*k = TK_LANGLE2_EQ, 3) : (*k = TK_LANGLE2, 2);
    case '=': return *k = TK_LANGLE_EQ, 2;
    default:  return *k = TK_LANGLE, 1;
    }
  case '>':
    switch (p[1]) {
    case '>': return (p[2] == '=') ? (*k = TK_RANGLE2_EQ, 3) : (*k = TK_RANGLE2, 2);
    case '=': return *k = TK_RANGLE_EQ, 2;
    default:  return *k = TK_RANGLE, 1;
    }
  }
  return 0;
}

TokenKind ident_keyword(Token *tok) {
  static HashMap map;

  if (map.capacity == 0) {
    hashmap_put(&map, "return", (void *)TK_return);
    hashmap_put(&map, "if", (void *)TK_if);
    hashmap_put(&map, "else", (void *)TK_else);
    hashmap_put(&map, "for", (void *)TK_for);
    hashmap_put(&map, "while", (void *)TK_while);
    hashmap_put(&map, "do", (void *)TK_do);
    hashmap_put(&map, "goto", (void *)TK_goto);
    hashmap_put(&map, "break", (void *)TK_break);
    hashmap_put(&map, "continue", (void *)TK_continue);
    hashmap_put(&map, "switch", (void *)TK_switch);
    hashmap_put(&map, "case", (void *)TK_case);
    hashmap_put(&map, "default", (void *)TK_default);
    hashmap_put(&map, "sizeof", (void *)TK_sizeof);
    hashmap_put(&map, "_Generic", (void *)TK_Generic);
    hashmap_put(&map, "_Countof", (void *)TK_Countof);

    hashmap_put(&map, "__label__", (void *)TK_KEYWORD);

    hashmap_put(&map, "__func__", (void *)TK_FUNCTION);
    hashmap_put(&map, "__FUNCTION__", (void *)TK_FUNCTION);

    if (opt_std >= STD_C23)
      hashmap_put(&map, "alignof", (void *)TK_alignof);
    hashmap_put(&map, "__alignof", (void *)TK_alignof);
    hashmap_put(&map, "__alignof__", (void *)TK_alignof);
    hashmap_put(&map, "_Alignof", (void *)TK_alignof);

    if (opt_gnu_keywords)
      hashmap_put(&map, "asm", (void *)TK_asm);
    hashmap_put(&map, "__asm", (void *)TK_asm);
    hashmap_put(&map, "__asm__", (void *)TK_asm);

    if (opt_std >= STD_C23)
      hashmap_put(&map, "static_assert", (void *)TK_static_assert);
    hashmap_put(&map, "_Static_assert", (void *)TK_static_assert);

    if (opt_std >= STD_C23) {
      hashmap_put(&map, "true", (void *)TK_true);
      hashmap_put(&map, "false", (void *)TK_false);
      hashmap_put(&map, "nullptr", (void *)TK_nullptr);
    }

    if (opt_fdefer_ts)
      hashmap_put(&map, "defer", (void *)TK_defer);
    hashmap_put(&map, "_Defer", (void *)TK_defer);

    hashmap_put(&map, "void", (void *)TK_void);
    hashmap_put(&map, "char", (void *)TK_char);
    hashmap_put(&map, "short", (void *)TK_short);
    hashmap_put(&map, "int", (void *)TK_int);
    hashmap_put(&map, "long", (void *)TK_long);
    hashmap_put(&map, "float", (void *)TK_float);
    hashmap_put(&map, "double", (void *)TK_double);
    hashmap_put(&map, "unsigned", (void *)TK_unsigned);
    hashmap_put(&map, "struct", (void *)TK_struct);
    hashmap_put(&map, "union", (void *)TK_union);
    hashmap_put(&map, "enum", (void *)TK_enum);
    hashmap_put(&map, "typedef", (void *)TK_typedef);
    hashmap_put(&map, "static", (void *)TK_static);
    hashmap_put(&map, "extern", (void *)TK_extern);
    hashmap_put(&map, "auto", (void *)TK_auto);
    hashmap_put(&map, "register", (void *)TK_register);
    hashmap_put(&map, "_Atomic", (void *)TK_Atomic);
    hashmap_put(&map, "_Noreturn", (void *)TK_Noreturn);
    hashmap_put(&map, "_BitInt", (void *)TK_BitInt);

    hashmap_put(&map, "__auto_type", (void *)TK_auto_type);

    if (opt_std >= STD_C23)
      hashmap_put(&map, "alignas", (void *)TK_alignas);
    hashmap_put(&map, "_Alignas", (void *)TK_alignas);

    if (opt_std >= STD_C23)
      hashmap_put(&map, "bool", (void *)TK_bool);
    hashmap_put(&map, "_Bool", (void *)TK_bool);

    hashmap_put(&map, "const", (void *)TK_const);
    hashmap_put(&map, "__const", (void *)TK_const);
    hashmap_put(&map, "__const__", (void *)TK_const);

    if (opt_std >= STD_C23)
      hashmap_put(&map, "constexpr", (void *)TK_constexpr);

    if (opt_std >= STD_C99 || opt_gnu_keywords)
      hashmap_put(&map, "inline", (void *)TK_inline);
    hashmap_put(&map, "__inline", (void *)TK_inline);
    hashmap_put(&map, "__inline__", (void *)TK_inline);

    if (opt_std >= STD_C99)
      hashmap_put(&map, "restrict", (void *)TK_restrict);
    hashmap_put(&map, "__restrict", (void *)TK_restrict);
    hashmap_put(&map, "__restrict__", (void *)TK_restrict);

    hashmap_put(&map, "signed", (void *)TK_signed);
    hashmap_put(&map, "__signed", (void *)TK_signed);
    hashmap_put(&map, "__signed__", (void *)TK_signed);

    if (opt_std >= STD_C23 || opt_gnu_keywords)
      hashmap_put(&map, "typeof", (void *)TK_typeof);
    hashmap_put(&map, "__typeof", (void *)TK_typeof);
    hashmap_put(&map, "__typeof__", (void *)TK_typeof);

    if (opt_std >= STD_C23)
      hashmap_put(&map, "typeof_unqual", (void *)TK_typeof_unqual);
    hashmap_put(&map, "__typeof_unqual", (void *)TK_typeof_unqual);
    hashmap_put(&map, "__typeof_unqual__", (void *)TK_typeof_unqual);

    if (opt_std >= STD_C23)
      hashmap_put(&map, "thread_local", (void *)TK_thread_local);
    hashmap_put(&map, "_Thread_local", (void *)TK_thread_local);
    hashmap_put(&map, "__thread", (void *)TK_thread_local);

    hashmap_put(&map, "volatile", (void *)TK_volatile);
    hashmap_put(&map, "__volatile", (void *)TK_volatile);
    hashmap_put(&map, "__volatile__", (void *)TK_volatile);
  }

  void *val = hashmap_get2(&map, tok->loc, tok->len);
  return val ? (TokenKind)(intptr_t)val : TK_IDENT;
}

static void chk_delimit_end(const char **p, bool *invalid) {
  if (**p == '}') {
    *p += 1;
    return;
  }
  *invalid = true;
}

static bool read_ucn(const char **new_pos, const char *p, uint32_t *val, bool *invalid) {
  if (!Casecmp(*p, 'u'))
    return false;

  int len = (*p == 'u') ? 4 : 8;

  bool has_brace = p[1] == '{';
  p += 1 + has_brace;

  const char *end = p + len;
  uint64_t c = 0;
  for (; has_brace || p != end; p++) {
    if (!Isxdigit(*p))
      break;
    c = (c << 4) | from_hex(*p);
    if (c >> 32)
      *invalid = true;
  }

  if (has_brace)
    chk_delimit_end(&p, invalid);
  else if (p != end)
    *invalid = true;
  if ((c >= 0xD800 && c <= 0xDFFF) || (c > 0x10FFFF))
    *invalid = true;

  *val = *invalid ? 0 : c;
  *new_pos = p;
  return true;
}

static uint32_t read_escape_seq(const char **new_pos, const char *p, bool *invalid) {
  if (Inrange(*p, '0', '7')) {
    uint32_t c = *p++ - '0';
    if (Inrange(*p, '0', '7')) {
      c = (c << 3) + (*p++ - '0');
      if (Inrange(*p, '0', '7'))
        c = (c << 3) + (*p++ - '0');
    }
    *new_pos = p;
    return c;
  }

  if (*p == 'o' && p[1] == '{') {
    p += 2;
    if (!Inrange(*p, '0', '7'))
      *invalid = true;

    uint64_t c = 0;
    for (; Inrange(*p, '0', '7'); p++) {
      c = (c << 3) + *p - '0';
      if (c >> 32)
        *invalid = true;
    }

    chk_delimit_end(&p, invalid);
    *new_pos = p;
    return c;
  }

  if (*p == 'x') {
    bool has_brace = p[1] == '{';
    p += 1 + has_brace;

    if (!Isxdigit(*p))
      *invalid = true;

    uint64_t c = 0;
    for (; Isxdigit(*p); p++) {
      c = (c << 4) + from_hex(*p);
      if (c >> 32)
        *invalid = true;
    }

    if (has_brace)
      chk_delimit_end(&p, invalid);
    *new_pos = p;
    return c;
  }

  uint32_t c;
  if (read_ucn(new_pos, p, &c, invalid))
    return c;

  *new_pos = p + 1;

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
  default:  return *p;
  }
}

static Token *asm_string_literal(const char *p, char end) {
  const char *start = p++;
  bool is_closed = false;
  for (;;) {
    if (*p == end) {
      is_closed = true;
      break;
    }
    if (*p == '\n' || *p == '\0')
      break;
    if (*p == '\\')
      p++;
    p++;
  }
  Token *tok = new_token(TK_ASM_STR, start, p + is_closed);
  tok->str = arena_copy_string(&pp_arena, start + 1, p - start - 1);
  return tok;
}

// Find a closing double-quote.
static const char *string_literal_end(const char *p) {
  const char *start = p;
  for (; *p != '"'; p++) {
    if (*p == '\n' || *p == '\0')
      error_at(start, "unclosed string literal");
    if (*p == '\\')
      p++;
  }
  return p;
}

static Token *read_string_literal(const char *start, const char *quote, Type *ty) {
  const char *end = string_literal_end(quote + 1);
  char *buf = calloc(1, end - quote);
  int len = 0;
  bool invalid = false;

  for (const char *p = quote + 1; p < end;) {
    if (*p == '\\') {
      uint32_t c;
      if (read_ucn(&p, p + 1, &c, &invalid)) {
        len += encode_utf8(&buf[len], c);
        continue;
      }
      c = read_escape_seq(&p, p + 1, &invalid);
      if (c >> 8)
        invalid = true;
      buf[len++] = (uint8_t)c;
      continue;
    }
    buf[len++] = *p++;
  }
  if (invalid) {
    free(buf);
    return new_token(TK_INVALID, start, end + 1);
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
static Token *read_utf16_string_literal(const char *start, const char *quote) {
  const char *end = string_literal_end(quote + 1);
  uint16_t *buf = calloc(2, end - start);
  int len = 0;
  bool invalid = false;

  for (const char *p = quote + 1; p < end;) {
    uint32_t c;
    if (*p == '\\')
      c = read_escape_seq(&p, p + 1, &invalid);
    else
      c = decode_utf8(&p, p);

    if (c < 0x10000) {
      // Encode a code point in 2 bytes.
      buf[len++] = c;
      continue;
    }
    if (c <= 0x10ffff) {
      // Encode a code point in 4 bytes.
      c -= 0x10000;
      buf[len++] = 0xd800 + (c >> 10);
      buf[len++] = 0xdc00 + (c & 0x3ff);
      continue;
    }
    invalid = true;
  }
  if (invalid) {
    free(buf);
    return new_token(TK_INVALID, start, end + 1);
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
static Token *read_utf32_string_literal(const char *start, const char *quote, Type *ty) {
  const char *end = string_literal_end(quote + 1);
  uint32_t *buf = calloc(4, end - quote);
  int len = 0;
  bool invalid = false;

  for (const char *p = quote + 1; p < end;) {
    if (*p == '\\')
      buf[len++] = read_escape_seq(&p, p + 1, &invalid);
    else
      buf[len++] = decode_utf8(&p, p);
  }
  if (invalid) {
    free(buf);
    return new_token(TK_INVALID, start, end + 1);
  }
  Token *tok = new_token(TK_STR, start, end + 1);
  tok->ty = array_of(ty, len + 1);
  tok->str = (char *)buf;
  return tok;
}

static Token *read_char_literal(const char *start) {
  uint64_t val = 0;
  bool is_multi = false;
  bool invalid = false;
  const char *p = start + 1;
  if (*p == '\'')
    error_at(p, "empty character literal");

  for (;;) {
    if (*p == '\0')
      error_at(start, "unclosed character literal");

    uint32_t c;
    if (*p == '\\')
      c = read_escape_seq(&p, p + 1, &invalid);
    else
      c = decode_utf8(&p, p);
    if (c >> 8)
      invalid = true;

    val <<= 8;
    val |= (uint8_t)c;
    if (val >> 32)
      invalid = true;
    if (*p == '\'')
      break;
    is_multi = true;
  }
  if (invalid)
    return new_token(TK_INVALID, start, p + 1);

  if (!is_multi && !ty_pchar->is_unsigned)
    val = (uint32_t)(int8_t)val;

  Token *tok = new_token(TK_INT_NUM, start, p + 1);
  tok->ival = val;
  tok->ty = ty_int;
  return tok;
}

static Token *read_unicode_char_literal(const char *start, const char *quote, Type *ty) {
  bool invalid = false;
  const char *p = quote + 1;
  if (*p == '\0')
    error_at(start, "unclosed character literal");

  uint32_t c;
  if (*p == '\\')
    c = read_escape_seq(&p, p + 1, &invalid);
  else
    c = decode_utf8(&p, p);

  if (*p != '\'')
    error_at(p, "invalid unicode character literal");

  if (invalid)
    return new_token(TK_INVALID, start, p + 1);

  Token *tok = new_token(TK_INT_NUM, start, p + 1);
  tok->ival = c;
  tok->ty = ty;
  return tok;
}

// The definition of the numeric literal at the preprocessing stage
// is more relaxed than the definition of that at the later stages.
// In order to handle that, a numeric literal is tokenized as a
// "pp-number" token first and then converted to a regular number
// token after preprocessing.
static Token *new_pp_number(const char *start, const char *p) {
  for (;;) {
    if (Is_c_ident(*p) || *p == '.') {
      p++;
      continue;
    }
    if ((*p == '+' || *p == '-') && (Casecmp(p[-1], 'e') || Casecmp(p[-1], 'p'))) {
      p++;
      continue;
    }
    if (*p == '\'' && Isalnum(p[1]) && opt_std >= STD_C23) {
      p += 2;
      continue;
    }
    if ((unsigned char)*p >= 128) {
      if (is_ident2(decode_utf8(&p, p)))
        continue;
    }
    break;
  }
  return new_token(TK_PP_NUM, start, p);
}

static void push_digit(uint32_t **data, size_t *limb_cnt, int base, int digit) {
  uint32_t *buf = *data;
  size_t cnt = *limb_cnt;

  uint64_t accum = digit;
  for (size_t i = 0; i < cnt; i++) {
    accum += (uint64_t)base * buf[i];
    buf[i] = (uint32_t)accum;
    accum >>= 32;
  }
  if (accum) {
    buf = realloc(buf, (cnt + 1) * sizeof(uint32_t));
    buf[cnt++] = (uint32_t)accum;
  }
  *data = buf;
  *limb_cnt = cnt;
}

static bool convert_pp_bitint(const char *begin, const char *end, Node *node, int base,
                              bool is_unsigned) {
  uint32_t *data = calloc(2, sizeof(uint32_t));
  size_t limb32 = 1;

  for (const char *p = begin; p != end; p++) {
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
  node->num.bitint_data = (uint64_t *)data;
  node->ty = new_bitint(bit_width, node->tok);
  node->ty->is_unsigned = is_unsigned;
  return true;
}

static bool convert_pp_int(const char *loc, int len, Node *node) {
  const char *p = loc;
  const char *p_end = loc + len;

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
      if (Casecmp(p[1], 'o') && Inrange(p[2], '0', '7'))
        p += 2;
      base = 8;
    }
  }

  const char *digit_begin = p;
  uint64_t val = strtoull(p, (char **)&p, base);
  const char *digit_end = p;

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
  } else if (Startswith2(p, 'w', 'b') || Startswith2(p, 'W', 'B')) {
    wb = true, p += 2;
  }

  if (!u && Casecmp(*p, 'u'))
    u = true, p++;

  if (p != p_end)
    return false;
  if (!node)
    return true;
  if (wb)
    return convert_pp_bitint(digit_begin, digit_end, node, base, u);

  Type *ty;
  Type *max_ty = (base == 10) ? ty_ullong : ty_ulong;
  if (ll && u)
    ty = ty_ullong;
  else if (l && u)
    ty = ty_ulong;
  else if (u)
    ty = (val > UINT32_MAX) ? ty_ulong : ty_uint;
  else if (ll)
    ty = (val > INT64_MAX) ? ty_ullong : ty_llong;
  else if (l)
    ty = (val > INT64_MAX) ? max_ty : ty_long;
  else if (val > INT64_MAX)
    ty = max_ty;
  else if (val > UINT32_MAX)
    ty = ty_long;
  else if (val > INT32_MAX)
    ty = (base == 10) ? ty_long : ty_uint;
  else
    ty = ty_int;

  node->num.val = val;
  node->ty = ty;
  return true;
}

static const char *filter_digit_sep(Token *tok, int *len) {
  if (opt_std < STD_C23) {
    *len = tok->len;
    return tok->loc;
  }

  static size_t buflen;
  static char *buf;

  if (tok->len >= buflen) {
    buflen = tok->len + 1;
    buf = realloc(buf, buflen);
  }
  int cnt = 0;
  for (int i = 0; i < tok->len; i++) {
    if (tok->loc[i] == '\'')
      continue;
    buf[cnt++] = tok->loc[i];
  }
  buf[cnt] = '\0';
  *len = cnt;
  return buf;
}

bool is_pp_token_int(Token *tok) {
  int len;
  const char *p = filter_digit_sep(tok, &len);

  return convert_pp_int(p, len, NULL);
}

void convert_pp_number(Token *tok, Node *node) {
  if (tok->kind == TK_INT_NUM) {
    if ((uint64_t)tok->ival >> tok->ty->size * 8)
      error_tok(tok, "character too large for literal type");
    node->num.val = eval_sign_extend(tok->ty, tok->ival);
    node->ty = tok->ty;
    return;
  }

  int len;
  const char *p = filter_digit_sep(tok, &len);

  if (convert_pp_int(p, len, node))
    return;

  // If it's not an integer, it must be a floating point constant.
  char *end;
  long_double_t val = strtod(p, &end);
  Type *ty;
  if (*end == 'f' || *end == 'F') {
    val = strtof(p, NULL);
    ty = ty_float;
    end++;
  } else if (*end == 'l' || *end == 'L') {
#ifndef BOOTSTRAP_NO_LDOUBLE
    val = strtold(p, NULL);
#endif
    ty = ty_ldouble;
    end++;
  } else {
    ty = ty_double;
  }

  if (&p[len] != end)
    error_tok(tok, "invalid numeric constant");

  node->num.fval = val;
  node->ty = ty;
}

// Initialize line info for all tokens.
static void add_line_numbers(Token *tok, File *file, SlashDelta *dlt) {
  const char *start = file->contents;
  const char *p = start;
  int n = 1;

  const char *delta_pos = (dlt && dlt->sp && !opt_cc1_asm_pp) ? (dlt->sp[0].pos + start)
                                                              : NULL;
  int delta_cnt = 0;
  int idx = 0;
  do {
    if (p == delta_pos) {
      delta_cnt = dlt->sp[idx++].cnt;

      if (idx < dlt->len)
        delta_pos = dlt->sp[idx].pos + start;
    }
    if (p == tok->loc) {
      tok->line_no = n;
      tok->display_line_no = delta_cnt;
      tok = tok->next;
    }
    if (*p == '\n')
      n++;
  } while (*p++);

  if (dlt)
    free(dlt->sp);
}

void tokenize_string_literal(Token *tok, Type *basety) {
  Token *tok2;
  if (basety->size == 2)
    tok2 = read_utf16_string_literal(tok->loc, tok->loc);
  else
    tok2 = read_utf32_string_literal(tok->loc, tok->loc, basety);
  tok->ty = tok2->ty;
  tok->str = tok2->str;
}

void convert_ucn_ident(Token *tok) {
  const char *p = tok->loc;
  const char *end = p + tok->len;
  char *buf = calloc(1, tok->len);
  char *q = buf;

  while (p != end) {
    if (p > end)
      error_tok(tok, "invalid token");

    if (*p != '\\') {
      *q++ = *p++;
      continue;
    }
    bool invalid = false;
    uint32_t c;
    if (read_ucn(&p, p + 1, &c, &invalid) && !invalid) {
      if (c <= 0x7F)
        break;
      if (q == buf ? is_ident1(c) : is_ident2(c)) {
        q += encode_utf8(q, c);
        continue;
      }
    }
  }
  Token *orig = tok->origin;
  if (!orig) {
    orig = malloc(sizeof(Token));
    *orig = *tok;
  }
  tok->origin = orig;
  tok->loc = buf;
  tok->len = q - buf;

  static File file = {.name = "<ucn_buffer>", .is_placeholder = true};
  tok->file = &file;
}

Token *tokenize(File *file, SlashDelta *delta, Token **end) {
  current_file = file;

  const char *p = file->contents;
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
    if (*p == ' ' || *p == '\t' || *p == '\v' || *p == '\f') {
      for (char c = *p; *(++p) == c;)
        ;
      has_space = true;
      continue;
    }

    // Skip line comments.
    if (Startswith2(p, '/', '/')) {
      p += 2;
      while (*p != '\n')
        p++;
      has_space = true;
      continue;
    }

    // Skip block comments.
    if (Startswith2(p, '/', '*')) {
      const char *q = p + 2;
      for (; *q; q++)
        if (Startswith2(q, '*', '/'))
          break;
      if (!*q)
        error_at(p, "unclosed block comment");
      p = q + 2;
      has_space = true;
      continue;
    }

    // Numeric literal
    const char *p2 = (*p == '.') ? p + 1 : p;
    if (Isdigit(*p2)) {
      cur = cur->next = new_pp_number(p, p2 + 1);
      p += cur->len;
      continue;
    }

    // Punctuators
    if (!Isalpha(*p)) {
      TokenKind k;
      int len = read_punct(p, &k);
      if (len) {
        cur = cur->next = new_token(k, p, p + len);
        p += len;
        continue;
      }
    }

    if (opt_cc1_asm_pp) {
      if (*p == '$') {
        cur = cur->next = new_token(TK_PUNCT, p, p + 1);
        p++;
        continue;
      }
      if (*p == '"') {
        cur = cur->next = asm_string_literal(p, '"');
        p += cur->len;
        continue;
      }
      if (*p == '\'') {
        cur = cur->next = asm_string_literal(p, '\'');
        p += cur->len;
        continue;
      }
    } else {
      // String literal
      if (*p == '"') {
        cur = cur->next = read_string_literal(p, p, ty_pchar);
        p += cur->len;
        continue;
      }

      // UTF-8 string literal
      if (Startswith3(p, 'u', '8', '\"')) {
        if (opt_std >= STD_C23)
          cur = cur->next = read_string_literal(p, p + 2, ty_uchar);
        else
          cur = cur->next = read_string_literal(p, p + 2, ty_pchar);
        p += cur->len;
        continue;
      }

      // UTF-16 string literal
      if (Startswith2(p, 'u', '\"')) {
        cur = cur->next = read_utf16_string_literal(p, p + 1);
        p += cur->len;
        continue;
      }

      // Wide string literal
      if (Startswith2(p, 'L', '\"')) {
        cur = cur->next = read_utf32_string_literal(p, p + 1, ty_wchar_t);
        p += cur->len;
        continue;
      }

      // UTF-32 string literal
      if (Startswith2(p, 'U', '\"')) {
        cur = cur->next = read_utf32_string_literal(p, p + 1, ty_char32_t);
        p += cur->len;
        continue;
      }

      // Character literal
      if (*p == '\'') {
        cur = cur->next = read_char_literal(p);
        p += cur->len;
        continue;
      }

      // UTF-8 character literal
      if (Startswith3(p, 'u', '8', '\'') && opt_std >= STD_C23) {
        cur = cur->next = read_unicode_char_literal(p, p + 2, ty_uchar);
        p += cur->len;
        continue;
      }

      // UTF-16 character literal
      if (Startswith2(p, 'u', '\'')) {
        cur = cur->next = read_unicode_char_literal(p, p + 1, ty_char16_t);
        p += cur->len;
        continue;
      }

      // Wide character literal
      if (Startswith2(p, 'L', '\'')) {
        cur = cur->next = read_unicode_char_literal(p, p + 1, ty_wchar_t);
        p += cur->len;
        continue;
      }

      // UTF-32 character literal
      if (Startswith2(p, 'U', '\'')) {
        cur = cur->next = read_unicode_char_literal(p, p + 1, ty_char32_t);
        p += cur->len;
        continue;
      }
    }

    // Identifier or keyword
    Token *ident = read_ident(p);
    if (ident) {
      cur = cur->next = ident;
      p += cur->len;
      continue;
    }

    if (*p == '\\') {
      cur = cur->next = new_token(TK_PUNCT, p, p + 1);
      p++;
      continue;
    }

    error_at(p, "invalid token");
  }

  if (end && cur != &head)
    *end = cur;
  cur->next = new_token(TK_EOF, p, p);
  cur->next->at_bol = true;
  add_line_numbers(head.next, file, delta);
  return head.next;
}

Token *tokenize_file(const char *path, Token *tok, Token **end) {
  FILE *fp;

  if (strcmp(path, "-") == 0) {
    // By convention, read from stdin if a given filename is "-".
    fp = stdin;
  } else {
    fp = fopen(path, "r");
    if (!fp) {
      if (tok)
        error_tok(tok, "%s: cannot open file: %s", path, strerror(errno));
      error("%s: cannot open file: %s", path, strerror(errno));
    }
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

  // Wipe BOM markers
  if (Startswith3(buf, (char)0xef, (char)0xbb, (char)0xbf))
    buf[0] = buf[1] = buf[2] = ' ';

  canonicalize_newline(buf);

  SlashDelta dlt = {0};
  remove_backslash_newline(buf, &dlt);

  return tokenize(new_file(path, buf), &dlt, end);
}

int add_display_file(const char *path) {
  static HashMap map;
  HashEntry *ent = hashmap_get_or_insert(&map, path, strlen(path));
  int *idx = ent->val;
  if (idx)
    return *idx;

  strarray_push(&display_files, path);

  idx = ent->val = arena_malloc(&pp_arena, sizeof(*idx));
  *idx = display_files.len - 1;
  return *idx;
}

File *new_file(const char *name, const char *contents) {
  File *file = calloc(1, sizeof(File));
  file->name = name;
  file->file_no = file->display_file_no = add_display_file(name);
  file->contents = contents;
  file->incl_idx = INCL_ABS;
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

static void delta_push(SlashDelta *dlt, int pos, int cnt) {
  if (!dlt->sp) {
    dlt->sp = malloc(8 * sizeof(SlashPos));
    dlt->capacity = 8;
  } else {
    if (dlt->sp[dlt->len - 1].pos == pos) {
      dlt->sp[dlt->len - 1].cnt = cnt;
      return;
    }
  }
  if (dlt->capacity == dlt->len)
    dlt->sp = realloc(dlt->sp, (dlt->capacity += 8) * sizeof(SlashPos));

  dlt->sp[dlt->len].pos = pos;
  dlt->sp[dlt->len].cnt = cnt;
  dlt->len++;
}

static void remove_backslash_newline(char *start, SlashDelta *dlt) {
  char *p = strstr(start, "\\\n");
  if (p) {
    char *q = p;
    int n = 0;

    do {
      if (Startswith2(p, '\\', '\n')) {
        p += 2;
        n++;
        delta_push(dlt, q - start, n);
        continue;
      }
      if (n && *p == '\n') {
        for (; n > 0; n--)
          *q++ = '\n';
        delta_push(dlt, q - start, 0);
      }
      *q++ = *p++;
    } while (*p);

    *q = '\0';
  }
}
