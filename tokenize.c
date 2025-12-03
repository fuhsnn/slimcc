#include "slimcc.h"

// Input file
static File *current_file;

// True if the current position is at the beginning of a line
static bool at_bol;

// True if the current position follows a space character
static bool has_space;

static void canonicalize_newline(char *p);
static void remove_backslash_newline(char *p);

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
  if (!tok->file) {
    tok = tok->origin;
    if (!tok)
      internal_error();
  }
  if (tok->display_line_no)
    if (tok->file->file_no != tok->display_file_no || tok->line_no != tok->display_line_no)
      fprintf(stderr, "%s:%d | ", display_files.data[tok->display_file_no], tok->display_line_no);

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

    if (!is_iso_std)
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

    if (opt_std >= STD_C99 || !is_iso_std)
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

    if (opt_std >= STD_C23 || !is_iso_std)
      hashmap_put(&map, "typeof", (void *)TK_typeof);
    hashmap_put(&map, "__typeof", (void *)TK_typeof);
    hashmap_put(&map, "__typeof__", (void *)TK_typeof);

    if (opt_std >= STD_C23)
      hashmap_put(&map, "typeof_unqual", (void *)TK_typeof_unqual);

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

static Token *asm_string_literal(char *p, char end) {
  char *start = p++;
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
  tok->str = strndup(start + 1, p - start - 1);
  return tok;
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

static Token *read_char_literal(char *start) {
  uint32_t val = 0;
  bool is_multi = false;
  char *p = start + 1;
  if (*p == '\'')
    error_at(p, "empty character literal");

  for (;;) {
    if (*p == '\0')
      error_at(start, "unclosed character literal");

    uint8_t c;
    if (*p == '\\')
      c = read_escape_seq(&p, p + 1);
    else
      c = decode_utf8(&p, p);

    val <<= 8;
    val |= c;
    if (*p == '\'')
      break;
    is_multi = true;
  }
  if (!is_multi && !ty_pchar->is_unsigned)
    val = (int8_t)val;

  Token *tok = new_token(TK_INT_NUM, start, p + 1);
  tok->ival = val;
  tok->ty = ty_int;
  return tok;
}

static Token *read_unicode_char_literal(char *start, char *quote, Type *ty) {
  char *p = quote + 1;
  if (*p == '\0')
    error_at(start, "unclosed character literal");

  uint32_t c;
  if (*p == '\\')
    c = read_escape_seq(&p, p + 1);
  else
    c = decode_utf8(&p, p);

  if (*p != '\'')
    error_at(p, "invalid unicode character literal");

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
    accum += (uint64_t)base * buf[i];
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
  node->num.bitint_data = (uint64_t *)data;
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
      if (Casecmp(p[1], 'o') && Inrange(p[2], '0', '7'))
        p += 2;
      base = 8;
    }
  }

  char *digit_begin = p;
  uint64_t val = strtoull(p, &p, base);
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

// Converts a pp-number token to a regular number token.
void convert_pp_number(Token *tok, Node *node) {
  if (tok->kind == TK_INT_NUM) {
    node->num.val = eval_sign_extend(tok->ty, tok->ival);
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
  long_double_t val = strtod(p, &end);
  Type *ty;
  if (*end == 'f' || *end == 'F') {
    val = strtof(p, NULL);
    ty = ty_float;
    end++;
  } else if (*end == 'l' || *end == 'L') {
#ifndef NO_LONG_DOUBLE
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

    if (opt_cc1_asm_pp) {
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
        cur = cur->next = read_char_literal(p);
        p += cur->len;
        continue;
      }

      // UTF-8 character literal
      if (startswith3(p, 'u', '8', '\'') && opt_std >= STD_C23) {
        cur = cur->next = read_unicode_char_literal(p, p + 2, ty_uchar);
        p += cur->len;
        continue;
      }

      // UTF-16 character literal
      if (startswith2(p, 'u', '\'')) {
        cur = cur->next = read_unicode_char_literal(p, p + 1, ty_char16_t);
        p += cur->len;
        continue;
      }

      // Wide character literal
      if (startswith2(p, 'L', '\'')) {
        cur = cur->next = read_unicode_char_literal(p, p + 1, ty_wchar_t);
        p += cur->len;
        continue;
      }

      // UTF-32 character literal
      if (startswith2(p, 'U', '\'')) {
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
File *read_file(char *path, Token *tok, bool canon) {
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

  if (canon)
    return new_file(path, buf);

  // Wipe BOM markers
  if (startswith3(buf, (char)0xef, (char)0xbb, (char)0xbf))
    buf[0] = buf[1] = buf[2] = ' ';

  canonicalize_newline(buf);
  remove_backslash_newline(buf);
  return new_file(path, buf);
}

int add_display_file(char *path) {
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

File *new_file(char *name, char *contents) {
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
