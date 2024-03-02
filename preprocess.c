// This file implements the C preprocessor.
//
// The preprocessor takes a list of tokens as an input and returns a
// new list of tokens as an output.
//
// The preprocessing language is designed in such a way that that's
// guaranteed to stop even if there is a recursive macro.
// Informally speaking, a macro is applied only once for each token.
// That is, if a macro token T appears in a result of direct or
// indirect macro expansion of T, T won't be expanded any further.
// For example, if T is defined as U, and U is defined as T, then
// token T is expanded to U and then to T and the macro expansion
// stops at that point.
//
// To achieve the above behavior, we lock an expanding macro until
// the next token following its expansion ("stop_tok") is reached.

#include "slimcc.h"

typedef struct MacroParam MacroParam;
struct MacroParam {
  MacroParam *next;
  char *name;
};

typedef struct MacroArg MacroArg;
struct MacroArg {
  MacroArg *next;
  char *name;
  bool is_va_args;
  bool omit_comma;
  Token *tok;
  Token *expanded;
};

typedef Token *macro_handler_fn(Token *);

typedef struct Macro Macro;
struct Macro {
  bool is_objlike; // Object-like or function-like
  bool is_locked;
  Token *stop_tok;
  Macro *locked_next;
  MacroParam *params;
  char *va_args_name;
  Token *body;
  macro_handler_fn *handler;
};

// `#if` can be nested, so we use a stack to manage nested `#if`s.
typedef struct CondIncl CondIncl;
struct CondIncl {
  CondIncl *next;
  enum { IN_THEN, IN_ELIF, IN_ELSE } ctx;
  Token *tok;
  bool included;
};

// A linked list of locked macros. Since macro nesting happens in
// LIFO fashion (inner expansions end first), we only need to check
// the lastest one for unlocking.
static Macro *locked_macros;

static HashMap macros;
static CondIncl *cond_incl;
static HashMap pragma_once;
static int include_next_idx;
static HashMap include_guards;

static Token *preprocess2(Token *tok);
static Macro *find_macro(Token *tok);
static bool expand_macro(Token **rest, Token *tok);
static Token *subst(Token *tok, MacroArg *args);
static long is_supported_attr(Token **vendor, Token *tok);

static bool is_hash(Token *tok) {
  return tok->at_bol && equal(tok, "#");
}

// Some preprocessor directives such as #include allow extraneous
// tokens before newline. This function skips such tokens.
static Token *skip_line(Token *tok) {
  if (tok->at_bol)
    return tok;
  warn_tok(tok, "extra token");
  while (!tok->at_bol)
    tok = tok->next;
  return tok;
}

static Token *copy_token(Token *tok) {
  Token *t = calloc(1, sizeof(Token));
  *t = *tok;
  t->next = NULL;
  return t;
}

static Token *new_eof(Token *tok) {
  Token *t = copy_token(tok);
  t->kind = TK_EOF;
  t->len = 0;
  t->at_bol = true;
  return t;
}

static Token *to_eof(Token *tok) {
  tok->kind = TK_EOF;
  tok->len = 0;
  tok->at_bol = true;
  return tok;
}

static Token *new_pmark(Token *tok){
  Token *t = copy_token(tok);
  t->kind = TK_PMARK;
  t->len = 0;
  return t;
}

static void push_macro_lock(Macro *m, Token *tok) {
  m->is_locked = true;
  m->stop_tok = tok;
  m->locked_next = locked_macros;
  locked_macros = m;
}

static void pop_macro_lock(Token *tok) {
  while (locked_macros && locked_macros->stop_tok == tok) {
    locked_macros->is_locked = false;
    locked_macros = locked_macros->locked_next;
  }
}

static void pop_macro_lock_until(Token *tok, Token *end) {
  for (; tok != end; tok = tok->next)
    pop_macro_lock(tok);
}

static Token *skip_cond_incl2(Token *tok) {
  while (tok->kind != TK_EOF) {
    if (is_hash(tok) &&
        (equal(tok->next, "if") || equal(tok->next, "ifdef") ||
         equal(tok->next, "ifndef"))) {
      tok = skip_cond_incl2(tok->next->next);
      continue;
    }
    if (is_hash(tok) && equal(tok->next, "endif"))
      return tok->next->next;
    tok = tok->next;
  }
  return tok;
}

// Skip until next `#else`, `#elif` or `#endif`.
// Nested `#if` and `#endif` are skipped.
static Token *skip_cond_incl(Token *tok) {
  while (tok->kind != TK_EOF) {
    if (is_hash(tok) &&
        (equal(tok->next, "if") || equal(tok->next, "ifdef") ||
         equal(tok->next, "ifndef"))) {
      tok = skip_cond_incl2(tok->next->next);
      continue;
    }

    if (is_hash(tok) &&
        (equal(tok->next, "elif") || equal(tok->next, "else") ||
         equal(tok->next, "endif")))
      break;
    tok = tok->next;
  }
  return tok;
}

// Double-quote a given string and returns it.
static char *quote_string(char *str) {
  int bufsize = 3;
  for (int i = 0; str[i]; i++) {
    if (str[i] == '\\' || str[i] == '"')
      bufsize++;
    bufsize++;
  }

  char *buf = calloc(1, bufsize);
  char *p = buf;
  *p++ = '"';
  for (int i = 0; str[i]; i++) {
    if (str[i] == '\\' || str[i] == '"')
      *p++ = '\\';
    *p++ = str[i];
  }
  *p++ = '"';
  *p++ = '\0';
  return buf;
}

static Token *new_str_token(char *str, Token *tmpl) {
  char *buf = quote_string(str);
  return tokenize(new_file(tmpl->file->name, tmpl->file->file_no, buf), NULL);
}

// Copy all tokens until the next newline, terminate them with
// an EOF token and then returns them. This function is used to
// create a new list of tokens for `#if` arguments.
static Token *copy_line(Token **rest, Token *tok) {
  Token head = {0};
  Token *cur = &head;

  for (; !tok->at_bol; tok = tok->next)
    cur = cur->next = copy_token(tok);

  cur->next = new_eof(tok);
  *rest = tok;
  return head.next;
}

// Split tokens before the next newline into an EOF-terminated list.
static Token *split_line(Token **rest, Token *tok) {
  Token head = {.next = tok};
  Token *cur = &head;

  while (!cur->next->at_bol)
    cur = cur->next;

  *rest = cur->next;
  cur->next = new_eof(tok);
  return head.next;
}

static Token *split_paren(Token **rest, Token *tok) {
  Token *start = tok;
  Token head = {0};
  Token *cur = &head;

  int level = 0;
  while (!(level == 0 && equal(tok, ")"))) {
    if (equal(tok, "("))
      level++;
    else if (equal(tok, ")"))
      level--;
    else if (tok->kind == TK_EOF)
      error_tok(start, "unterminated list");

    cur = cur->next = tok;
    tok = tok->next;
  }
  *rest = tok->next;
  cur->next = to_eof(tok);
  return head.next;
}

static Token *split_bracket(Token **rest, Token *tok) {
  Token *start = tok;
  Token head = {0};
  Token *cur = &head;

  int level = 0;
  while (!(level == 0 && equal(tok, "]"))) {
    if (equal(tok, "["))
      level++;
    else if (equal(tok, "]"))
      level--;
    else if (tok->kind == TK_EOF)
      error_tok(start, "unterminated list");

    cur = cur->next = tok;
    tok = tok->next;
  }
  *rest = tok->next;
  cur->next = to_eof(tok);
  return head.next;
}

static Token *new_num_token(int val, Token *tmpl) {
  char *buf = format("%d\n", val);
  return tokenize(new_file(tmpl->file->name, tmpl->file->file_no, buf), NULL);
}

static Token *read_const_expr(Token **rest, Token *tok) {
  tok = split_line(rest, tok);

  Token head = {0};
  Token *cur = &head;

  while (tok->kind != TK_EOF) {
    // "defined(foo)" or "defined foo" becomes "1" if macro "foo"
    // is defined. Otherwise "0".
    if (equal(tok, "defined")) {
      Token *start = tok;
      tok = tok->next;
      bool has_paren = consume(&tok, tok, "(");

      if (tok->kind != TK_IDENT)
        error_tok(start, "macro name must be an identifier");
      Macro *m = find_macro(tok);
      tok = tok->next;

      if (has_paren)
        tok = skip(tok, ")");

      cur = cur->next = new_num_token(m ? 1 : 0, start);
      continue;
    }

    cur = cur->next = tok;
    tok = tok->next;
  }

  cur->next = tok;
  return head.next;
}

// Read and evaluate a constant expression.
static long eval_const_expr(Token **rest, Token *tok) {
  Token *start = tok;
  Token *expr = read_const_expr(rest, tok->next);
  expr = preprocess2(expr);

  if (expr->kind == TK_EOF)
    error_tok(start, "no expression");

  // [https://www.sigbus.info/n1570#6.10.1p4] The standard requires
  // we replace remaining non-macro identifiers with "0" before
  // evaluating a constant expression. For example, `#if foo` is
  // equivalent to `#if 0` if foo is not defined.
  for (Token *t = expr; t->kind != TK_EOF; t = t->next) {
    if (t->kind == TK_IDENT) {
      Token *next = t->next;
      if (equal(next, "("))
        error_tok(t, "undefined function-like macro");
      *t = *new_num_token(0, t);
      t->next = next;
    }
  }
  Token *rest2;
  long val = const_expr(&rest2, expr);
  if (rest2->kind != TK_EOF)
    error_tok(rest2, "extra token");
  return val;
}

static CondIncl *push_cond_incl(Token *tok, bool included) {
  CondIncl *ci = calloc(1, sizeof(CondIncl));
  ci->next = cond_incl;
  ci->ctx = IN_THEN;
  ci->tok = tok;
  ci->included = included;
  cond_incl = ci;
  return ci;
}

static Macro *find_macro(Token *tok) {
  if (tok->kind != TK_IDENT)
    return NULL;
  return hashmap_get2(&macros, tok->loc, tok->len);
}

static Macro *add_macro(char *name, bool is_objlike, Token *body) {
  Macro *m = calloc(1, sizeof(Macro));
  m->is_objlike = is_objlike;
  m->body = body;
  hashmap_put(&macros, name, m);
  return m;
}

static MacroParam *read_macro_params(Token **rest, Token *tok, char **va_args_name) {
  MacroParam head = {0};
  MacroParam *cur = &head;

  while (!equal(tok, ")")) {
    if (cur != &head)
      tok = skip(tok, ",");

    if (equal(tok, "...")) {
      *va_args_name = "__VA_ARGS__";
      *rest = skip(tok->next, ")");
      return head.next;
    }

    if (tok->kind != TK_IDENT)
      error_tok(tok, "expected an identifier");

    if (equal(tok->next, "...")) {
      *va_args_name = strndup(tok->loc, tok->len);
      *rest = skip(tok->next->next, ")");
      return head.next;
    }

    MacroParam *m = calloc(1, sizeof(MacroParam));
    m->name = strndup(tok->loc, tok->len);
    cur = cur->next = m;
    tok = tok->next;
  }

  *rest = tok->next;
  return head.next;
}

static void read_macro_definition(Token **rest, Token *tok) {
  if (tok->kind != TK_IDENT)
    error_tok(tok, "macro name must be an identifier");
  char *name = strndup(tok->loc, tok->len);
  tok = tok->next;

  if (!tok->has_space && equal(tok, "(")) {
    // Function-like macro
    char *va_args_name = NULL;
    MacroParam *params = read_macro_params(&tok, tok->next, &va_args_name);

    Macro *m = add_macro(name, false, split_line(rest, tok));
    m->params = params;
    m->va_args_name = va_args_name;
  } else {
    // Object-like macro
    add_macro(name, true, split_line(rest, tok));
  }
}

static MacroArg *read_macro_arg_one(Token **rest, Token *tok, bool read_rest) {
  Token head = {0};
  Token *cur = &head;
  int level = 0;
  Token *start = tok;

  for (;;) {
    pop_macro_lock(tok);
    if (locked_macros && tok->kind == TK_IDENT) {
      Macro *m = find_macro(tok);
      if (m && m->is_locked)
        tok->dont_expand = true;
    }

    if (level == 0 && equal(tok, ")"))
      break;
    if (level == 0 && !read_rest && equal(tok, ","))
      break;

    if (tok->kind == TK_EOF)
      error_tok(start, "unterminated list");

    if (equal(tok, "("))
      level++;
    else if (equal(tok, ")"))
      level--;

    cur = cur->next = copy_token(tok);
    tok = tok->next;
  }

  cur->next = new_eof(tok);

  MacroArg *arg = calloc(1, sizeof(MacroArg));
  arg->tok = head.next;
  *rest = tok;
  return arg;
}

static MacroArg *
read_macro_args(Token **rest, Token *tok, MacroParam *params, char *va_args_name) {
  pop_macro_lock(tok->next);
  pop_macro_lock(tok->next->next);
  tok = tok->next->next;

  MacroArg head = {0};
  MacroArg *cur = &head;

  for (MacroParam *pp = params; pp; pp = pp->next) {
    if (cur != &head)
      tok = skip(tok, ",");
    cur = cur->next = read_macro_arg_one(&tok, tok, false);
    cur->name = pp->name;
  }

  if (va_args_name) {
    Token *start = tok;
    if (!equal(tok, ")") && params)
      tok = skip(tok, ",");

    MacroArg *arg = read_macro_arg_one(&tok, tok, true);
    arg->omit_comma = equal(start, ")");
    arg->name = va_args_name;
    arg->is_va_args = true;
    cur->next = arg;
  }

  *rest = skip(tok, ")");
  return head.next;
}

static Token *expand_arg(MacroArg *arg) {
  if (arg->expanded)
    return arg->expanded;

  Token *tok = arg->tok;
  Token head = {0};
  Token *cur = &head;
  Macro *start_m = locked_macros;

  for (; tok->kind != TK_EOF; pop_macro_lock(tok)) {
    if (expand_macro(&tok, tok))
      continue;

    cur = cur->next = copy_token(tok);
    tok = tok->next;
  }

  if (start_m != locked_macros) {
    internal_error();
  }
  cur->next = new_eof(tok);
  return arg->expanded = head.next;
}

static MacroArg *find_arg(Token **rest, Token *tok, MacroArg *args) {
  for (MacroArg *ap = args; ap; ap = ap->next) {
    if (equal(tok, ap->name)) {
      if (rest)
        *rest = tok->next;
      return ap;
    }
  }

  // __VA_OPT__(x) is treated like a parameter which expands to parameter-
  // substituted (x) if macro-expanded __VA_ARGS__ is not empty.
  if (equal(tok, "__VA_OPT__") && equal(tok->next, "(")) {
    MacroArg *arg = read_macro_arg_one(&tok, tok->next->next, true);

    MacroArg *va = NULL;
    for (MacroArg *ap = args; ap; ap = ap->next)
      if (ap->is_va_args)
        va = ap;

    if (va && expand_arg(va)->kind != TK_EOF)
      arg->tok = subst(arg->tok, args);
    else
      arg->tok = new_eof(tok);

    arg->expanded = arg->tok;
    if (rest)
      *rest = tok->next;
    return arg;
  }
  return NULL;
}

// Concatenates all tokens in `tok` and returns a new string.
static char *join_tokens(Token *tok, Token *end) {
  // Compute the length of the resulting token.
  int len = 1;
  for (Token *t = tok; t != end && t->kind != TK_EOF; t = t->next) {
    if (t->kind == TK_PMARK)
      continue;
    if ((t->has_space || t->at_bol) && len != 1)
      len++;
    len += t->len;
  }

  char *buf = calloc(1, len);

  // Copy token texts.
  int pos = 0;
  for (Token *t = tok; t != end && t->kind != TK_EOF; t = t->next) {
    if (t->kind == TK_PMARK)
      continue;
    if ((t->has_space || t->at_bol) && pos != 0)
      buf[pos++] = ' ';
    strncpy(buf + pos, t->loc, t->len);
    pos += t->len;
  }
  buf[pos] = '\0';
  return buf;
}

// Concatenates all tokens in `arg` and returns a new string token.
// This function is used for the stringizing operator (#).
static Token *stringize(Token *hash, Token *arg) {
  // Create a new string token. We need to set some value to its
  // source location for error reporting function, so we use a macro
  // name token as a template.
  char *s = join_tokens(arg, NULL);
  return new_str_token(s, hash);
}

static void align_token(Token *tok1, Token *tok2) {
  tok1->at_bol = tok2->at_bol;
  tok1->has_space = tok2->has_space;
}

// Concatenate two tokens to create a new token.
static Token *paste(Token *lhs, Token *rhs) {
  // Paste the two tokens.
  char *buf = format("%.*s%.*s", lhs->len, lhs->loc, rhs->len, rhs->loc);

  // Tokenize the resulting string.
  Token *tok = tokenize(new_file(lhs->file->name, lhs->file->file_no, buf), NULL);
  align_token(tok, lhs);
  if (tok->next->kind != TK_EOF)
    error_tok(lhs, "pasting forms '%s', an invalid token", buf);
  return tok;
}

// Replace func-like macro parameters with given arguments.
static Token *subst(Token *tok, MacroArg *args) {
  Token head = {0};
  Token *cur = &head;

  while (tok->kind != TK_EOF) {
    Token *start = tok;

    // "#" followed by a parameter is replaced with stringized actuals.
    if (equal(tok, "#")) {
      MacroArg *arg = find_arg(&tok, tok->next, args);
      if (!arg)
        error_tok(tok->next, "'#' is not followed by a macro parameter");
      cur = cur->next = stringize(start, arg->tok);
      align_token(cur, start);
      continue;
    }

    // [GNU] If __VA_ARGS__ is empty, `,##__VA_ARGS__` is expanded
    // to an empty token list. Otherwise, it's expanded to `,` and
    // __VA_ARGS__.
    if (equal(tok, ",") && equal(tok->next, "##")) {
      MacroArg *arg = find_arg(NULL, tok->next->next, args);
      if (arg && arg->is_va_args) {
        if (arg->omit_comma) {
          tok = tok->next->next->next;
          continue;
        }
        cur = cur->next = copy_token(tok);
        tok = tok->next->next;
        continue;
      }
    }

    if (equal(tok, "##")) {
      if (cur == &head)
        error_tok(tok, "'##' cannot appear at start of macro expansion");

      if (tok->next->kind == TK_EOF)
        error_tok(tok, "'##' cannot appear at end of macro expansion");

      if (cur->kind == TK_PMARK) {
        tok = tok->next;
        continue;
      }

      MacroArg *arg = find_arg(&tok, tok->next, args);
      if (arg) {
        if (arg->tok->kind == TK_EOF)
          continue;

        if (arg->tok->kind != TK_PMARK)
          *cur = *paste(cur, arg->tok);

        for (Token *t = arg->tok->next; t->kind != TK_EOF; t = t->next)
          cur = cur->next = copy_token(t);
        continue;
      }
      *cur = *paste(cur, tok->next);
      tok = tok->next->next;
      continue;
    }

    MacroArg *arg = find_arg(&tok, tok, args);
    if (arg) {
      Token *t;
      if (equal(tok, "##"))
        t = arg->tok;
      else
        t = expand_arg(arg);

      if (t->kind == TK_EOF) {
        cur = cur->next = new_pmark(t);
        continue;
      }

      align_token(t, start);
      for (; t->kind != TK_EOF; t = t->next)
        cur = cur->next = copy_token(t);
      continue;
    }

    // Handle a non-parameter token.
    cur = cur->next = copy_token(tok);
    tok = tok->next;
    continue;
  }

  cur->next = tok;
  return head.next;
}

static Token *insert_objlike(Token *tok, Token *tok2, Token *orig) {
  Token head = {0};
  Token *cur = &head;
  if (orig->origin)
    orig = orig->origin;

  for (; tok->kind != TK_EOF; tok = tok->next) {
    if (equal(tok, "##")) {
      if (cur == &head || tok->next->kind == TK_EOF)
        error_tok(tok, "'##' cannot appear at either end of macro expansion");

      tok = tok->next;
      *cur = *paste(cur, tok);
    } else {
      cur = cur->next = copy_token(tok);
    }
    cur->origin = orig;
  }
  cur->next = tok2;
  return head.next;
}

static Token *insert_funclike(Token *tok, Token *tok2, Token *orig) {
  Token head = {0};
  Token *cur = &head;
  if (orig->origin)
    orig = orig->origin;

  for (; tok->kind != TK_EOF; tok = tok->next) {
    if (tok->kind == TK_PMARK)
      continue;

    cur = cur->next = tok;
    cur->origin = orig;
  }
  cur->next = tok2;
  return head.next;
}

// If tok is a macro, expand it and return true.
// Otherwise, do nothing and return false.
static bool expand_macro(Token **rest, Token *tok) {
  if (tok->dont_expand)
    return false;

  Macro *m = find_macro(tok);
  if (!m)
    return false;

  if (m->is_locked) {
    tok->dont_expand = true;
    return false;
  }

  // Built-in dynamic macro application such as __LINE__
  if (m->handler) {
    *rest = m->handler(tok);
    align_token(*rest, tok);
    return true;
  }

  // If a funclike macro token is not followed by an argument list,
  // treat it as a normal identifier.
  if (!m->is_objlike && !equal(tok->next, "("))
    return false;

  if (!m->is_objlike && m->body->kind == TK_EOF && equal(tok, "__attribute__")) {
    char *slash = strrchr(m->body->file->name, '/');
    if (slash && !strcmp(slash + 1, "cdefs.h")) {
      tok->is_hidden_attr = true;
      push_macro_lock(m, skip_paren(skip(tok->next, "(")));
      return true;
    }
  }

  // The token right after the macro. For funclike, after parentheses.
  Token *stop_tok;

  if (m->is_objlike) {
    stop_tok = tok->next;
    *rest = insert_objlike(m->body, stop_tok, tok);
  } else {
    MacroArg *args = read_macro_args(&stop_tok, tok, m->params, m->va_args_name);
    Token *body = subst(m->body, args);
    *rest = insert_funclike(body, stop_tok, tok);
  }

  if (*rest != stop_tok) {
    push_macro_lock(m, stop_tok);
    align_token(*rest, tok);
  } else if (!m->is_objlike) {
    (*rest)->at_bol |= tok->at_bol;
    (*rest)->has_space |= tok->has_space;
  }
  return true;
}

char *search_include_paths(char *filename) {
  if (filename[0] == '/')
    return filename;

  static HashMap cache;
  char *cached = hashmap_get(&cache, filename);
  if (cached)
    return cached;

  // Search a file from the include paths.
  for (int i = 0; i < include_paths.len; i++) {
    char *path = format("%s/%s", include_paths.data[i], filename);
    if (!file_exists(path))
      continue;
    hashmap_put(&cache, filename, path);
    include_next_idx = i + 1;
    return path;
  }
  return NULL;
}

static char *search_include_next(char *filename) {
  for (; include_next_idx < include_paths.len; include_next_idx++) {
    char *path = format("%s/%s", include_paths.data[include_next_idx], filename);
    if (file_exists(path))
      return path;
  }
  return NULL;
}

// Read an #include argument.
static char *read_include_filename(Token *tok, bool *is_dquote) {
  // Pattern 3: #include FOO
  // In this case FOO must be macro-expanded to either
  // a single string token or a sequence of "<" ... ">".
  if (tok->kind == TK_IDENT)
    tok = preprocess2(tok);

  // Pattern 1: #include "foo.h"
  if (tok->kind == TK_STR) {
    // A double-quoted filename for #include is a special kind of
    // token, and we don't want to interpret any escape sequences in it.
    // For example, "\f" in "C:\foo" is not a formfeed character but
    // just two non-control characters, backslash and f.
    // So we don't want to use token->str.
    *is_dquote = true;
    skip_line(tok->next);
    return strndup(tok->loc + 1, tok->len - 2);
  }

  // Pattern 2: #include <foo.h>
  if (equal(tok, "<")) {
    // Reconstruct a filename from a sequence of tokens between
    // "<" and ">".
    Token *start = tok;

    // Find closing ">".
    for (; !equal(tok, ">"); tok = tok->next)
      if (tok->kind == TK_EOF)
        error_tok(tok, "expected '>'");

    *is_dquote = false;
    skip_line(tok->next);
    return join_tokens(start->next, tok);
  }

  error_tok(tok, "expected a filename");
}

static Token *include_file(Token *tok, char *path, Token *filename_tok) {
  // Check for "#pragma once"
  if (hashmap_get(&pragma_once, path))
    return tok;

  char *guard_name = hashmap_get(&include_guards, path);
  if (guard_name && hashmap_get(&macros, guard_name))
    return tok;

  Token *end = NULL;
  Token *start = tokenize_file(path, &end);
  if (!start)
    error_tok(filename_tok, "%s: cannot open file: %s", path, strerror(errno));
  if (!end)
    return tok;

  if (is_hash(start) && equal(start->next, "ifndef") &&
      start->next->next->kind == TK_IDENT && equal(end, "endif"))
    start->next->guard_file = end->guard_file = path;

  end->next = tok;
  return start;
}

// Read #line arguments
static void read_line_marker(Token **rest, Token *tok) {
  Token *start = tok;
  tok = preprocess2(copy_line(rest, tok));
  convert_pp_number(tok);

  if (tok->kind != TK_NUM || tok->ty->kind != TY_INT)
    error_tok(tok, "invalid line marker");
  start->file->line_delta = tok->val - start->line_no - 1;

  tok = tok->next;
  if (tok->kind == TK_EOF)
    return;

  if (tok->kind != TK_STR)
    error_tok(tok, "filename expected");

  start->file->display_file = add_input_file(tok->str, NULL);
}

static void add_loc_info(Token *tok) {
  Token *tmpl = tok;
  if (tmpl->origin)
    tmpl = tmpl->origin;

  tok->display_file_no = tmpl->file->display_file->file_no;
  tok->display_line_no = tmpl->line_no + tmpl->file->line_delta;
}

// Visit all tokens in `tok` while evaluating preprocessing
// macros and directives.
static Token *preprocess2(Token *tok) {
  Token head = {0};
  Token *cur = &head;
  Macro *start_m = locked_macros;

  for (; tok->kind != TK_EOF; pop_macro_lock(tok)) {
    // If it is a macro, expand it.
    if (expand_macro(&tok, tok))
      continue;

    if (!is_hash(tok) || locked_macros) {
      if (opt_g)
        add_loc_info(tok);
      cur = cur->next = tok;
      tok = tok->next;
      continue;
    }

    Token *start = tok;
    tok = tok->next;

    if (equal(tok, "include")) {
      bool is_dquote;
      char *filename = read_include_filename(split_line(&tok, tok->next), &is_dquote);
      if (filename[0] != '/' && is_dquote) {
        char *path = format("%s/%s", dirname(strdup(start->file->name)), filename);
        if (file_exists(path)) {
          tok = include_file(tok, path, start->next->next);
          continue;
        }
      }

      char *path = search_include_paths(filename);
      tok = include_file(tok, path ? path : filename, start->next->next);
      continue;
    }

    if (equal(tok, "include_next")) {
      bool ignore;
      char *filename = read_include_filename(split_line(&tok, tok->next), &ignore);
      char *path = search_include_next(filename);
      tok = include_file(tok, path ? path : filename, start->next->next);
      continue;
    }

    if (equal(tok, "define")) {
      read_macro_definition(&tok, tok->next);
      continue;
    }

    if (equal(tok, "undef")) {
      tok = tok->next;
      if (tok->kind != TK_IDENT)
        error_tok(tok, "macro name must be an identifier");
      undef_macro(strndup(tok->loc, tok->len));
      tok = skip_line(tok->next);
      continue;
    }

    if (equal(tok, "if")) {
      long val = eval_const_expr(&tok, tok);
      push_cond_incl(start, val);
      if (!val)
        tok = skip_cond_incl(tok);
      continue;
    }

    if (equal(tok, "ifdef")) {
      bool defined = find_macro(tok->next);
      push_cond_incl(tok, defined);
      tok = skip_line(tok->next->next);
      if (!defined)
        tok = skip_cond_incl(tok);
      continue;
    }

    if (equal(tok, "ifndef")) {
      bool defined = find_macro(tok->next);
      push_cond_incl(tok, !defined);
      tok = skip_line(tok->next->next);
      if (defined)
        tok = skip_cond_incl(tok);
      continue;
    }

    if (equal(tok, "elif")) {
      if (!cond_incl || cond_incl->ctx == IN_ELSE)
        error_tok(start, "stray #elif");
      cond_incl->ctx = IN_ELIF;

      if (!cond_incl->included && eval_const_expr(&tok, tok))
        cond_incl->included = true;
      else
        tok = skip_cond_incl(tok);
      continue;
    }

    if (equal(tok, "else")) {
      if (!cond_incl || cond_incl->ctx == IN_ELSE)
        error_tok(start, "stray #else");
      cond_incl->ctx = IN_ELSE;
      tok = skip_line(tok->next);

      if (cond_incl->included)
        tok = skip_cond_incl(tok);
      continue;
    }

    if (equal(tok, "endif")) {
      if (!cond_incl)
        error_tok(start, "stray #endif");

      if (tok->guard_file && tok->guard_file == cond_incl->tok->guard_file) {
        Token *name_tok = cond_incl->tok->next;
        char *guard_name = strndup(name_tok->loc, name_tok->len);
        hashmap_put(&include_guards, tok->guard_file, guard_name);
      }

      cond_incl = cond_incl->next;
      tok = skip_line(tok->next);
      continue;
    }

    if (equal(tok, "line")) {
      read_line_marker(&tok, tok->next);
      continue;
    }

    if (tok->kind == TK_PP_NUM) {
      read_line_marker(&tok, tok);
      continue;
    }

    if (equal(tok, "pragma") && equal(tok->next, "once")) {
      hashmap_put(&pragma_once, tok->file->name, (void *)1);
      tok = skip_line(tok->next->next);
      continue;
    }

    if (equal(tok, "pragma") && opt_E) {
      tok = start;
      do {
        cur = cur->next = tok;
        tok = tok->next;
      } while (!tok->at_bol);
      continue;
    }

    if (equal(tok, "pragma")) {
      do {
        tok = tok->next;
      } while (!tok->at_bol);
      continue;
    }

    if (equal(tok, "error"))
      error_tok(tok, "error");

    if (equal(tok, "warning")) {
      warn_tok(tok, "warning");
      do {
        tok = tok->next;
      } while (!tok->at_bol);
      continue;
    }

    if (opt_cc1_asm_pp) {
      tok = start;
      do {
        cur = cur->next = tok;
        tok = tok->next;
      } while (!tok->at_bol);
      continue;
    }

    // `#`-only line is legal. It's called a null directive.
    if (tok->at_bol)
      continue;

    error_tok(tok, "invalid preprocessor directive");
  }

  if (start_m != locked_macros) {
    internal_error();
  }
  cur->next = tok;
  return head.next;
}

void define_macro(char *name, char *buf) {
  Token *tok = tokenize(new_file("<built-in>", 1, buf), NULL);
  add_macro(name, true, tok);
}

void undef_macro(char *name) {
  hashmap_delete(&macros, name);
}

static Macro *add_builtin(char *name, macro_handler_fn *fn) {
  Macro *m = add_macro(name, true, NULL);
  m->handler = fn;
  return m;
}

static Token *file_macro(Token *start) {
  Token *tok = start;
  if (tok->origin)
    tok = tok->origin;
  tok = new_str_token(tok->file->display_file->name, tok);
  tok->next = start->next;
  return tok;
}

static Token *line_macro(Token *start) {
  Token *tok = start;
  if (tok->origin)
    tok = tok->origin;
  int i = tok->line_no + tok->file->line_delta;
  tok = new_num_token(i, tok);
  tok->next = start->next;
  return tok;
}

// __COUNTER__ is expanded to serial values starting from 0.
static Token *counter_macro(Token *start) {
  static int i = 0;
  Token *tok = new_num_token(i++, start);
  tok->next = start->next;
  return tok;
}

// __TIMESTAMP__ is expanded to a string describing the last
// modification time of the current file. E.g.
// "Fri Jul 24 01:32:50 2020"
static Token *timestamp_macro(Token *start) {
  Token *tok;
  struct stat st;
  if (stat(start->file->name, &st) != 0) {
    tok = new_str_token("??? ??? ?? ??:??:?? ????", start);
  } else {
    char buf[30];
    ctime_r(&st.st_mtime, buf);
    buf[24] = '\0';
    tok = new_str_token(buf, start);
  }
  tok->next = start->next;
  return tok;
}

static Token *base_file_macro(Token *start) {
  Token *tok = new_str_token(base_file, start);
  tok->next = start->next;
  return tok;
}

static Token *pragma_macro(Token *start) {
  Token *tok = start->next;
  Token *str;
  for (int progress = 0;;) {
    if (tok->kind == TK_EOF)
      error_tok(start, "unterminated _Pragma sequence");

    pop_macro_lock(tok);
    if (expand_macro(&tok, tok))
      continue;

    switch (progress++) {
    case 0:
      tok = skip(tok, "(");
      continue;
    case 1:
      if (tok->kind != TK_STR || tok->len < 2)
        error_tok(tok, "expected string literal");
      str = tok;
      tok = tok->next;
      continue;
    case 2:
      tok = skip(tok, ")");
      tok->at_bol = true;
    }
    break;
  }
  char *buf = calloc(1, str->len + 7);
  memcpy(buf, "#pragma ", 8);
  memcpy(buf + 8, str->loc + 1, str->len - 2);

  Token *end;
  Token *hash = tokenize(new_file(start->file->name, start->file->file_no, buf), &end);
  end->next = tok;
  return hash;
}

static Token *has_include_macro(Token *start) {
  Token *tok = skip(start->next, "(");

  bool is_dquote;
  char *filename = read_include_filename(split_paren(&tok, tok), &is_dquote);

  bool found = false;
  if (filename[0] != '/' && is_dquote) {
    char *path = format("%s/%s", dirname(strdup(start->file->name)), filename);
    found = file_exists(path);
  }
  if (!found)
    found = search_include_paths(filename);

  pop_macro_lock_until(start, tok);
  Token *tok2 = new_num_token(found, start);
  tok2->next = tok;
  return tok2;
}

static Token *has_attribute_macro(Token *start) {
  Token *tok = skip(start->next, "(");

  long val = is_supported_attr(NULL, tok);

  tok = skip(tok->next, ")");
  pop_macro_lock_until(start, tok);
  Token *tok2 = new_num_token(val, start);
  tok2->next = tok;
  return tok2;
}

static Token *has_c_attribute_macro(Token *start) {
  Token *tok = skip(start->next, "(");

  Token *vendor = NULL;
  if (tok->kind == TK_IDENT && equal(tok->next, ":")) {
    vendor = tok;
    tok = skip(tok->next->next, ":");
  }
  long val = is_supported_attr(&vendor, tok);

  tok = skip(tok->next, ")");
  pop_macro_lock_until(start, tok);
  Token *tok2 = new_num_token(val, start);
  tok2->next = tok;
  return tok2;
}

static Token *has_builtin_macro(Token *start) {
  Token *tok = skip(start->next, "(");

  bool has_it = equal(tok, "__builtin_offsetof") ||
    equal(tok, "__builtin_types_compatible_p") ||
    equal(tok, "__builtin_va_start") ||
    equal(tok, "__builtin_va_copy") ||
    equal(tok, "__builtin_va_end") ||
    equal(tok, "__builtin_va_arg");

  tok = skip(tok->next, ")");
  pop_macro_lock_until(start, tok);
  Token *tok2 = new_num_token(has_it, start);
  tok2->next = tok;
  return tok2;
}

// __DATE__ is expanded to the current date, e.g. "May 17 2020".
static char *format_date(struct tm *tm) {
  static char mon[][4] = {
    "Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
  };

  return format("\"%s %2d %d\"", mon[tm->tm_mon], tm->tm_mday, tm->tm_year + 1900);
}

// __TIME__ is expanded to the current time, e.g. "13:34:03".
static char *format_time(struct tm *tm) {
  return format("\"%02d:%02d:%02d\"", tm->tm_hour, tm->tm_min, tm->tm_sec);
}

void init_macros(void) {
  // Define predefined macros
  define_macro("_LP64", "1");
  define_macro("__BYTE_ORDER__", "1234");
  define_macro("__C99_MACRO_WITH_VA_ARGS", "1");
  define_macro("__ELF__", "1");
  define_macro("__LP64__", "1");
  define_macro("__ORDER_BIG_ENDIAN__", "4321");
  define_macro("__ORDER_LITTLE_ENDIAN__", "1234");
  define_macro("__SIZEOF_DOUBLE__", "8");
  define_macro("__SIZEOF_FLOAT__", "4");
  define_macro("__SIZEOF_INT__", "4");
  define_macro("__SIZEOF_LONG_DOUBLE__", "8");
  define_macro("__SIZEOF_LONG_LONG__", "8");
  define_macro("__SIZEOF_LONG__", "8");
  define_macro("__SIZEOF_POINTER__", "8");
  define_macro("__SIZEOF_PTRDIFF_T__", "8");
  define_macro("__SIZEOF_SHORT__", "2");
  define_macro("__SIZEOF_SIZE_T__", "8");
  define_macro("__SIZE_TYPE__", "unsigned long");
  define_macro("__STDC_HOSTED__", "1");
  define_macro("__STDC_NO_COMPLEX__", "1");
  define_macro("__STDC_UTF_16__", "1");
  define_macro("__STDC_UTF_32__", "1");
  define_macro("__STDC__", "1");
  define_macro("__USER_LABEL_PREFIX__", "");
  define_macro("__alignof__", "_Alignof");
  define_macro("__amd64", "1");
  define_macro("__amd64__", "1");
  define_macro("__const__", "const");
  define_macro("__gnu_linux__", "1");
  define_macro("__inline__", "inline");
  define_macro("__linux", "1");
  define_macro("__linux__", "1");
  define_macro("__signed__", "signed");
  define_macro("__slimcc__", "1");
  define_macro("__unix", "1");
  define_macro("__unix__", "1");
  define_macro("__volatile__", "volatile");
  define_macro("__x86_64", "1");
  define_macro("__x86_64__", "1");
  define_macro("linux", "1");
  define_macro("unix", "1");

  switch (opt_std) {
  case STD_C89: break;
  case STD_C99: define_macro("__STDC_VERSION__", "199901L"); break;
  case STD_C11: define_macro("__STDC_VERSION__", "201112L"); break;
  case STD_C17: define_macro("__STDC_VERSION__", "201710L"); break;
  case STD_C23: define_macro("__STDC_VERSION__", "202311L"); break;
  default: define_macro("__STDC_VERSION__", "201710L");
  }

  add_builtin("__FILE__", file_macro);
  add_builtin("__LINE__", line_macro);
  add_builtin("__COUNTER__", counter_macro);
  add_builtin("__TIMESTAMP__", timestamp_macro);
  add_builtin("__BASE_FILE__", base_file_macro);

  add_builtin("_Pragma", pragma_macro);

  add_builtin("__has_attribute", has_attribute_macro);
  add_builtin("__has_c_attribute", has_c_attribute_macro);
  add_builtin("__has_builtin", has_builtin_macro);
  add_builtin("__has_include", has_include_macro);

  time_t now = time(NULL);
  struct tm *tm = localtime(&now);
  define_macro("__DATE__", format_date(tm));
  define_macro("__TIME__", format_time(tm));
}

typedef enum {
  STR_NONE, STR_UTF8, STR_UTF16, STR_UTF32, STR_WIDE,
} StringKind;

static StringKind getStringKind(Token *tok) {
  if (!strcmp(tok->loc, "u8"))
    return STR_UTF8;

  switch (tok->loc[0]) {
  case '"': return STR_NONE;
  case 'u': return STR_UTF16;
  case 'U': return STR_UTF32;
  case 'L': return STR_WIDE;
  }
  internal_error();
}

// Concatenate adjacent string literals into a single string literal
// as per the C spec.
static void join_adjacent_string_literals(Token *tok) {
  Token *end = tok->next->next;
  while (end->kind == TK_STR)
    end = end->next;

  int fileno = tok->display_file_no;
  int lineno = tok->display_line_no;

  // If regular string literals are adjacent to wide string literals,
  // regular string literals are converted to the wide type.
  StringKind kind = getStringKind(tok);
  Type *basety = tok->ty->base;

  for (Token *t = tok->next; t != end; t = t->next) {
    StringKind k = getStringKind(t);
    if (kind == STR_NONE) {
      kind = k;
      basety = t->ty->base;
    } else if (k != STR_NONE && kind != k) {
      error_tok(t, "unsupported non-standard concatenation of string literals");
    }
  }

  if (basety->size > 1)
    for (Token *t = tok; t != end; t = t->next)
      if (t->ty->base->size == 1)
        *t = *tokenize_string_literal(t, basety);

  // Concatenate adjacent string literals.
  int len = tok->ty->array_len;
  for (Token *t = tok->next; t != end; t = t->next)
    len = len + t->ty->array_len - 1;

  char *buf = calloc(basety->size, len);

  int i = 0;
  for (Token *t = tok; t != end; t = t->next) {
    memcpy(buf + i, t->str, t->ty->size);
    i = i + t->ty->size - t->ty->base->size;
  }

  tok->display_file_no = fileno;
  tok->display_line_no = lineno;

  tok->ty = array_of(basety, len);
  tok->str = buf;
  tok->next = end;
}

static long is_supported_attr(Token **vendor, Token *tok) {
  if (tok->kind != TK_IDENT)
    error_tok(tok, "expected attribute name");

  bool vendor_gnu = vendor && *vendor && equal(*vendor, "gnu");

  if ((equal(tok, "packed") || equal(tok, "__packed__")) &&
    (!vendor || vendor_gnu)) {
    return 1;
  }
  return 0;
}

static void filter_attr(Token *tok, Token *lst, bool is_hidden, bool is_bracket) {
  bool first = true;
  for (; tok->kind != TK_EOF; first = false) {
    if (!first)
      tok = skip(tok, ",");

    if (is_bracket) {
      Token *vendor = NULL;
      if (tok->kind == TK_IDENT && equal(tok->next, ":")) {
        vendor = tok;
        tok = skip(tok->next->next, ":");
      }
      if (is_supported_attr(&vendor, tok)) {
        tok->kind = TK_BATTR;
        lst = lst->attr_next = tok;
      }
    } else {
      if (is_supported_attr(NULL, tok)) {
        tok->kind = TK_ATTR;
        lst = lst->attr_next = tok;
      }
    }
    if (consume(&tok, tok->next, "(")) {
      tok = skip_paren(tok);
      continue;
    }
    tok = tok->next;
    continue;
  }
}

static Token *preprocess3(Token *tok) {
  Token head = {0};
  Token *cur = &head;

  while (tok->kind != TK_EOF) {
    if (equal(tok, "__attribute__")) {
      bool is_hidden = tok->is_hidden_attr;

      tok = skip(tok->next, "(");
      tok = skip(tok, "(");
      Token *list = split_paren(&tok, tok);
      tok = skip(tok, ")");

      filter_attr(list, tok, is_hidden, false);
      continue;
    }

    if (equal(tok, "[") && consume(&tok, tok->next, "[")) {
      Token *list = split_bracket(&tok, tok);
      tok = skip(tok, "]");

      filter_attr(list, tok, false, true);
      continue;
    }

    if (tok->kind == TK_IDENT && is_keyword(tok))
      tok->kind = TK_KEYWORD;

    if (tok->kind == TK_STR && tok->next->kind == TK_STR)
      join_adjacent_string_literals(tok);

    cur = cur->next = tok;
    tok = tok->next;
    continue;
  }
  cur->next = tok;
  return head.next;
}

// Entry point function of the preprocessor.
Token *preprocess(Token *tok) {
  tok = preprocess2(tok);
  if (cond_incl)
    error_tok(cond_incl->tok, "unterminated conditional directive");

  if (opt_E)
    return tok;

  return preprocess3(tok);
}
