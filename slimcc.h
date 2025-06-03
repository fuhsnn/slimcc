#define _XOPEN_SOURCE 700
#include <assert.h>
#include <errno.h>
#include <glob.h>
#include <inttypes.h>
#include <libgen.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

#if defined(__SANITIZE_ADDRESS__)
#define USE_ASAN 1
#elif defined(__has_feature)
#if __has_feature(address_sanitizer)
#define USE_ASAN 1
#endif
#endif

#ifdef __clang__
#pragma clang diagnostic ignored "-Wswitch"
#endif

#define MAX(x, y) ((x) < (y) ? (y) : (x))
#define MIN(x, y) ((x) < (y) ? (x) : (y))

#define Ucast(c) (unsigned int)(unsigned char)(c)
#define Inrange(c, x, y) ((Ucast(c) - Ucast(x)) <= (Ucast(y) - Ucast(x)))
#define Isdigit(c) Inrange(c, '0', '9')
#define Isalnum(c) (Inrange((c) | 0x20, 'a', 'z') || Isdigit(c))
#define Isxdigit(c) (Isdigit(c) || Inrange((c) | 0x20, 'a', 'f'))
#define Casecmp(c, a) (((c) | 0x20) == a)

#if defined(__GNUC__) && (__GNUC__ >= 3)
#define FMTCHK(x,y) __attribute__((format(printf,(x),(y))))
#define NORETURN __attribute__((noreturn))
#elif defined(__has_attribute)
#if __has_attribute(format)
#define FMTCHK(x,y) __attribute__((format(printf,(x),(y))))
#endif
#if __has_attribute(noreturn)
#define NORETURN __attribute__((noreturn))
#endif
#endif

#ifndef FMTCHK
#define FMTCHK(x,y)
#endif
#ifndef NORETURN
#define NORETURN
#endif

#if defined(__GNUC__)
#define BUFF_CAST(__t, __ptr) (((union{char __m1; __t __m2;}*)(__ptr))->__m2)
#else
#define BUFF_CAST(__t, __ptr) (*((__t*)(__ptr)))
#endif

#if __STDC_VERSION__ >= 201112L
#define ANON_UNION_START union {
#define ANON_UNION_END };
#else
#define ANON_UNION_START
#define ANON_UNION_END
#endif

typedef struct Type Type;
typedef struct Node Node;
typedef struct Member Member;
typedef struct Relocation Relocation;

//
// alloc.c

typedef struct Page Page;
typedef struct {
  int used;
  bool on;
  Page *head_page;
  Page *page;
} Arena;

void arena_on(Arena *arena);
void arena_off(Arena *arena);
void *arena_calloc(Arena *a, size_t sz);
void *arena_malloc(Arena *a, size_t sz);
void *ast_arena_malloc(size_t sz);
void *ast_arena_calloc(size_t sz);

bool check_mem_usage(void);

extern Arena ast_arena;
extern Arena node_arena;
extern Arena pp_arena;
extern bool free_alloc;

//
// hashmap.c
//

typedef struct {
  char *key;
  int keylen;
  void *val;
} HashEntry;

typedef struct {
  HashEntry *buckets;
  int capacity;
  int used;
} HashMap;

void *hashmap_get(HashMap *map, char *key);
void *hashmap_get2(HashMap *map, char *key, int keylen);
void hashmap_put(HashMap *map, char *key, void *val);
void hashmap_put2(HashMap *map, char *key, int keylen, void *val);
void hashmap_delete(HashMap *map, char *key);
void hashmap_delete2(HashMap *map, char *key, int keylen);
void hashmap_test(void);

//
// strings.c
//

typedef struct {
  char **data;
  int capacity;
  int len;
} StringArray;

void strarray_push(StringArray *arr, char *s);
char *format(char *fmt, ...) FMTCHK(1,2);

//
// tokenize.c
//

// Token
typedef enum {
  TK_IDENT,   // Identifiers
  TK_PUNCT,   // Punctuators
  TK_KEYWORD, // Keywords
  TK_TYPEKW,  // Keywords
  TK_STR,     // String literals
  TK_INT_NUM, // Integer Numeric literals
  TK_PP_NUM,  // Preprocessing numbers
  TK_FMARK,   // Filemarkers for -E
  TK_PMARK,   // Placermarkers
  TK_ATTR,    // GNU attribute
  TK_BATTR,   // C23 attribute
  TK_EOF,     // End-of-file markers
} TokenKind;

typedef struct File File;
struct File {
  char *name;
  int file_no;
  int incl_no;
  char *contents;

  // For #line directive
  File *display_file;
  int line_delta;
  bool is_input;
  bool is_syshdr;
};

// Token type
typedef struct Token Token;
struct Token {
  Token *next;            // Next token
  TokenKind kind : 16;    // Token kind
  bool at_bol : 1;        // True if this token is at beginning of line
  bool has_space : 1;     // True if this token follows a space character
  bool dont_expand : 1;   // True if a macro token is encountered during the macro's expansion
  bool is_incl_guard : 1;
  bool is_root : 1;
  bool is_live : 1;
  bool is_generated : 1;
  int len;                // Token length
  char *loc;              // Token location
  File *file;             // Source location
  Token *origin;          // If this is expanded from a macro, the original token
  int line_no;            // Line number
  int display_line_no;
  int display_file_no;
  Type *ty;               // Used if TK_INT_NUM or TK_STR
ANON_UNION_START
    Token *attr_next;
    Token *alloc_next;
ANON_UNION_END
ANON_UNION_START
    int64_t ival;         // If kind is TK_INT_NUM, its value
    char *str;            // String literal contents including terminating '\0'
    Token *label_next;
ANON_UNION_END
};

void error(char *fmt, ...) FMTCHK(1,2) NORETURN;
void error_ice(char *file, int32_t line) NORETURN;
void error_at(char *loc, char *fmt, ...) FMTCHK(2,3) NORETURN;
void error_tok(Token *tok, char *fmt, ...) FMTCHK(2,3) NORETURN;
void warn_tok(Token *tok, char *fmt, ...) FMTCHK(2,3);
void verror_at_tok(Token *tok, char *fmt, va_list ap);
bool equal(Token *tok, char *op);
bool equal_ext(Token *tok, char *op);
Token *skip(Token *tok, char *op);
bool consume(Token **rest, Token *tok, char *str);
File **get_input_files(void);
char *read_file(char *path);
File *new_file(char *name, int file_no, char *contents);
Token *tokenize_string_literal(Token *tok, Type *basety);
Token *tokenize(File *file, Token **end);
Token *tokenize_file(char *filename, Token **end, int *incl_no);
File *add_input_file(char *path, char *content, int *incl_no);
void convert_pp_number(Token *tok, Node *node);
TokenKind ident_keyword(Token *tok);

#define internal_error() \
  error_ice(__FILE__, __LINE__)

//
// preprocess.c
//

char *search_include_paths(char *filename);
void init_macros(void);
void define_macro(char *name, char *buf);
void define_macro_cli(char *str);
void undef_macro(char *name);
Token *preprocess(Token *tok, char *input_file);
extern Token *last_alloc_tok;
extern Token *tok_freelist;

//
// parse.c
//

// Variable or function
typedef struct Obj Obj;
struct Obj {
  Obj *next;
  char *name;    // Variable name
  Type *ty;      // Type
  bool is_local; // local or global/function
  bool is_compound_lit;
  int align;     // alignment

  Token *asm_str;

  // Local variable
  int ofs;
  char *ptr;
  Obj *param_next;
  bool pass_by_stack;
  int stack_offset;
  Node *arg_expr;
  Obj *param_promoted;

  // Global variable or function
  bool is_definition;
  bool is_static;
  bool is_weak;
  Obj *static_lvars;
  char *alias_name;
  char *visibility;
  char *asm_name;

  // Global variable
  bool is_tentative;
  bool is_tls;
  char *section_name;
  char *init_data;
  Relocation *rel;

  // constexpr variable
  char *constexpr_data;

  // Function
  bool is_inline;
  bool only_inline;
  bool returns_twice;
  bool dont_reuse_stk;
  bool dealloc_vla;
  bool is_ctor;
  bool is_dtor;
  uint16_t ctor_prior;
  uint16_t dtor_prior;
  Node *body;
  void *output; // backend defined output object

  // Static inline function
  bool is_live;
  bool is_referenced;
  StringArray refs;
};

// Global variable can be initialized either by a constant expression
// or a pointer to another global variable. This struct represents the
// latter.
struct Relocation {
  Relocation *next;
  int offset;
  char **label;
  long addend;
};

typedef enum {
  DF_VLA_DEALLOC,
  DF_CLEANUP_FN,
  DF_DEFER_STMT,
} DeferKind;

typedef struct DeferStmt DeferStmt;
struct DeferStmt {
  DeferKind kind;
  DeferStmt *next;
  Obj *vla;
  Node *cleanup_fn;
  Node *stmt;
};

typedef enum {
  ASMOP_NULL = 0,
  ASMOP_NUM,
  ASMOP_SYMBOLIC,
  ASMOP_MEM,
  ASMOP_REG,
  ASMOP_FLAG,
} AsmOpKind;

typedef struct AsmParam AsmParam;
struct AsmParam {
  AsmParam *next;
  AsmOpKind kind;
  Token *name;
  Token *constraint;
  char *flag;
  AsmParam *match;
  Node *arg;
  Obj *ptr;
  Obj *var;
  int64_t val;
  int label_id;
  int reg;
  bool is_mem;
  bool is_gp;
  bool is_fp;
  bool is_x87;
  bool is_early_clobber;
  bool is_gp_highbyte;
  bool is_gp_legacy;
  bool is_gp_free;
  bool is_clobbered_x87;
};

// AST node
typedef enum {
  ND_NULL_STMT,
  ND_NULL_EXPR, // Do nothing
  ND_ADD,       // +
  ND_SUB,       // -
  ND_MUL,       // *
  ND_DIV,       // /
  ND_POS,       // unary +
  ND_NEG,       // unary -
  ND_MOD,       // %
  ND_BITAND,    // &
  ND_BITOR,     // |
  ND_BITXOR,    // ^
  ND_SHL,       // <<
  ND_SHR,       // >>
  ND_SAR,       // arithmetic >>
  ND_EQ,        // ==
  ND_NE,        // !=
  ND_LT,        // <
  ND_LE,        // <=
  ND_GT,        // >
  ND_GE,        // >=
  ND_ASSIGN,    // =
  ND_COND,      // ?:
  ND_COMMA,     // ,
  ND_MEMBER,    // . (struct member access)
  ND_ADDR,      // unary &
  ND_DEREF,     // unary *
  ND_NOT,       // !
  ND_BITNOT,    // ~
  ND_LOGAND,    // &&
  ND_LOGOR,     // ||
  ND_RETURN,    // "return"
  ND_IF,        // "if"
  ND_FOR,       // "for" or "while"
  ND_DO,        // "do"
  ND_SWITCH,    // "switch"
  ND_BLOCK,     // { ... }
  ND_GOTO,      // "goto"
  ND_GOTO_EXPR, // "goto" labels-as-values
  ND_LABEL,     // Labeled statement
  ND_LABEL_VAL, // [GNU] Labels-as-values
  ND_FUNCALL,   // Function call
  ND_EXPR_STMT, // Expression statement
  ND_STMT_EXPR, // Statement expression
  ND_VAR,       // Variable
  ND_NUM,       // Integer
  ND_CAST,      // Type cast
  ND_INIT_SEQ,
  ND_ASM,       // "asm"
  ND_CAS,       // Atomic compare-and-swap
  ND_EXCH,      // Atomic exchange
  ND_VA_START,  // "va_start"
  ND_VA_COPY,   // "va_copy"
  ND_VA_ARG,    // "va_arg"
  ND_CHAIN,     // ND_COMMA without array-to-pointer conversion
  ND_ALLOCA,
  ND_ALLOCA_ZINIT,
  ND_ARITH_ASSIGN,
  ND_POST_INCDEC,
  ND_CKD_ARITH,
  ND_RTN_ADDR,
} NodeKind;

typedef struct CaseRange CaseRange;
struct CaseRange {
  CaseRange *next;
  char *label;
  int64_t lo;
  int64_t hi;
};

// AST node type
struct Node {
  NodeKind kind; // Node kind
  Node *next;    // Next node
  Type *ty;      // Type, e.g. int or pointer to int
  Token *tok;    // Representative token

  Node *lhs;     // Left-hand side
  Node *rhs;     // Right-hand side

  // "if" or "for" statement
  Node *cond;
  Node *then;
  Node *els;
  Node *init;
  Node *inc;

  // "break" and "continue" labels
  char *brk_label;
  char *cont_label;

  // Block or statement expression
  Node *body;

  // Struct member access
  Member *member;

  // Function call
  Obj *ret_buffer;
  Obj *args;

  // Goto or labeled statement, or labels-as-values
  Token *labels;
  char *unique_label;
  Node *goto_next;

  // Switch
  CaseRange *cases;
  CaseRange *default_case;

  DeferStmt *defr_start;
  DeferStmt *defr_end;

  // "asm" string literal
  Token *asm_str;
  AsmParam *asm_outputs;
  AsmParam *asm_inputs;
  Token *asm_clobbers;
  AsmParam *asm_labels;
  int output_tmp_gp;
  int alt_frame_ptr;
  int alt_frame_ptr2;
  int clobber_mask;

  // Atomic compare-and-swap
  Node *cas_addr;
  Node *cas_old;
  Node *cas_new;

  // Variable
  Obj *var;

  // Numeric literal
  int64_t val;
  long double fval;
  uint64_t *bitint_data;

  // Arithmetic Assignment
  NodeKind arith_kind;
};

// Represents a block scope.
typedef struct Scope Scope;
struct Scope {
  Scope *parent;
  Scope *children;
  Scope *sibling_next;

  Obj *locals;
  bool is_temporary;
  // C has two block scopes; one is for variables/typedefs and
  // the other is for struct/union/enum tags.
  HashMap vars;
  HashMap tags;
};

Node *new_cast(Node *expr, Type *ty);
int64_t const_expr(Token **rest, Token *tok);
Obj *parse(Token *tok);
Token *skip_paren(Token *tok);
Obj *new_lvar(char *name, Type *ty);
bool is_const_var(Obj *var);
bool equal_tok(Token *a, Token *b);

//
// bitint.c
//

int32_t eval_bitint_first_set(int32_t bits, void *lp);
bool eval_bitint_to_bool(int32_t bits, void *lp);
void eval_bitint_sign_ext(int32_t bits, void *lp, int32_t bits2, bool is_unsigned);
void eval_bitint_neg(int32_t bits, void *lp);
void eval_bitint_bitnot(int32_t bits, void *lp);
void eval_bitint_bitand(int32_t bits, void *lp, void *rp);
void eval_bitint_bitor(int32_t bits, void *lp, void *rp);
void eval_bitint_bitxor(int32_t bits, void *lp, void *rp);
void eval_bitint_shl(int32_t bits, void *sp, void *dp, int32_t amount);
void eval_bitint_shr(int32_t bits, void *sp, void *dp, int32_t amount, bool is_unsigned);
void *eval_bitint_bitfield_load(int32_t bits, void *sp, void *dp, int32_t width, int32_t ofs, bool is_unsigned);
void eval_bitint_bitfield_save(int32_t bits, void *sp, void *dp, int32_t width, int32_t ofs);
void eval_bitint_add(int32_t bits, void *lp, void *rp);
void eval_bitint_sub(int32_t bits, void *lp, void *rp);
void eval_bitint_mul(int32_t bits, void *lp, void *rp);
void eval_bitint_div(int32_t bits, void *lp, void *rp, bool is_unsigned, bool is_div);
int eval_bitint_cmp(int32_t bits, void *lp, void *rp, bool is_unsigned);

//
// type.c
//

typedef enum {
  TY_VOID,
  TY_BOOL,
  TY_PCHAR,
  TY_CHAR,
  TY_SHORT,
  TY_INT,
  TY_LONG,
  TY_LONGLONG,
  TY_FLOAT,
  TY_DOUBLE,
  TY_LDOUBLE,
  TY_ENUM,
  TY_PTR,
  TY_NULLPTR,
  TY_FUNC,
  TY_ARRAY,
  TY_VLA, // variable-length array
  TY_STRUCT,
  TY_UNION,
  TY_BITINT
} TypeKind;

struct Type {
  TypeKind kind;
  int64_t size;       // sizeof() value
  int align;          // alignment
  bool is_unsigned;   // unsigned or signed
  bool is_atomic;     // true if _Atomic
  bool is_const;
  bool is_volatile;
  bool is_restrict;
  bool is_unspec_enum;
  Type *origin;       // for type compatibility check
  Type *decl_next;    // forward declarations
  Token *tag;

  // Pointer-to or array-of type. We intentionally use the same member
  // to represent pointer/array duality in C.
  //
  // In many contexts in which a pointer is expected, we examine this
  // member instead of "kind" member to determine whether a type is a
  // pointer or not. That means in many contexts "array of T" is
  // naturally handled as if it were "pointer to T", as required by
  // the C spec.
  Type *base;

  // _BitInt
  int64_t bit_cnt;

  // Array
  int64_t array_len;

  // Variable-length array
  Node *vla_len; // # of elements
  Obj *vla_cnt;  // _Countof() value

  // Struct
  Member *members;
  bool is_flexible;
  bool is_packed;

  // Function type
  Scope *scopes;
  Type *return_ty;
  Obj *param_list;
  Node *pre_calc;
  bool is_variadic;
  bool is_oldstyle;
};

// Struct member
struct Member {
  Member *next;
  Type *ty;
  Token *name;
  int idx;
  int offset;
  int alt_align;

  // Bitfield
  bool is_bitfield;
  int bit_offset;
  int bit_width;
};

extern Type *ty_void;
extern Type *ty_bool;
extern Type *ty_nullptr;

extern Type *ty_pchar;

extern Type *ty_char;
extern Type *ty_short;
extern Type *ty_int;
extern Type *ty_long;
extern Type *ty_llong;

extern Type *ty_uchar;
extern Type *ty_ushort;
extern Type *ty_uint;
extern Type *ty_ulong;
extern Type *ty_ullong;

extern Type *ty_float;
extern Type *ty_double;
extern Type *ty_ldouble;

extern Type *ty_size_t;
extern Type *ty_intptr_t;
extern Type *ty_ptrdiff_t;

bool is_integer(Type *ty);
bool is_flonum(Type *ty);
bool is_numeric(Type *ty);
bool is_array(Type *ty);
bool is_bitfield(Node *node);
bool is_redundant_cast(Node *expr, Type *ty);
bool is_compatible(Type *t1, Type *t2);
bool is_compatible2(Type *t1, Type *t2);
bool is_const_expr(Node *node, int64_t *val);
bool is_const_double(Node *node, long double *fval);
bool is_nullptr(Node *node);
void init_ty_lp64(void);
Type *copy_type(Type *ty);
Type *pointer_to(Type *base);
Type *ptr_decay(Type *ty);
void ptr_convert(Node **node);
Type *func_type(Type *return_ty, Token *tok);
Type *array_of(Type *base, int64_t size);
Type *vla_of(Type *base, Node *expr);
Type *new_type(TypeKind kind, int64_t size, int align);
Type *new_bitint(int64_t width, Token *tok);
void add_type(Node *node);
Type *unqual(Type *ty);
Type *new_qualified_type(Type *ty);
Obj *eval_var_opt(Node *node, int *ofs, bool let_subarray, bool let_atomic);
bool mem_iter(Member **mem);

//
// codegen.c
//

int codegen(Obj *prog, FILE *out);
void prepare_funcall(Node *node, Scope *scope);
void prepare_inline_asm(Node *node);
int align_to(int n, int align);
bool va_arg_need_copy(Type *ty);
bool bitint_rtn_need_copy(size_t width);
void emit_text(Obj *fn);


//
// unicode.c
//

int encode_utf8(char *buf, uint32_t c);
uint32_t decode_utf8(char **new_pos, char *p);
bool is_ident1(uint32_t c);
bool is_ident2(uint32_t c);
int display_width(char *p, int len);

//
// platform.c
//

void platform_init(void);
void platform_stdinc_paths(StringArray *paths);
void run_assembler(StringArray *as_args, char *input, char *output);
void run_linker(StringArray *paths, StringArray *inputs, char *output);

//
// main.c
//
typedef enum {
  STD_NONE = 0,
  STD_C89,
  STD_C99,
  STD_C11,
  STD_C17,
  STD_C23
} StdVer;

bool file_exists(char *path);
bool in_sysincl_path(char *path);

char *find_dir_w_file(char *pattern);
void run_subprocess(char **argv);
void set_fpic(char *lvl);
void set_fpie(char *lvl);
void add_include_path(StringArray *arr, char *s);
void run_assembler_gnustyle(StringArray *as_args, char *input, char *output);
void run_linker_gnustyle(StringArray *paths, StringArray *inputs, char *output,
  char *ldso_path, char *libpath, char *gcclibpath);

extern char *argv0;
extern StringArray include_paths;
extern StringArray iquote_paths;
extern bool opt_E;
extern bool opt_enable_universal_char;
extern bool opt_fpic;
extern bool opt_fpie;
extern bool opt_femulated_tls;
extern bool opt_use_plt;
extern bool opt_fcommon;
extern bool opt_optimize;
extern bool opt_reuse_stack;
extern bool opt_g;
extern bool opt_func_sections;
extern bool opt_data_sections;
extern bool opt_werror;
extern char *opt_visibility;
extern bool opt_cc1_asm_pp;
extern StdVer opt_std;
extern bool opt_fdefer_ts;

extern bool opt_pie;
extern bool opt_nopie;
extern bool opt_pthread;
extern bool opt_r;
extern bool opt_rdynamic;
extern bool opt_static;
extern bool opt_static_pie;
extern bool opt_static_libgcc;
extern bool opt_shared;
extern bool opt_nostartfiles;
extern bool opt_nodefaultlibs;
extern bool opt_nolibc;
extern char *opt_use_ld;
extern char *opt_use_as;
