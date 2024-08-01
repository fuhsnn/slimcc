#ifndef __has_embed
#error
#endif

#if !defined(__STDC_EMBED_NOT_FOUND__) || !defined(__STDC_EMBED_FOUND__) || !defined(__STDC_EMBED_EMPTY__)
#error
#endif

#if __STDC_EMBED_NOT_FOUND__ != 0 || __STDC_EMBED_FOUND__ != 1 || __STDC_EMBED_EMPTY__ != 2
#error
#endif

#if __has_embed("embed_???") != __STDC_EMBED_NOT_FOUND__
#error
#endif

#if __has_embed("embed_empty") != __STDC_EMBED_EMPTY__
#error
#endif

#if __has_embed("embed.c") != __STDC_EMBED_FOUND__
#error
#endif

#if __has_embed("embed.c" unsupported::directive(abc)) != __STDC_EMBED_NOT_FOUND__
#error
#endif

#if __has_embed("embed.c" limit(0)) != __STDC_EMBED_EMPTY__
#error
#endif

#include "test.h"


const char sudoku_ref[] = {
  "1.3B.2.F.64EG5.."
  ".7A4.3E..C.B..68"
  ".9C2.1....75A..E"
  "E...9B....1DF74."
  "3C81...27.EG.6.."
  "...7.6.EA8......"
  "...F.93.B5..C.2."
  "....C4.8D.F19E5."
  "851..D9AFE.6BG7."
  ".....E5B.7.C.29."
  ".27...6....A.C.F"
  "6..DF.C.4.2...3."
  "..5.3G4...9...8."
  "9A.61.B.5GD.2FE3"
  "...8E..9..A.64G."
  "GD23.AF..4..51C."
};

const char sudoku_full[] = {
#embed "embed_sudoku"
};

const char sudoku_limit[] = {
#embed "embed_sudoku" limit(27)
};

const char sudoku_lim_prepost[] = {
#embed "embed_sudoku" suffix(, 'B', 'A', 'R', 0) limit(3) prefix('F', 'O', 'O',)
};

const char sudoku_empty[] = {
#embed "embed_empty" if_empty("empty")
};

int main(void) {
  ASSERT(0, memcmp(sudoku_ref, sudoku_full, 256));
  ASSERT(27, sizeof(sudoku_limit));
  ASSERT(10, sizeof(sudoku_lim_prepost));
  ASSERT(0, strcmp(sudoku_lim_prepost, "FOO1.3BAR"));
  ASSERT(0, strcmp(sudoku_empty, "empty"));

  printf("OK\n");
}

