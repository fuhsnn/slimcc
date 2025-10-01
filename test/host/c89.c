
float mixed1(float f, _Bool, char i);

float mixed1(f, b, c)
char c; float f; _Bool b; {
  return f - b + c;
}

float conv1(
#ifndef __clang__
foo, bar
#endif
);

float conv1(f, b, c, i)
char c; float f; _Bool b; {
  return f - b + c + i;
}
