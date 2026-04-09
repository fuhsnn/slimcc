
float mixed(float f, _Bool, char i);

float mixed(f, b, c)
char c; float f; _Bool b; {
  return f - b + c;
}

float conv(
#ifndef __clang__
foo, bar
#endif
);

float conv(f, b, c, i)
char c; float f; _Bool b; {
  return f - b + c + i;
}
