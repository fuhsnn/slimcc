SRCS=alloc.c codegen.c hashmap.c main.c parse.c preprocess.c strings.c tokenize.c type.c unicode.c

TEST_SRCS!=ls test/*.c

TEST_FLAGS=-Itest -fenable-universal-char -std=c23

.SUFFIXES: .exe .stage2.o .stage2.exe

# Stage 1

OBJS=$(SRCS:.c=.o)

slimcc: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS)

$(OBJS): slimcc.h

TESTS=$(TEST_SRCS:.c=.exe)

test/host/common.o: test/host/common.c
	$(CC) -o $@ -c $<

$(TESTS): slimcc test/host/common.o

.c.exe:
	./slimcc $(TEST_FLAGS) -o $@ $< test/host/common.o -pthread

test: $(TESTS)
	for i in $(TESTS); do echo $$i; ./$$i || exit 1; echo; done
	sh test/driver.sh $(PWD)/slimcc $(CC)
	./slimcc -hashmap-test

# Stage 2

OBJS_S2=$(SRCS:.c=.stage2.o)

$(OBJS_S2): slimcc

.c.stage2.o:
	./slimcc -o $@ -c $<

slimcc-stage2: $(OBJS_S2)
	./slimcc -o $@ $(OBJS_S2) $(LDFLAGS)

TESTS_S2=$(TEST_SRCS:.c=.stage2.exe)

$(TESTS_S2): slimcc-stage2 test/host/common.o

.c.stage2.exe:
	./slimcc-stage2 $(TEST_FLAGS) -o $@ $< test/host/common.o -pthread

test-stage2: $(TESTS_S2)
	for i in $(TESTS_S2); do echo $$i; ./$$i || exit 1; echo; done
	sh test/driver.sh $(PWD)/slimcc-stage2 $(CC)
	./slimcc-stage2 -hashmap-test

# Misc.

test-all: test test-stage2

warn: $(SRCS)
	$(CC) -fsyntax-only -Wall -Wpedantic -Wno-switch $(CFLAGS) $(SRCS)

asan: clean
	$(MAKE) CFLAGS="-O2 -fsanitize=address -Wno-switch" LDFLAGS=

lto: clean
	$(MAKE) CFLAGS="-O2 -flto=auto -Wno-switch"

lto-je: clean
	$(MAKE) CFLAGS="-O2 -flto=auto -Wno-switch" LDFLAGS="-ljemalloc"

lto-mi: clean
	$(MAKE) CFLAGS="-O2 -flto=auto -Wno-switch" LDFLAGS="-lmimalloc"

clean:
	rm -rf slimcc slimcc-stage2
	find * -type f '(' -name '*~' -o -name '*.o' ')' -exec rm {} ';'
	find test/* -type f '(' -name '*~' -o -name '*.exe' ')' -exec rm {} ';'

.PHONY: test clean test-stage2
