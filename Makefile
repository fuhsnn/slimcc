SRCS=alloc.c codegen.c hashmap.c main.c parse.c preprocess.c strings.c tokenize.c type.c unicode.c

TEST_SRCS!=ls test/*.c

TEST_FLAGS=-Itest -fenable-universal-char -fcommon -std=c23

.SUFFIXES: .exe .stage2.o .stage2.exe .asan.o .asan.exe

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
	for i in $(TESTS); do echo $$i; ./$$i >/dev/null || exit 1; echo; done
	$(SHELL) test/driver.sh $(PWD)/slimcc $(CC)
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
	for i in $(TESTS_S2); do echo $$i; ./$$i >/dev/null || exit 1; echo; done
	$(SHELL) test/driver.sh $(PWD)/slimcc-stage2 $(CC)
	./slimcc-stage2 -hashmap-test

# Asan build

OBJS_ASAN=$(SRCS:.c=.asan.o)

$(OBJS_ASAN): slimcc.h

.c.asan.o:
	$(CC) $(CFLAGS) -fsanitize=address -g -o $@ -c $<

slimcc-asan: $(OBJS_ASAN)
	$(CC) $(CFLAGS) -fsanitize=address -g -o $@ $(OBJS_ASAN) $(LDFLAGS)

TESTS_ASAN=$(TEST_SRCS:.c=.asan.exe)

$(TESTS_ASAN): slimcc-asan test/host/common.o

.c.asan.exe:
	./slimcc-asan $(TEST_FLAGS) -o $@ $< test/host/common.o -pthread

test-asan: $(TESTS_ASAN)
	for i in $(TESTS_ASAN); do echo $$i; ./$$i >/dev/null || exit 1; echo; done
	$(SHELL) test/driver.sh $(PWD)/slimcc-asan $(CC)
	$(MAKE) slimcc CC=./slimcc-asan -B
	./slimcc-asan -hashmap-test

# Misc.

test-all: test test-stage2

warn: $(SRCS)
	$(CC) -fsyntax-only -Wall -Wpedantic -Wno-switch $(CFLAGS) $(SRCS)

lto: clean
	$(MAKE) CFLAGS="-O2 -flto=auto -Wno-switch"

lto-je: clean
	$(MAKE) CFLAGS="-O2 -flto=auto -Wno-switch" LDFLAGS="-ljemalloc"

lto-mi: clean
	$(MAKE) CFLAGS="-O2 -flto=auto -Wno-switch" LDFLAGS="-lmimalloc"

clean:
	rm -f slimcc slimcc-stage2 slimcc-asan
	rm -f *.o test/*.o test/*.exe test/host/*.o

.PHONY: test clean test-stage2 test-asan
