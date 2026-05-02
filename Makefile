SRCS=alloc.c bitint.c codegen.c hashmap.c main.c parse.c platform.c preprocess.c strings.c tokenize.c type.c unicode.c

TEST_SRCS!=ls test/*.c

TEST_FLAGS=-Itest -std=gnu23

STG2_FLAGS=

SAN_FLAGS=-fsanitize=address,undefined -fno-sanitize=alignment -fno-sanitize-recover=undefined

.SUFFIXES: .exe .stage2.o .stage2.exe .san.o .san.exe .filc.o .filc.exe

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
	$(SHELL) scripts/test_driver.sh $(PWD)/slimcc $(CC)
	./slimcc -hashmap-test

# Stage 2

OBJS_S2=$(SRCS:.c=.stage2.o)

$(OBJS_S2): slimcc

.c.stage2.o:
	./slimcc $(STG2_FLAGS) -o $@ -c $<

slimcc-stage2: $(OBJS_S2)
	./slimcc $(STG2_FLAGS) -o $@ $(OBJS_S2) $(LDFLAGS)

TESTS_S2=$(TEST_SRCS:.c=.stage2.exe)

$(TESTS_S2): slimcc-stage2 test/host/common.o

.c.stage2.exe:
	./slimcc-stage2 $(TEST_FLAGS) -o $@ $< test/host/common.o -pthread

test-stage2: $(TESTS_S2)
	for i in $(TESTS_S2); do echo $$i; ./$$i >/dev/null || exit 1; echo; done
	$(SHELL) scripts/test_driver.sh $(PWD)/slimcc-stage2 $(CC)
	./slimcc-stage2 -hashmap-test

# -fsanitize build

OBJS_SAN=$(SRCS:.c=.san.o)

$(OBJS_SAN): slimcc.h

.c.san.o:
	$(CC) $(SAN_FLAGS) -g -fno-omit-frame-pointer -o $@ -c $<

slimcc-san: $(OBJS_SAN)
	$(CC) $(SAN_FLAGS) -g -fno-omit-frame-pointer -o $@ $(OBJS_SAN)

TESTS_SAN=$(TEST_SRCS:.c=.san.exe)

$(TESTS_SAN): slimcc-san test/host/common.o

.c.san.exe:
	./slimcc-san $(TEST_FLAGS) -o $@ $< test/host/common.o -pthread

test-san: $(TESTS_SAN)
	for i in $(TESTS_SAN); do echo $$i; ./$$i >/dev/null || exit 1; echo; done
	$(SHELL) scripts/test_driver.sh $(PWD)/slimcc-san $(CC)
	./slimcc-san scripts/amalgamation.c -c -o/dev/null
	./slimcc-san -hashmap-test

test-misc: slimcc-san test/host/common.o
	$(SHELL) scripts/test_abi.sh $(PWD)/slimcc-san $(CC)
	$(SHELL) scripts/test_abi.sh $(CC) $(PWD)/slimcc-san
	$(SHELL) scripts/test_include_next.sh $(PWD)/slimcc-san
	FILE=file $(SHELL) scripts/test_linker.sh $(PWD)/slimcc-san

# Fil-C build

OBJS_FILC=$(SRCS:.c=.filc.o)

$(OBJS_FILC): slimcc.h

.c.filc.o:
	$(FILC) -g -o $@ -c $<

slimcc-filc: $(OBJS_FILC)
	$(FILC) -g -o $@ $(OBJS_FILC)

TESTS_FILC=$(TEST_SRCS:.c=.filc.exe)

$(TESTS_FILC): slimcc-filc test/host/common.o

.c.filc.exe:
	./slimcc-filc $(TEST_FLAGS) -o $@ $< test/host/common.o -pthread

test-filc: $(TESTS_FILC)
	for i in $(TESTS_FILC); do echo $$i; ./$$i >/dev/null || exit 1; echo; done
	$(SHELL) scripts/test_driver.sh $(PWD)/slimcc-filc $(CC)
	./slimcc-filc scripts/amalgamation.c -c -o/dev/null
	./slimcc-filc -hashmap-test

# Misc.

test-all: test test-stage2

format:
	$(CLANG_FORMAT) -i $(SRCS) slimcc.h platform/*.c
	perl -i -p0e 's|\:[ ]+\{\n|: {\n|g' $(SRCS) slimcc.h platform/*.c

clean:
	rm -f slimcc slimcc-stage2 slimcc-san slimcc-filc
	rm -f *.o test/*.o test/*.exe test/host/*.o test/abi/*.o

.PHONY: clean test test-stage2 test-all test-san test-filc
