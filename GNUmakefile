CFLAGS=-std=c11 -fsanitize=address -g -fno-common -Wall -pedantic -Wno-switch

TEST_FLAGS=-Iinclude -Itest -fenable-universal-char

SRCS=$(wildcard *.c)
OBJS=$(SRCS:.c=.o)

TEST_SRCS=$(wildcard test/*.c)
TESTS=$(TEST_SRCS:.c=.exe)

TEST_C23_SRCS=$(wildcard test/c23/*.c)
TESTS_C23=$(TEST_C23_SRCS:.c=.exe)

# Stage 1

slimcc: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

$(OBJS): slimcc.h

test/%.exe: slimcc test/%.c
	./slimcc $(TEST_FLAGS) -c -o test/$*.o test/$*.c
	$(CC) -std=c11 -pthread -Wno-psabi -no-pie -o $@ test/$*.o -xc test/common

test/c23/%.exe: slimcc test/c23/%.c
	./slimcc -std=c23 $(TEST_FLAGS) -c -o test/c23/$*.o test/c23/$*.c
	$(CC) -std=c11 -pthread -Wno-psabi -no-pie -o $@ test/c23/$*.o -xc test/common

test: $(TESTS) $(TESTS_C23)
	for i in $^; do echo $$i; ./$$i || exit 1; echo; done
	bash test/driver.sh ./slimcc $(CC)

test-all: test test-stage2

# Stage 2

stage2/slimcc: $(OBJS:%=stage2/%)
	./slimcc -o $@ $^ $(LDFLAGS)

stage2/%.o: slimcc %.c
	mkdir -p stage2/test
	./slimcc -c -o $(@D)/$*.o $*.c

stage2/test/%.exe: stage2/slimcc test/%.c
	mkdir -p stage2/test
	./stage2/slimcc $(TEST_FLAGS) -c -o stage2/test/$*.o test/$*.c
	$(CC) -std=c11 -pthread -Wno-psabi -no-pie -o $@ stage2/test/$*.o -xc test/common

stage2/test/c23/%.exe: stage2/slimcc test/c23/%.c
	mkdir -p stage2/test/c23
	./stage2/slimcc -std=c23 $(TEST_FLAGS) -c -o stage2/test/c23/$*.o test/c23/$*.c
	$(CC) -std=c11 -pthread -Wno-psabi -no-pie -o $@ stage2/test/c23/$*.o -xc test/common

test-stage2: $(TESTS:test/%=stage2/test/%) $(TESTS_C23:test/c23/%=stage2/test/c23/%)
	for i in $^; do echo $$i; ./$$i || exit 1; echo; done
	bash test/driver.sh ./stage2/slimcc $(CC)

# Misc.

clean:
	rm -rf slimcc stage2
	find * -type f '(' -name '*~' -o -name '*.o' ')' -exec rm {} ';'
	find test/* -type f '(' -name '*~' -o -name '*.exe' ')' -exec rm {} ';'

.PHONY: test clean test-stage2
