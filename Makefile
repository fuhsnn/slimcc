CFLAGS=-std=c99 -g -fno-common -Wall -pedantic -Wno-return-type -Wno-switch

SRCS=$(wildcard *.c)
OBJS=$(SRCS:.c=.o)

TEST_SRCS=$(wildcard test/*.c)
TESTS=$(TEST_SRCS:.c=.exe)

# Stage 1

slimcc: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

$(OBJS): slimcc.h

test/%.exe: slimcc test/%.c
	./slimcc -Iinclude -Itest -c -o test/$*.o test/$*.c
	$(CC) -std=c99 -pthread -o $@ test/$*.o -xc test/common

test: $(TESTS)
	for i in $^; do echo $$i; ./$$i || exit 1; echo; done
	sh test/driver.sh ./slimcc

test-all: test test-stage2

# Stage 2

stage2/slimcc: $(OBJS:%=stage2/%)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

stage2/%.o: slimcc %.c
	mkdir -p stage2/test
	./slimcc -c -o $(@D)/$*.o $*.c

stage2/test/%.exe: stage2/slimcc test/%.c
	mkdir -p stage2/test
	./stage2/slimcc -Iinclude -Itest -c -o stage2/test/$*.o test/$*.c
	$(CC) -std=c99 -pthread -o $@ stage2/test/$*.o -xc test/common

test-stage2: $(TESTS:test/%=stage2/test/%)
	for i in $^; do echo $$i; ./$$i || exit 1; echo; done
	sh test/driver.sh ./stage2/slimcc

# Misc.

clean:
	rm -rf slimcc tmp* $(TESTS) test/*.s test/*.exe stage2
	find * -type f '(' -name '*~' -o -name '*.o' ')' -exec rm {} ';'

.PHONY: test clean test-stage2
