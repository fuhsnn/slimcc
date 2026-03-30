
ifneq (, $(shell printf '#if __GNUC__>=12||__clang_major__>=14\n1\n#endif' | $(CC) -xc - -E -P 2>/dev/null ))
  CFLAGS := -O3 -std=gnu23 -fvisibility=hidden -flto=auto $(CFLAGS)
else
  CFLAGS := -O2 -std=gnu23 $(CFLAGS)
endif

LDFLAGS ?= $(shell pkg-config mimalloc --libs 2>/dev/null || pkg-config jemalloc --libs 2>/dev/null )

slimcc:
%: force
	@$(MAKE) -f Makefile CFLAGS="$(CFLAGS)" LDFLAGS="$(LDFLAGS)" $@
force: ;
