// This is an implementation of the open-addressing hash table.

#include "slimcc.h"

// Initial hash bucket size
#define INIT_SIZE 16

#define SHOULD_REHASH(_used, _cap) ((_used) * 11 / 8 >= _cap)

S_ASSERT(INIT_SIZE > 0 && SHOULD_REHASH(INIT_SIZE - 1, INIT_SIZE))

// Represents a deleted hash entry
#define TOMBSTONE ((void *)-1)
#define TOMBSTONE_CASE ((uintptr_t)-1)

static uint64_t fnv_hash(const char *s, int len) {
  uint64_t hash = 0xcbf29ce484222325;
  for (int i = 0; i < len; i++) {
    hash *= 0x100000001b3;
    hash ^= (unsigned char)s[i];
  }
  return hash;
}

// Make room for new entries in a given hashmap by removing
// tombstones and possibly extending the bucket size.
static void rehash(HashMap *map) {
  // Compute the size of the new hashmap.
  int32_t nkeys = 0;
  for (int32_t i = 0; i < map->capacity; i++)
    if (map->buckets[i].key && map->buckets[i].key != TOMBSTONE)
      nkeys++;

  int64_t cap = map->capacity;
  if ((int64_t)nkeys * 2 > cap) {
    cap *= 2;
    if (cap * sizeof(HashEntry) > 0x4000000)
      internal_error();
  }

  // Create a new hashmap and copy all key-values.
  HashMap map2 = {0};
  map2.buckets = calloc(cap, sizeof(HashEntry));
  map2.capacity = cap;

  for (int i = 0; i < map->capacity; i++) {
    HashEntry *ent = &map->buckets[i];
    if (ent->key && ent->key != TOMBSTONE)
      hashmap_put2(&map2, ent->key, ent->keylen, ent->val);
  }

  assert(map2.used == nkeys);
  free(map->buckets);
  *map = map2;
}

static HashEntry *get_entry(HashMap *map, const char *key, int keylen) {
  if (!map->buckets)
    return NULL;

  uint64_t hash = fnv_hash(key, keylen);
  uint64_t msk = map->capacity - 1;
  HashEntry *buckets = map->buckets;

  for (;;) {
    HashEntry *ent = &buckets[hash++ & msk];

    switch ((uintptr_t)ent->key) {
    case 0: {
      return NULL;
    }
    default:
      if (ent->keylen == keylen && !memcmp(ent->key, key, keylen))
        return ent;
    case TOMBSTONE_CASE: {
      break;
    }
    }
  }
}

HashEntry *hashmap_get_or_insert(HashMap *map, const char *key, int keylen) {
  if (!map->buckets) {
    map->buckets = calloc(INIT_SIZE, sizeof(HashEntry));
    map->capacity = INIT_SIZE;
  } else if (SHOULD_REHASH((int64_t)map->used, map->capacity)) {
    rehash(map);
  }

  uint64_t hash = fnv_hash(key, keylen);
  uint64_t msk = map->capacity - 1;
  HashEntry *buckets = map->buckets;

  for (;;) {
    HashEntry *ent = &buckets[hash++ & msk];

    switch ((uintptr_t)ent->key) {
    case 0:
      ent->key = key;
      ent->keylen = keylen;
      map->used++;
      return ent;
    default:
      if (ent->keylen == keylen && !memcmp(ent->key, key, keylen))
        return ent;
    case TOMBSTONE_CASE: {
      break;
    }
    }
  }
}

void *hashmap_get(HashMap *map, const char *key) {
  return hashmap_get2(map, key, strlen(key));
}

void *hashmap_get2(HashMap *map, const char *key, int keylen) {
  HashEntry *ent = get_entry(map, key, keylen);
  return ent ? ent->val : NULL;
}

void hashmap_put(HashMap *map, const char *key, void *val) {
  hashmap_put2(map, key, strlen(key), val);
}

void hashmap_put2(HashMap *map, const char *key, int keylen, void *val) {
  HashEntry *ent = hashmap_get_or_insert(map, key, keylen);
  ent->val = val;
}

void hashmap_delete(HashMap *map, const char *key) {
  hashmap_delete2(map, key, strlen(key));
}

void hashmap_delete2(HashMap *map, const char *key, int keylen) {
  HashEntry *ent = get_entry(map, key, keylen);
  if (ent)
    ent->key = TOMBSTONE;
}

void hashmap_test(void) {
  HashMap *map = calloc(1, sizeof(HashMap));
  Arena test_arena = {0};
  arena_on(&test_arena);

  for (int i = 0; i < 5000; i++)
    hashmap_put(map, arena_format(&test_arena, "key %d", i), (void *)(size_t)i);
  for (int i = 1000; i < 2000; i++)
    hashmap_delete(map, arena_format(&test_arena, "key %d", i));
  for (int i = 1500; i < 1600; i++)
    hashmap_put(map, arena_format(&test_arena, "key %d", i), (void *)(size_t)i);
  for (int i = 6000; i < 7000; i++)
    hashmap_put(map, arena_format(&test_arena, "key %d", i), (void *)(size_t)i);

  for (int i = 0; i < 1000; i++)
    assert((size_t)hashmap_get(map, arena_format(&test_arena, "key %d", i)) == i);
  for (int i = 1000; i < 1500; i++)
    assert(hashmap_get(map, "no such key") == NULL);
  for (int i = 1500; i < 1600; i++)
    assert((size_t)hashmap_get(map, arena_format(&test_arena, "key %d", i)) == i);
  for (int i = 1600; i < 2000; i++)
    assert(hashmap_get(map, "no such key") == NULL);
  for (int i = 2000; i < 5000; i++)
    assert((size_t)hashmap_get(map, arena_format(&test_arena, "key %d", i)) == i);
  for (int i = 5000; i < 6000; i++)
    assert(hashmap_get(map, "no such key") == NULL);
  for (int i = 6000; i < 7000; i++)
    hashmap_put(map, arena_format(&test_arena, "key %d", i), (void *)(size_t)i);

  assert(hashmap_get(map, "no such key") == NULL);

  arena_off(&test_arena);

  printf("OK\n");
}
