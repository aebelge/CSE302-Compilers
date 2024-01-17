#include <sys/types.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

void print_int(int64_t value) {
  (void) printf("%ld\n", value);
}

void print_bool(int64_t value) {
  (void) printf("%s\n", value ? "true" : "false");
}

void* alloc(size_t size) {
  return malloc(size);
}
