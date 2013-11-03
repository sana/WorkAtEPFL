#include <stdlib.h>
#include <stdint.h>
#include <assert.h>

#include "memory.h"
#include "fail.h"

static value_t* memory_start = NULL;
static value_t* memory_end = NULL;
static value_t* free_boundary = NULL;

static value_t header_pack(tag_t tag, unsigned int size) {
  assert(size <= 0x00FFFFFF);
  assert(tag <= 0xFF);
  return (size << 8) | tag;
}

static tag_t header_unpack_tag(value_t header) {
  return header & 0xFF;
}

static unsigned int header_unpack_size(value_t header) {
  return header >> 8;
}

char* memory_get_identity() {
  return "no free";
}

void memory_setup(size_t total_byte_size) {
  memory_start = calloc(total_byte_size, 1);
  if (memory_start == NULL)
    fail("cannot allocate %zd bytes of memory", total_byte_size);
  memory_end = memory_start + (total_byte_size / sizeof(value_t));
}

void memory_cleanup() {
  assert(memory_start != NULL);
  free(memory_start);
  memory_start = memory_end = free_boundary = NULL;
}

void* memory_get_start() {
  return memory_start;
}

void* memory_get_end() {
  return memory_end;
}

void memory_set_heap_start(void* heap_start) {
  assert(free_boundary == NULL);
  free_boundary = heap_start;
}

void* memory_allocate(tag_t tag, unsigned int size) {
  assert(free_boundary != NULL);

  const unsigned int total_size = size + 1;
  if (free_boundary + total_size > memory_end)
    fail("no memory left (block of size %u requested)", size);

  *free_boundary = header_pack(tag, size);
  value_t* res = free_boundary + 1;
  free_boundary += total_size;

  return res;
}

size_t memory_get_block_size(value_t* block) {
  return header_unpack_size(block[-1]);
}

tag_t memory_get_block_tag(value_t* block) {
  return header_unpack_tag(block[-1]);
}
