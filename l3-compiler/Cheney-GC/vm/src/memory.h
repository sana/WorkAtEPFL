#ifndef __MEMORY_H
#define __MEMORY_H

#include <stdlib.h>
#include "vmtypes.h"

typedef enum {
  tag_String = 200,
  tag_RegisterFrame = 201,
  tag_Function = 202
} tag_t;

char* memory_get_identity();

void memory_setup(size_t total_size);
void memory_cleanup();

void* memory_get_start();
void* memory_get_end();

void memory_set_heap_start(void* heap_start);

void* memory_allocate(tag_t tag, unsigned int size);
size_t memory_get_block_size(value_t* block);
tag_t memory_get_block_tag(value_t* block);

#endif
