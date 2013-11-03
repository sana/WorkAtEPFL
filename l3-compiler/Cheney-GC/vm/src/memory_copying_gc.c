/* Laurentiu Dascalu */
#include <assert.h>
#include <string.h>
#include <stdbool.h>
#include <stdio.h>

#include "engine.h"
#include "memory.h"
#include "fail.h"

//#define DEBUG
#define SPECIAL_HEADER 0xFF

int c1 = 0, c2 = 0;

static value_t* memory_start = NULL;
static value_t* memory_end = NULL;
static value_t* free_boundary = NULL;
static value_t *from_heap = NULL, *to_heap = NULL;
static unsigned long region_size = 0;

/* Bitmaps */
static unsigned int bitmap_size = 0;

static void invoke_garbage_collector();
static void set_addr_is_pointer(value_t *v, value_t* heap);
static int is_value_pointer(value_t v, value_t* heap);

static value_t* addr_v_to_p(value_t v_addr) {
	return memory_start + (v_addr / sizeof(value_t));
}

static value_t addr_p_to_v(value_t* p_addr) {
	return (p_addr - memory_start) * sizeof(value_t);
}

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

static char *get_base_ptr(value_t *v, unsigned int *value, value_t* heap) {
	assert(v >= heap + bitmap_size);
	assert(v < heap + region_size);

	unsigned long offset = (unsigned long) (v - heap - bitmap_size);
	unsigned int key = (unsigned int) offset / 8;
	*value = offset % 8;
	return (char *) heap + key;
}

static void set_addr_is_pointer(value_t *v, value_t* heap) {
	unsigned int value;
	char *base_ptr = get_base_ptr(v, &value, heap);
	*base_ptr |= (1 << value);
}

static int is_value_pointer(value_t v, value_t* heap) {
	value_t *p = addr_v_to_p(v);

	if (!(p >= heap + bitmap_size && p < heap + region_size))
		return 0;

	unsigned int value;
	char *base_ptr = get_base_ptr(p, &value, heap);
	return !!(*base_ptr & (1 << value));
}

static void adjust_region_size() {
	// (end - heap_start) / 2, because we have two regions: "from" and "to"
	region_size = ((unsigned long) memory_end - (unsigned long) free_boundary)
			>> 1;
	region_size /= sizeof(value_t);

	/* "from" and "to" heaps */
	from_heap = free_boundary;
	to_heap = free_boundary + region_size;

	/* bitmaps size */
	bitmap_size = region_size / (8 * sizeof(value_t)) + 1;
	memset(from_heap, 0, bitmap_size * sizeof(value_t));
}

#ifdef DEBUG
FILE *m_debug;
#endif

char* memory_get_identity() {
	return "copying garbage collector";
}

void memory_setup(size_t total_byte_size) {
#ifdef DEBUG
	m_debug = fopen("gc.log", "w");
	fprintf(m_debug, "[%s]\n", __FUNCTION__);
#endif

	memory_start = calloc(total_byte_size, 1);
	if (memory_start == NULL)
		fail("cannot allocate %zd bytes of memory", total_byte_size);
	memory_end = memory_start + (total_byte_size / sizeof(value_t));
}

void memory_cleanup() {
#ifdef DEBUG
	fprintf(m_debug, "[%s]\n", __FUNCTION__);
#endif

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

void memory_set_heap_start(void* start) {
#ifdef DEBUG
	fprintf(m_debug, "[%s]\n", __FUNCTION__);
#endif

	assert(free_boundary == NULL);
	free_boundary = start;
	adjust_region_size();
	free_boundary = from_heap + bitmap_size;
}

static void* memory_allocate_aux(tag_t tag, unsigned int size, int invoke_gc) {
	assert(free_boundary != NULL);

	const unsigned int total_size = size + 1;
	if (free_boundary + total_size >= from_heap + region_size) {
		if (invoke_gc) {
			invoke_garbage_collector();
			return NULL;
		} else
			fail("no memory left (block of size %u requested)", size);
	}

	c1++;
	*free_boundary = header_pack(tag, size);
	value_t* res = free_boundary + 1;
	set_addr_is_pointer(res, from_heap);
	free_boundary += total_size;

#ifdef DEBUG
	fprintf(m_debug, "tag = %d, size = %d, pointer = %p\n", tag, size, res);
#endif

	return res;
}

void* memory_allocate(tag_t tag, unsigned int size) {
	void *res = memory_allocate_aux(tag, size, 1);

	// Try one more time, with the GC deactivated
	if (res == NULL)
		res = memory_allocate_aux(tag, size, 0);

	return res;
}

size_t memory_get_block_size(value_t* block) {
	return header_unpack_size(block[-1]);
}

tag_t memory_get_block_tag(value_t* block) {
	return header_unpack_tag(block[-1]);
}

static value_t* copy_cheney(reg_bank_t bank, value_t* free_boundary) {
	int init_bank = 0, init_copy = 0;
	value_t *root;
	value_t *scan, *free, *aux;

	root = engine_get_base_register(bank);

	if (from_heap < to_heap) {
		if (!(root >= from_heap + bitmap_size && root < to_heap))
			return free_boundary;
	} else if (from_heap > to_heap) {
		if (!(root >= from_heap + bitmap_size && root < memory_end))
			return free_boundary;
	}

	scan = free = free_boundary;

	do {
		unsigned int size = header_unpack_size(root[-1]), i, j, _size;
		tag_t _tag;

		if (size == 0) {
			scan += 1;
			root = scan + 1;
			if (scan >= free)
				break;
			continue;
		}

		assert(size > 0);

		if (!init_copy) {
			/* Copy the header and adjust the bitmap */
			*free = root[-1];
			free++;
		}

#ifdef DEBUG
		fprintf(m_debug, "Encountered block of size %u\n", size);
#endif

		if (!init_bank) {
			if (to_heap > from_heap)
				assert(free > to_heap && free < memory_end);
			else
				assert(free > to_heap && free < from_heap);

			init_bank = 1;
			engine_set_base_register(bank, free);
		}

		if (!init_copy) {
			set_addr_is_pointer(free, to_heap);
			assert(is_value_pointer(addr_p_to_v(free), to_heap));

			/* Override the header, because we already saved it in "to" */
			root[-1] = header_pack(SPECIAL_HEADER, addr_p_to_v(free));

			/* Initially, copy the whole block */
			for (i = 0; i < size; i++)
				free[i] = root[i];
			free += size;
			init_copy = 1;
		}

		/* Check if copied data contains pointers */
		for (i = 0; i < size; i++) {
			if (is_value_pointer(root[i], from_heap)) {
				aux = addr_v_to_p(root[i]);

				if (from_heap < to_heap) {
					assert(aux >= from_heap + bitmap_size);
					assert(aux < to_heap);
				} else {
					assert(aux >= to_heap + bitmap_size);
					assert(aux < memory_end);
				}

				_tag = header_unpack_tag(aux[-1]);
				_size = header_unpack_size(aux[-1]);

				if (_tag == SPECIAL_HEADER) {
					/* I already copied this pointer and I have to relink it to the new address */
					scan[i + 1] = _size;
#ifdef DEBUG
					fprintf(m_debug, "Block with address %p already copied to %p\n", aux, addr_v_to_p(_size));
#endif
				} else {
					// I didn't copy this pointer and I mark it as copied (tag 0)
					*free = aux[-1];
					free++;

					// Update the memory reference
					scan[i + 1] = addr_p_to_v(free);
					set_addr_is_pointer(free, to_heap);
					assert(is_value_pointer(scan[i + 1], to_heap));

					// Adjust the old header with the new offset
					aux[-1] = header_pack(SPECIAL_HEADER, (unsigned int) scan[i + 1]);

					// Copy the referenced block
					for (j = 0; j < _size; j++)
					    free[j] = aux[j];
					free += _size;
					c2++;

#ifdef DEBUG
					printf("Copied block %p with tag %d and size %d\n", aux, _tag, _size);
#endif
				}
			}
		}

		scan += size + 1;
		root = scan + 1;
	} while (scan < free);

	return scan;
}

static void invoke_garbage_collector() {
	memset(to_heap, 0, region_size * sizeof(value_t));

	value_t *boundary = to_heap + bitmap_size;
	boundary = copy_cheney(Ib, boundary);
	boundary = copy_cheney(Lb, boundary);
	boundary = copy_cheney(Ob, boundary);

	// Swap the heaps
	value_t *aux = from_heap;
	from_heap = to_heap;
	to_heap = aux;
	memset(to_heap, 0, region_size * sizeof(value_t));
	free_boundary = boundary;

	c1 = c2 = 0;
}
