#ifndef __VMTYPES_H
#define __VMTYPES_H

#include <limits.h>
#include <stdint.h>

typedef int32_t value_t;        /* value (integer or [virtual] pointer) */
typedef uint32_t instr_t;       /* instruction */
typedef uint_fast8_t reg_id_t;  /* register identity */

#define VALUE_BITS (sizeof(value_t) * CHAR_BIT)

#endif
