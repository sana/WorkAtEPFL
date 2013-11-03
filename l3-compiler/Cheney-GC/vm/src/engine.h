#ifndef __ENGINE__H
#define __ENGINE__H

#include "vmtypes.h"

typedef enum {
  Lb, Lb1, Lb2, Lb3, Lb4, Lb5, Lb6, Lb7, Lb8, Lb9, Lb10, Lb11, Lb12, Lb13,
  Ib, Ob
} reg_bank_t;

char* engine_get_identity(void);
void engine_setup(void);
void engine_cleanup(void);
void engine_emit(instr_t instr);
void* engine_get_next_address();
value_t* engine_get_base_register(reg_bank_t bank);
void engine_set_base_register(reg_bank_t bank, value_t* new_value);
void engine_run();

#endif // __ENGINE__H
