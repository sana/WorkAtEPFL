#ifndef __OPCODE_H
#define __OPCODE_H

typedef enum {
  opcode_ADD, opcode_SUB, opcode_MUL, opcode_DIV, opcode_MOD,
  opcode_ASL, opcode_ASR, opcode_AND, opcode_OR,
  opcode_JLT, opcode_JLE, opcode_JEQ, opcode_JNE, opcode_JGE, opcode_JGT,
  opcode_JI, opcode_TCAL, opcode_CALL, opcode_RET, opcode_HALT,
  opcode_LDLO, opcode_LDHI, opcode_MOVE,
  opcode_RALO, opcode_BALO, opcode_BSIZ, opcode_BTAG, opcode_BGET, opcode_BSET,
  opcode_CREA, opcode_CPRI,
} opcode_t;

#define OPCODE_COUNT (opcode_CPRI+1)

#endif // __OPCODE_H
