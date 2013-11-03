#include <assert.h>
#include <stdio.h>

#include "engine.h"

void loader_load_file(char* file_name) {
  FILE* file = fopen(file_name, "r");
  assert(file != NULL);

  for (;;) {
    char line[1000];
    if (fgets(line, sizeof(line), file) == NULL)
      break;

    unsigned int instr;
    int read_count = sscanf(line, "%8x", &instr);
    assert(read_count == 1);

    engine_emit(instr);
  }

  fclose(file);
}
