#include <string.h>
#include <stdio.h>
#include <assert.h>

#include "memory.h"
#include "engine.h"
#include "loader.h"
#include "fail.h"

typedef struct {
  size_t memory_size;
  char* file_name;
} options_t;

static options_t default_options = { 1000000, NULL };

static void parse_args(int argc, char* argv[], options_t* opts);
static void display_usage(char* prog_name);

int main (int argc, char* argv[]) {
  assert(sizeof(value_t) == sizeof(instr_t));

  options_t options = default_options;
  parse_args(argc, argv, &options);
  if (options.file_name == NULL) {
    display_usage(argv[0]);
    fail("missing input file name");
  }
  if (options.memory_size <= 0)
    fail("invalid memory size %zd", options.memory_size);

  memory_setup(options.memory_size);
  engine_setup();

  loader_load_file(options.file_name);
  memory_set_heap_start(engine_get_next_address());
  engine_run();

  engine_cleanup();
  memory_cleanup();
  return 0;
}

static void parse_args(int argc, char* argv[], options_t* opts) {
  int i = 1;
  while (i < argc) {
    char* arg = argv[i++];
    int arg_len = strlen(arg);

    if (arg_len == 2 && arg[0] == '-') {
      switch (arg[1]) {
      case 'h': {
        display_usage(argv[0]);
        exit(0);
      }

      case 'm': {
        if (i >= argc) {
          display_usage(argv[0]);
          fail("missing argument to -m");
        }
        opts->memory_size = atoi(argv[i++]);
      } break;

      case 'v': {
        printf("vm v1.0\n");
        printf("  memory module: %s\n", memory_get_identity());
        printf("  engine module: %s\n", engine_get_identity());
        exit(0);
      } break;

      default:
        display_usage(argv[0]);
        fail("invalid option %s", arg);
      }
    } else
      opts->file_name = arg;
  }
}

static void display_usage(char* prog_name) {
  printf("Usage: %s [<options>] <asm_file>\n", prog_name);
  printf("\noptions:\n");
  printf("  -h         display this help message and exit\n");
  printf("  -m <size>  set memory size in bytes (default %zd)\n",
         default_options.memory_size);
  printf("  -v         display version and exit\n");
}
