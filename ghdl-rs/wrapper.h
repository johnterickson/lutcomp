#include "vpi_thunk.h"

typedef void (*grt_initPtr) (void);
typedef int (*grt_main_optionsPtr) (const char *progname, int argc, const char * const argv[]);
typedef int (*grt_main_elabPtr) (void);
typedef void (*__ghdl_simulation_initPtr) (void);
typedef int (*__ghdl_simulation_stepPtr) (void);