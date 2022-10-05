#include "vpi_thunk.h"

typedef void (*grt_initPtr) (void);
typedef int (*grt_main_optionsPtr) (const char *progname, int argc, const char * const argv[]);
typedef int (*grt_main_elabPtr) (void);
typedef void (*__ghdl_simulation_initPtr) (void);
typedef int (*__ghdl_simulation_stepPtr) (void);
//  Return value:
//  0: delta cycle
//  1: non-delta cycle
//  2: stop
//  3: finished
//  4: stop-time reached
//  5: stop-delta reached