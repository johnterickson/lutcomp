#include "vpi_thunk.h"

typedef void (*grt_initPtr) (void);
typedef int (*grt_main_optionsPtr) (const char *progname, int argc, const char * const argv[]);
typedef int (*grt_main_elabPtr) (void);
typedef void (*__ghdl_simulation_initPtr) (void);
typedef int (*__ghdl_simulation_stepPtr) (void);
const int GHDL_STEP_RESULT_DELTA = 0;
const int GHDL_STEP_RESULT_NON_DELTA = 1;
const int GHDL_STEP_RESULT_STOP = 2;
const int GHDL_STEP_RESULT_FINISHED = 3;
const int GHDL_STEP_RESULT_STOP_TIME_REACHED = 4;
const int GHDL_STEP_RESULT_STOP_DELTA_REACHED = 5;
