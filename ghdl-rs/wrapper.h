#include "vpi_thunk.h"
// #include "vhpi_user.h"
#include "grt.h"
#include "grt-cdynload.h"

typedef int (*loadVpiModulePtr)(const char* modulename);

extern int loadVpiModule(const char* modulename);

// extern void *__ghdl_rti_top_instance;

extern int ghdl_main(int argc, const char* const* argv);
typedef int (*ghdl_mainPtr)(int argc, const char* const* argv);