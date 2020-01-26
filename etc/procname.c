#include <sys/prctl.h>
#include <stdlib.h>

static void __attribute__ ((constructor)) procname_init() {
  prctl(PR_SET_NAME, "fury");
}
