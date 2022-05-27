#include "sir.h"

static struct rb_sir_options sir_options;
static int initialized_p;

int
rb_sir_enabled_p(void) { return sir_options.sir_enabled_p; }

int
rb_sir_debug_p(void) { return sir_options.debug_p; }

void
rb_sir_init(struct rb_sir_options *options) {
    sir_options = *options;
    rb_sir_gen_init();
    initialized_p = TRUE;
}

void
rb_sir_finish(void) {
    if (initialized_p) rb_sir_gen_finish();
}
