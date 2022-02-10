#include "sir.h"

static struct rb_rtl_options rtl_options;

int
rb_rtl_enabled_p(void) { return rtl_options.rtl_enabled_p; }

int
rb_rtl_debug_p(void) { return rtl_options.debug_p; }

void
rb_rtl_init(struct rb_rtl_options *options) {
    rtl_options = *options;
    rb_rtl_gen_init();
}

void
rb_rtl_finish(struct rb_rtl_options *options) {
    rb_rtl_gen_finish();
}
