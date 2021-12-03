/**********************************************************************

  rtl.h - RTL interface.

  Copyright (C) 2021 Vladimir Makarov vmakarov@redhat.com

**********************************************************************/
#ifndef RTL_H
#define RTL_H 1

#include "ruby/internal/config.h"
#include "ruby_assert.h"
#include "vm_core.h"

/* RTL_STATS controls supporting runtime counters in generation and execution of RTL code.
   Switching this on slows down interpretation enormously. */
#ifndef RTL_EXEC_STATS
# define RTL_EXEC_STATS RUBY_DEBUG
#endif

struct rb_rtl_options {
    int rtl_enabled_p; /* true means generation and execution of RTL BBV */
    /* maximal versions for RTL BB, one version is always reserved for unknown incoming attrs: */
    unsigned max_versions;
    int debug_p;
    int exec_stats_p; /* print RTL execution stats if collecting stats is enabled */
};

int rb_rtl_enabled_p(void);
int rb_rtl_debug_p(void);
void rb_rtl_init(struct rb_rtl_options *options);
/* rtl_gen.c: */
int rb_rtl_gen_init(void);
void rb_rtl_gen_finish(void);
void rb_rtl_create_default_bbvs(rb_iseq_t *iseq);
int rb_rtl_make_bbv(rb_control_frame_t *cfp);
int rb_rtl_gen_bbv(rb_control_frame_t *cfp, struct rb_iseq_bbv *bbv, int branch_offset, int first_p);
void rb_rtl_change_send_without_block(const rb_execution_context_t *ec, rb_control_frame_t *caller_cfp,
                                      CALL_DATA cd, char *attrs);

void rb_rtl_iseq_free(struct rb_iseq_constant_body *body);

#endif /* #ifndef RTL_H */
