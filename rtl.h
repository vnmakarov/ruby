/**********************************************************************

  rtl.h - RTL interface.

  Copyright (C) 2021 Vladimir Makarov vmakarov@redhat.com

**********************************************************************/
#ifndef RTL_H
#define RTL_H 1

#include "ruby/internal/config.h"
#include "ruby_assert.h"
#include "vm_core.h"

#if defined(__GNUC__)
#define rtl_inline inline __attribute__ ((__always_inline__))
#define rtl_noinline __attribute__ ((__noinline__))
#else
#define rtl_inline inline
#define rtl_noinline
#endif

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

struct rb_rtl_send_info {
    struct rb_iseq_bbv *caller_bbv;
    char *attrs;
};

int rb_rtl_enabled_p(void);
int rb_rtl_debug_p(void);
void rb_rtl_init(struct rb_rtl_options *options);
/* rtl_gen.c: */
int rb_rtl_gen_init(void);
void rb_rtl_gen_finish(void);
void rb_rtl_trace_update(const rb_iseq_t *iseq);
void rb_rtl_stub_gen(rb_iseq_t *iseq);
VALUE *rb_rtl_make_bbv(rb_control_frame_t *cfp, int iseq_bbv_start);
VALUE *rb_rtl_make_optpc_bbv(rb_control_frame_t *cfp, int opt);
VALUE *rb_rtl_make_catch_bbv(rb_control_frame_t *cfp, int entry_ind);
void rb_rtl_inspect_type(rb_control_frame_t *cfp, VALUE val, int stack_p);
VALUE *rb_rtl_gen_bbv(rb_control_frame_t *cfp, struct rb_iseq_bbv *bbv, int branch_offset, int first_p);
VALUE *rb_rtl_gen_bbv_key(rb_control_frame_t *cfp, struct rb_iseq_bbv *bbv, int case_offset, VALUE key);
void rb_rtl_bop_redefined(enum ruby_basic_operators bop, int klass_flag);
void rb_rtl_cme_invalidate(rb_callable_method_entry_t *cme);
VALUE *rb_rtl_specialize_bbv(rb_control_frame_t *cfp, struct rb_iseq_bbv *bbv);
void rb_rtl_change_getivar(VALUE self, rb_control_frame_t *caller_cfp, IVC ic, VALUE val);
void rb_rtl_change_setivar(VALUE self, rb_control_frame_t *caller_cfp, IVC ic);
void rb_rtl_change_send_without_block(const rb_execution_context_t *ec, rb_control_frame_t *caller_cfp,
                                      CALL_DATA cd, struct rb_rtl_send_info *si);
const VALUE *rb_rtl_trace_stack_insn_pc(rb_execution_context_t *ec, rb_control_frame_t *reg_cfp);
const VALUE *rb_rtl_get_safe_pc(const rb_control_frame_t *cfp, const VALUE *pc);

void rb_rtl_iseq_free(struct rb_iseq_constant_body *body);

static rtl_inline VALUE rtl_dbl2val(double d) {
    union {
        double d;
        VALUE v;
    } u = {d};
    return u.v;
}
static rtl_inline double rtl_val2dbl(VALUE v) {
    union {
        VALUE v;
        double d;
    } u = {v};
    return u.d;
}

#endif /* #ifndef RTL_H */
