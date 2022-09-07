/**********************************************************************

  mirjit.h - MIRJIT interface.

  Copyright (C) 2022 Vladimir Makarov vmakarov@redhat.com

**********************************************************************/
#ifndef MIRJIT_H
#define MIRJIT_H 1

#define JRET
//#define MIRJIT_RESTART_STAT
//#define MIRJIT_ITER_INSNS

#include "vm_core.h"

int rb_mirjit_enabled_p(void);
void *rb_mirjit_get_pc_dispatch_addr(VALUE *pc);
enum ruby_vminsn_type rb_mirjit_insn_decode(VALUE *slot);
void rb_mirjit_mark_bbv_change(struct rb_iseq_bbv *bbv);
void rb_rejit_bbv(struct rb_iseq_bbv *bbv);

#ifdef HAVE_LIBMIR

/* MIRJIT_STATS controls supporting runtime counters in generation and execution of MIRJIT code.
   Switching this on slows down interpretation enormously. */
#ifndef MIRJIT_EXEC_STATS
# define MIRJIT_EXEC_STATS RUBY_DEBUG
#endif

struct rb_mirjit_options {
    int mirjit_enabled_p; /* true means generation and execution of MIRJIT BBV */
    int debug_p;
    int exec_stats_p; /* print MIRJIT execution stats if collecting stats is enabled */
};

int rb_mirjit_debug_p(void);
void rb_mirjit_set_pc_dispatch_addr(VALUE *pc, void *addr);
void rb_jit_bbv(struct rb_iseq_bbv *bbv);
void rb_mirjit_init(struct rb_mirjit_options *options);
void rb_mirjit_finish(void);

/* vm.c: */
void rb_mirjit_resolver_init(void);
void *rb_mirjit_import_resolver(const char *name);
void rb_mirjit_resolver_finish(void);

#endif /* #ifdef HAVE_LIBMIR */
#endif /* #ifndef MIRJIT_H */
