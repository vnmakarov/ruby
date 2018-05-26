/**********************************************************************

  mjit.h - Interface to MRI method JIT compiler

  Copyright (C) 2017 Vladimir Makarov <vmakarov@redhat.com>.

**********************************************************************/

#ifndef RUBY_MJIT_H
#define RUBY_MJIT_H 1

#include "ruby.h"

/* Make it non-zero if you want to collect and print statistics about
   execution of byte code insns and JIT generated code for the insns.
   Use non-zero for a performance analysis as it might considerably
   decrease performance of MRI in JIT and non-JIT mode.  */
#define MJIT_INSN_STATISTICS 0

/* Special address values of a function generated from the
   corresponding iseq by MJIT: */
enum rb_mjit_iseq_func {
    /* ISEQ was not queued yet for the machine code generation */
    NOT_ADDED_JIT_ISEQ_FUNC = 0,
    /* ISEQ is already queued for the machine code generation but the
       code is not ready yet for the execution */
    NOT_READY_JIT_ISEQ_FUNC = 1,
    /* ISEQ included not compilable insn or some assertion failed  */
    NOT_COMPILABLE_JIT_ISEQ_FUNC = 2,
    /* End mark */
    LAST_JIT_ISEQ_FUNC = 3
};

/* MJIT options which can be defined on the MRI command line.  */
struct mjit_options {
    char on; /* flag of MJIT usage  */
    /* Flag of collecting and printing info about ISEQ execution as a
       byte code and as JIT generated code.  */
    char profile;
    /* Save temporary files after MRI finish.  The temporary files
       include the pre-compiled header, C code file generated for ISEQ,
       and the corresponding object file.  */
    char save_temps;
    /* Print MJIT warnings to stderr.  */
    char warnings;
    /* Number of max mutations of iseq JIT code.  */
    int max_mutations;
    /* Disable compiler optimization and add debug symbols. It can be
       very slow.  */
    char debug;
    /* If not 0, all ISeqs are synchronously compiled. For testing. */
    unsigned int wait;
    /* Number of calls to trigger JIT compilation. For testing. */
    unsigned int min_calls;
    /* Force printing info about MJIT work of level VERBOSE or
       less. 0=silence, 1=medium, 2=verbose.  */
    int verbose;
    /* Maximal permitted number of iseq JIT codes in a MJIT memory
       cache.  */
    int max_cache_size;
};

struct rb_mjit_compile_info;

#if MJIT_INSN_STATISTICS
extern unsigned long byte_code_insns_num;
#endif

typedef VALUE (*mjit_func_t)(rb_execution_context_t *, rb_control_frame_t *);

RUBY_SYMBOL_EXPORT_BEGIN
extern struct mjit_options mjit_opts;

#ifndef MJIT_HEADER
extern int mjit_init_p;
#else
/* Make it const to permit more optimizations in JIT code  */
static const int mjit_init_p = 1;
#endif

extern void mjit_add_iseq_to_process(const rb_iseq_t *iseq);
extern mjit_func_t mjit_get_iseq_func(struct rb_iseq_constant_body *body);
RUBY_SYMBOL_EXPORT_END

extern int mjit_compile(FILE *f, const struct rb_iseq_constant_body *body, const char *funcname);
extern int mjit_rtl_compile(FILE *f, const rb_iseq_t *iseq,
			    struct rb_mjit_compile_info *ci, const char *funcname);
extern struct rb_mjit_compile_info *mjit_create_rtl_compile_info(const rb_iseq_t *iseq);
extern void mjit_free_rtl_compile_info(struct rb_mjit_compile_info *ci);

extern struct rb_mjit_compile_info *mjit_iseq_compile_info(const rb_iseq_t *iseq);
extern void mjit_init(struct mjit_options *opts);
extern void mjit_finish(void);
extern void mjit_gc_start_hook(void);
extern void mjit_gc_finish_hook(void);
extern void mjit_free_iseq(const rb_iseq_t *iseq);
extern void mjit_mark(void);
extern struct mjit_cont *mjit_cont_new(rb_execution_context_t *ec);
extern void mjit_cont_free(struct mjit_cont *cont);
extern void mjit_add_class_serial(rb_serial_t class_serial);
extern void mjit_remove_class_serial(rb_serial_t class_serial);
extern int mjit_valid_class_serial_p(rb_serial_t class_serial);
extern void mjit_recompile_iseq(rb_iseq_t *iseq, int (*guard)(struct rb_mjit_compile_info *compile_info));

extern void mjit_cancel_all(void);
extern void mjit_redo_iseq(rb_iseq_t *iseq, int spec_fail_p);
extern void mjit_ivar_spec_fail(rb_iseq_t *iseq);
extern void mjit_ep_eq_bp_fail(rb_iseq_t *iseq);
extern void mjit_store_failed_spec_insn(rb_iseq_t *iseq, size_t pc, int mutation_num);

/* A threshold used to reject long iseqs from JITting as such iseqs
   takes too much time to be compiled.  */
#define JIT_ISEQ_SIZE_THRESHOLD 1000

/* An used external.  */
extern int vm_call_iseq_setup_normal_p(vm_call_handler h);
extern VALUE vm_call_ivar(rb_execution_context_t *ec, rb_control_frame_t *cfp, struct rb_calling_info *calling,
			  const struct rb_call_info *ci, struct rb_call_cache *cc);
extern VALUE vm_call_attrset(rb_execution_context_t *ec, rb_control_frame_t *cfp, struct rb_calling_info *calling,
			     const struct rb_call_info *ci, struct rb_call_cache *cc);
extern VALUE
vm_call_cfunc(rb_execution_context_t *ec, rb_control_frame_t *reg_cfp, struct rb_calling_info *calling,
	      const struct rb_call_info *ci, struct rb_call_cache *cc);

/* TRUE if we are collecting statistics about JIT calls.  */
static const char mjit_profile_p;

/* The function is called when ISEQ is changed.  SPEC_FAIL_P flags
   that it happens becuase of failure in speculation of a particular
   insn.  */
static do_inline void
mjit_change_iseq(rb_iseq_t *iseq, int spec_fail_p) {
    if ((ptrdiff_t)iseq->body->jit_func >= (ptrdiff_t)LAST_JIT_ISEQ_FUNC) {
	if (spec_fail_p)
	    iseq->body->failed_jit_calls++;
	mjit_redo_iseq(iseq, spec_fail_p);
    }
}

/* Return TRUE if given iseq BODY of TYPE should be compiled by
   MJIT.  */
static inline int
mjit_target_iseq_p(struct rb_iseq_constant_body *body, int body_type)
{
    return ((body_type == ISEQ_TYPE_METHOD || body_type == ISEQ_TYPE_BLOCK)
	    && body->iseq_size < JIT_ISEQ_SIZE_THRESHOLD);
}

/* Try to execute the current ISEQ with BODY of TYPE in ec.  Use JIT
   code if it is ready.  If it is not, add ISEQ to the compilation
   queue and return Qundef.  */
static inline VALUE
mjit_exec_iseq(rb_execution_context_t *ec, const rb_iseq_t *iseq,
	       struct rb_iseq_constant_body *body, int type)
{
    long unsigned total_calls;
    mjit_func_t func;

    total_calls = ++body->total_calls;

    func = body->jit_func;
    if (UNLIKELY(mjit_opts.wait && mjit_opts.min_calls == total_calls && mjit_target_iseq_p(body, type)
                 && func == (mjit_func_t)NOT_ADDED_JIT_ISEQ_FUNC)) {
        mjit_add_iseq_to_process(iseq);
        func = mjit_get_iseq_func(body);
    }

    if (UNLIKELY((ptrdiff_t)func <= (ptrdiff_t)LAST_JIT_ISEQ_FUNC)) {
        switch ((enum rb_mjit_iseq_func)func) {
          case NOT_ADDED_JIT_ISEQ_FUNC:
	      if (total_calls == mjit_opts.min_calls && mjit_target_iseq_p(body, type)) {
                mjit_add_iseq_to_process(iseq);
            }
            return Qundef;
          case NOT_READY_JIT_ISEQ_FUNC:
          case NOT_COMPILABLE_JIT_ISEQ_FUNC:
            return Qundef;
          default: /* to avoid warning with LAST_JIT_ISEQ_FUNC */
            break;
        }
    }

    return func(ec, ec->cfp);
}

/* Analogous to mjit_exec_iseq but for the current iseq in ec.  */
static inline VALUE
mjit_exec(rb_execution_context_t *ec)
{
    const rb_iseq_t *iseq;
    struct rb_iseq_constant_body *body;

    if (!mjit_init_p)
        return Qundef;

    iseq = ec->cfp->iseq;
    body = iseq->body;
    return mjit_exec_iseq(ec, iseq, body, body->type);
}

#endif /* RUBY_MJIT_H */
