/**********************************************************************

  mjit.h - Interface to MRI method JIT compiler

  Copyright (C) 2017 Vladimir Makarov <vmakarov@redhat.com>.

**********************************************************************/

/* Special address values of a function generated from the
   corresponding iseq by MJIT: */
enum rb_mjit_iseq_fun {
    /* ISEQ was not queued yet for the machine code generation */
    NOT_ADDED_JIT_ISEQ_FUN = 0,
    /* ISEQ was not queued and will be never queued for the machine code
       generation */
    NEVER_JIT_ISEQ_FUN = 1,
    /* ISEQ is already queued for the machine code generation but the
       code is not ready yet for the execution */
    NOT_READY_JIT_ISEQ_FUN = 2,
    /* The same as above but in AOT mode */
    NOT_READY_AOT_ISEQ_FUN = 3,
    /* End mark */
    LAST_JIT_ISEQ_FUN = 4,
};

/* A forward declaration  */
struct rb_mjit_batch_iseq;
/* Flag of successful MJIT initialization and intention to use it */
extern int mjit_init_p;

/* Make it non-zero if you want to collect and print statistics about
   execution of byte code insns and JIT generated code for the insns.
   Use non-zero for a performance analysis as it might considerably
   decrease performance of MRI in JIT and non-JIT mode.  */
#define MJIT_INSN_STATISTICS 0

/* MJIT options which can be defined on the MRI command line.  */
struct mjit_options {
    char on; /* flag of MJIT usage  */
    char aot; /* Ahead of time compilation */
    /* Flag of collecting and printing info about ISEQ execution as a
       byte code and as JIT generated code.  */
    char profile;
    /* Flag of printing info about MJIT work including ISEQ compiling and
       loading their machine code.  */
    char verbose;
    /* Flag to use LLVM Clang instead of default GCC for MJIT . */
    char llvm;
    /* Save temporary files after MRI finish.  The temporary files
       include the pre-compiled header, C code file generated for ISEQ,
       and the corresponding object file.  */
    char save_temps;
    /* Print MJIT warnings to stderr.  */
    char warnings;
    /* Use debug mode.  It can be very slow as no optimizations are
       used.  */
    char debug;
    /* Number of threads processing MJIT queue.  */
    int threads;
    /* Number of max mutations of iseq JIT code.  */
    int max_mutations;
};

extern struct mjit_options mjit_opts;
extern unsigned long byte_code_insns_num;

typedef VALUE (*mjit_fun_t)(rb_thread_t *, rb_control_frame_t *);

extern void mjit_init(struct mjit_options *opts);
extern void mjit_add_iseq_to_process(rb_iseq_t *iseq);
extern mjit_fun_t mjit_get_iseq_fun(const rb_iseq_t *iseq);
extern void mjit_cancel_all(void);
extern void mjit_increase_iseq_priority(const rb_iseq_t *iseq);
extern void mjit_redo_iseq(rb_iseq_t *iseq);
extern void mjit_free_iseq(const rb_iseq_t *iseq);
extern void mjit_store_failed_spec_insn(rb_iseq_t *iseq, size_t pc, int mutation_num);
extern void mjit_finish(void);

/* A threshold used to move iseq to the queue head. */
#define NUM_CALLS_TO_PRIORITY_INCREASE 500

/* A forward declaration */
extern VALUE vm_exec(rb_thread_t *th, int no_mjit_p);

/* An used external.  */
extern int vm_call_iseq_setup_normal_p(vm_call_handler h);
extern VALUE vm_call_ivar(rb_thread_t *th, rb_control_frame_t *cfp, struct rb_calling_info *calling,
			  const struct rb_call_info *ci, struct rb_call_cache *cc);
extern VALUE vm_call_attrset(rb_thread_t *th, rb_control_frame_t *cfp, struct rb_calling_info *calling,
			     const struct rb_call_info *ci, struct rb_call_cache *cc);
extern VALUE
vm_call_cfunc(rb_thread_t *th, rb_control_frame_t *reg_cfp, struct rb_calling_info *calling,
	      const struct rb_call_info *ci, struct rb_call_cache *cc);

/* TRUE if we are collecting statistics about JIT calls.  */
static const char mjit_profile_p;

/* Try to execute the current ISEQ with BODY and TYPE in thread TH.
   Use JIT code if it is ready.  If it is not, add ISEQ to the
   compilation queue and return Qundef.  The function might increase
   ISEQ compilation priority by putting ISEQ at the queue head.  */
static inline VALUE
mjit_execute_iseq_0(rb_thread_t *th, rb_iseq_t *iseq,
		    struct rb_iseq_constant_body *body, int type) {
    unsigned long n_calls;
    mjit_fun_t fun;
    VALUE v;
    
    fun = body->jit_code;
    n_calls = ++body->overall_calls;

    if (UNLIKELY((ptrdiff_t) fun <= (ptrdiff_t) LAST_JIT_ISEQ_FUN)) {
	switch ((enum rb_mjit_iseq_fun) fun) {
	case NOT_ADDED_JIT_ISEQ_FUN:
	    if (type != ISEQ_TYPE_METHOD && type != ISEQ_TYPE_BLOCK) {
		body->jit_code = (void *) NEVER_JIT_ISEQ_FUN;
		return Qundef;
	    } else {
		body->jit_code = (void *) NOT_READY_JIT_ISEQ_FUN;
		mjit_add_iseq_to_process(iseq);
		return Qundef;
	    }
	    break;
	case NEVER_JIT_ISEQ_FUN:
	    return Qundef;
	case NOT_READY_AOT_ISEQ_FUN:
	    if ((ptrdiff_t) (fun = mjit_get_iseq_fun(iseq)) <= (ptrdiff_t) LAST_JIT_ISEQ_FUN)
		return Qundef;
	    break;
	case NOT_READY_JIT_ISEQ_FUN:
	    if (n_calls == NUM_CALLS_TO_PRIORITY_INCREASE)
		mjit_increase_iseq_priority(iseq);
	    return Qundef;
	default: /* To avoid a warning on LAST_JIT_ISEQ_FUN */
	    break;
	}
    }
    if (mjit_profile_p)
	body->jit_calls++;
    v = fun(th, th->cfp);
    return v;
}

/* See the above function.  */
static inline VALUE
mjit_execute_iseq(rb_thread_t *th) {
    rb_iseq_t *iseq;
    struct rb_iseq_constant_body *body;
    
    if (! mjit_init_p)
	return Qundef;
    iseq = th->cfp->iseq;
    body = iseq->body;
    return mjit_execute_iseq_0(th, iseq, body, body->type);
}

/* Queue AOT compilation of ISEQ right after forming it.  */
static inline void
mjit_aot_process(rb_iseq_t *iseq) {
    struct rb_iseq_constant_body *body = iseq->body;

    if (! mjit_init_p || ! mjit_opts.aot)
	return;

    if (body->type != ISEQ_TYPE_METHOD && body->type != ISEQ_TYPE_BLOCK
	&& body->type != ISEQ_TYPE_TOP && body->type != ISEQ_TYPE_MAIN) {
	body->jit_code = (void *) NEVER_JIT_ISEQ_FUN;
    } else {
	body->jit_code = (void *) NOT_READY_AOT_ISEQ_FUN;
	mjit_add_iseq_to_process(iseq);
    }
}

/* The function is called when ISEQ is changed.  It can happens as we
   have speculative insns.  */
static inline void
mjit_change_iseq(rb_iseq_t *iseq) {
    if (iseq->body->jit_code >= (void *) LAST_JIT_ISEQ_FUN) {
	iseq->body->failed_jit_calls++;
	mjit_redo_iseq(iseq);
    }
}
