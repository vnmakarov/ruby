/**********************************************************************

  sir.h - SIR interface.

  Copyright (C) 2021 Vladimir Makarov vmakarov@redhat.com

**********************************************************************/
#ifndef SIR_H
#define SIR_H 1

#include <stdlib.h>
#include "ruby/internal/config.h"
#include "ruby_assert.h"
#include "vm_core.h"

#if defined(__GNUC__)
#define sir_inline inline __attribute__ ((__always_inline__))
#define sir_noinline __attribute__ ((__noinline__))
#define SIR_UNUSED __attribute__ ((__unused__))
#else
#define sir_inline inline
#define sir_noinline
#define SIR_UNUSED
#endif

//#define SIR_SPEC_CHECK
//#define SIR_PROFILE
//#define MIRJIT_STAT

extern long rb_sir_profile[];
extern long rb_sir_cancel[];

/* SIR_STATS controls supporting runtime counters in generation and execution of SIR code.
   Switching this on slows down interpretation enormously. */
#ifndef SIR_EXEC_STATS
# define SIR_EXEC_STATS RUBY_DEBUG
#endif

#define FLONUM_INSNS

struct rb_sir_options {
    int sir_enabled_p; /* true means generation and execution of SIR BBV */
    /* maximal versions for SIR BB, one version is always reserved for unknown incoming attrs: */
    unsigned max_bb_versions;
     /* 0 - no debugging, 1 - print SIR code changes and BB headers, 2 (default) - print SIR BB code,
        3 - print info for buitins and prelude, make bbv area insns, final BBV CFG */
    int debug_level;
    int exec_stats_p; /* print SIR execution stats if collecting stats is enabled */
};

MJIT_FUNC_EXPORTED const void * const *sir_dispatch_table;

MJIT_FUNC_EXPORTED int rb_sir_enabled_p(void);
int rb_sir_debug_level(void);
void rb_sir_set_debug_level(int level);
void rb_sir_init(struct rb_sir_options *options);
void rb_sir_finish(void);
/* sir_gen.c: */
int rb_sir_gen_init(void);
void rb_sir_gen_finish(void);
void rb_sir_trace_update(const rb_iseq_t *iseq);
void rb_sir_print_bbv(struct rb_iseq_bbv *bbv);
void rb_sir_stub_gen(rb_iseq_t *iseq);
VALUE *rb_sir_make_bbv(rb_control_frame_t *cfp, int iseq_bbv_start);
VALUE *rb_sir_make_optpc_bbv(rb_control_frame_t *cfp, int opt);
VALUE *rb_sir_make_catch_bbv(rb_control_frame_t *cfp, int entry_ind);
void rb_sir_inspect_type(rb_control_frame_t *cfp, VALUE val, int stack_p);
VALUE *rb_sir_gen_bbv(rb_control_frame_t *cfp, struct rb_iseq_bbv *bbv, int branch_offset, int first_p);
VALUE *rb_sir_gen_bbv_key(rb_control_frame_t *cfp, struct rb_iseq_bbv *bbv, int case_offset, VALUE key);
void rb_sir_bop_redefined(enum ruby_basic_operators bop, int klass_flag);
MJIT_FUNC_EXPORTED int rb_sir_singleton_method_defined(VALUE old_class, ID id);
void rb_sir_cme_invalidate(const rb_callable_method_entry_t *cme);
void rb_sir_trace_invalidate(void);
VALUE *rb_sir_specialize_bbv(rb_control_frame_t *cfp, struct rb_iseq_bbv *bbv);
void rb_sir_change_getivar(VALUE self, rb_control_frame_t *caller_cfp, IVC ic);
void rb_sir_change_setivar(VALUE self, rb_control_frame_t *caller_cfp, IVC ic);
void rb_sir_change_send(const rb_execution_context_t *ec, rb_control_frame_t *caller_cfp,
                        CALL_DATA cd, struct rb_sir_send_info *si);
void rb_sir_change_bbv(struct rb_iseq_bbv *bbv);
MJIT_FUNC_EXPORTED const VALUE *rb_sir_trace_stack_insn_pc(rb_execution_context_t *ec, rb_control_frame_t *reg_cfp);
const VALUE *rb_sir_get_safe_pc(const rb_control_frame_t *cfp, const VALUE *pc);
const VALUE *rb_sir_get_jump_safe_pc(const rb_control_frame_t *cfp, const VALUE *pc);

void rb_sir_iseq_free(struct rb_iseq_constant_body *body);
void rb_sir_iseq_update_references(const struct rb_iseq_constant_body *body);
void rb_sir_iseq_data_mark(const struct rb_iseq_constant_body *body);

static sir_inline VALUE sir_dbl2val(double d) {
    union {
        double d;
        VALUE v;
    } u = {d};
    return u.v;
}
static sir_inline double sir_val2dbl(VALUE v) {
    union {
        VALUE v;
        double d;
    } u = {v};
    return u.d;
}

MJIT_FUNC_EXPORTED VALUE (*sir_fixnum_ntimes_func)(VALUE);
MJIT_FUNC_EXPORTED VALUE (*sir_range_each_func)(VALUE);

int sir_get_iter_funcs(void *cfunc, int (**start_func)(VALUE, VALUE *),
                       int (**arg_func)(VALUE, VALUE, VALUE *),
                       VALUE (**cont_func)(VALUE, VALUE),
                       char *);

#define SIR_ALLOC_N(type, n) malloc(sizeof(type) * (n))
#define SIR_ZALLOC_N(type, n) calloc((n), sizeof(type))
#define SIR_REALLOC_N(ptr, type, n) (ptr = realloc(ptr, sizeof(type) * (n)))
#define SIR_FREE(ptr) free(ptr)
/*---------- Variable length arrays (VARR) --------------------------*/
/*------- This implementaion supports static type checking. -------- */

extern rb_jmpbuf_t sir_gen_jump_buf;

/* Asserts for VARR:  */
#ifdef ENABLE_CHECKING
#define VARR_ASSERT_FAIL(OP,VARR) varr_assert_fail(OP,#VARR)

static inline void varr_assert_fail(const char *op, const char *var) {
    fprintf(stderr, "wrong %s for %s", op, var);
    SIR_GEN_ASSERT(0);
}

#define VARR_ASSERT(EXPR,OP,T)					\
  (void)((EXPR) ? 0 : (VARR_ASSERT_FAIL(OP,VARR(T)), 0))

#else
#define VARR_ASSERT(EXPR,OP,T) ((void)(EXPR))
#endif

static inline RBIMPL_ATTR_NORETURN() void
varr_malloc_failure(void) {
    RUBY_LONGJMP(sir_gen_jump_buf, 1);
}

/* Name of type for VARR of elements of type T.  */
#define VARR(T) VARR_##T
/* Name of function implementing OP for VARR of elements of type
   T.  */
#define VARR_OP(T, OP) VARR_##T##_##OP

/* Definition of type of VAR of elements of type T.  */
#define VARR_T(T)							      \
typedef struct VARR(T) {                                                      \
    size_t els_num; /* number of elements currently in VARR */  	      \
    size_t size;    /* curr size of container VARR in elements */             \
    T *varr;          /* the elements container */			      \
} VARR(T)

/* Default initial size for variable array elements container.  */
#define VARR_DEFAULT_SIZE 64

/* Definition of VARR of elements of type T and all its functions:  */
#define DEF_VARR(T)                                                            \
VARR_T(T);								       \
									       \
static inline SIR_UNUSED void VARR_OP(T, create)(VARR(T) **varr_, size_t size_) {         \
    VARR(T) *va;						       	       \
    if (size_ == 0)							       \
        size_ = VARR_DEFAULT_SIZE;					       \
    *varr_ = va = (VARR(T) *)SIR_ALLOC_N(VARR(T), 1);                  	       \
    if (va == NULL) varr_malloc_failure();				       \
    va->els_num = 0; va->size = size_;					       \
    va->varr = (T *)SIR_ALLOC_N(T, size_);                    	       	       \
    if (va->varr == NULL) varr_malloc_failure();			       \
}                                                                              \
                                                                               \
static inline SIR_UNUSED void VARR_OP(T, destroy)(VARR(T) **varr_) {	               \
    VARR(T) *va = *varr_;						       \
    VARR_ASSERT(va && va->varr, "destroy", T);				       \
    free(va->varr);							       \
    free(va); 								       \
    *varr_ = NULL;							       \
}									       \
                                                                               \
static inline SIR_UNUSED size_t VARR_OP(T, length)(const VARR(T) *varr_) {	               \
    VARR_ASSERT(varr_, "length", T);					       \
    return varr_->els_num;						       \
}                                                                              \
                                                                               \
static inline SIR_UNUSED T *VARR_OP(T, addr)(const VARR(T) *varr_) {	               \
    VARR_ASSERT(varr_, "addr", T);					       \
    return &varr_->varr[0];						       \
}                                                                              \
                                                                               \
static inline SIR_UNUSED T VARR_OP(T, last)(const VARR(T) *varr_) {                       \
    VARR_ASSERT(varr_ && varr_->varr && varr_->els_num, "last", T);	       \
    return varr_->varr[varr_->els_num - 1];				       \
}                                                                              \
                                                                               \
static inline SIR_UNUSED T VARR_OP(T, get)(const VARR(T) *varr_, size_t ix_) { 	       \
    VARR_ASSERT(varr_ && varr_->varr && ix_ < varr_->els_num, "get", T);       \
    return varr_->varr[ix_];						       \
}                                                                              \
                                                                               \
static inline SIR_UNUSED T VARR_OP(T, set)(const VARR(T) *varr_, size_t ix_, T obj_) {    \
    T old_obj_;								       \
    VARR_ASSERT(varr_ && varr_->varr && ix_ < varr_->els_num, "set", T);       \
    old_obj_ = varr_->varr[ix_];					       \
    varr_->varr[ix_] = obj_;						       \
    return old_obj_;							       \
}                                                                              \
                                                                               \
static inline SIR_UNUSED void VARR_OP(T,trunc)(VARR(T) *varr_, size_t size_) {            \
    VARR_ASSERT(varr_ && varr_->varr && varr_->els_num >= size_, "trunc", T);  \
    varr_->els_num = size_;						       \
}									       \
                                                                               \
static inline SIR_UNUSED void VARR_OP(T,expand)(VARR(T) *varr_, size_t size_) {	       \
    VARR_ASSERT(varr_  && varr_->varr, "expand", T);			       \
    if (varr_->size < size_) {						       \
	size_ += size_ / 2;						       \
	SIR_REALLOC_N(varr_->varr, T, size_);                 	       	       \
        if (varr_->varr == NULL) varr_malloc_failure();			       \
	varr_->size = size_;						       \
    }                                                                          \
}									       \
									       \
static inline SIR_UNUSED void VARR_OP(T, push)(VARR(T) *varr_, T obj_) {	               \
    T *slot_;								       \
    VARR_OP(T, expand)(varr_, varr_->els_num + 1);			       \
    slot_ = &varr_->varr[varr_->els_num++];				       \
    *slot_ = obj_;							       \
}                                                                              \
                                                                               \
static inline SIR_UNUSED T VARR_OP(T, pop)(VARR(T) *varr_) {			       \
    T obj_;								       \
    VARR_ASSERT(varr_ && varr_->varr && varr_->els_num, "pop", T);	       \
    obj_ = varr_->varr[--varr_->els_num];				       \
    return obj_;							       \
}

/* Macros implementing operations for VARR V of elements of type T: */
#define VARR_CREATE(T, V, L) (VARR_OP(T, create)(&(V), L))
#define VARR_DESTROY(T, V) (VARR_OP(T, destroy)(&(V)))
#define VARR_LENGTH(T, V) (VARR_OP(T, length)(V))
#define VARR_ADDR(T, V) (VARR_OP(T, addr)(V))
#define VARR_LAST(T, V) (VARR_OP(T, last)(V))
#define VARR_GET(T, V, I) (VARR_OP(T, get)(V, I))
#define VARR_SET(T, V, I, O) (VARR_OP(T, set)(V, I, O))
#define VARR_TRUNC(T, V, S) (VARR_OP(T, trunc)(V, S))
#define VARR_EXPAND(T, V, S) (VARR_OP(T, expand)(V, S))
#define VARR_PUSH(T, V, O) (VARR_OP(T, push)(V, O))
#define VARR_POP(T, V) (VARR_OP(T, pop)(V))

#endif /* #ifndef SIR_H */
