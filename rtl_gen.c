/**********************************************************************

  rtl_gen.c - Generate RTL insns.

  Copyright (C) 2021 Vladimir Makarov vmakarov@redhat.com

**********************************************************************/

#include "rtl.h"
#include "internal/hash.h"
#include "internal/compile.h"
#include "internal/variable.h"
#include "vm_callinfo.h"

#include "builtin.h"
#include "insns.inc"
#include "insns_info.inc"

/* To generate RTL insns we passes stack insns several times.  First
   we calculate possible stack values on the label.  It is a forward
   data flow problem (the final fixed point is only temporaries on the
   emulated stack).  Then using this info we actually generate RTL
   insns on the 2nd pass.

   We emulate VM stack to generate RTL insns for another RTL insn
   operands in a lazy way.  Therefore the order of RTL insns for
   calculating some simple operands can be different from
   corresponding stack insns.  */

static const int longer_paths_p = TRUE;
static const int follow_jumps_p = TRUE;

#define RTL_GEN_ASSERT(expr) RUBY_ASSERT_ALWAYS(expr)

/* Use nonzero to print debug info abou the generator.  */
#define RTL_GEN_DEBUG 0

/* Long jump buffer used to finish the generator work in case of a
   failure.  */
static rb_jmpbuf_t rtl_gen_jump_buf;

/*---------- Variable length arrays (VARR) --------------------------*/
/*------- This implementaion supports static type checking. -------- */

/* Asserts for VARR:  */
#ifdef ENABLE_CHECKING
#define VARR_ASSERT_FAIL(OP,VARR) varr_assert_fail(OP,#VARR)

static inline void varr_assert_fail(const char *op, const char *var) {
    fprintf(stderr, "wrong %s for %s", op, var);
    RTL_GEN_ASSERT(0);
}

#define VARR_ASSERT(EXPR,OP,T)					\
  (void)((EXPR) ? 0 : (VARR_ASSERT_FAIL(OP,VARR(T)), 0))

#else
#define VARR_ASSERT(EXPR,OP,T) ((void)(EXPR))
#endif

static void
varr_malloc_failure(void) {
    RUBY_LONGJMP(rtl_gen_jump_buf, 1);
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
static inline void VARR_OP(T, create)(VARR(T) **varr_, size_t size_) {         \
    VARR(T) *va;						       	       \
    if (size_ == 0)							       \
        size_ = VARR_DEFAULT_SIZE;					       \
    *varr_ = va = (VARR(T) *) xmalloc(sizeof(VARR(T)));			       \
    if (va == NULL) varr_malloc_failure();				       \
    va->els_num = 0; va->size = size_;					       \
    va->varr = (T *) xmalloc(size_ * sizeof(T));			       \
    if (va->varr == NULL) varr_malloc_failure();			       \
}                                                                              \
                                                                               \
static inline void VARR_OP(T, destroy)(VARR(T) **varr_) {	               \
    VARR(T) *va = *varr_;						       \
    VARR_ASSERT(va && va->varr, "destroy", T);				       \
    free(va->varr);							       \
    free(va); 								       \
    *varr_ = NULL;							       \
}									       \
                                                                               \
static inline size_t VARR_OP(T, length)(const VARR(T) *varr_) {	               \
    VARR_ASSERT(varr_, "length", T);					       \
    return varr_->els_num;						       \
}                                                                              \
                                                                               \
static inline T *VARR_OP(T, addr)(const VARR(T) *varr_) {	               \
    VARR_ASSERT(varr_, "addr", T);					       \
    return &varr_->varr[0];						       \
}                                                                              \
                                                                               \
static inline T VARR_OP(T, last)(const VARR(T) *varr_) {                       \
    VARR_ASSERT(varr_ && varr_->varr && varr_->els_num, "last", T);	       \
    return varr_->varr[varr_->els_num - 1];				       \
}                                                                              \
                                                                               \
static inline T VARR_OP(T, get)(const VARR(T) *varr_, size_t ix_) { 	       \
    VARR_ASSERT(varr_ && varr_->varr && ix_ < varr_->els_num, "get", T);       \
    return varr_->varr[ix_];						       \
}                                                                              \
                                                                               \
static inline T VARR_OP(T, set)(const VARR(T) *varr_, size_t ix_, T obj_) {    \
    T old_obj_;								       \
    VARR_ASSERT(varr_ && varr_->varr && ix_ < varr_->els_num, "set", T);       \
    old_obj_ = varr_->varr[ix_];					       \
    varr_->varr[ix_] = obj_;						       \
    return old_obj_;							       \
}                                                                              \
                                                                               \
static inline void VARR_OP(T,trunc)(VARR(T) *varr_, size_t size_) {            \
    VARR_ASSERT(varr_ && varr_->varr && varr_->els_num >= size_, "trunc", T);  \
    varr_->els_num = size_;						       \
}									       \
                                                                               \
static inline void VARR_OP(T,expand)(VARR(T) *varr_, size_t size_) {	       \
    VARR_ASSERT(varr_  && varr_->varr, "expand", T);			       \
    if (varr_->size < size_) {						       \
	size_ += size_ / 2;						       \
	varr_->varr = (T *) xrealloc(varr_->varr, sizeof(T) * size_);	       \
        if (varr_->varr == NULL) varr_malloc_failure();			       \
	varr_->size = size_;						       \
    }                                                                          \
}									       \
									       \
static inline void VARR_OP(T, push)(VARR(T) *varr_, T obj_) {	               \
    T *slot_;								       \
    VARR_OP(T, expand)(varr_, varr_->els_num + 1);			       \
    slot_ = &varr_->varr[varr_->els_num++];				       \
    *slot_ = obj_;							       \
}                                                                              \
                                                                               \
static inline T VARR_OP(T, pop)(VARR(T) *varr_) {			       \
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

DEF_VARR(int);
/* The emulated VM stack: positions of insn producing stack slot values  */
static VARR(int) *stack;

DEF_VARR(char);
/* Always the same size as stack.  Flag of that input operands of the
   corresponding insn are changed and a combination of the insn and
   setlocal can not be issued on the setlocal place. */
static VARR(char) *operands_changed_p;

#define CONCAT(a, b) a ## b
static enum ruby_vminsn_type
get_rtl_insn2_id(enum ruby_vminsn_type id, int local2_p, int local3_p, VALUE val3) {
#define RTL_INSN_ID_CASE(insn_name, fname)				\
    case insn_name:							\
        if (local2_p && local3_p) return CONCAT(fname, svv);		\
        if (local2_p) {							\
            if (val3 != Qnil) return CONCAT(fname, svi);		\
            return CONCAT(fname, svs);					\
        }								\
        if (local3_p) return CONCAT(fname, ssv);			\
        if (val3 != Qnil) return CONCAT(fname, ssi);			\
        return BIN(nop);

    switch (id) {
    RTL_INSN_ID_CASE(BIN(opt_plus), BIN(opt_plus))
    RTL_INSN_ID_CASE(BIN(opt_minus), BIN(opt_minus))
    RTL_INSN_ID_CASE(BIN(opt_mult), BIN(opt_mult))
    RTL_INSN_ID_CASE(BIN(opt_div), BIN(opt_div))
    RTL_INSN_ID_CASE(BIN(opt_mod), BIN(opt_mod))
    RTL_INSN_ID_CASE(BIN(opt_eq), BIN(opt_eq))
    RTL_INSN_ID_CASE(BIN(opt_neq), BIN(opt_neq))
    RTL_INSN_ID_CASE(BIN(opt_lt), BIN(opt_lt))
    RTL_INSN_ID_CASE(BIN(opt_gt), BIN(opt_gt))
    RTL_INSN_ID_CASE(BIN(opt_le), BIN(opt_le))
    RTL_INSN_ID_CASE(BIN(opt_ge), BIN(opt_ge))
    default: return BIN(nop);
    }
}

static enum ruby_vminsn_type
get_rtl_spec_insn2_id(enum ruby_vminsn_type id, int attr) {
#define RTL_SPEC_INSN_ID_CASE(insn_name, fname)                      \
    case insn_name: return CONCAT(fname, sss);                  \
    case CONCAT(insn_name, svv): return CONCAT(fname, svv);	\
    case CONCAT(insn_name, svi): return CONCAT(fname, svi);	\
    case CONCAT(insn_name, svs): return CONCAT(fname, svs);	\
    case CONCAT(insn_name, ssv): return CONCAT(fname, ssv);	\
    case CONCAT(insn_name, ssi): return CONCAT(fname, ssi);

    if (attr == ATTR_FIXNUM) {
        switch (id) {
        RTL_SPEC_INSN_ID_CASE(BIN(opt_plus), BIN(opt_iplus))
        RTL_SPEC_INSN_ID_CASE(BIN(opt_minus), BIN(opt_iminus))
        RTL_SPEC_INSN_ID_CASE(BIN(opt_mult), BIN(opt_imult))
        RTL_SPEC_INSN_ID_CASE(BIN(opt_div), BIN(opt_idiv))
        RTL_SPEC_INSN_ID_CASE(BIN(opt_mod), BIN(opt_imod))
        RTL_SPEC_INSN_ID_CASE(BIN(opt_or), BIN(opt_ior))
        RTL_SPEC_INSN_ID_CASE(BIN(opt_and), BIN(opt_iand))
        RTL_SPEC_INSN_ID_CASE(BIN(opt_eq), BIN(opt_ieq))
        RTL_SPEC_INSN_ID_CASE(BIN(opt_neq), BIN(opt_ineq))
        RTL_SPEC_INSN_ID_CASE(BIN(opt_lt), BIN(opt_ilt))
        RTL_SPEC_INSN_ID_CASE(BIN(opt_gt), BIN(opt_igt))
        RTL_SPEC_INSN_ID_CASE(BIN(opt_le), BIN(opt_ile))
        RTL_SPEC_INSN_ID_CASE(BIN(opt_ge), BIN(opt_ige))
        RTL_SPEC_INSN_ID_CASE(BIN(opt_aref), BIN(opt_iaref))
        default: return BIN(nop);
        }
    } else if (attr == ATTR_FLONUM) {
        switch (id) {
#ifdef FLONUM_INSNS
        RTL_SPEC_INSN_ID_CASE(BIN(opt_plus), BIN(opt_fplus))
        RTL_SPEC_INSN_ID_CASE(BIN(opt_minus), BIN(opt_fminus))
        RTL_SPEC_INSN_ID_CASE(BIN(opt_mult), BIN(opt_fmult))
        RTL_SPEC_INSN_ID_CASE(BIN(opt_div), BIN(opt_fdiv))
        RTL_SPEC_INSN_ID_CASE(BIN(opt_mod), BIN(opt_fmod))
        RTL_SPEC_INSN_ID_CASE(BIN(opt_eq), BIN(opt_feq))
        RTL_SPEC_INSN_ID_CASE(BIN(opt_neq), BIN(opt_fneq))
        RTL_SPEC_INSN_ID_CASE(BIN(opt_lt), BIN(opt_flt))
        RTL_SPEC_INSN_ID_CASE(BIN(opt_gt), BIN(opt_fgt))
        RTL_SPEC_INSN_ID_CASE(BIN(opt_le), BIN(opt_fle))
        RTL_SPEC_INSN_ID_CASE(BIN(opt_ge), BIN(opt_fge))
#endif
        default: return BIN(nop);
        }
    }
    return BIN(nop);
}

static enum ruby_vminsn_type
get_rtl_insn3_id(enum ruby_vminsn_type id, int local_p, int local2_p, int local3_p, VALUE val2) {
#define RTL_INSN3_ID_CASE(insn_name, fname)				\
    case insn_name:							\
    if (local_p) {							\
      if (local2_p && local3_p) return CONCAT(fname, vvv);              \
      if (local3_p) {							\
        if (val2 == Qnil) return CONCAT(fname, vsv);                    \
        return CONCAT(fname, viv);					\
      }                                                                 \
      if (local2_p) return CONCAT(fname, vvs);                          \
      if (val2 != Qnil) return CONCAT(fname, vis);			\
    } else {								\
      if (local2_p && local3_p) return CONCAT(fname, svv);              \
      if (local3_p) {							\
        if (val2 == Qnil) return CONCAT(fname, ssv);                    \
        return CONCAT(fname, siv);					\
      }                                                                 \
      if (local2_p) return CONCAT(fname, svs);                          \
      if (val2 != Qnil) return CONCAT(fname, sis);			\
    }									\
    return BIN(nop);

    switch (id) {
    RTL_INSN3_ID_CASE(BIN(opt_aset), BIN(opt_aset))
    default: return BIN(nop);
    }
}


static enum ruby_vminsn_type
get_var_res_rtl_insn_id(enum ruby_vminsn_type id) {
#define VAR_RES_RTL_INSN_ID_CASE(insn_name)			\
    case CONCAT(insn_name, sss): return CONCAT(insn_name, vss);	\
    case CONCAT(insn_name, svv): return CONCAT(insn_name, vvv);	\
    case CONCAT(insn_name, svi): return CONCAT(insn_name, vvi);	\
    case CONCAT(insn_name, svs): return CONCAT(insn_name, vvs);	\
    case CONCAT(insn_name, ssv): return CONCAT(insn_name, vsv);	\
    case CONCAT(insn_name, ssi): return CONCAT(insn_name, vsi);

    switch (id) {
    VAR_RES_RTL_INSN_ID_CASE(BIN(opt_iplus))
    VAR_RES_RTL_INSN_ID_CASE(BIN(opt_iminus))
    VAR_RES_RTL_INSN_ID_CASE(BIN(opt_imult))
    VAR_RES_RTL_INSN_ID_CASE(BIN(opt_idiv))
    VAR_RES_RTL_INSN_ID_CASE(BIN(opt_imod))
    VAR_RES_RTL_INSN_ID_CASE(BIN(opt_ior))
    VAR_RES_RTL_INSN_ID_CASE(BIN(opt_iand))
    VAR_RES_RTL_INSN_ID_CASE(BIN(opt_ieq))
    VAR_RES_RTL_INSN_ID_CASE(BIN(opt_ineq))
    VAR_RES_RTL_INSN_ID_CASE(BIN(opt_ilt))
    VAR_RES_RTL_INSN_ID_CASE(BIN(opt_igt))
    VAR_RES_RTL_INSN_ID_CASE(BIN(opt_ile))
    VAR_RES_RTL_INSN_ID_CASE(BIN(opt_ige))
#ifdef FLONUM_INSNS
    VAR_RES_RTL_INSN_ID_CASE(BIN(opt_fplus))
    VAR_RES_RTL_INSN_ID_CASE(BIN(opt_fminus))
    VAR_RES_RTL_INSN_ID_CASE(BIN(opt_fmult))
    VAR_RES_RTL_INSN_ID_CASE(BIN(opt_fdiv))
    VAR_RES_RTL_INSN_ID_CASE(BIN(opt_fmod))
    VAR_RES_RTL_INSN_ID_CASE(BIN(opt_feq))
    VAR_RES_RTL_INSN_ID_CASE(BIN(opt_fneq))
    VAR_RES_RTL_INSN_ID_CASE(BIN(opt_flt))
    VAR_RES_RTL_INSN_ID_CASE(BIN(opt_fgt))
    VAR_RES_RTL_INSN_ID_CASE(BIN(opt_fle))
    VAR_RES_RTL_INSN_ID_CASE(BIN(opt_fge))
#endif
    default: return BIN(nop);
    }
}

static enum ruby_vminsn_type
get_branch_rtl_insn_id(enum ruby_vminsn_type cmp_id) {
#define BRANCH_RTL_INSN_ID_CASE(cmp_name, br_name)		\
    case CONCAT(cmp_name, sss): return CONCAT (br_name, ss);	\
    case CONCAT(cmp_name, svv): return CONCAT (br_name, vv);	\
    case CONCAT(cmp_name, svi): return CONCAT (br_name, vi);	\
    case CONCAT(cmp_name, svs): return CONCAT (br_name, vs);	\
    case CONCAT(cmp_name, ssv): return CONCAT (br_name, sv);	\
    case CONCAT(cmp_name, ssi): return CONCAT (br_name, si);

    switch (cmp_id) {
    BRANCH_RTL_INSN_ID_CASE(BIN(opt_ieq), BIN(opt_ibeq))
    BRANCH_RTL_INSN_ID_CASE(BIN(opt_ineq), BIN(opt_ibneq))
    BRANCH_RTL_INSN_ID_CASE(BIN(opt_ilt), BIN(opt_iblt))
    BRANCH_RTL_INSN_ID_CASE(BIN(opt_igt), BIN(opt_ibgt))
    BRANCH_RTL_INSN_ID_CASE(BIN(opt_ile), BIN(opt_ible))
    BRANCH_RTL_INSN_ID_CASE(BIN(opt_ige), BIN(opt_ibge))
#ifdef FLONUM_INSNS
    BRANCH_RTL_INSN_ID_CASE(BIN(opt_feq), BIN(opt_fbeq))
    BRANCH_RTL_INSN_ID_CASE(BIN(opt_fneq), BIN(opt_fbneq))
    BRANCH_RTL_INSN_ID_CASE(BIN(opt_flt), BIN(opt_fblt))
    BRANCH_RTL_INSN_ID_CASE(BIN(opt_fgt), BIN(opt_fbgt))
    BRANCH_RTL_INSN_ID_CASE(BIN(opt_fle), BIN(opt_fble))
    BRANCH_RTL_INSN_ID_CASE(BIN(opt_fge), BIN(opt_fbge))
#endif
    default: return BIN(nop);
    }
}

static int
get_temp_num(rb_iseq_t *iseq, int n) { /* n is 0, 1, ... -- 0 corresponds stack bottom */
    return n + iseq->body->local_table_size;
}

static int
get_lvar_num(rb_iseq_t *iseq, int idx) {
    /* make 1st arg having zero number: */
    int res = (int)iseq->body->local_table_size - (idx - VM_ENV_DATA_SIZE) - 1;
    RTL_GEN_ASSERT(0 <= res && res < (int)iseq->body->local_table_size);
    return res;
}

static int
get_lvar_index(rb_iseq_t *iseq, int num) {
    /* 1st arg having zero number: */
    RTL_GEN_ASSERT(0 <= num && num < (int)iseq->body->local_table_size);
    return (int)iseq->body->local_table_size - (num - VM_ENV_DATA_SIZE) - 1;
}

static VARR(char) *attrs;
static VARR(char) *arg_attrs;

static void
check_final_attrs(int n_attrs, char *attrs) {
    static_assert(ATTR_END_MARKER == 0); /* to use strcmp/strlen functions */
    RTL_GEN_ASSERT(attrs == NULL
                   || (n_attrs > 0 && attrs[n_attrs - 1] == ATTR_END_MARKER
                       && (n_attrs < 2 || attrs[n_attrs - 2] != ATTR_UNKNOWN) /* compact attrs */
                       && (int)strlen(attrs) + 1 == n_attrs)); /* no end marker in the middle */
}

static void
compact_attrs(VARR(char) *attributes) { /* Truncate attrs and add the end marker */
    int i, len = (int)VARR_LENGTH(char, attributes);

    if (len != 0) {
        if (VARR_LAST(char, attributes) == ATTR_END_MARKER)
            len--;
        for (i = len - 1; i >= 0; i--) {
            RTL_GEN_ASSERT(VARR_GET(char, attributes, i) != ATTR_END_MARKER);
            if (VARR_GET(char, attributes, i) != ATTR_UNKNOWN)
                break;
        }
        VARR_TRUNC(char, attributes, i + 1);
    }
    VARR_PUSH(char, attributes, ATTR_END_MARKER);
    check_final_attrs((int)VARR_LENGTH(char, attributes), VARR_ADDR(char, attributes));
}

static void
copy_attrs(const char *from) {
    int i;

    VARR_TRUNC(char, attrs, 0);
    for (i = 0; from[i] != ATTR_END_MARKER; i++)
        VARR_PUSH(char, attrs, from[i]);
}

static void
truncate_attrs(int len) {
    RTL_GEN_ASSERT(len >= 0);
    if ((int)VARR_LENGTH(char, attrs) > len)
        VARR_TRUNC(char, attrs, len);
}

static enum rb_attr_val
get_attr(int n_attr) {
    return n_attr >= (int)VARR_LENGTH(char, attrs) ? ATTR_UNKNOWN : VARR_GET(char, attrs, n_attr);
}

static void
setup_attr(int n_attr, int val) {
    while (n_attr >= (int)VARR_LENGTH(char, attrs))
        VARR_PUSH(char, attrs, ATTR_UNKNOWN);
    VARR_SET(char, attrs, n_attr, val);
}

static char *
get_call_attrs(rb_iseq_t *iseq, int argc, int stack_size) {
    int i, last_i, start;
    char *call_attrs;

    if (argc == 0) return NULL;
    RTL_GEN_ASSERT(stack_size >= argc);
    start = get_temp_num(iseq, stack_size - argc);
    last_i = -1;
    for (i = 0; i < argc && i + start < (int)VARR_LENGTH(char, attrs); i++)
        if (VARR_GET(char, attrs, i + start) != ATTR_UNKNOWN)
            last_i = i;
    if (last_i < 0) return NULL;
    call_attrs = ALLOC_N(char, last_i + 2);
    for (i = 0; i <= last_i; i++)
        call_attrs[i] = VARR_GET(char, attrs, i + start);
    call_attrs[last_i + 1] = ATTR_END_MARKER;
    return call_attrs;
}

static void
print_attrs(const char *attrs) {
    int i;

    if (attrs != NULL && attrs[0] != ATTR_END_MARKER) {
        fprintf (stderr, " attrs =");
        for (i = 0; attrs[i] != ATTR_END_MARKER; i++)
            if (attrs[i] != ATTR_UNKNOWN)
                fprintf (stderr, " %d:%s", i, attrs[i] == ATTR_FIXNUM ? "fix" : "flo");
    }
    fprintf(stderr, "\n");
}

typedef struct rb_iseq_bbv *rb_iseq_bbv_t;

static int
get_bbv_nums(rb_iseq_bbv_t head) {
    int n = 0;
    rb_iseq_bbv_t curr;

    RTL_GEN_ASSERT(head->attrs[0] == ATTR_END_MARKER);
    for (curr = head; curr != NULL; curr = curr->next_bbv)
        n++;
    return n;
}

static int
compare_bbv_with_keys(rb_iseq_bbv_t bbv, enum rb_bbv_type type, int index, int start) {
    if (bbv->iseq_start < start) return -1;
    if (bbv->iseq_start > start) return 1;
    if (bbv->type < type) return -1;
    if (bbv->type > type) return 1;
    if (bbv->index < index) return -1;
    if (bbv->index > index) return 1;
    return 0;
}

static rb_iseq_bbv_t
get_bbv_list_head(rb_iseq_t *iseq, enum rb_bbv_type type, int index, int start) {
    struct rb_iseq_constant_body *body = iseq->body;
    int cmp, left, right, middle;

    for (left = 0, right = body->n_bbvs - 1; left <= right; ) {
        middle = (left + right) / 2;
        if ((cmp = compare_bbv_with_keys(body->bbvs[middle], type, index, start)) < 0)
            left = middle + 1;
        else if (cmp > 0)
            right = middle - 1;
        else
            return body->bbvs[middle];
    }
    RTL_GEN_ASSERT(FALSE);
    return NULL;
}

static rb_iseq_bbv_t
find_bbv(rb_iseq_bbv_t head, int n_attrs, char *attrs) {
    rb_iseq_bbv_t curr;

    check_final_attrs(n_attrs, attrs);
    for (curr = head->next_bbv; curr != NULL; curr = curr->next_bbv) {
        if ((curr->attrs[0] == ATTR_END_MARKER && attrs == NULL)
            || (attrs != NULL && strcmp(curr->attrs, attrs) == 0))
            return curr;
    }
    return NULL;
}

static int
compare_bbvs(const void *b1, const void *b2) {
    rb_iseq_bbv_t bbv1 = *(rb_iseq_bbv_t *)b1, bbv2 = *(rb_iseq_bbv_t *)b2;
    return compare_bbv_with_keys(bbv1, bbv2->type, bbv2->index, bbv2->iseq_start);
}

static rb_iseq_bbv_t
create_bbv(rb_iseq_t *iseq, enum rb_bbv_type type, int index, int start, int bound, int n_attrs, char *attrs) {
    rb_iseq_bbv_t bbv;

    check_final_attrs(n_attrs, attrs);
    bbv = (rb_iseq_bbv_t)ALLOC_N(char, sizeof (struct rb_iseq_bbv) + (attrs == NULL ? 0 : n_attrs - 1));
    bbv->iseq = iseq;
    bbv->iseq_start = start;
    bbv->iseq_bound = bound;
    bbv->type = type;
    bbv->index = index;
    bbv->next_bbv = NULL;
    bbv->head_bbv = NULL;
    bbv->bbv_start = -1; /* flag of the code is not generated */
    bbv->start_stack_size = -1;
    bbv->in_edges = bbv->out_edges = NULL;
    if (attrs == NULL) {
        bbv->attrs[0] = ATTR_END_MARKER;
    } else {
        memcpy(bbv->attrs, attrs, n_attrs);
    }
    return bbv;
}

static rb_iseq_bbv_t
add_new_bbv(rb_iseq_t *iseq, rb_iseq_bbv_t head, int start, int bound, int n_attrs, char *attrs) {
    rb_iseq_bbv_t bbv = create_bbv(iseq, head->type, head->index, start, bound, n_attrs, attrs);

    bbv->next_bbv = head->next_bbv;
    head->next_bbv = bbv;
    bbv->head_bbv = head;
    return bbv;
}

#define MAX_BBV_NUM 10

static rb_iseq_bbv_t
get_bbv(rb_iseq_t *iseq, enum rb_bbv_type type, int index, int start, int n_attrs, char *attrs) {
    rb_iseq_bbv_t bbv, head_bbv = get_bbv_list_head(iseq, type, index, start);

    if ((bbv = find_bbv(head_bbv, n_attrs, attrs)) != NULL) return bbv;
    if (get_bbv_nums(head_bbv) >= MAX_BBV_NUM) return head_bbv;
    bbv = add_new_bbv(iseq, head_bbv, start, head_bbv->iseq_bound, n_attrs, attrs);
    return bbv;
}

static rb_iseq_bbv_t
get_bbv_by_head(rb_iseq_t *iseq, rb_iseq_bbv_t head, int n_attrs, char *attrs) {
    rb_iseq_bbv_t bbv;

    RTL_GEN_ASSERT(head != NULL && head->head_bbv == NULL);
    if ((bbv = find_bbv(head, n_attrs, attrs)) != NULL) return bbv;
    if (get_bbv_nums(head) >= MAX_BBV_NUM) return head;
    bbv = add_new_bbv(iseq, head, head->iseq_start, head->iseq_bound, n_attrs, attrs);
    return bbv;
}


static int
non_rtl_bb_end_p(enum ruby_vminsn_type insn_id) {
    switch (insn_id) {
    case BIN(jump):
    case BIN(branchif):
    case BIN(branchunless):
    case BIN(branchnil):
    case BIN(opt_getinlinecache):
    case BIN(leave):
    case BIN(throw):
        return TRUE;
    case BIN(opt_case_dispatch):
        return TRUE;
    default:
        return FALSE;
    }
}

static void
add_bbv_edge(rb_iseq_bbv_t in, rb_iseq_bbv_t out, int gen_bbv_disp) {
    struct rb_iseq_bbv_edge *e = (struct rb_iseq_bbv_edge *)ALLOC_N(struct rb_iseq_bbv_edge, 1);

    e->in = in;
    e->out = out;
    e->gen_bbv_disp = gen_bbv_disp;
    e->next_in_edge = out->in_edges;
    out->in_edges = e;
    e->next_out_edge = in->out_edges;
    in->out_edges = e;
}

static int
get_bbv_code_space(rb_iseq_t *iseq, int len) {
    struct rb_iseq_constant_body *body = iseq->body;
    const rb_execution_context_t *ec;
    rb_control_frame_t *cfp;
    VALUE *old_bbv_code;
    int add_len, bbv_start = body->bbv_code_free;

    if (bbv_start + len > body->bbv_code_bound) {
        add_len = body->bbv_code == NULL ? (int)body->iseq_size : len;
        body->bbv_code_bound = (bbv_start + add_len) * 30 / 2;
        old_bbv_code = body->bbv_code;
        RB_REALLOC_N(body->bbv_code, VALUE, body->bbv_code_bound);
        RB_REALLOC_N(body->bbv_pos_map, int, body->bbv_code_bound);
        if (old_bbv_code != NULL && body->bbv_code != old_bbv_code) {
            abort();
            fprintf(stderr, "==> Repositioning bbv_code\n");
            ec = GET_EC();
            for (cfp = ec->cfp + 1; (void *)cfp < (void *)(ec->vm_stack + ec->vm_stack_size); cfp++) {
                if (cfp->iseq == iseq)
                    cfp->pc += body->bbv_code - old_bbv_code;
            }
        }
    }
    body->bbv_code_free += len;
    return bbv_start;
}

DEF_VARR(VALUE);
static VARR(VALUE) *bbv_encoded;

static VARR(int) *pos_map;

static void
update_pos_map(int pos, int orig_pos) {
    while ((int)VARR_LENGTH(int, pos_map) <= pos)
        VARR_PUSH(int, pos_map, orig_pos < 0 ? 0 : -1);
    VARR_SET(int, pos_map, pos, orig_pos);
}

static void
print_pos_map(rb_iseq_t *iseq, int start, int bound, int spec_p) {
    int i, pos, n;

    fprintf (stderr, "## BBV position in %s:\n", spec_p ? "bbv_code" : "iseq_encoded");
    for (i = start, n = 0; i < bound; i++) {
        pos = iseq->body->bbv_pos_map[i];
        if (spec_p)
            pos = -pos - 1;
        if (pos < 0) continue;
        fprintf(stderr, n != 0 ? "," : "    ");
        fprintf(stderr, " %4d->%4d", i, pos);
        n++;
        if (n == 8) {
            fprintf(stderr, "\n");
            n = 0;
        }
    }
    if (n != 0) fprintf(stderr, "\n");
}

static int
add_insn(int orig_pos, enum ruby_vminsn_type insn_id, int nops, ...) {
    int start, i;
    va_list va;

    va_start (va, nops);
    start = (int)VARR_LENGTH(VALUE, bbv_encoded);
    VARR_PUSH(VALUE, bbv_encoded, (VALUE)insn_id);
    for (i = 0; i < nops; i++) {
        VARR_PUSH(VALUE, bbv_encoded, va_arg(va, VALUE));
    }
    va_end (va);
    update_pos_map(start, orig_pos);
    return start;
}

static int
add_insn_arr(int orig_pos, enum ruby_vminsn_type insn_id, int nops, VALUE *operands) {
    int start, i;

    start = (int)VARR_LENGTH(VALUE, bbv_encoded);
    VARR_PUSH(VALUE, bbv_encoded, (VALUE)insn_id);
    for (i = 0; i < nops; i++) {
        VARR_PUSH(VALUE, bbv_encoded, operands[i]);
    }
    update_pos_map(start, orig_pos);
    return start;
}

DEF_VARR(rb_iseq_bbv_t);
static VARR(rb_iseq_bbv_t) *bbvs;

static VARR(char) *labeled_p;

static void
update_labeled(int pos) {
    while ((int)VARR_LENGTH(char, labeled_p) <= pos)
        VARR_PUSH(char, labeled_p, FALSE);
    VARR_SET(char, labeled_p, pos, TRUE);
}

static int
mark_case_hash_el(VALUE key, VALUE val, VALUE ptr)
{
    int *pos = (int *)ptr;

    *pos += (OFFSET)val;
    RTL_GEN_ASSERT(*pos >= 0);
    update_labeled(*pos);
    return ST_CONTINUE;
}

static const void * const *dispatch_table;

static void
bbv_encode(VALUE *bbv_encoded, int len) {
    int i;
    enum ruby_vminsn_type insn_id;

    for (i = 0; i < len; i += insn_len(insn_id)) {
	insn_id = (enum ruby_vminsn_type)bbv_encoded[i];
        bbv_encoded[i] = (VALUE)dispatch_table[insn_id];
    }
}

VALUE rb_insn_operand_intern(const rb_iseq_t *iseq,
			     VALUE insn, int op_no, VALUE op,
			     int len, size_t pos, VALUE *pnop, VALUE child);

static void
print_insn(const rb_iseq_t *iseq, int insn_id, VALUE *code, size_t pos, const char *prefix)
{
    int len = insn_len(insn_id);
    int i;
    const char *types = insn_op_types(insn_id);
    VALUE str = rb_str_new(0, 0);
    const char *insn_name_buff;

    insn_name_buff = insn_name(insn_id);
    extern const int rb_vm_max_insn_name_size;
    rb_str_catf(str, "%04"PRIuSIZE" %-*s ", pos, rb_vm_max_insn_name_size, insn_name_buff);

    for (i = 0; types[i]; i++) {
	VALUE opstr = rb_insn_operand_intern(iseq, insn_id, i, code[pos + i + 1],
					     len, pos, &code[pos + i + 2],
					     0);
	rb_str_concat(str, opstr);

	if (types[i + 1]) {
	    rb_str_cat2(str, ", ");
	}
    }
    fprintf(stderr, "%s%.*s\n", prefix, (int)RSTRING_LEN(str), RSTRING_PTR(str));
}

static void
print_bb(rb_iseq_t *iseq, VALUE *code, int start, int bound, const char *prefix) {
    enum ruby_vminsn_type insn_id;
    int i, len;

    for (i = start;;) {
	insn_id = rb_vm_insn_decode((VALUE)code[i]);
        if (insn_id == BIN(gen_rtl_bbv) || insn_id == BIN(gen_rtl_bbv2)) {
            rb_iseq_bbv_t bbv = (rb_iseq_bbv_t)code[i + 1];
            fprintf(stderr, "%s  # %04d: %s(BBV 0x%llx)", prefix,
                    i, insn_name(insn_id), (unsigned long long) bbv);
            print_attrs(bbv->attrs);
        }
        print_insn(iseq, insn_id, code, i, prefix);
        len = insn_len(insn_id);
        if (longer_paths_p) {
            if (follow_jumps_p && insn_id == BIN(jump) && (long)code[i + 1] > 0)
                i = i + (long)code[i + 1];
            else if (bound < 0 && non_rtl_bb_end_p(insn_id))
                break;
        }
        i += len;
        if (bound >= 0 && i >= bound) break;
    }
}

static void
print_iseq_title(rb_iseq_t *iseq) {
    const rb_code_location_t *loc = &iseq->body->location.code_location;
    fprintf(stderr, "#<ISeq %s@%s:%d (%d,%d)-(%d,%d)>",
            RSTRING_PTR(iseq->body->location.label), RSTRING_PTR(rb_iseq_path(iseq)), loc->beg_pos.lineno,
            loc->beg_pos.lineno, loc->beg_pos.column, loc->end_pos.lineno, loc->end_pos.column);
}

void
rb_rtl_stub_gen(rb_iseq_t *iseq) {
    int i, curr_bb_start, pos, len, make_bbv_area_start;
    enum ruby_vminsn_type insn_id;
    struct rb_iseq_constant_body *body = iseq->body;
    VALUE *encoded = (VALUE *)body->iseq_encoded;
    rb_iseq_bbv_t bbv;

    VARR_TRUNC(rb_iseq_bbv_t, bbvs, 0);
    VARR_TRUNC(char, labeled_p, 0);
    update_labeled(0);
    for (i = 0; i < (int)body->iseq_size; i += len) { /* mark labels and positions after branches */
        insn_id = rb_vm_insn_addr2insn((void *)encoded[i]);
        len = insn_len(insn_id);
        switch (insn_id) {
        case BIN(jump):
        case BIN(branchif):
        case BIN(branchunless):
        case BIN(branchnil):
        case BIN(opt_getinlinecache):
            update_labeled(i + len);
            pos = i + len + (OFFSET)encoded[i + 1]; /* dst */
            RTL_GEN_ASSERT(pos >= 0);
            update_labeled(pos);
            break;
        case BIN(opt_case_dispatch):
            update_labeled(i + len);
            pos = i + len + (OFFSET)encoded[i + 2]; /* else_dst */
            RTL_GEN_ASSERT(pos >= 0);
            update_labeled(pos);
            pos = i + len;
            rb_hash_foreach(encoded[i + 1], mark_case_hash_el, (VALUE)&pos);
            break;
        default:
            break;
        }
    }
    if (body->param.opt_table != NULL) {
        for (i = 0; i <= body->param.opt_num; i++) { /* mark optional parameter pc */
            update_labeled(body->param.opt_table[i]);
        }
    }
    if (body->catch_table != NULL) {
        for (i = 0; i < (int)body->catch_table->size; i++) { /* mark exception continuation pc */
            update_labeled(body->catch_table->entries[i].cont);
        }
    }
    if (longer_paths_p) { /* create bbv at the **start** of the BB */
        for (i = 0; i < (int)body->iseq_size; i += insn_len(insn_id)) {
            insn_id = rb_vm_insn_addr2insn((void *)encoded[i]);
            if (i < (int)VARR_LENGTH(char, labeled_p) && VARR_GET(char, labeled_p, i)) {
                bbv = create_bbv(iseq, RB_BBV_REGULAR, 0, i, -1, 0, NULL);
                VARR_PUSH (rb_iseq_bbv_t, bbvs, bbv);
            }
        }
    } else { /* create bbv at the **end** of the BB */
        curr_bb_start = 0;
        for (i = 0; i < (int)body->iseq_size; i += insn_len(insn_id)) {
            insn_id = rb_vm_insn_addr2insn((void *)encoded[i]);
            if (i != 0 && i < (int)VARR_LENGTH(char, labeled_p) && VARR_GET(char, labeled_p, i)) {
                bbv = create_bbv(iseq, RB_BBV_REGULAR, 0, curr_bb_start, i, 0, NULL);
                VARR_PUSH (rb_iseq_bbv_t, bbvs, bbv);
                curr_bb_start = i;
            }
        }
        if (curr_bb_start != (int)body->iseq_size) {
            bbv = create_bbv(iseq, RB_BBV_REGULAR, 0, curr_bb_start, (int)body->iseq_size, 0, NULL);
            VARR_PUSH (rb_iseq_bbv_t, bbvs, bbv);
        }
    }
    /* Add make_rtl_xxx_bbv insns for all entry points and create bbvs for opt_table and catch_table: */
    VARR_TRUNC(VALUE, bbv_encoded, 0);
    VARR_TRUNC(int, pos_map, 0);
    add_insn(0, BIN(make_rtl_bbv), 1, 0);
    if (body->param.opt_num == 0) {
        body->curr_param_opt_table = NULL;
    } else {
        body->curr_param_opt_table = ALLOC_N(VALUE, body->param.opt_num + 1);
        for (i = 0; i <= body->param.opt_num; i++) { /* optional parameter pc */
            pos = body->param.opt_table[i];
            bbv = create_bbv(iseq, RB_BBV_OPT, i, pos, -1, 0, NULL);
            VARR_PUSH (rb_iseq_bbv_t, bbvs, bbv);
            body->curr_param_opt_table[i] = VARR_LENGTH(VALUE, bbv_encoded);
            add_insn(pos, BIN(make_rtl_optpc_bbv), 1, i);
        }
    }
    if (body->catch_table != NULL) {
        for (i = 0; i < (int)body->catch_table->size; i++) { /* exception continuation pc */
            pos = body->catch_table->entries[i].cont;
            bbv = create_bbv(iseq, RB_BBV_CATCH, i, pos, -1, 0, NULL);
            VARR_PUSH (rb_iseq_bbv_t, bbvs, bbv);
            body->catch_table->entries[i].curr_cont = VARR_LENGTH(VALUE, bbv_encoded);
            add_insn(pos, BIN(make_rtl_catch_bbv), 1, i);
        }
    }
    body->n_bbvs = VARR_LENGTH(rb_iseq_bbv_t, bbvs);
    body->bbvs = ALLOC_N(rb_iseq_bbv_t, body->n_bbvs);
    memcpy (body->bbvs, VARR_ADDR(rb_iseq_bbv_t, bbvs), body->n_bbvs * sizeof (rb_iseq_bbv_t));
    qsort(body->bbvs, body->n_bbvs, sizeof(rb_iseq_bbv_t), compare_bbvs);
    len = (int)VARR_LENGTH(VALUE, bbv_encoded);
    make_bbv_area_start = get_bbv_code_space(iseq, len);
    RTL_GEN_ASSERT(make_bbv_area_start == 0);
    memcpy(&body->bbv_code[make_bbv_area_start], VARR_ADDR(VALUE, bbv_encoded), len * sizeof(VALUE));
    memcpy(&body->bbv_pos_map[make_bbv_area_start],
           VARR_ADDR(int, pos_map), VARR_LENGTH(int, pos_map) * sizeof(int));
    body->iseq_code = body->iseq_start_pc = &body->bbv_code[make_bbv_area_start];
    bbv_encode(&body->bbv_code[make_bbv_area_start], len);
    if (rb_rtl_debug_p()) {
        fprintf(stderr, "## Make BBV area of ");
        print_iseq_title(iseq);
        fprintf(stderr, "\n");
        print_bb(iseq, body->bbv_code, make_bbv_area_start, make_bbv_area_start + len, "  ");
        print_pos_map(iseq, make_bbv_area_start, make_bbv_area_start + len, FALSE);
    }
}

static void
nullify(int pos) {
    int i, len = insn_len((enum ruby_vminsn_type)VARR_GET(VALUE, bbv_encoded, pos));
    for (i = 0; i < len; i++)
        VARR_SET(VALUE, bbv_encoded, pos + i, BIN(nop));
}

static int
remove_nops(int *nops_num) {
    int curr_ind, new_curr_ind, last_insn_p, nel, len, last_rtl_ind = 0;
    enum ruby_vminsn_type insn_id;

    *nops_num = 0;
    for (curr_ind = new_curr_ind = 0;
         curr_ind < (int)VARR_LENGTH(VALUE, bbv_encoded);
         curr_ind += len) { /* remove nops */
	insn_id = (enum ruby_vminsn_type)VARR_GET(VALUE, bbv_encoded, curr_ind);
        len = insn_len(insn_id);
        if (insn_id == BIN(nop)
            && (curr_ind >= (int)VARR_LENGTH(char, labeled_p)
                || !VARR_GET(char, labeled_p, curr_ind))) {
            (*nops_num)++;
            continue;
        }
        last_rtl_ind = new_curr_ind;
        if (curr_ind == new_curr_ind) {
            new_curr_ind += len;
            continue;
        }
        last_insn_p = curr_ind + len >= (int)VARR_LENGTH(VALUE, bbv_encoded);
        for (nel = 0; nel < len; nel++, new_curr_ind++) {
            VARR_SET(VALUE, bbv_encoded, new_curr_ind, VARR_GET(VALUE, bbv_encoded, curr_ind + nel));
            if (nel == 0 || !last_insn_p)
                VARR_SET(int, pos_map, new_curr_ind, VARR_GET(int, pos_map, curr_ind + nel));
        }
    }
    VARR_TRUNC(VALUE, bbv_encoded, new_curr_ind);
    VARR_TRUNC(int, pos_map, last_rtl_ind + 1);
    return last_rtl_ind;
}

static void
get_op(int op_insn_pos, int *local_p, VALUE *local, VALUE *val) {
    *local_p = FALSE;
    *local = Qnil;
    if (val != NULL) *val = Qnil;
    if (op_insn_pos >= 0) {
        enum ruby_vminsn_type op_insn_id = (enum ruby_vminsn_type)VARR_GET(VALUE, bbv_encoded, op_insn_pos);
        if ((op_insn_id == BIN(getlocal) && VARR_GET(VALUE, bbv_encoded, op_insn_pos + 2) == 0)
            || op_insn_id == BIN(getlocal_WC_0)) {
            *local_p = TRUE;
            *local = VARR_GET(VALUE, bbv_encoded, op_insn_pos + 1);
        } else if (val == NULL) {
        } else if (op_insn_id == BIN(putobject_INT2FIX_0_)) {
            *val = INT2FIX(0);
        } else if (op_insn_id == BIN(putobject_INT2FIX_1_)) {
            *val = INT2FIX(1);
        } else if (op_insn_id == BIN(putobject)) {
            *val = VARR_GET(VALUE, bbv_encoded, op_insn_pos + 1);
        }
    }
}

static int
pos_in_catch_table_p(rb_iseq_t *iseq, int pos) {
    int i;
    const struct rb_iseq_constant_body *const body = iseq->body;

    if (body->catch_table != NULL) {
        for (i = 0; i < (int)body->catch_table->size; i++) {
            const struct iseq_catch_table_entry *entry =
                UNALIGNED_MEMBER_PTR(body->catch_table, entries[i]);
            if (pos == (int)entry->start) return TRUE;
            if (pos == (int)entry->end) return TRUE;
            if (pos == (int)entry->cont) return TRUE;
        }
    }
    return FALSE;
}

static VARR(rb_iseq_bbv_t) *out_bbvs;
static VARR(int) *out_gen_bbv_disps;

static const struct st_hash_type cdhash_type = {
    rb_iseq_cdhash_cmp,
    rb_iseq_cdhash_hash,
};

struct cdhash_set_el {
    rb_iseq_t *iseq;
    int origin_case_start, case_start;
    VALUE map;
};

static int
set_case_hash_el(VALUE key, VALUE val, VALUE ptr)
{
    struct cdhash_set_el *data = (struct cdhash_set_el *)ptr;
    rb_iseq_t *iseq = data->iseq;
    int dst, disp, len = insn_len(BIN(opt_case_dispatch));
    rb_iseq_bbv_t dst_bbv;

    RTL_GEN_ASSERT(iseq->body->iseq_encoded[data->origin_case_start]
                   == (VALUE)dispatch_table[BIN(opt_case_dispatch)]);
    dst = data->origin_case_start + len + FIX2LONG(val);
    dst_bbv = get_bbv(iseq, RB_BBV_REGULAR, 0, dst, (int)VARR_LENGTH(char, attrs), VARR_ADDR(char, attrs));
    disp = add_insn(dst, BIN(gen_rtl_bbv_key), 3, (VALUE)dst_bbv, (VALUE)0, (VALUE) key);
    VARR_PUSH(rb_iseq_bbv_t, out_bbvs, dst_bbv);
    VARR_PUSH(int, out_gen_bbv_disps, disp);
    VARR_SET(VALUE, bbv_encoded, disp + 2, (VALUE)(data->case_start - disp - insn_len(BIN(gen_rtl_bbv_key))));
    rb_hash_aset(data->map, key, INT2FIX(disp - (data->case_start + len)));
    return ST_CONTINUE;
}

static int
contains_input_lvar_p(int insn_pos, int lvar) {
    if (insn_pos < 0) return FALSE;

    enum ruby_vminsn_type insn_id = (enum ruby_vminsn_type) VARR_GET(VALUE, bbv_encoded, insn_pos);
    VALUE *operands = VARR_ADDR(VALUE, bbv_encoded) + insn_pos + 1;
    RTL_GEN_ASSERT(lvar >= 0);

#define RTL_INSN_LVAR_CASE(insn_name)                                   			\
    case CONCAT(insn_name, svv):								\
    case CONCAT(insn_name, vvv): return operands[0] == (VALUE)lvar || operands[1] == (VALUE)lvar;\
    case CONCAT(insn_name, svi):								\
    case CONCAT(insn_name, vvi):								\
    case CONCAT(insn_name, svs):								\
    case CONCAT(insn_name, vvs): return operands[0] == (VALUE)lvar;				\
    case CONCAT(insn_name, ssv):								\
    case CONCAT(insn_name, vsv): return operands[1] == (VALUE)lvar;

#define RTL_INSN_LVAR_CASE0(insn_name)                            				\
    case CONCAT(insn_name, svv): return operands[0] == (VALUE)lvar || operands[1] == (VALUE)lvar;\
    case CONCAT(insn_name, svi):								\
    case CONCAT(insn_name, svs): return operands[0] == (VALUE)lvar;				\
    case CONCAT(insn_name, ssv): return operands[1] == (VALUE)lvar;

    switch (insn_id) {
    RTL_INSN_LVAR_CASE(BIN(opt_iplus))
    RTL_INSN_LVAR_CASE(BIN(opt_iminus))
    RTL_INSN_LVAR_CASE(BIN(opt_imult))
    RTL_INSN_LVAR_CASE(BIN(opt_idiv))
    RTL_INSN_LVAR_CASE(BIN(opt_imod))
    RTL_INSN_LVAR_CASE(BIN(opt_ior))
    RTL_INSN_LVAR_CASE(BIN(opt_iand))
    RTL_INSN_LVAR_CASE(BIN(opt_ieq))
    RTL_INSN_LVAR_CASE(BIN(opt_ineq))
    RTL_INSN_LVAR_CASE(BIN(opt_ilt))
    RTL_INSN_LVAR_CASE(BIN(opt_igt))
    RTL_INSN_LVAR_CASE(BIN(opt_ile))
    RTL_INSN_LVAR_CASE(BIN(opt_ige))
#ifdef FLONUM_INSNS
    RTL_INSN_LVAR_CASE(BIN(opt_fplus))
    RTL_INSN_LVAR_CASE(BIN(opt_fminus))
    RTL_INSN_LVAR_CASE(BIN(opt_fmult))
    RTL_INSN_LVAR_CASE(BIN(opt_fdiv))
    RTL_INSN_LVAR_CASE(BIN(opt_fmod))
    RTL_INSN_LVAR_CASE(BIN(opt_feq))
    RTL_INSN_LVAR_CASE(BIN(opt_fneq))
    RTL_INSN_LVAR_CASE(BIN(opt_flt))
    RTL_INSN_LVAR_CASE(BIN(opt_fgt))
    RTL_INSN_LVAR_CASE(BIN(opt_fle))
    RTL_INSN_LVAR_CASE(BIN(opt_fge))
#endif
    RTL_INSN_LVAR_CASE0(BIN(opt_plus))
    RTL_INSN_LVAR_CASE0(BIN(opt_minus))
    RTL_INSN_LVAR_CASE0(BIN(opt_mult))
    RTL_INSN_LVAR_CASE0(BIN(opt_div))
    RTL_INSN_LVAR_CASE0(BIN(opt_mod))
    RTL_INSN_LVAR_CASE0(BIN(opt_or))
    RTL_INSN_LVAR_CASE0(BIN(opt_and))
    RTL_INSN_LVAR_CASE0(BIN(opt_eq))
    RTL_INSN_LVAR_CASE0(BIN(opt_neq))
    RTL_INSN_LVAR_CASE0(BIN(opt_lt))
    RTL_INSN_LVAR_CASE0(BIN(opt_gt))
    RTL_INSN_LVAR_CASE0(BIN(opt_le))
    RTL_INSN_LVAR_CASE0(BIN(opt_ge))
    case BIN (getlocal):
        if (operands[1] != 0) return FALSE; /*op1==level*/
        /* fallthrough */
    case BIN(getlocal_WC_0):
        return (VALUE)lvar == operands[0];
    default: return FALSE;
    }
}

#define RTL_INSN2_IMM_CASES(name)               \
        case CONCAT(name, svi):                 \
        case CONCAT(name, ssi)

#define RTL_INSN2_NON_IMM_CASES(name)           \
        case name:                          	\
        case CONCAT(name, svv):                 \
        case CONCAT(name, svs):                 \
        case CONCAT(name, ssv)

#define RTL_INSN2_CASES(name)                   \
    RTL_INSN2_IMM_CASES(name):                  \
    RTL_INSN2_NON_IMM_CASES(name)

/* Generate non specialized (or non-speculative) RTL code for BBV
   which should be the head BBV (sometimes called the default BBV):
   Move producers to setlocals and to branch insns to make specialized
   code for other BBVs later. */
static void
gen_rtl_from_stack_insns(rb_iseq_t *iseq, rb_iseq_bbv_t bbv) {
    enum ruby_vminsn_type insn_id = BIN(nop);
    enum ruby_vminsn_type new_insn_id, op_insn_id;
    int init_stack_size = bbv->start_stack_size;
    int i, n, curr_orig_insn_pos, len, nops, inc, retn, br_p, dst2_p, stack_size;
    int op_insn_pos, op3_insn_pos, op2_insn_pos, local_p, local2_p, local3_p;
    int last_original_rtl_ind, last_rtl_ind, nops_num;
    int debug_p = rb_rtl_debug_p();
    int disp = 0, disp2 = 0, else_disp = 0; /* for removing uninitialized warning */
    OFFSET dst = 0, dst2 = 0, else_dst = 0; /* for removing uninitialized warning */
    VALUE val, val2, val3, local, local2, local3, *operands, new_operands[10];
    int offset, offset2;
    struct rb_iseq_constant_body *body = iseq->body;
    rb_iseq_bbv_t dst_bbv, else_dst_bbv;

    RTL_GEN_ASSERT(bbv->head_bbv == NULL && bbv->attrs[0] == ATTR_END_MARKER && init_stack_size >= 0);
    if (debug_p) {
        fprintf(stderr, "\n## Generation of General RTL BBV 0x%llx:", (unsigned long long) bbv);
        print_attrs(bbv->attrs);
        fprintf(stderr, "Original BB of ");
        print_iseq_title(iseq);
        fprintf(stderr, "\n");
        print_bb(iseq, body->iseq_encoded, bbv->iseq_start, bbv->iseq_bound, "  ");
    }
    RTL_GEN_ASSERT(bbv->bbv_start < 0);
    br_p = dst2_p = FALSE;
    disp = disp2 = dst = dst2 = 0;
    VARR_TRUNC(VALUE, bbv_encoded, 0);
    VARR_TRUNC(char, labeled_p, 0);
    VARR_TRUNC(int, stack, 0);
    VARR_TRUNC(char, operands_changed_p, 0);
    VARR_TRUNC(int, pos_map, 0);
    VARR_TRUNC(rb_iseq_bbv_t, out_bbvs, 0);
    VARR_TRUNC(int, out_gen_bbv_disps, 0);
    while (init_stack_size > (int)VARR_LENGTH(int, stack)) {
        VARR_PUSH(int, stack, -1); /* stack temp definitions are out of block */
        VARR_PUSH(char, operands_changed_p, FALSE);
    }
    RTL_GEN_ASSERT(bbv->iseq_bound < 0 || bbv->iseq_start < bbv->iseq_bound);
    for (curr_orig_insn_pos = bbv->iseq_start;;) {
        last_rtl_ind = (int)VARR_LENGTH(VALUE, bbv_encoded);
	insn_id = rb_vm_insn_addr2insn((void *)body->iseq_encoded[curr_orig_insn_pos]);
        len = insn_len(insn_id);
        operands = &body->iseq_encoded[curr_orig_insn_pos + 1];
        switch (insn_id) {
        case BIN(setn):
            stack_size = VARR_LENGTH(int, stack);
            n = (int) operands[0];
            RTL_GEN_ASSERT(stack_size > n);
            for (i = 0; i <= n; i++) /* we can not use stack above n w/o change setn arg */
                VARR_SET(int, stack, stack_size - i - 1, -1); /* flag of unknown insn */
            add_insn_arr(curr_orig_insn_pos, insn_id, len - 1, operands);
            break;
        case BIN(dup):
            n = 1;
            goto dup;
        case BIN(dupn): /* ??? optimize: when values are only putobject|getlocal */
            n = (int) operands[0];
        dup:
            stack_size = VARR_LENGTH(int, stack);
            RTL_GEN_ASSERT(stack_size >= n);
            for (i = 0; i < n; i++) {
                VARR_SET(int, stack, stack_size - 1 - i, -1); /* flag of unknown insn */
            }
            for (i = 0; i < n; i++) {
                VARR_PUSH(int, stack, -1); /* flag of unknown insn */
                VARR_PUSH(char, operands_changed_p, FALSE);
            }
            add_insn_arr(curr_orig_insn_pos, insn_id, len - 1, operands);
            break;
        case BIN(nop):
        case BIN(make_rtl_bbv): // ???
        case BIN(jump2): /* nops not labeled by catch table will be removed */
            if (pos_in_catch_table_p (iseq, curr_orig_insn_pos)) {
                last_rtl_ind = add_insn(curr_orig_insn_pos, BIN(nop), 0);
                update_labeled(last_rtl_ind);
            }
            break;
        case BIN(opt_eq):
        case BIN(opt_neq):
        case BIN(opt_lt):
        case BIN(opt_gt):
        case BIN(opt_le):
        case BIN(opt_ge):
        case BIN(opt_plus):
        case BIN(opt_minus):
        case BIN(opt_mult):
        case BIN(opt_div):
        case BIN(opt_mod):
        case BIN(opt_or):
        case BIN(opt_and):
        case BIN(opt_aref):
            stack_size = VARR_LENGTH(int, stack);
            RTL_GEN_ASSERT(stack_size >= 2);
            op3_insn_pos = VARR_LAST(int, stack);
            op2_insn_pos = VARR_GET(int, stack, stack_size - 2);
            get_op(op3_insn_pos, &local3_p, &local3, &val3);
            get_op(op2_insn_pos, &local2_p, &local2, NULL);
            new_insn_id = get_rtl_insn2_id(insn_id, local2_p, local3_p, val3);
            if (new_insn_id == BIN(nop)) goto rest_cases;
            nops = (local2_p ? 1 : 0) + (local3_p || val3 != Qnil ? 1 : 0);
            RTL_GEN_ASSERT(nops >= 1);
            if (local2_p) {
                new_operands[0] = local2;
                if (local3_p || val3 != Qnil)
                    new_operands[1] = local3_p ? local3 : val3;
            } else {
                RTL_GEN_ASSERT(local3_p || val3 != Qnil);
                new_operands[0] = local3_p ? local3 : val3;
            }
            if (nops + 2 == insn_len(new_insn_id)) { /* cd */
                new_operands[nops] = operands[0];
                nops++;
            } else { /* 2 cds */
                RTL_GEN_ASSERT(nops + 3 == insn_len(new_insn_id));
                new_operands[nops] = operands[0];
                new_operands[nops + 1] = operands[1];
                nops += 2;
            }
            last_rtl_ind = add_insn_arr(curr_orig_insn_pos, new_insn_id, nops, new_operands);
            if (local2_p)
                nullify(op2_insn_pos);
            if (local3_p || val3 != Qnil)
                nullify(op3_insn_pos);
            VARR_TRUNC(int, stack, stack_size - 2);
            VARR_TRUNC(char, operands_changed_p, stack_size - 2);
            goto push;
        case BIN(opt_aset):
            stack_size = VARR_LENGTH(int, stack);
            RTL_GEN_ASSERT(stack_size >= 3);
            if (bbv->iseq_bound >= 0 && curr_orig_insn_pos + len >= bbv->iseq_bound) goto rest_cases;
            op_insn_pos = VARR_GET(int, stack, stack_size - 3);
            op2_insn_pos = VARR_GET(int, stack, stack_size - 2);
            op3_insn_pos = VARR_GET(int, stack, stack_size - 1);
            get_op(op_insn_pos, &local_p, &local, NULL);
            get_op(op2_insn_pos, &local2_p, &local2, &val2);
            get_op(op3_insn_pos, &local3_p, &local3, NULL);
            new_insn_id = get_rtl_insn3_id(insn_id, local_p, local2_p, local3_p, val2);
            if (new_insn_id == BIN(nop)) goto rest_cases;
            nops = (local_p ? 1 : 0) + (local2_p || val2 != Qnil ? 1 : 0) + (local3_p ? 1 : 0);
            RTL_GEN_ASSERT(nops >= 1);
            if (local_p) {
                new_operands[0] = local;
                if (local2_p || val2 != Qnil) {
                    new_operands[1] = local2_p ? local2 : val2;
                    if (local3_p)
                        new_operands[2] = local3;
                } else if (local3_p) {
                    new_operands[1] = local3;
                }
            } else {
                if (local2_p || val2 != Qnil) {
                    new_operands[0] = local2_p ? local2 : val2;
                    if (local3_p)
                        new_operands[1] = local3;
                } else if (local3_p) {
                    new_operands[0] = local3;
                }
            }
            if (nops + 2 == insn_len (new_insn_id)) { /* cd */
                new_operands[nops] = operands[0];
                nops++;
            } else {
                RTL_GEN_ASSERT(nops + 1 == insn_len(new_insn_id));
            }
            last_rtl_ind = add_insn_arr(curr_orig_insn_pos, new_insn_id, nops, new_operands);
            if (local_p)
                nullify(op_insn_pos);
            if (local2_p || val2 != Qnil)
                nullify(op2_insn_pos);
            if (local3_p)
                nullify(op3_insn_pos);
            VARR_TRUNC(int, stack, stack_size - 2);
            VARR_TRUNC(char, operands_changed_p, stack_size - 2);
            goto push;
        case BIN(jump): // ??? use leaf attr for dst calculations
            br_p = TRUE;
            dst = curr_orig_insn_pos + len + operands[0];
            if (!longer_paths_p || !follow_jumps_p || (long)operands[0] <= 0)
                add_insn(curr_orig_insn_pos, BIN(jump2), 1, (VALUE)0);
            goto stack_update;
        case BIN(branchif):
        case BIN(branchunless):
            stack_size = (int)VARR_LENGTH(int, stack);
            if (stack_size != 0 && (op_insn_pos = VARR_LAST(int, stack)) >= 0) {
                /* put compare producer adjacent to the branch for specialized insns: */
                op_insn_id = (enum ruby_vminsn_type)VARR_GET(VALUE, bbv_encoded, op_insn_pos);
                switch (op_insn_id) {
                RTL_INSN2_CASES(BIN(opt_eq)):
                RTL_INSN2_CASES(BIN(opt_neq)):
                RTL_INSN2_CASES(BIN(opt_lt)):
                RTL_INSN2_CASES(BIN(opt_gt)):
                RTL_INSN2_CASES(BIN(opt_le)):
                RTL_INSN2_CASES(BIN(opt_ge)):
                    add_insn_arr(VARR_GET(int, pos_map, op_insn_pos), op_insn_id, (int)insn_len(op_insn_id) - 1,
                                 VARR_ADDR(VALUE, bbv_encoded) + op_insn_pos + 1);
                    nullify(op_insn_pos);
                    break;
                default: break;
                }
            }
            add_insn(curr_orig_insn_pos, BIN(branch2), 4, (VALUE)0, (VALUE)0, (VALUE)0, (VALUE)0);
            br_p = dst2_p = TRUE;
            if (insn_id == BIN(branchunless)) {
                dst2 = curr_orig_insn_pos + len + (OFFSET)operands[0];
                dst = curr_orig_insn_pos + len;
            } else {
                dst = curr_orig_insn_pos + len + (OFFSET)operands[0];
                dst2 = curr_orig_insn_pos + len;
            }
            goto stack_update;
        case BIN(branchnil):
            br_p = dst2_p = TRUE;
            dst = curr_orig_insn_pos + len + (OFFSET)operands[0];
            dst2 = curr_orig_insn_pos + len;
            add_insn(curr_orig_insn_pos, BIN(branchnil2), 2, (VALUE)0, (VALUE)0);
            goto stack_update;
        case BIN(opt_getinlinecache):
            br_p = dst2_p = TRUE;
            dst = curr_orig_insn_pos + len + (OFFSET)operands[0];
            dst2 = curr_orig_insn_pos + len;
            add_insn(curr_orig_insn_pos, BIN(opt_getinlinecache2), 3, (VALUE)0, (VALUE)0, operands[1]);
            goto stack_update;
        case BIN(setlocal):
            if (operands[1] != 0) goto rest_cases;
            /* fallthrough */
        case BIN(setlocal_WC_0):
            stack_size = (int)VARR_LENGTH(int, stack);
            RTL_GEN_ASSERT(stack_size > 0);
            if ((op_insn_pos = VARR_LAST(int, stack)) < 0) goto rest_cases;
            if (VARR_LAST(char, operands_changed_p)) goto rest_cases;
            op_insn_id = (enum ruby_vminsn_type) VARR_GET(VALUE, bbv_encoded, op_insn_pos);
            new_insn_id = get_var_res_rtl_insn_id(op_insn_id);
            if (new_insn_id == BIN(nop)) {
                switch (op_insn_id) {
                case BIN(putobject):
                    val = VARR_GET(VALUE, bbv_encoded, op_insn_pos + 1);
                    goto move_imm;
                case BIN(putobject_INT2FIX_0_):
                    val = INT2FIX(0);
                    goto move_imm;
                case BIN(putobject_INT2FIX_1_):
                    val = INT2FIX(1);
                move_imm:
                    add_insn(curr_orig_insn_pos, BIN(movi), 2, operands[0], val);
                    nullify(op_insn_pos);
                    goto change_setlocal;
                case BIN (getlocal):
                    if (VARR_GET(VALUE, bbv_encoded, op_insn_pos + 2) != 0) goto rest_cases; /*op1==level*/
                    /* fallthrough */
                case BIN(getlocal_WC_0):
                    add_insn(curr_orig_insn_pos, BIN(mov), 2, operands[0],
                             VARR_GET(VALUE, bbv_encoded, op_insn_pos + 1));
                    nullify(op_insn_pos);
                    goto change_setlocal;
                RTL_INSN2_CASES(BIN(opt_plus)):
                RTL_INSN2_CASES(BIN(opt_minus)):
                RTL_INSN2_CASES(BIN(opt_mult)):
                RTL_INSN2_CASES(BIN(opt_div)):
                RTL_INSN2_CASES(BIN(opt_mod)):
                RTL_INSN2_CASES(BIN(opt_or)):
                RTL_INSN2_CASES(BIN(opt_and)):
                RTL_INSN2_CASES(BIN(opt_aref)):
                RTL_INSN2_CASES(BIN(opt_eq)):
                RTL_INSN2_CASES(BIN(opt_neq)):
                RTL_INSN2_CASES(BIN(opt_lt)):
                RTL_INSN2_CASES(BIN(opt_gt)):
                RTL_INSN2_CASES(BIN(opt_le)):
                RTL_INSN2_CASES(BIN(opt_ge)):
                    /* put producer adjacent to setlocal */
                    add_insn_arr(VARR_GET(int, pos_map, op_insn_pos),
                                 op_insn_id, (int)insn_len(op_insn_id) - 1,
                                 VARR_ADDR(VALUE, bbv_encoded) + op_insn_pos + 1);
                    nullify(op_insn_pos);
                    break;
                default: break;
                }
            }
            add_insn_arr(curr_orig_insn_pos, insn_id, len - 1, operands);
        change_setlocal:
            for (i = 0; i < stack_size - 1; i++) {
                if (contains_input_lvar_p(VARR_GET(int, stack, i), operands[0]))
                    VARR_SET(char, operands_changed_p, i, TRUE);
            }
            goto stack_update;
        case BIN(putobject):
            val = operands[0];
            goto add_and_push;
        case BIN(putobject_INT2FIX_0_):
        case BIN(putobject_INT2FIX_1_):
            goto add_and_push;
        case BIN (getlocal):
            if (operands[1] != 0) goto rest_cases;
            /* fallthrough */
        case BIN(getlocal_WC_0):
        add_and_push:
            add_insn_arr(curr_orig_insn_pos, insn_id, len - 1, operands);
        push:
            VARR_PUSH(int, stack, last_rtl_ind); /* insn producing temp */
            VARR_PUSH(char, operands_changed_p, FALSE);
            break;
        case BIN(opt_case_dispatch):
            br_p = TRUE;
            /* fall through */
        default:
        rest_cases:
            if (insn_id != BIN(gen_rtl_bbv) && insn_id != BIN(gen_rtl_bbv2))
                add_insn_arr(curr_orig_insn_pos, insn_id, len - 1, operands);
        stack_update:
            if (insn_id == BIN(send) || insn_id == BIN(opt_send_without_block) || insn_id == BIN(invokesuper)) {
                inc = sp_inc_of_sendish(((CALL_DATA)operands[0])->ci);
            } else {
                inc = insn_stack_increase(insn_id, operands);
            }
            if (inc >= 0) {
                for (i = 0; i < inc; i++) {
                    VARR_PUSH(int, stack, -1);
                    VARR_PUSH(char, operands_changed_p, FALSE);
                }
            } else {
                int new_size = (int)VARR_LENGTH(int, stack) + inc;
                RTL_GEN_ASSERT(-inc <= (int)VARR_LENGTH(int, stack));
                VARR_TRUNC(int, stack, new_size);
                VARR_TRUNC(char, operands_changed_p, new_size);
            }
            retn = insn_stack_retn(insn_id, operands);
            RTL_GEN_ASSERT(inc <= retn && (int)VARR_LENGTH(int, stack) >= retn);
            for (i = 0; i < retn; i++) {
                VARR_SET(int, stack, VARR_LENGTH(int, stack) - retn + i, -1);
            }
        }
        curr_orig_insn_pos += len;
        if (bbv->iseq_bound >= 0 && curr_orig_insn_pos >= bbv->iseq_bound) break;
        if (longer_paths_p && follow_jumps_p) {
            if (insn_id == BIN(jump) && (long)operands[0] > 0)
                curr_orig_insn_pos = dst; /* follow jump destination */
            else if (non_rtl_bb_end_p(insn_id))
                break;
        }
    }
    last_original_rtl_ind = last_rtl_ind = remove_nops(&nops_num);
    if (insn_id == BIN(throw)) { /* to get original position after throw */
        last_rtl_ind = add_insn(curr_orig_insn_pos, BIN(nop), 0);
    }
    if (insn_id != BIN(leave) && insn_id != BIN(throw) && insn_id != BIN(opt_case_dispatch)) {
        if (!br_p) {
            last_rtl_ind = add_insn(curr_orig_insn_pos, BIN(jump2), 1, (VALUE)0);
            dst = insn_id == BIN(opt_case_dispatch) ? curr_orig_insn_pos : bbv->iseq_bound;
        }
        dst_bbv = get_bbv(iseq, RB_BBV_REGULAR, 0, dst, 0, NULL);
        len = insn_len(BIN(gen_rtl_bbv));
        disp = add_insn(curr_orig_insn_pos, BIN(gen_rtl_bbv), 2, (VALUE)dst_bbv, (VALUE)0);
        VARR_PUSH(rb_iseq_bbv_t, out_bbvs, dst_bbv);
        VARR_PUSH(int, out_gen_bbv_disps, disp);
        VARR_SET(VALUE, bbv_encoded, disp + 2, (VALUE)(last_rtl_ind - (disp + len)));
        if (dst2_p) {
            len = insn_len(BIN(gen_rtl_bbv2));
            dst_bbv = get_bbv(iseq, RB_BBV_REGULAR, 0, dst2, 0, NULL);
            disp2 = add_insn(curr_orig_insn_pos, BIN(gen_rtl_bbv2), 2, (VALUE)dst_bbv, (VALUE)0);
            VARR_PUSH(rb_iseq_bbv_t, out_bbvs, dst_bbv);
            VARR_PUSH(int, out_gen_bbv_disps, disp2);
            VARR_SET(VALUE, bbv_encoded, disp2 + 2, (VALUE)(last_rtl_ind - (disp2 + len)));
        }
    }
    if (insn_id == BIN(opt_case_dispatch)) { /* add gen_rtl_bbv for cdhash and else_offset */
        struct cdhash_set_el data;

        else_dst = curr_orig_insn_pos + (OFFSET)operands[1]; /* end of opt_case_dispatch + else dst */
        else_dst_bbv = get_bbv(iseq, RB_BBV_REGULAR, 0, else_dst, 0, NULL);
        else_disp = add_insn(curr_orig_insn_pos, BIN(gen_rtl_bbv2), 2, (VALUE)else_dst_bbv, (VALUE)0);
        VARR_PUSH(rb_iseq_bbv_t, out_bbvs, else_dst_bbv);
        VARR_PUSH(int, out_gen_bbv_disps, else_disp);
        len = insn_len(BIN(gen_rtl_bbv2));
        VARR_SET(VALUE, bbv_encoded, else_disp + 2, (VALUE)(last_original_rtl_ind - (else_disp + len)));
        VARR_SET(VALUE, bbv_encoded, last_original_rtl_ind + 2, (VALUE)(else_disp - last_original_rtl_ind - len));
        /* set up new hash and add insns gen_rtl_bb_key */
        data.iseq = iseq;
        data.map = rb_hash_new();
        RHASH_TBL_RAW(data.map)->type = &cdhash_type;
        data.case_start = last_original_rtl_ind;
        data.origin_case_start = curr_orig_insn_pos - insn_len(BIN(opt_case_dispatch));
        rb_hash_foreach(operands[0], set_case_hash_el, (VALUE)&data);
        RB_OBJ_WRITTEN(iseq, Qundef, data.map);
        VARR_SET(VALUE, bbv_encoded, last_original_rtl_ind + 1, data.map);
    }
    len = (int)VARR_LENGTH(VALUE, bbv_encoded);
    bbv->bbv_start = get_bbv_code_space(iseq, len);
    bbv->bbv_bound = bbv->bbv_start + len;
    memcpy(&body->bbv_code[bbv->bbv_start], VARR_ADDR(VALUE, bbv_encoded), len * sizeof(VALUE));
    memcpy(&body->bbv_pos_map[bbv->bbv_start], VARR_ADDR(int, pos_map), VARR_LENGTH(int, pos_map) * sizeof(int));
    if (insn_id != BIN(leave) && insn_id != BIN(throw)) {
        insn_id = body->bbv_code[bbv->bbv_start + last_rtl_ind];
        offset = last_rtl_ind + insn_len(insn_id); /* offset of the branch end from zero */
        offset = disp - offset; /* offset relative to the 1st gen_rtl_code */
        body->bbv_code[bbv->bbv_start + last_rtl_ind + 1] = (VALUE)offset;
        if (dst2_p) {
            offset2 = last_rtl_ind + insn_len(insn_id); /* offset of the branch end from zero */
            offset2 = disp2 - offset2;  /* offset relative to the 2nd gen_rtl_code */
            body->bbv_code[bbv->bbv_start + last_rtl_ind + 2] = (VALUE)offset2;
        }
    }
    RTL_GEN_ASSERT(VARR_LENGTH(rb_iseq_bbv_t, out_bbvs) == VARR_LENGTH(int, out_gen_bbv_disps));
    for (i = 0; i < (int)VARR_LENGTH(rb_iseq_bbv_t, out_bbvs); i++)
        add_bbv_edge(bbv, VARR_GET(rb_iseq_bbv_t, out_bbvs, i), VARR_GET(int, out_gen_bbv_disps, i));
    bbv_encode(&body->bbv_code[bbv->bbv_start], len);
    if (debug_p) {
        fprintf(stderr, "## RTL BBV:\n");
        print_bb(iseq, body->bbv_code, bbv->bbv_start, bbv->bbv_bound, "  ");
        print_pos_map(iseq, bbv->bbv_start, bbv->bbv_bound, FALSE);
    }
}

static int
add_spec_insn_arr(int rtl_pos, enum ruby_vminsn_type insn_id, int nops, VALUE *operands) {
    RTL_GEN_ASSERT(rtl_pos >= 0);
    return add_insn_arr(-(rtl_pos + 1), insn_id, nops, operands);
}

#define BRANCH_CASES(name)      \
    case CONCAT(name, ss):	\
    case CONCAT(name, sv):	\
    case CONCAT(name, si):	\
    case CONCAT(name, vs):	\
    case CONCAT(name, vv):	\
    case CONCAT(name, vi)

static void
remove_out_edges(rb_iseq_bbv_t src_bbv) {
    rb_iseq_bbv_t bbv;
    struct rb_iseq_bbv_edge *out_e, *next_out_e, *in_e, *next_in_e, *prev_in_e;

    RTL_GEN_ASSERT(src_bbv->bbv_start >= 0);
    for (out_e = src_bbv->out_edges; out_e != NULL; out_e = next_out_e) {
        RTL_GEN_ASSERT(out_e->in == src_bbv);
        next_out_e = out_e->next_out_edge;
        bbv = out_e->out;
        for (prev_in_e = NULL, in_e = bbv->in_edges; in_e != NULL; in_e = next_in_e) {
            RTL_GEN_ASSERT(out_e->out == bbv);
            next_in_e = in_e->next_in_edge;
            if (in_e->in != src_bbv) {
                prev_in_e = in_e;
            } else {
                if (prev_in_e == NULL)
                    bbv->in_edges = next_in_e;
                else
                    prev_in_e->next_in_edge = next_in_e;
            }
        }
        ruby_xfree((void *)out_e);
    }
    src_bbv->out_edges = NULL;
}

#define RTL_INSN2_SPEC_STACK_CASES(name)         \
    case CONCAT(name, svv):                      \
    case CONCAT(name, svs):                      \
    case CONCAT(name, ssv):                      \
    case CONCAT(name, sss):                      \
    case CONCAT(name, svi):                      \
    case CONCAT(name, ssi)                       \

#define RTL_INSN2_SPEC_LVAR_CASES(name)          \
    case CONCAT(name, vvv):                      \
    case CONCAT(name, vvs):                      \
    case CONCAT(name, vsv):                      \
    case CONCAT(name, vss):                      \
    case CONCAT(name, vvi):                      \
    case CONCAT(name, vsi)


/* Make type-specialized RTL from general RTL or make speculative
   type-specialized RTL based on profile (inspect_types_num).  Add
   inspect_type insn for 1st inspect_types_num variables (args) with
   unknown attributes. */
static int
gen_specialized_rtl(rb_iseq_t *iseq, rb_iseq_bbv_t bbv, int inspect_types_num) {
    int profile_p, n_attrs;
    enum ruby_vminsn_type insn_id = BIN(nop), new_insn_id, prev_new_insn_id, branch_id;
    int init_stack_size = bbv->start_stack_size;
    int i, n, curr_rtl_insn_pos, len, nops, inc, retn, stack_size;
    int orig_pos, new_insn_pos, prev_new_insn_pos, branch_pos, prev_new_insn_nops;
    int branch2_offset_add, attr, op1_attr, op2_attr, imm_p, unknown_p, neq_p;
    int debug_p = rb_rtl_debug_p(), compacted_attrs_p = FALSE;
    int disp, nops_num;
    VALUE val, *operands, new_operands[10];
    int offset, specialize_bbv_pos = -1;
    struct rb_iseq_constant_body *body = iseq->body;
    rb_iseq_bbv_t head_bbv = bbv->head_bbv, orig_bbv, dst_bbv, old_dst_bbv;

    profile_p = inspect_types_num < 0;
    RTL_GEN_ASSERT(profile_p || bbv->bbv_start < 0);
    if (head_bbv == NULL)
        head_bbv = bbv;
    RTL_GEN_ASSERT(init_stack_size >= 0 && head_bbv->start_stack_size == init_stack_size);
    orig_bbv = profile_p ? bbv : head_bbv;
    if (debug_p) {
        fprintf(stderr, "\n## Making %s Specialized BBV 0x%llx:",
                profile_p ? "Profile" : "Type", (unsigned long long)bbv);
        print_attrs(bbv->attrs);
        fprintf(stderr, "Original RTL BBV of ");
        print_iseq_title(iseq);
        fprintf(stderr, "\n");
        print_bb(iseq, body->bbv_code, orig_bbv->bbv_start, orig_bbv->bbv_bound, "  ");
    }
    VARR_TRUNC(VALUE, bbv_encoded, 0);
    copy_attrs(bbv->attrs);
    VARR_TRUNC(int, pos_map, 0);
    VARR_TRUNC(char, labeled_p, 0);
    VARR_TRUNC(rb_iseq_bbv_t, out_bbvs, 0);
    VARR_TRUNC(int, out_gen_bbv_disps, 0);
    stack_size = init_stack_size;
    new_insn_pos = -1;
    branch2_offset_add = 0;
    if (!profile_p && bbv->iseq_start == 0
        && (body->type == ISEQ_TYPE_METHOD || body->type == ISEQ_TYPE_BLOCK)
        && !body->param.flags.has_opt) { // ??? opt
        n_attrs = strlen(bbv->attrs);
        for (i = 0; i < body->param.lead_num; i++) {
            RTL_GEN_ASSERT(!profile_p);
            if (i < n_attrs && bbv->attrs[i] == ATTR_FIXNUM) continue;
            new_operands[0] = get_lvar_index(iseq, i);
            add_spec_insn_arr(orig_bbv->bbv_start, BIN(inspect_type), 1, new_operands);
        }
    }
    for (curr_rtl_insn_pos = orig_bbv->bbv_start;
         curr_rtl_insn_pos < orig_bbv->bbv_bound;
         curr_rtl_insn_pos += len) {
        prev_new_insn_pos = new_insn_pos;
        new_insn_pos = (int)VARR_LENGTH(VALUE, bbv_encoded);
	insn_id = rb_vm_insn_addr2insn((void *)body->bbv_code[curr_rtl_insn_pos]);
        len = insn_len(insn_id);
        operands = &body->bbv_code[curr_rtl_insn_pos + 1];
        neq_p = imm_p = unknown_p = FALSE; /* immediate operand and unknown result */
        orig_pos = curr_rtl_insn_pos;
        if (profile_p) {
            orig_pos = body->bbv_pos_map[curr_rtl_insn_pos];
            if (orig_pos < 0)
                orig_pos = -orig_pos - 1;
        }
        switch (insn_id) {
        case BIN(nop2):
            new_insn_pos = prev_new_insn_pos;
            break; /* remove it */
        case BIN(inspect_type):
            RTL_GEN_ASSERT(profile_p);
            setup_attr(get_lvar_num(iseq, (int)operands[0]), ATTR_UNKNOWN);
            new_insn_pos = prev_new_insn_pos;
            break;/* it was not changed -- remove it */
        case BIN(inspect_stack_type):
            RTL_GEN_ASSERT(profile_p);
            setup_attr(get_temp_num(iseq, stack_size - (int)operands[0] - 1), ATTR_UNKNOWN);
            new_insn_pos = prev_new_insn_pos;
            break;/* it was not changed -- remove it */
        case BIN(check_fixtype):
            RTL_GEN_ASSERT(profile_p);
            setup_attr(get_lvar_num(iseq, (int)operands[0]), ATTR_FIXNUM);
            add_spec_insn_arr(orig_pos, insn_id, len - 1, operands);
            break;
        case BIN(check_stack_fixtype):
            RTL_GEN_ASSERT(profile_p);
            setup_attr(get_temp_num(iseq, stack_size - (int)operands[0] - 1), ATTR_FIXNUM);
            add_spec_insn_arr(orig_pos, insn_id, len - 1, operands);
            break;
        case BIN(swap):
            attr = get_attr(get_temp_num(iseq, stack_size - 1));
            RTL_GEN_ASSERT(stack_size >= 2);
            setup_attr(get_temp_num(iseq, stack_size - 1), get_attr(get_temp_num(iseq, stack_size - 2)));
            setup_attr(get_temp_num(iseq, stack_size - 2), attr);
            add_spec_insn_arr(orig_pos, insn_id, len - 1, operands);
            break;
        case BIN(setn):
            n = (int)operands[0];
            RTL_GEN_ASSERT(stack_size > n);
            setup_attr(get_temp_num(iseq, stack_size - n - 1), get_attr(get_temp_num(iseq, stack_size - 1)));
            add_spec_insn_arr(orig_pos, insn_id, len - 1, operands);
            break;
        case BIN(dup):
            n = 1;
            goto dup;
        case BIN(dupn): /* ??? optimize: when values are only putobject|getlocal */
            n = (int) operands[0];
        dup:
            RTL_GEN_ASSERT(stack_size >= n);
            for (i = 0; i < n; i++)
                setup_attr(get_temp_num(iseq, stack_size + i), get_attr(get_temp_num(iseq, stack_size - 1 - i)));
            stack_size += n;
            add_spec_insn_arr(orig_pos, insn_id, len - 1, operands);
            break;
        case BIN(nop):
            update_labeled(new_insn_pos);
            goto rest_cases;
        RTL_INSN2_NON_IMM_CASES(BIN(opt_neq)):
            neq_p = unknown_p = TRUE;
            goto insn_op2;
        RTL_INSN2_IMM_CASES(BIN(opt_neq)):
            neq_p = TRUE;
            /* fall through */
        RTL_INSN2_IMM_CASES(BIN(opt_eq)):
        RTL_INSN2_IMM_CASES(BIN(opt_lt)):
        RTL_INSN2_IMM_CASES(BIN(opt_gt)):
        RTL_INSN2_IMM_CASES(BIN(opt_le)):
        RTL_INSN2_IMM_CASES(BIN(opt_ge)):
        RTL_INSN2_IMM_CASES(BIN(opt_aref)):
            imm_p = TRUE;
            /* fall through */
        RTL_INSN2_NON_IMM_CASES(BIN(opt_eq)):
        RTL_INSN2_NON_IMM_CASES(BIN(opt_lt)):
        RTL_INSN2_NON_IMM_CASES(BIN(opt_gt)):
        RTL_INSN2_NON_IMM_CASES(BIN(opt_le)):
        RTL_INSN2_NON_IMM_CASES(BIN(opt_ge)):
        RTL_INSN2_NON_IMM_CASES(BIN(opt_aref)):
            unknown_p = TRUE;
            goto insn_op2;
        RTL_INSN2_IMM_CASES(BIN(opt_plus)):
        RTL_INSN2_IMM_CASES(BIN(opt_minus)):
        RTL_INSN2_IMM_CASES(BIN(opt_mult)):
        RTL_INSN2_IMM_CASES(BIN(opt_div)):
        RTL_INSN2_IMM_CASES(BIN(opt_mod)):
        RTL_INSN2_IMM_CASES(BIN(opt_or)):
        RTL_INSN2_IMM_CASES(BIN(opt_and)):
            imm_p = TRUE;
            /* fall through */
        RTL_INSN2_NON_IMM_CASES(BIN(opt_plus)):
        RTL_INSN2_NON_IMM_CASES(BIN(opt_minus)):
        RTL_INSN2_NON_IMM_CASES(BIN(opt_mult)):
        RTL_INSN2_NON_IMM_CASES(BIN(opt_div)):
        RTL_INSN2_NON_IMM_CASES(BIN(opt_mod)):
        RTL_INSN2_NON_IMM_CASES(BIN(opt_or)):
        RTL_INSN2_NON_IMM_CASES(BIN(opt_and)):
        insn_op2:
            nops = len - (neq_p ? 3 : 2); /* insn operands w/o cd(s) */
            RTL_GEN_ASSERT(nops <= 2 && stack_size + nops >= 2);
            if (nops == 0) {
                op1_attr = get_attr(get_temp_num(iseq, stack_size - 1));
                op2_attr = get_attr(get_temp_num(iseq, stack_size - 2));
            } else if (nops == 1) {
                op1_attr = get_attr(get_temp_num(iseq, stack_size - 1));
                if (imm_p) {
                    op2_attr = (FIXNUM_P(operands[0]) ? ATTR_FIXNUM
                                : FLONUM_P(operands[0]) ? ATTR_FLONUM : ATTR_UNKNOWN);
                } else {
                    op2_attr = get_attr(get_lvar_num(iseq, operands[0]));
                }
            } else {
                op1_attr = get_attr(get_lvar_num(iseq, operands[0]));
                if (imm_p) {
                    op2_attr = (FIXNUM_P(operands[1]) ? ATTR_FIXNUM
                                : FLONUM_P(operands[1]) ? ATTR_FLONUM : ATTR_UNKNOWN);
                } else {
                    op2_attr = get_attr(get_lvar_num(iseq, operands[1]));
                }
            }
            new_insn_id = get_rtl_spec_insn2_id(insn_id, op1_attr != op2_attr ? ATTR_UNKNOWN : op1_attr);
            if (new_insn_id == BIN(nop)) goto rest_cases;
            stack_size -= (2 - nops);
            attr = op1_attr != op2_attr || unknown_p ? ATTR_UNKNOWN : op1_attr;
            add_spec_insn_arr(orig_pos, new_insn_id, nops, operands);
            goto push;
        RTL_INSN2_SPEC_STACK_CASES(BIN(opt_iplus)):
        RTL_INSN2_SPEC_STACK_CASES(BIN(opt_iminus)):
        RTL_INSN2_SPEC_STACK_CASES(BIN(opt_imult)):
        RTL_INSN2_SPEC_STACK_CASES(BIN(opt_idiv)):
        RTL_INSN2_SPEC_STACK_CASES(BIN(opt_imod)):
        RTL_INSN2_SPEC_STACK_CASES(BIN(opt_ior)):
        RTL_INSN2_SPEC_STACK_CASES(BIN(opt_iand)):
            inc = insn_stack_increase(insn_id, operands);
            stack_size += inc - 1;
            attr = ATTR_FIXNUM; /* attr of temp */
            goto add_and_push;
        RTL_INSN2_SPEC_LVAR_CASES(BIN(opt_iplus)):
        RTL_INSN2_SPEC_LVAR_CASES(BIN(opt_iminus)):
        RTL_INSN2_SPEC_LVAR_CASES(BIN(opt_imult)):
        RTL_INSN2_SPEC_LVAR_CASES(BIN(opt_idiv)):
        RTL_INSN2_SPEC_LVAR_CASES(BIN(opt_imod)):
        RTL_INSN2_SPEC_LVAR_CASES(BIN(opt_ior)):
        RTL_INSN2_SPEC_LVAR_CASES(BIN(opt_iand)):
            setup_attr(get_lvar_num(iseq, (int)operands[0]), ATTR_FIXNUM); /* attr of lvar */
            goto rest_cases;
        case BIN(jump2):
        case BIN(opt_getinlinecache2):
        BRANCH_CASES(BIN(opt_ibeq)):
        BRANCH_CASES(BIN(opt_ibneq)):
        BRANCH_CASES(BIN(opt_iblt)):
        BRANCH_CASES(BIN(opt_ibgt)):
        BRANCH_CASES(BIN(opt_ible)):
        BRANCH_CASES(BIN(opt_ibge)):
#ifdef FLONUM_INSNS
        BRANCH_CASES(BIN(opt_fbeq)):
        BRANCH_CASES(BIN(opt_fbneq)):
        BRANCH_CASES(BIN(opt_fblt)):
        BRANCH_CASES(BIN(opt_fbgt)):
        BRANCH_CASES(BIN(opt_fble)):
        BRANCH_CASES(BIN(opt_fbge)):
#endif
            if (!profile_p) {
                new_operands[0] = (VALUE)bbv;
                add_spec_insn_arr(orig_pos, BIN(specialize_bbv), 1, new_operands);
            }
            goto rest_cases;
        case BIN(branch2):
            if (!profile_p) {
                new_operands[0] = (VALUE)bbv;
                add_spec_insn_arr(orig_pos, BIN(specialize_bbv), 1, new_operands);
            }
            if (prev_new_insn_pos < 0) goto rest_cases;
            prev_new_insn_id = (enum ruby_vminsn_type) VARR_GET(VALUE, bbv_encoded, prev_new_insn_pos);
            new_insn_id = get_branch_rtl_insn_id(prev_new_insn_id);
            if (new_insn_id == BIN(nop)) goto rest_cases;
            nops = insn_len(new_insn_id) - 1;
            new_operands[0] = operands[0];
            new_operands[1] = operands[1];
            prev_new_insn_nops = insn_len(prev_new_insn_id) - 1;
            RTL_GEN_ASSERT(prev_new_insn_nops <= 2);
            if (prev_new_insn_nops >= 1)
                new_operands[nops - 1] = VARR_GET(VALUE, bbv_encoded, prev_new_insn_pos + prev_new_insn_nops);
            if (prev_new_insn_nops >= 2)
                new_operands[nops - 2] = VARR_GET(VALUE, bbv_encoded, prev_new_insn_pos + prev_new_insn_nops - 1);
            add_spec_insn_arr(orig_pos, new_insn_id, nops, new_operands);
            nullify(prev_new_insn_pos);
            branch2_offset_add = len - insn_len(new_insn_id);
            goto stack_update;
        case BIN(setlocal):
            if (operands[1] != 0) goto rest_cases;
            /* fallthrough */
        case BIN(setlocal_WC_0):
            RTL_GEN_ASSERT(stack_size > 0);
            attr = get_attr(get_temp_num(iseq, stack_size - 1));
            setup_attr(get_lvar_num(iseq, operands[0]), attr); /* attr of temp */
            if (prev_new_insn_pos < 0) goto rest_cases;
            prev_new_insn_id = (enum ruby_vminsn_type)VARR_GET(VALUE, bbv_encoded, prev_new_insn_pos);
            new_insn_id = get_var_res_rtl_insn_id(prev_new_insn_id);
            if (new_insn_id == BIN(nop)) goto rest_cases;
            nops = insn_len (new_insn_id) - 1;
            RTL_GEN_ASSERT(nops <= 3); /* longest variant is vvv or vvi */
            new_operands[0] = operands[0];
            if (nops > 1)
                new_operands[1] = VARR_GET(VALUE, bbv_encoded, prev_new_insn_pos + 1); /* ops[0] */
            if (nops > 2)
                new_operands[2] = VARR_GET(VALUE, bbv_encoded, prev_new_insn_pos + 2); /* ops[1] */
            add_insn_arr(VARR_GET(int, pos_map, prev_new_insn_pos), new_insn_id, nops, new_operands);
            nullify(prev_new_insn_pos);
            goto stack_update;
        case BIN(getinstancevariable): /* to do specialized insn at the execution */
            RTL_GEN_ASSERT(len == 3);
            if (profile_p) goto rest_cases;
            add_spec_insn_arr(orig_pos, BIN(make_rtl_getivar), 2, operands);
            goto stack_update;
        case BIN(setinstancevariable): /* to do specialized insn at the execution */
            RTL_GEN_ASSERT(len == 3);
            if (profile_p) goto rest_cases;
            add_spec_insn_arr(orig_pos, BIN(make_rtl_setivar), 2, operands);
            goto stack_update;
        case BIN(opt_send_without_block): {
            CALL_DATA cd = (CALL_DATA)operands[0];
            int argc = vm_ci_argc(cd->ci);
            unsigned int flags = vm_ci_flag(cd->ci);

            RTL_GEN_ASSERT(len == 2 && (flags & VM_CALL_ARGS_BLOCKARG) == 0);
            if (profile_p || (flags & (VM_CALL_TAILCALL | VM_CALL_ARGS_SPLAT)) != 0) goto rest_cases;
            new_operands[0] = operands[0];
            new_operands[1] = (VALUE)get_call_attrs(iseq, argc, stack_size);
            add_spec_insn_arr(orig_pos, BIN(make_rtl_send_without_block), 2, new_operands);
            goto stack_update;
        }
        case BIN(rtl_igetivar):
            RTL_GEN_ASSERT(profile_p);
            attr = ATTR_FIXNUM;
            goto add_and_push;
        case BIN(rtl_getivar):
        case BIN(rtl_setivar):
        case BIN(rtl_opt_send_ivget):
        case BIN(rtl_opt_send_ivset):
        case BIN(rtl_opt_send_without_block):
        case BIN(make_rtl_send_without_block):
            RTL_GEN_ASSERT(profile_p);
            goto rest_cases;
        case BIN(rtl_iseq_send_without_block): {
            /* attrs can be changed so the target bbv can */
            int argc = vm_ci_argc(((CALL_DATA)operands[0])->ci);

            RTL_GEN_ASSERT(profile_p);
            VARR_TRUNC(char, arg_attrs, 0);
            for (i = 0; i < argc; i++)
                VARR_PUSH(char, arg_attrs, get_attr(get_temp_num(iseq, stack_size - argc + i)));
            compact_attrs(arg_attrs);
            old_dst_bbv = (rb_iseq_bbv_t)operands[1];
            dst_bbv = get_bbv(old_dst_bbv->iseq, old_dst_bbv->type, old_dst_bbv->index, old_dst_bbv->iseq_start,
                              (int)VARR_LENGTH(char, arg_attrs), VARR_ADDR(char, arg_attrs));
            RTL_GEN_ASSERT(len == 3);
            if (dst_bbv != old_dst_bbv) {
                if (debug_p)
                    fprintf(stderr, "--> Changing rtl_iseq_send_without_block from BBVx%llx to BBVx%llx\n",
                            (unsigned long long)old_dst_bbv, (unsigned long long)dst_bbv);
                if (dst_bbv->bbv_start < 0)
                    VARR_PUSH(rb_iseq_bbv_t, bbvs, dst_bbv);
                operands[1] = (VALUE)dst_bbv;
            }
            goto rest_cases;
        }
        case BIN(putobject):
            val = operands[0];
            attr = FIXNUM_P(val) ? ATTR_FIXNUM : FLONUM_P(val) ? ATTR_FLONUM : ATTR_UNKNOWN;
            goto add_and_push;
        case BIN(mov):
            val = operands[1];
            attr = get_attr(get_lvar_num(iseq, operands[1]));
            setup_attr(get_lvar_num(iseq, operands[0]), attr);
            add_spec_insn_arr(orig_pos, insn_id, len - 1, operands);
            break;
        case BIN(movi):
            val = operands[1];
            attr = FIXNUM_P(val) ? ATTR_FIXNUM : FLONUM_P(val) ? ATTR_FLONUM : ATTR_UNKNOWN;
            setup_attr(get_lvar_num(iseq, operands[0]), attr);
            add_spec_insn_arr(orig_pos, insn_id, len - 1, operands);
            break;
        case BIN(putobject_INT2FIX_0_):
        case BIN(putobject_INT2FIX_1_):
            attr = ATTR_FIXNUM;
            goto add_and_push;
        case BIN (getlocal):
            if (operands[1] != 0) goto rest_cases;
            /* fallthrough */
        case BIN(getlocal_WC_0):
            attr = get_attr(get_lvar_num(iseq, operands[0]));
        add_and_push:
            add_spec_insn_arr(orig_pos, insn_id, len - 1, operands);
        push:
            setup_attr(get_temp_num(iseq, stack_size), attr); /* attr of temp */
            stack_size++;
            break;
        case BIN(jump):
        case BIN(branchif):
        case BIN(branchunless):
        case BIN(branchnil):
        case BIN(opt_getinlinecache):
        case BIN(make_rtl_bbv):
            RTL_GEN_ASSERT(FALSE); /* cannot occur in RTL */
            break;
        case BIN(opt_case_dispatch):
            if (!profile_p) {
                new_operands[0] = (VALUE)bbv;
                add_spec_insn_arr(orig_pos, BIN(specialize_bbv), 1, new_operands);
            }
            RTL_GEN_ASSERT(len == 3);
            new_operands[0] = rb_hash_new(); /* to duplicate */
            new_operands[1] = operands[1];
            add_spec_insn_arr(orig_pos, insn_id, 2, new_operands);
            goto stack_update;
        case BIN(gen_rtl_bbv):
        case BIN(gen_rtl_bbv2):
        case BIN(gen_rtl_bbv_key):
            if (!compacted_attrs_p) {
                compact_attrs(attrs);
                compacted_attrs_p = TRUE;
            }
            old_dst_bbv = (rb_iseq_bbv_t)operands[0];
            dst_bbv = get_bbv_by_head(iseq, old_dst_bbv->head_bbv == NULL ? old_dst_bbv : old_dst_bbv->head_bbv,
                                      (int)VARR_LENGTH(char, attrs), VARR_ADDR(char, attrs));
            new_operands[0] = (VALUE)dst_bbv;
            new_operands[1] = (VALUE)((OFFSET)operands[1] + branch2_offset_add); /* offset of the branch */
            /* Set up offset in the branch as it can be redirected from gen_rtl_bbv */
            branch_pos = (int)VARR_LENGTH(VALUE, bbv_encoded) + len + (OFFSET)new_operands[1];
            branch_id = (enum ruby_vminsn_type)VARR_GET(VALUE, bbv_encoded, branch_pos);
            offset = VARR_LENGTH(VALUE, bbv_encoded) - branch_pos - insn_len(branch_id);
            if (insn_id == BIN(gen_rtl_bbv)) {
                VARR_SET(VALUE, bbv_encoded, branch_pos + 1, (VALUE)(OFFSET)offset);
            } else if (insn_id == BIN(gen_rtl_bbv2)) {
                VARR_SET(VALUE, bbv_encoded, branch_pos + 2, (VALUE)(OFFSET)offset);
            } else {
                RTL_GEN_ASSERT(branch_id == BIN(opt_case_dispatch));
                new_operands[2] = operands[2];
                rb_hash_aset(VARR_GET(VALUE, bbv_encoded, branch_pos + 1) /*hash*/,
                             operands[2] /*key*/, INT2FIX(offset));
            }
            disp = add_spec_insn_arr(orig_pos, insn_id, len - 1, new_operands);
            VARR_PUSH(rb_iseq_bbv_t, out_bbvs, dst_bbv);
            VARR_PUSH(int, out_gen_bbv_disps, disp);
            break;
        case BIN(leave):
        case BIN(throw):
            if (!profile_p) {
                new_operands[0] = (VALUE)bbv;
                add_spec_insn_arr(orig_pos, BIN(specialize_bbv), 1, new_operands);
            }
            goto rest_cases;
        case BIN(specialize_bbv):
            specialize_bbv_pos = new_insn_pos;
            // ???? new_insn_pos = prev_new_insn_pos; -- keeping it stack inconsistency
            // crash because icmpsxx;branch combining without popping val made by the icmp
            break; /* remove */
        default:
        rest_cases:
            add_spec_insn_arr(orig_pos, insn_id, len - 1, operands);
        stack_update:
            if (insn_id == BIN(send) || insn_id == BIN(opt_send_without_block) || insn_id == BIN(invokesuper)
                || insn_id == BIN(rtl_iseq_send_without_block)
                || insn_id == BIN(rtl_opt_send_without_block)) {
                inc = sp_inc_of_sendish(((CALL_DATA)operands[0])->ci);
            } else {
                inc = insn_stack_increase(insn_id, operands);
            }
            if (inc >= 0) {
                for (i = 0; i < inc; i++)
                    setup_attr(get_temp_num(iseq, stack_size + i), ATTR_UNKNOWN);
                stack_size += inc;
            } else {
                int new_size = stack_size + inc;
                RTL_GEN_ASSERT(-inc <= stack_size);
                stack_size = new_size;
                truncate_attrs(get_temp_num(iseq, new_size));
            }
            retn = insn_stack_retn(insn_id, operands);
            RTL_GEN_ASSERT(inc <= retn && stack_size >= retn);
            for (i = 0; i < retn; i++) {
                setup_attr(get_temp_num(iseq, stack_size - retn + i), ATTR_UNKNOWN);
            }
            if (!profile_p && (insn_id == BIN(send) || insn_id == BIN(opt_send_without_block))
                && (curr_rtl_insn_pos + len < head_bbv->bbv_bound)) {
                enum ruby_vminsn_type next_insn_id = rb_vm_insn_addr2insn((void *)body->bbv_code[curr_rtl_insn_pos + len]);
                if (next_insn_id != BIN(pop)) {
                    new_operands[0] = 0;
                    add_spec_insn_arr(orig_pos + len, BIN(inspect_stack_type), 1, new_operands);
                }
            }
        }
    }
    remove_nops(&nops_num); // ??? modify out_gen_bbv_disps ... */
    len = (int)VARR_LENGTH(VALUE, bbv_encoded);
    bbv->bbv_start = get_bbv_code_space(iseq, len);
    bbv->bbv_bound = bbv->bbv_start + len;
    bbv_encode(VARR_ADDR(VALUE, bbv_encoded), len);
    if (!compacted_attrs_p)
        compact_attrs(attrs);
    if (profile_p) { /* attrs can change -- remove targets */
        remove_out_edges(bbv);
    }
    RTL_GEN_ASSERT(bbv->out_edges == NULL);
    RTL_GEN_ASSERT(VARR_LENGTH(rb_iseq_bbv_t, out_bbvs) == VARR_LENGTH(int, out_gen_bbv_disps));
    for (i = 0; i < (int)VARR_LENGTH(rb_iseq_bbv_t, out_bbvs); i++)
        add_bbv_edge(bbv, VARR_GET(rb_iseq_bbv_t, out_bbvs, i), VARR_GET(int, out_gen_bbv_disps, i) - nops_num);
    memcpy(&body->bbv_code[bbv->bbv_start], VARR_ADDR(VALUE, bbv_encoded), len * sizeof(VALUE));
    memcpy(&body->bbv_pos_map[bbv->bbv_start], VARR_ADDR(int, pos_map),
           VARR_LENGTH(int, pos_map) * sizeof(int));
    if (debug_p) {
        fprintf(stderr, "## %s specialized BBV x%llx:\n", profile_p ? "Profile" : "Type", (unsigned long long)bbv);
        print_bb(iseq, body->bbv_code, bbv->bbv_start, bbv->bbv_bound, "  ");
        print_pos_map(iseq, bbv->bbv_start, bbv->bbv_bound, TRUE);
    }
    return specialize_bbv_pos < 0 ? -1 : bbv->bbv_start + specialize_bbv_pos - nops_num;
}

void
rb_rtl_inspect_type(rb_control_frame_t *cfp, VALUE val, int stack_p) {
    if (FIXNUM_P(val)) {
        VALUE *insn_pc = (VALUE *)cfp->pc;
        int debug_p = rb_rtl_debug_p();

        if (debug_p) {
            if (stack_p)
                fprintf(stderr, "--> Changing inspect_stack_type to %4ld check_stack_fixtype\n",
                        insn_pc - cfp->iseq->body->bbv_code);
            else
                fprintf(stderr, "--> Changing inspect_type to %4ld check_fixtype\n",
                        insn_pc - cfp->iseq->body->bbv_code);
        }
        *insn_pc = (VALUE)dispatch_table[stack_p ? BIN(check_stack_fixtype) : BIN(check_fixtype)];
    }
}

static void
gen_rtl_bbv_code(rb_iseq_t *iseq, rb_iseq_bbv_t bbv, int init_stack_size, int inspect_types_num) {
    RTL_GEN_ASSERT(bbv->bbv_start < 0 && bbv->start_stack_size < 0 && init_stack_size >= 0);
    bbv->start_stack_size = init_stack_size;
    if (bbv->head_bbv == NULL) { /* head bbv */
        gen_rtl_from_stack_insns(iseq, bbv);
        return;
    }
    if (bbv->head_bbv->bbv_start < 0) {
        RTL_GEN_ASSERT(bbv->head_bbv->start_stack_size < 0);
        bbv->head_bbv->start_stack_size = init_stack_size;
        /* we generate specialized RTL code from non-speculative RTL code */
        gen_rtl_from_stack_insns(iseq, bbv->head_bbv);
    }
    gen_specialized_rtl(iseq, bbv, inspect_types_num); /* type specialization of BBV */
}

VALUE *
rb_rtl_make_bbv(rb_control_frame_t *cfp, int iseq_bbv_start) { /* called from make bbv area only */
    rb_iseq_t *iseq = (rb_iseq_t *)cfp->iseq;
    struct rb_iseq_constant_body *body = iseq->body;
    rb_iseq_bbv_t bbv = get_bbv((rb_iseq_t *)iseq, RB_BBV_REGULAR, 0, iseq_bbv_start, 0, NULL);

    if (bbv->bbv_start < 0)
        gen_rtl_bbv_code((rb_iseq_t *)iseq, bbv, cfp->sp - cfp->__bp__, 0);
    if (iseq_bbv_start == 0)
        body->iseq_start_pc = body->bbv_code + bbv->bbv_start;
    return body->bbv_code + bbv->bbv_start;
}

VALUE *
rb_rtl_make_optpc_bbv(rb_control_frame_t *cfp, int opt) { /* called from make bbv area only */
    rb_iseq_t *iseq = (rb_iseq_t *)cfp->iseq;
    struct rb_iseq_constant_body *body = iseq->body;
    int iseq_bbv_start = body->param.opt_table[opt];
    rb_iseq_bbv_t bbv = get_bbv((rb_iseq_t *)iseq, RB_BBV_OPT, opt, iseq_bbv_start, 0, NULL);

    if (bbv->bbv_start < 0)
        gen_rtl_bbv_code((rb_iseq_t *)iseq, bbv, cfp->sp - cfp->__bp__, 0);
    RTL_GEN_ASSERT(body->bbv_code + bbv->bbv_start >= body->iseq_start_pc);
    body->curr_param_opt_table[opt] = body->bbv_code + bbv->bbv_start - body->iseq_start_pc;
    return body->bbv_code + bbv->bbv_start;
}

VALUE *
rb_rtl_make_catch_bbv(rb_control_frame_t *cfp, int entry_ind) { /* called from make bbv area only */
    rb_iseq_t *iseq = (rb_iseq_t *)cfp->iseq;
    struct rb_iseq_constant_body *body = iseq->body;
    int iseq_bbv_start = body->catch_table->entries[entry_ind].cont;
    rb_iseq_bbv_t bbv = get_bbv((rb_iseq_t *)iseq, RB_BBV_CATCH, entry_ind, iseq_bbv_start, 0, NULL);

    if (bbv->bbv_start < 0)
        gen_rtl_bbv_code((rb_iseq_t *)iseq, bbv, cfp->sp - cfp->__bp__, 0);
    body->catch_table->entries[entry_ind].curr_cont = bbv->bbv_start;
    return body->bbv_code + bbv->bbv_start;
}

int
rb_rtl_gen_bbv(rb_control_frame_t *cfp, rb_iseq_bbv_t bbv, int branch_offset, int first_p) {
    struct rb_iseq_constant_body *body = cfp->iseq->body;
    int gen_bbv_insn_pos = cfp->pc - body->bbv_code; /* position of gen_rtl_bbv end */
    enum ruby_vminsn_type branch_id = rb_vm_insn_decode(body->bbv_code[gen_bbv_insn_pos + branch_offset]);
    int branch_len = insn_len(branch_id), gen_rtl_bbv_len = insn_len(BIN(gen_rtl_bbv));

    RTL_GEN_ASSERT(gen_bbv_insn_pos >= 0 && gen_bbv_insn_pos <= body->bbv_code_bound
                   && insn_len(BIN(gen_rtl_bbv)) == insn_len(BIN(gen_rtl_bbv2)));
    gen_bbv_insn_pos -= gen_rtl_bbv_len;
    if (bbv->bbv_start < 0) /* can be generated by another gen_rtl_bbv insn */
        gen_rtl_bbv_code((rb_iseq_t *)cfp->iseq, bbv, cfp->sp - cfp->__bp__, 0);
    /* redirect hash from rtl_gen_bbv[2] to the generated bbv start */
    body->bbv_code[gen_bbv_insn_pos + gen_rtl_bbv_len + branch_offset + (first_p ? 1 : 2)]
        = bbv->bbv_start - (gen_bbv_insn_pos + gen_rtl_bbv_len + branch_offset + branch_len);
    return bbv->bbv_start;
}

int
rb_rtl_gen_bbv_key(rb_control_frame_t *cfp, rb_iseq_bbv_t bbv, int case_offset, VALUE key) {
    struct rb_iseq_constant_body *body = cfp->iseq->body;
    int gen_bbv_insn_pos = cfp->pc - body->bbv_code; /* position of gen_rtl_code end */
    enum ruby_vminsn_type branch_id = rb_vm_insn_decode(body->bbv_code[gen_bbv_insn_pos + case_offset]);
    int branch_len = insn_len(branch_id), gen_rtl_bbv_len = insn_len(BIN(gen_rtl_bbv));
    CDHASH hash = body->bbv_code[gen_bbv_insn_pos + case_offset + 1];

    RTL_GEN_ASSERT(branch_id == BIN(opt_case_dispatch)
                   && gen_bbv_insn_pos >= 0 && gen_bbv_insn_pos <= body->bbv_code_bound);
    gen_bbv_insn_pos -= gen_rtl_bbv_len;
    if (bbv->bbv_start < 0) /* can be generated by another gen_rtl_bbv insn */
        gen_rtl_bbv_code((rb_iseq_t *)cfp->iseq, bbv, cfp->sp - cfp->__bp__, 0);
    /* redirect hash from rtl_gen_bbv_key to the generated bbv start */
    rb_hash_aset(hash, key, INT2FIX(bbv->bbv_start
                                    - (gen_bbv_insn_pos + gen_rtl_bbv_len + case_offset + branch_len)));
    return bbv->bbv_start;
}

int
rb_rtl_specialize_bbv(rb_control_frame_t *cfp, rb_iseq_bbv_t bbv) {
    rb_iseq_t *iseq = (rb_iseq_t *)cfp->iseq;
    struct rb_iseq_constant_body *body = iseq->body;
    rb_iseq_bbv_t called_bbv;
    /* specialize curr bbv by profile: */
    VARR_TRUNC(rb_iseq_bbv_t, bbvs, 0);
    int bbv_cont_pos = gen_specialized_rtl(bbv->iseq, bbv, -1);
    RTL_GEN_ASSERT(bbv_cont_pos >= 0);
    if (VARR_LENGTH(rb_iseq_bbv_t, bbvs) != 0) {
        int debug_p = rb_rtl_debug_p();
        if (debug_p)
            fprintf(stderr, "<<< Start of further type specialization of called BBVs\n");
        /* Here we can reuse globals.  Remember bbvs is updated only for
           profile_p and gen_rtl_bbv_code never means profile_p. */
        while (VARR_LENGTH(rb_iseq_bbv_t, bbvs) != 0) {
            called_bbv = VARR_POP(rb_iseq_bbv_t, bbvs);
            if (called_bbv->bbv_start < 0) /* bbv can occur more one time in bbvs */
                gen_rtl_bbv_code(called_bbv->iseq, called_bbv, 0, 0);
        }
        if (debug_p)
            fprintf(stderr, ">>> End of further type specialization of called BBVs\n");
    }
    /* Change to nop2 */
    ((VALUE *)cfp->pc)[0] = (VALUE)dispatch_table[BIN(nop2)]; /* we use nop2 as we keep nop */
    ((VALUE *)cfp->pc)[1] = INT2FIX(0);
    if (bbv->type == RB_BBV_REGULAR) {
        if (bbv->iseq_start == 0)
            body->iseq_start_pc = body->bbv_code + bbv->bbv_start;
    } else if (bbv->type == RB_BBV_OPT) {
        body->curr_param_opt_table[bbv->index] = bbv->bbv_start + body->bbv_code - body->iseq_start_pc;
    } else {
        RTL_GEN_ASSERT(bbv->type == RB_BBV_CATCH);
        body->catch_table->entries[bbv->index].curr_cont = bbv->bbv_start;
    }
    return bbv_cont_pos;
}

void
rb_rtl_change_getivar(VALUE self, rb_control_frame_t *cfp, IVC ic, VALUE val) {
    VALUE *insn_pc = (VALUE *)cfp->pc - insn_len(BIN(make_rtl_getivar));
    int debug_p = rb_rtl_debug_p(), fixnum_p = FIXNUM_P(val);

    if (RB_TYPE_P(self, T_OBJECT)) {
        if (debug_p)
            fprintf(stderr, "--> Changing make_rtl_getivar to %4ld %s\n",
                    insn_pc - cfp->iseq->body->bbv_code, fixnum_p ? "rtl_igetivar" : "rtl_getivar");
        *insn_pc = (VALUE)dispatch_table[fixnum_p ? BIN(rtl_igetivar) : BIN(rtl_getivar)];
        return;
    }
    if (debug_p)
        fprintf(stderr, "--> Changing make_rtl_getivar to getinstancevariable\n");
    *insn_pc = (VALUE)dispatch_table[BIN(getinstancevariable)];
}

void
rb_rtl_change_setivar(VALUE self, rb_control_frame_t *cfp, IVC ic) {
    VALUE *insn_pc = (VALUE *)cfp->pc - insn_len(BIN(make_rtl_setivar));
    int debug_p = rb_rtl_debug_p();

    RTL_GEN_ASSERT(ic->entry != NULL);
    if (RB_TYPE_P(self, T_OBJECT)) {
        if (debug_p)
            fprintf(stderr, "--> Changing make_rtl_setivar to rtl_setivar\n");
        *insn_pc = (VALUE)dispatch_table[BIN(rtl_setivar)];
        return;
    }
    if (debug_p)
        fprintf(stderr, "--> Changing make_rtl_set_ivar to setinstancevariable\n");
    *insn_pc = (VALUE)dispatch_table[BIN(setinstancevariable)];
}

extern bool rb_simple_iseq_p(const rb_iseq_t *iseq);
extern bool rb_iseq_only_optparam_p(const rb_iseq_t *iseq);

void
rb_rtl_change_send_without_block(const rb_execution_context_t *ec, rb_control_frame_t *caller_cfp,
                                 CALL_DATA cd, char *attrs) {
    VALUE *call_pc = (VALUE *)caller_cfp->pc - insn_len(BIN(make_rtl_send_without_block));
    VALUE recv;
    const struct rb_callinfo *ci = cd->ci;
    int argc = vm_ci_argc(ci);
    const struct rb_callcache *cc = cd->cc;
    const rb_callable_method_entry_t *cme = vm_cc_cme(cc);
    rb_iseq_bbv_t bbv;
    rb_iseq_t *iseq;
    int debug_p = rb_rtl_debug_p();

    if (cme != NULL) {
        recv = *caller_cfp->sp;
        if (cme->def->type == VM_METHOD_TYPE_ISEQ
            && (iseq = (rb_iseq_t *)def_iseq_ptr(cme->def)) != NULL
            && (rb_simple_iseq_p(iseq) || rb_iseq_only_optparam_p(iseq))) {
            struct rb_iseq_constant_body *body = iseq->body;
            const int lead_num = iseq->body->param.lead_num;
            const int opt = argc - lead_num;

            RTL_GEN_ASSERT(0 <= opt && opt <= body->param.opt_num);
            if (debug_p) {
                fprintf(stderr,
                        "--> Changing make_rtl_send_without_block to rtl_iseq_send_without_block: argc=%d",
                        argc);
                print_attrs(attrs);
            }
            if (body->param.opt_table == NULL)
                bbv = get_bbv(iseq, RB_BBV_REGULAR, 0, 0, attrs == NULL ? 0 : strlen(attrs) + 1, attrs);
            else
                bbv = get_bbv(iseq, RB_BBV_OPT, opt, body->param.opt_table[opt],
                              attrs == NULL ? 0 : strlen(attrs) + 1, attrs);
            if (bbv->bbv_start < 0)
                gen_rtl_bbv_code(iseq, bbv, 0, argc);
            RTL_GEN_ASSERT(bbv->iseq == iseq);
            ec->cfp->pc = &iseq->body->bbv_code[bbv->bbv_start]; /* to start with BBV code */
            /* Change make_rtl_send_without_block(cd, attrs) to rtl_iseq_send_without_block(cd, bbv) */
            *call_pc = (VALUE)dispatch_table[BIN(rtl_iseq_send_without_block)];
            call_pc[2] = (VALUE)bbv;
            return;
        } else if (cme->def->type == VM_METHOD_TYPE_IVAR
                   && (vm_ci_flag(ci) & (VM_CALL_ARGS_SPLAT | VM_CALL_KW_SPLAT)) == 0
                   && RB_TYPE_P(recv, T_OBJECT)) {
            rb_check_arity(argc, 0, 0);
            if (vm_cc_attr_index(cc) <= 0)
                return; /* do nothing -- no attr index yet */
            if (debug_p)
                fprintf(stderr, "--> Changing make_rtl_send_without_block to rtl_opt_send_ivget\n");
            RTL_GEN_ASSERT(vm_cc_attr_index(cc) > 0);
            *call_pc = (VALUE)dispatch_table[BIN(rtl_opt_send_ivget)];
            call_pc[1] = (VALUE)cc->klass;
            call_pc[2] = (VALUE)vm_cc_attr_index(cc) - 1;
            return;
        } else if (cme->def->type == VM_METHOD_TYPE_ATTRSET
                   && (vm_ci_flag(ci) & (VM_CALL_ARGS_SPLAT | VM_CALL_KW_SPLAT | VM_CALL_KWARG)) == 0
                   && RB_TYPE_P(recv, T_OBJECT)) {
            rb_check_arity(argc, 1, 1);
            if (vm_cc_attr_index(cc) <= 0)
                return; /* do nothing -- no attr index yet */
            if (debug_p)
                fprintf(stderr, "--> Changing make_rtl_send_without_block to rtl_opt_send_ivset\n");
            RTL_GEN_ASSERT(vm_cc_attr_index(cc) > 0);
            *call_pc = (VALUE)dispatch_table[BIN(rtl_opt_send_ivset)];
            call_pc[1] = (VALUE)cc->klass;
            call_pc[2] = (VALUE)vm_cc_attr_index(cc) - 1;
            return;
        }
    }
    if (debug_p) {
        fprintf(stderr,
                "--> Changing make_rtl_send_without_block to rtl_opt_send_without_block: argc=%d",
                argc);
        print_attrs(attrs);
    }
    *call_pc = (VALUE)dispatch_table[BIN(rtl_opt_send_without_block)];
}

void
rb_rtl_iseq_free(struct rb_iseq_constant_body *body) {
    rb_iseq_bbv_t bbv, curr_bbv, next_bbv;
    int i;

    for (i = 0; i < body->n_bbvs; i++) {
        bbv = body->bbvs[i];
        for (curr_bbv = bbv; curr_bbv != NULL; curr_bbv = next_bbv) {
            next_bbv = curr_bbv->next_bbv;
            /* ??? free attrs in make_rtl_send_without_block */
            ruby_xfree((void *)curr_bbv);
        }
    }
    if (body->curr_param_opt_table != NULL && body->curr_param_opt_table != body->param.opt_table)
        ruby_xfree((void *)body->curr_param_opt_table);
    ruby_xfree((void *)body->bbvs);
    ruby_xfree((void *)body->bbv_code);
    ruby_xfree((void *)body->bbv_pos_map);
}

/* Initiate stack insns to RTL generator.  Return FALSE if we failed
   to do this.  */
int
rb_rtl_gen_init(void) {
    if (RUBY_SETJMP(rtl_gen_jump_buf) != 0)
	return FALSE;
    dispatch_table = rb_vm_get_insns_address_table();
    VARR_CREATE(int, stack, 0);
    VARR_CREATE(char, operands_changed_p, 0);
    VARR_CREATE(char, attrs, 0);
    VARR_CREATE(char, arg_attrs, 0);
    VARR_CREATE(char, labeled_p, 0);
    VARR_CREATE(int, pos_map, 0);
    VARR_CREATE(VALUE, bbv_encoded, 0);
    VARR_CREATE(rb_iseq_bbv_t, bbvs, 0);
    VARR_CREATE(rb_iseq_bbv_t, out_bbvs, 0);
    VARR_CREATE(int, out_gen_bbv_disps, 0);
    return TRUE;
}

/* Finish stack insns to RTL generator.  */
void
rb_rtl_gen_finish(void) {
    VARR_DESTROY(int, stack);
    VARR_DESTROY(char, operands_changed_p);
    VARR_DESTROY(char, attrs);
    VARR_DESTROY(char, arg_attrs);
    VARR_DESTROY(char, labeled_p);
    VARR_DESTROY(int, pos_map);
    VARR_DESTROY(VALUE, bbv_encoded);
    VARR_DESTROY(rb_iseq_bbv_t, bbvs);
    VARR_DESTROY(rb_iseq_bbv_t, out_bbvs);
    VARR_DESTROY(int, out_gen_bbv_disps);
}
