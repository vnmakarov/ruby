/**********************************************************************

  rtl_gen.c - Generate RTL insns.

  Copyright (C) 2021 Vladimir Makarov vmakarov@redhat.com

**********************************************************************/

#include "rtl.h"
#include "internal/compile.h"
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

#define CONCAT(a, b) a ## b
static enum ruby_vminsn_type
get_rtl_insn2_id(enum ruby_vminsn_type id, int local2_p, int local3_p, VALUE val3, int fix_p) {
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

    if (fix_p) {
        switch (id) {
        RTL_INSN_ID_CASE(BIN(opt_plus), BIN(opt_iplus))
        RTL_INSN_ID_CASE(BIN(opt_minus), BIN(opt_iminus))
        RTL_INSN_ID_CASE(BIN(opt_mult), BIN(opt_imult))
        RTL_INSN_ID_CASE(BIN(opt_div), BIN(opt_idiv))
        RTL_INSN_ID_CASE(BIN(opt_mod), BIN(opt_imod))
        RTL_INSN_ID_CASE(BIN(opt_or), BIN(opt_ior))
        RTL_INSN_ID_CASE(BIN(opt_and), BIN(opt_iand))
        RTL_INSN_ID_CASE(BIN(opt_eq), BIN(opt_ieq))
        RTL_INSN_ID_CASE(BIN(opt_neq), BIN(opt_ineq))
        RTL_INSN_ID_CASE(BIN(opt_lt), BIN(opt_ilt))
        RTL_INSN_ID_CASE(BIN(opt_gt), BIN(opt_igt))
        RTL_INSN_ID_CASE(BIN(opt_le), BIN(opt_ile))
        RTL_INSN_ID_CASE(BIN(opt_ge), BIN(opt_ige))
        RTL_INSN_ID_CASE(BIN(opt_aref), BIN(opt_iaref))
        default: return BIN(nop);
        }
    }
    switch (id) {
    RTL_INSN_ID_CASE(BIN(opt_plus), BIN(opt_fplus))
    RTL_INSN_ID_CASE(BIN(opt_minus), BIN(opt_fminus))
    RTL_INSN_ID_CASE(BIN(opt_mult), BIN(opt_fmult))
    RTL_INSN_ID_CASE(BIN(opt_div), BIN(opt_fdiv))
    RTL_INSN_ID_CASE(BIN(opt_mod), BIN(opt_fmod))
    RTL_INSN_ID_CASE(BIN(opt_eq), BIN(opt_feq))
    RTL_INSN_ID_CASE(BIN(opt_neq), BIN(opt_fneq))
    RTL_INSN_ID_CASE(BIN(opt_lt), BIN(opt_flt))
    RTL_INSN_ID_CASE(BIN(opt_gt), BIN(opt_fgt))
    RTL_INSN_ID_CASE(BIN(opt_le), BIN(opt_fle))
    RTL_INSN_ID_CASE(BIN(opt_ge), BIN(opt_fge))
    default: return BIN(nop);
    }
}

static enum ruby_vminsn_type
get_rtl_insn3_id(enum ruby_vminsn_type id, int local_p, int local2_p, int local3_p, VALUE val2, int fix_p) {
#define RTL_INSN3_ID_CASE(insn_name, fname)				\
    case insn_name:							\
    if (local_p) {							\
      if (local2_p && local3_p) return CONCAT(fname, vvv);              \
      if (local3_p) {							\
        if (val2 != Qnil) return CONCAT(fname, vsv);                    \
        return CONCAT(fname, viv);					\
      }                                                                 \
      if (local2_p) return CONCAT(fname, vvs);                          \
      if (val2 != Qnil) return CONCAT(fname, vis);			\
    } else {								\
      if (local2_p && local3_p) return CONCAT(fname, svv);              \
      if (local3_p) {							\
        if (val2 != Qnil) return CONCAT(fname, ssv);                    \
        return CONCAT(fname, siv);					\
      }                                                                 \
      if (local2_p) return CONCAT(fname, svs);                          \
      if (val2 != Qnil) return CONCAT(fname, sis);			\
    }									\
    return BIN(nop);

    if (fix_p) {
        switch (id) {
        RTL_INSN3_ID_CASE(BIN(opt_aset), BIN(opt_iaset))
        default: return BIN(nop);
        }
    }
    switch (id) {
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
    BRANCH_RTL_INSN_ID_CASE(BIN(opt_feq), BIN(opt_fbeq))
    BRANCH_RTL_INSN_ID_CASE(BIN(opt_fneq), BIN(opt_fbneq))
    BRANCH_RTL_INSN_ID_CASE(BIN(opt_flt), BIN(opt_fblt))
    BRANCH_RTL_INSN_ID_CASE(BIN(opt_fgt), BIN(opt_fbgt))
    BRANCH_RTL_INSN_ID_CASE(BIN(opt_fle), BIN(opt_fble))
    BRANCH_RTL_INSN_ID_CASE(BIN(opt_fge), BIN(opt_fbge))
    default: return BIN(nop);
    }
}

static int
get_bbv_nums(struct rb_iseq_bbv *head) {
    int n = 0;
    struct rb_iseq_bbv *curr;

    RTL_GEN_ASSERT(head->n_attrs == 0);
    for (curr = head; curr != NULL; curr = curr->next_bbv)
        n++;
    return n;
}

static struct rb_iseq_bbv *
get_bbv_list_head(rb_iseq_t *iseq, int start) {
    struct rb_iseq_constant_body *body = iseq->body;
    int left, right, middle;

    for (left = 0, right = body->n_bbvs - 1; left <= right; ) {
        middle = (left + right) / 2;
        if (body->bbvs[middle]->iseq_start < start)
            left = middle + 1;
        else if (body->bbvs[middle]->iseq_start > start)
            right = middle - 1;
        else
            return body->bbvs[middle];
    }
    RTL_GEN_ASSERT(FALSE);
    return NULL;
}

static struct rb_iseq_bbv *
find_bbv(struct rb_iseq_bbv *head, int n_attrs, char *attrs) {
    struct rb_iseq_bbv *curr;

    RTL_GEN_ASSERT(n_attrs == 0 || attrs[n_attrs - 1] != ATTR_UNKNOWN);
    for (curr = head; curr != NULL; curr = curr->next_bbv) {
        if (curr->n_attrs == n_attrs && (n_attrs == 0 || memcmp(curr->attrs, attrs, n_attrs) == 0))
            return curr;
    }
    return NULL;
}

static struct rb_iseq_bbv *
create_bbv(rb_iseq_t *iseq, int start, int bound, int n_attrs, char *attrs) {
    struct rb_iseq_bbv *bbv;

    bbv = (struct rb_iseq_bbv *) ALLOC_N(char, sizeof (struct rb_iseq_bbv) + (n_attrs == 0 ? 0 : n_attrs - 1));
    bbv->iseq_start = start;
    bbv->iseq_bound = bound;
    bbv->next_bbv = NULL;
    bbv->bbv_start = -1; /* flag of the code is not generated */
    bbv->n_attrs = n_attrs;
    memcpy (bbv->attrs, attrs, n_attrs);
    return bbv;
}

static struct rb_iseq_bbv *
add_new_bbv(rb_iseq_t *iseq, struct rb_iseq_bbv *head, int start, int bound, int n_attrs, char *attrs) {
    struct rb_iseq_bbv *bbv = create_bbv(iseq, start, bound, n_attrs, attrs);

    bbv->next_bbv = head->next_bbv;
    head->next_bbv = bbv;
    return bbv;
}

#define MAX_BBV_NUM 4

struct rb_iseq_bbv *
get_bbv(rb_iseq_t *iseq, int start, int n_attrs, char *attrs) {
    struct rb_iseq_bbv *bbv, *default_bbv = get_bbv_list_head (iseq, start);

    if ((bbv = find_bbv(default_bbv, n_attrs, attrs)) != NULL) return bbv;
    if (get_bbv_nums(default_bbv) >= MAX_BBV_NUM) return default_bbv;
    bbv = add_new_bbv(iseq, default_bbv, start, default_bbv->iseq_bound, n_attrs, attrs);
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
        return TRUE;
    case BIN(opt_case_dispatch):
        abort();
    default:
        return FALSE;
    }
}

typedef struct rb_iseq_bbv *rb_iseq_bbv_t;
DEF_VARR(rb_iseq_bbv_t);
static VARR(rb_iseq_bbv_t) *bbvs;

void
rb_rtl_create_default_bbvs(rb_iseq_t *iseq) {
    int i, curr_bb_start;
    enum ruby_vminsn_type insn_id;
    struct rb_iseq_constant_body *body = iseq->body;
    VALUE *encoded = (VALUE *)body->iseq_encoded;
    struct rb_iseq_bbv *bbv;

    VARR_TRUNC(rb_iseq_bbv_t, bbvs, 0);
    if (longer_paths_p) { /* create bbv at the **start** of the BB */
        for (i = 0; i < (int)body->iseq_size; i += insn_len(insn_id)) {
            insn_id = (enum ruby_vminsn_type)encoded[i];
            if (insn_id == BIN(make_rtl_bbv)) {
                bbv = create_bbv (iseq, i, -1, 0, NULL);
                VARR_PUSH (rb_iseq_bbv_t, bbvs, bbv);
            }
        }
    } else { /* create bbv at the **end** of the BB */
        curr_bb_start = 0;
        for (i = 0; i < (int)body->iseq_size; i += insn_len(insn_id)) {
            insn_id = (enum ruby_vminsn_type)encoded[i];
            if (insn_id == BIN(make_rtl_bbv) && i != 0) {
                bbv = create_bbv (iseq, curr_bb_start, i, 0, NULL);
                VARR_PUSH (rb_iseq_bbv_t, bbvs, bbv);
                curr_bb_start = i;
            }
        }
        if (curr_bb_start != (int)body->iseq_size) {
            bbv = create_bbv (iseq, curr_bb_start, (int)body->iseq_size, 0, NULL);
            VARR_PUSH (rb_iseq_bbv_t, bbvs, bbv);
        }
    }
    body->n_bbvs = VARR_LENGTH(rb_iseq_bbv_t, bbvs);
    body->bbvs = ALLOC_N(rb_iseq_bbv_t, body->n_bbvs);
    memcpy (body->bbvs, VARR_ADDR(rb_iseq_bbv_t, bbvs), body->n_bbvs * sizeof (rb_iseq_bbv_t));
}

DEF_VARR(VALUE);
static VARR(VALUE) *bbv_encoded;

static VARR(int) *pos_map;

static void
update_pos_map(int pos, int orig_pos) {
    while ((int)VARR_LENGTH(int, pos_map) <= pos)
        VARR_PUSH(int, pos_map, -1);
    VARR_SET(int, pos_map, pos, orig_pos);
}

static void
print_pos_map(void) {
    int i, pos, n;

    fprintf (stderr, "BBV position map:\n");
    for (i = n = 0; i < (int)VARR_LENGTH(int, pos_map); i++)
        if ((pos = VARR_GET(int, pos_map, i)) >= 0) {
            fprintf(stderr, n != 0 ? "," : "    ");
            fprintf(stderr, " %d->%d", i, pos);
            n++;
            if (n == 10) {
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

DEF_VARR(char);
static VARR(char) *labeled_p;

static void
update_labeled(int pos) {
    while ((int)VARR_LENGTH(char, labeled_p) <= pos)
        VARR_PUSH(char, labeled_p, FALSE);
    VARR_SET(char, labeled_p, pos, TRUE);
}

static void
nullify(int pos) {
    int i, len = insn_len((enum ruby_vminsn_type)VARR_GET(VALUE, bbv_encoded, pos));
    for (i = 0; i < len; i++)
        VARR_SET(VALUE, bbv_encoded, pos + i, BIN(nop));
}

static int
remove_nops(void) {
    int curr_ind, new_curr_ind, last_insn_p, nel, len, last_rtl_ind = 0;
    enum ruby_vminsn_type insn_id;

    for (curr_ind = new_curr_ind = 0;
         curr_ind < (int)VARR_LENGTH(VALUE, bbv_encoded);
         curr_ind += len) { /* remove nops */
	insn_id = (enum ruby_vminsn_type)VARR_GET(VALUE, bbv_encoded, curr_ind);
        len = insn_len(insn_id);
        if (insn_id == BIN(nop)
            && (curr_ind >= (int)VARR_LENGTH(char, labeled_p)
                || !VARR_GET(char, labeled_p, curr_ind)))
            continue;
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

static VARR(char) *attrs;

static void
compact_attrs(void) {
    int i;

    for (i = (int)VARR_LENGTH(char, attrs) - 1; i >= 0; i--)
        if (VARR_GET(char, attrs, i) != ATTR_UNKNOWN)
            break;
    VARR_TRUNC(char, attrs, i + 1);
}

static void
copy_attrs(const char *from, int n_attrs) {
    int i;

    VARR_TRUNC(char, attrs, 0);
    for (i = 0; i < n_attrs; i++)
        VARR_PUSH(char, attrs, from[i]);
}

static void
truncate_attrs(int len) {
    RTL_GEN_ASSERT(len >= 0);
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

static int
get_temp_num(rb_iseq_t *iseq, int n) { /* n is 0, 1, ... -- 0 corresponds stack bottom */
    return n + iseq->body->local_table_size;
}

static int
get_lvar_num(rb_iseq_t *iseq, int idx) {
    return idx - VM_ENV_DATA_SIZE;
}

VALUE rb_insn_operand_intern(const rb_iseq_t *iseq,
			     VALUE insn, int op_no, VALUE op,
			     int len, size_t pos, VALUE *pnop, VALUE child);

static void
print_insn(const rb_iseq_t *iseq, int insn_id, VALUE *code, size_t pos)
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
    fprintf(stderr, "%.*s\n", (int)RSTRING_LEN(str), RSTRING_PTR(str));
}

static void
print_bb(rb_iseq_t *iseq, VALUE *code, int start, int bound) {
    enum ruby_vminsn_type insn_id;
    int i, len;

    for (i = start;;) {
	insn_id = rb_vm_insn_decode((VALUE)code[i]);
        if (insn_id == BIN(gen_rtl_bbv)) {
            rb_iseq_bbv_t bbv = (rb_iseq_bbv_t)code[i + 1];
            fprintf(stderr, "  # %04d: gen_rtl_bbv(BBV 0x%llx)\n",
                    i, (unsigned long long) bbv);
        }
        print_insn(iseq, insn_id, code, i);
        len = insn_len(insn_id);
        if (longer_paths_p) {
            if (follow_jumps_p && insn_id == BIN(jump))
                i = i + (long) code[i + 1];
            else if (non_rtl_bb_end_p(insn_id))
                break;
        }
        i += len;
        if (bound >= 0 && i >= bound) break;
    }
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

static int
get_bbv_code_space(rb_iseq_t *iseq, int len) {
    struct rb_iseq_constant_body *body = iseq->body;
    int bbv_start = body->bbv_code_free;
    if (bbv_start + len > body->bbv_code_bound) {
        body->bbv_code_bound = (bbv_start + len) * 3 / 2;
        RB_REALLOC_N(body->bbv_code, VALUE, body->bbv_code_bound);
        RB_REALLOC_N(body->bbv_pos_map, int, body->bbv_code_bound);
    }
    body->bbv_code_free += len;
    return bbv_start;
}

static void
gen_rtl_bbv_code(rb_iseq_t *iseq, rb_iseq_bbv_t bbv, int init_stack_size) {
    enum ruby_vminsn_type insn_id = BIN(nop), next_insn_id;
    enum ruby_vminsn_type new_insn_id, op_insn_id;
    int i, curr_orig_insn_pos, len, nops, last_rtl_ind, inc, br_p, dest2_p, stack_size;
    int op_insn_pos, op3_insn_pos, op2_insn_pos, local_p, local2_p, local3_p;
    int debug_p = rb_rtl_debug_p();
    int disp = 0, disp2 = 0; /* for removing uninitialized warning */
    OFFSET dest = 0, dest2 = 0; /* for removing uninitialized warning */
    VALUE val, val2, val3, local, local2, local3, *operands, new_operands[10];
    int offset, offset2, dest_bbv_start, dest_bbv_start2 = -1; /* to remove false warning */
    struct rb_iseq_constant_body *body = iseq->body;
    rb_iseq_bbv_t dest_bbv;
    enum rb_attr_val attr, op2_attr, op3_attr;

    if (debug_p) {
        fprintf(stderr, "\nGeneration BBV 0x%llx:", (unsigned long long) bbv);
        for (i = 0; i < bbv->n_attrs; i++)
            if (bbv->attrs[i] != ATTR_UNKNOWN)
                fprintf (stderr, " %d:%s", i, bbv->attrs[i] == ATTR_FIXNUM ? "fix" : "flo");
        fprintf(stderr, "\n");
        fprintf(stderr, "Original BB:\n");
        print_bb(iseq, body->iseq_encoded, bbv->iseq_start, bbv->iseq_bound);
    }
    RTL_GEN_ASSERT(bbv->bbv_start < 0);
    br_p = dest2_p = FALSE;
    disp = disp2 = dest = dest2 = 0;
    VARR_TRUNC(VALUE, bbv_encoded, 0);
    VARR_TRUNC(char, labeled_p, 0);
    copy_attrs(bbv->attrs, bbv->n_attrs);
    VARR_TRUNC(int, stack, 0);
    VARR_TRUNC(int, pos_map, 0);
    while (init_stack_size > (int)VARR_LENGTH(int, stack))
        VARR_PUSH(int, stack, -1); /* stack temp definitions are out of block */
    RTL_GEN_ASSERT(bbv->iseq_bound < 0 || bbv->iseq_start < bbv->iseq_bound);
    for (curr_orig_insn_pos = bbv->iseq_start;;) {
        last_rtl_ind = (int)VARR_LENGTH(VALUE, bbv_encoded);
	insn_id = rb_vm_insn_addr2insn((void *)body->iseq_encoded[curr_orig_insn_pos]);
        len = insn_len(insn_id);
        operands = &body->iseq_encoded[curr_orig_insn_pos + 1];
        switch (insn_id) {
        case BIN(nop):
        case BIN(make_rtl_bbv):
        case BIN(jump2): /* nops not labeled by catch table will be removed */
            if (pos_in_catch_table_p (iseq, curr_orig_insn_pos)) {
                last_rtl_ind = add_insn(curr_orig_insn_pos, BIN(nop), 0);
                update_labeled(last_rtl_ind);
            }
            break;
        case BIN(opt_plus):
        case BIN(opt_minus):
        case BIN(opt_mult):
        case BIN(opt_div):
        case BIN(opt_mod):
        case BIN(opt_or):
        case BIN(opt_and):
        case BIN(opt_eq):
        case BIN(opt_neq):
        case BIN(opt_lt):
        case BIN(opt_gt):
        case BIN(opt_le):
        case BIN(opt_ge):
            stack_size = VARR_LENGTH(int, stack);
            RTL_GEN_ASSERT(stack_size >= 2);
            op3_attr = get_attr(get_temp_num(iseq, stack_size - 1));
            op2_attr = get_attr(get_temp_num(iseq, stack_size - 2));
            op3_insn_pos = VARR_LAST(int, stack);
            op2_insn_pos = VARR_GET(int, stack, stack_size - 2);
            get_op(op3_insn_pos, &local3_p, &local3, &val3);
            get_op(op2_insn_pos, &local2_p, &local2, NULL);
            if (op2_attr != op3_attr || op2_attr == ATTR_UNKNOWN) goto rest_cases;
            new_insn_id = get_rtl_insn2_id(insn_id, local2_p, local3_p, val3, op2_attr == ATTR_FIXNUM);
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
            last_rtl_ind = add_insn_arr(curr_orig_insn_pos, new_insn_id, nops, new_operands);
            if (local2_p)
                nullify(op2_insn_pos);
            if (local3_p || val3 != Qnil)
                nullify(op3_insn_pos);
            VARR_TRUNC (int, stack, stack_size - 2);
            attr = op2_attr;
            goto push;
        case BIN(opt_aref):
            stack_size = VARR_LENGTH(int, stack);
            RTL_GEN_ASSERT(stack_size >= 2);
            op3_attr = get_attr(get_temp_num(iseq, stack_size - 1));
            op3_insn_pos = VARR_LAST(int, stack);
            op2_insn_pos = VARR_GET(int, stack, stack_size - 2);
            get_op(op3_insn_pos, &local3_p, &local3, &val3);
            get_op(op2_insn_pos, &local2_p, &local2, NULL);
            if (op3_attr != ATTR_FIXNUM) goto rest_cases;
            new_insn_id = get_rtl_insn2_id(insn_id, local2_p, local3_p, val3, TRUE);
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
            last_rtl_ind = add_insn_arr(curr_orig_insn_pos, new_insn_id, nops, new_operands);
            if (local2_p)
                nullify(op2_insn_pos);
            if (local3_p || val3 != Qnil)
                nullify(op3_insn_pos);
            VARR_TRUNC (int, stack, stack_size - 2);
            attr = ATTR_UNKNOWN;
            goto push;
        case BIN(opt_aset):
            stack_size = VARR_LENGTH(int, stack);
            RTL_GEN_ASSERT(stack_size >= 3);
            if (bbv->iseq_bound >= 0 && curr_orig_insn_pos + len >= bbv->iseq_bound) goto rest_cases;
            next_insn_id = rb_vm_insn_addr2insn((void *)body->iseq_encoded[curr_orig_insn_pos + len]);
            if (next_insn_id != BIN(pop)) goto rest_cases;
            op2_attr = get_attr(get_temp_num(iseq, stack_size - 2));
            if (op2_attr != ATTR_FIXNUM) goto rest_cases;
            op_insn_pos = VARR_GET(int, stack, stack_size - 3);
            op2_insn_pos = VARR_GET(int, stack, stack_size - 2);
            op3_insn_pos = VARR_GET(int, stack, stack_size - 1);
            get_op(op_insn_pos, &local_p, &local, NULL);
            get_op(op2_insn_pos, &local2_p, &local2, &val2);
            get_op(op3_insn_pos, &local3_p, &local3, NULL);
            new_insn_id = get_rtl_insn3_id(insn_id, local_p, local2_p, local3_p, val2, TRUE);
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
                    new_operands[0] = local3;
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
            last_rtl_ind = add_insn_arr(curr_orig_insn_pos, new_insn_id, nops, new_operands);
            if (local_p)
                nullify(op_insn_pos);
            if (local2_p || val2 != Qnil)
                nullify(op2_insn_pos);
            if (local3_p)
                nullify(op3_insn_pos);
            len += insn_len(BIN(pop));
            VARR_TRUNC (int, stack, stack_size - 3); /* including pop */
            break;
        case BIN(jump): // ??? use leaf attr for dest calculations
            br_p = TRUE;
            dest = curr_orig_insn_pos + len + operands[0];
            if (!longer_paths_p || !follow_jumps_p) add_insn(curr_orig_insn_pos, BIN(jump2), 1, (VALUE)0);
            goto stack_update;
        case BIN(branchif):
        case BIN(branchunless):
            stack_size = (int)VARR_LENGTH(int, stack);
            if (stack_size == 0 || (op_insn_pos = VARR_LAST(int, stack)) < 0) {
                add_insn(curr_orig_insn_pos, BIN(branch2), 4, (VALUE)0, (VALUE)0, (VALUE)0, (VALUE)0);
            } else {
                new_insn_id = get_branch_rtl_insn_id ((enum ruby_vminsn_type)
                                                      VARR_GET(VALUE, bbv_encoded, op_insn_pos));
                if (new_insn_id == BIN(nop)) {
                    add_insn(curr_orig_insn_pos, BIN(branch2), 2, (VALUE)0, (VALUE)0);
                } else {
                    nops = insn_len(new_insn_id) - 1;
                    new_operands[0] = (VALUE)0;
                    new_operands[1] = (VALUE)0;
                    new_operands[nops - 2] = VARR_GET(VALUE, bbv_encoded, op_insn_pos + 1); /* ops[0] */
                    new_operands[nops - 1] = VARR_GET(VALUE, bbv_encoded, op_insn_pos + 2); /* ops[1] */
                    add_insn_arr(curr_orig_insn_pos, new_insn_id, nops, new_operands);
                    nullify(op_insn_pos);
                }
            }
            br_p = dest2_p = TRUE;
            if (insn_id == BIN(branchunless)) {
                dest2 = curr_orig_insn_pos + len + (OFFSET)operands[0];
                dest = curr_orig_insn_pos + len;
            } else {
                dest = curr_orig_insn_pos + len + (OFFSET)operands[0];
                dest2 = curr_orig_insn_pos + len;
            }
            goto stack_update;
        case BIN(branchnil):
            br_p = dest2_p = TRUE;
            dest = curr_orig_insn_pos + len + (OFFSET)operands[0];
            dest2 = curr_orig_insn_pos + len;
            add_insn(curr_orig_insn_pos, BIN(branchnil2), 2, (VALUE)0, (VALUE)0);
            goto stack_update;
        case BIN(opt_getinlinecache):
            br_p = dest2_p = TRUE;
            dest = curr_orig_insn_pos + len + (OFFSET)operands[0];
            dest2 = curr_orig_insn_pos + len;
            add_insn(curr_orig_insn_pos, BIN(opt_getinlinecache2), 3, (VALUE)0, (VALUE)0, operands[1]);
            goto stack_update;
        case BIN(setlocal):
            if (operands[1] != 0) goto rest_cases;
            /* fallthrough */
        case BIN(setlocal_WC_0):
            stack_size = (int)VARR_LENGTH(int, stack);
            RTL_GEN_ASSERT(stack_size > 0);
            attr = get_attr(get_temp_num(iseq, stack_size - 1));
            setup_attr(get_lvar_num(iseq, operands[0]), attr); /* attr of temp */
            if ((op_insn_pos = VARR_LAST(int, stack)) < 0) goto rest_cases;
            op_insn_id = (enum ruby_vminsn_type) VARR_GET(VALUE, bbv_encoded, op_insn_pos);
            new_insn_id = get_var_res_rtl_insn_id (op_insn_id);
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
                    goto stack_update;
                case BIN (getlocal):
                    if (operands[1] != 0) goto rest_cases;
                    /* fallthrough */
                case BIN(getlocal_WC_0):
                    add_insn(curr_orig_insn_pos, BIN(mov), 2, operands[0], VARR_GET(VALUE, bbv_encoded, op_insn_pos + 1));
                    nullify(op_insn_pos);
                    goto stack_update;
                default: goto rest_cases;
                }
            }
            nops = insn_len (new_insn_id) - 1;
            new_operands[0] = operands[0];
            if (nops > 1)
                new_operands[1] = VARR_GET(VALUE, bbv_encoded, op_insn_pos + 1); /* ops[0] */
            if (nops > 2)
                new_operands[2] = VARR_GET(VALUE, bbv_encoded, op_insn_pos + 2); /* ops[1] */
            add_insn_arr(VARR_GET(int, pos_map, op_insn_pos), new_insn_id, nops, new_operands);
            nullify(op_insn_pos);
            goto stack_update;
        case BIN(putobject):
            val = operands[0];
            attr = FIXNUM_P(val) ? ATTR_FIXNUM : FLONUM_P(val) ? ATTR_FLONUM : ATTR_UNKNOWN;
            goto add_and_push;
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
            add_insn_arr(curr_orig_insn_pos, insn_id, len - 1, operands);
        push:
            setup_attr(get_temp_num(iseq, VARR_LENGTH(int, stack)), attr); /* attr of temp */
            VARR_PUSH(int, stack, last_rtl_ind); /* insn producing temp */
            break;
        default:
        rest_cases:
            if (insn_id != BIN(gen_rtl_bbv) && insn_id != BIN(gen_rtl_bbv2))
                add_insn_arr(curr_orig_insn_pos, insn_id, len - 1, operands);
        stack_update:
            if (insn_id == BIN(send) || insn_id == BIN(opt_send_without_block) || insn_id == BIN(invokesuper)) { //???
                inc = sp_inc_of_sendish(((CALL_DATA)operands[0])->ci);
            } else {
                inc = comptime_insn_stack_increase_dispatch(insn_id, operands);
            }
            if (inc >= 0) {
                for (i = 0; i < inc; i++) {
                    setup_attr(get_temp_num(iseq, VARR_LENGTH(int, stack)), ATTR_UNKNOWN);
                    VARR_PUSH(int, stack, -1);
                }
            } else {
                RTL_GEN_ASSERT(-inc <= (int)VARR_LENGTH(int, stack));
                VARR_TRUNC(int, stack, (int)VARR_LENGTH(int, stack) + inc);
                truncate_attrs(get_temp_num(iseq, VARR_LENGTH(int, stack)));
            }
        }
        curr_orig_insn_pos += len;
        if (bbv->iseq_bound >= 0 && curr_orig_insn_pos >= bbv->iseq_bound) break;
        if (longer_paths_p && follow_jumps_p) {
            if (insn_id == BIN(jump))
                curr_orig_insn_pos = dest; /* follow jump destination */
            else if (non_rtl_bb_end_p(insn_id))
                break;
        }
    }
    last_rtl_ind = remove_nops();
    if (insn_id != BIN(leave)) {
        if (!br_p) {
            RTL_GEN_ASSERT(!longer_paths_p || !follow_jumps_p);
            last_rtl_ind = add_insn(curr_orig_insn_pos, BIN(jump2), 1, (VALUE)0);
            dest = bbv->iseq_bound;
        }
        compact_attrs();
        dest_bbv = get_bbv(iseq, dest, (int)VARR_LENGTH(char, attrs), VARR_ADDR(char, attrs));
        len = insn_len(BIN(gen_rtl_bbv));
        if ((dest_bbv_start = dest_bbv->bbv_start) < 0) { // ??? attr leaf
            if (dest_bbv == bbv) {
                disp = INT_MAX;
            } else {
                disp = add_insn(curr_orig_insn_pos, BIN(gen_rtl_bbv), 2, (VALUE)dest_bbv, (VALUE)0);
                VARR_SET(VALUE, bbv_encoded, disp + 2, (VALUE)(last_rtl_ind - (disp + len)));
            }
        }
        if (dest2_p) {
            len = insn_len(BIN(gen_rtl_bbv2));
            dest_bbv = get_bbv(iseq, dest2, (int)VARR_LENGTH(char, attrs), VARR_ADDR(char, attrs));
            if ((dest_bbv_start2 = dest_bbv->bbv_start) < 0) { /* no code yet */
                if (dest_bbv == bbv) {
                    disp2 = INT_MAX;
                } else {
                    disp2 = add_insn(curr_orig_insn_pos, BIN(gen_rtl_bbv2), 2, (VALUE)dest_bbv, (VALUE)0);
                    VARR_SET(VALUE, bbv_encoded, disp2 + 2, (VALUE)(last_rtl_ind - (disp2 + len)));
                }
            }
        }
    }
    len = (int)VARR_LENGTH(VALUE, bbv_encoded);
    bbv->bbv_start = get_bbv_code_space(iseq, len);
    memcpy(&body->bbv_code[bbv->bbv_start], VARR_ADDR(VALUE, bbv_encoded), len * sizeof(VALUE));
    memcpy(&body->bbv_pos_map[bbv->bbv_start], VARR_ADDR(int, pos_map), VARR_LENGTH(int, pos_map) * sizeof(int));
    if (insn_id != BIN(leave)) {
        insn_id = body->bbv_code[bbv->bbv_start + last_rtl_ind];
        offset = last_rtl_ind + insn_len(insn_id); /* offset of the branch end from zero */
        if (dest_bbv_start < 0) /* offset relative to the 1st gen_rtl_code */
            offset = disp == INT_MAX ? -offset : disp - offset;
        else
            offset = dest_bbv_start - (bbv->bbv_start + offset);
        body->bbv_code[bbv->bbv_start + last_rtl_ind + 1] = (VALUE)offset;
        if (dest2_p) {
            offset2 = last_rtl_ind + insn_len(insn_id); /* offset of the branch end from zero */
            if (dest_bbv_start2 < 0) /* offset relative to the 2nd gen_rtl_code */
                offset2 = disp2 == INT_MAX ? -offset2 : disp2 - offset2;
            else
                offset2 = dest_bbv_start2 - (bbv->bbv_start + offset2);
            body->bbv_code[bbv->bbv_start + last_rtl_ind + 2] = (VALUE)offset2;
        }
    }
    bbv_encode(&body->bbv_code[bbv->bbv_start], len);
    if (debug_p) {
        fprintf(stderr, "Specialized BBV:\n");
        print_bb(iseq, body->bbv_code, bbv->bbv_start, bbv->bbv_start + len);
        print_pos_map();
    }
}

int
rb_rtl_make_bbv(rb_control_frame_t *cfp) {
    VALUE *pc = (VALUE *)cfp->pc;
    struct rb_iseq_bbv *bbv = get_bbv((rb_iseq_t *)cfp->iseq, pc - cfp->iseq->body->iseq_encoded, 0, NULL);

    gen_rtl_bbv_code((rb_iseq_t *)cfp->iseq, bbv, cfp->sp - cfp->__bp__);
    /* change make_rtl_bbv to jump2rtl */
    RTL_GEN_ASSERT(insn_len(BIN(jump2rtl)) == 2 && insn_len(BIN(make_rtl_bbv)) == 2);
    pc[0] = (VALUE)dispatch_table[BIN(jump2rtl)];
    pc[1] = (VALUE)bbv->bbv_start;
    return bbv->bbv_start;
}

int
rb_rtl_gen_bbv(rb_control_frame_t *cfp, struct rb_iseq_bbv *bbv, int branch_offset, int first_p) {
    struct rb_iseq_constant_body *body = cfp->iseq->body;
    int gen_bbv_insn_pos = cfp->pc - body->bbv_code; /* position of gen_rtl_code end */
    enum ruby_vminsn_type branch_id = rb_vm_insn_decode(body->bbv_code[gen_bbv_insn_pos + branch_offset]);
    int branch_len = insn_len(branch_id);

    RTL_GEN_ASSERT(gen_bbv_insn_pos >= 0 && gen_bbv_insn_pos <= body->bbv_code_bound);
    gen_rtl_bbv_code((rb_iseq_t *)cfp->iseq, bbv, cfp->sp - cfp->__bp__);
    body->bbv_code[gen_bbv_insn_pos + branch_offset + (first_p ? 1 : 2)]
        = bbv->bbv_start - (gen_bbv_insn_pos + branch_offset + branch_len);
    return bbv->bbv_start;
}

void
rb_rtl_iseq_free(struct rb_iseq_constant_body *body) {
    struct rb_iseq_bbv *bbv, *curr_bbv, *next_bbv;
    int i;

    for (i = 0; i < body->n_bbvs; i++) {
        bbv = body->bbvs[i];
        for (curr_bbv = bbv; curr_bbv != NULL; curr_bbv = next_bbv) {
            next_bbv = curr_bbv->next_bbv;
            ruby_xfree((void *)curr_bbv);
        }
    }
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
    VARR_CREATE(char, attrs, 0);
    VARR_CREATE(char, labeled_p, 0);
    VARR_CREATE(int, pos_map, 0);
    VARR_CREATE(VALUE, bbv_encoded, 0);
    VARR_CREATE(rb_iseq_bbv_t, bbvs, 0);
    return TRUE;
}

/* Finish stack insns to RTL generator.  */
void
rb_rtl_gen_finish(void) {
    VARR_DESTROY(int, stack);
    VARR_DESTROY(char, attrs);
    VARR_DESTROY(char, labeled_p);
    VARR_DESTROY(int, pos_map);
    VARR_DESTROY(VALUE, bbv_encoded);
    VARR_DESTROY(rb_iseq_bbv_t, bbvs);
}
