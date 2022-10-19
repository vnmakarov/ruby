/**********************************************************************

  sir_gen.c - Generate SIR insns.

  Copyright (C) 2022 Vladimir Makarov vmakarov@redhat.com

**********************************************************************/

#include "sir.h"
#include "internal/hash.h"
#include "internal/compile.h"
#include "internal/variable.h"
#include "internal/class.h"
#include "vm_callinfo.h"
#include "vm_sync.h"

#define YJIT_STATS 0 /* used in vm_insnhelper.h */
#include "vm_insnhelper.h"

#include "builtin.h"
#include "insns.inc"
#include "insns_info.inc"

#include "mirjit.h"

/* To generate SIR insns we passes stack insns several times.  First
   we calculate possible stack values on the label.  It is a forward
   data flow problem (the final fixed point is only temporaries on the
   emulated stack).  Then using this info we actually generate SIR
   insns on the 2nd pass.

   We emulate VM stack to generate SIR insns for another SIR insn
   operands in a lazy way.  Therefore the order of SIR insns for
   calculating some simple operands can be different from
   corresponding stack insns.  */

static const int longer_paths_p = TRUE;
static const int follow_jumps_p = TRUE;

#define SIR_GEN_ASSERT(expr) RUBY_ASSERT_ALWAYS(expr)

/* Use nonzero to print debug info abou the generator.  */
#define SIR_GEN_DEBUG 0

/* Long jump buffer used to finish the generator work in case of a
   failure.  */
rb_jmpbuf_t sir_gen_jump_buf;

DEF_VARR(int);
/* The emulated VM stack: positions of insn producing stack slot values  */
static VARR(int) *stack;

DEF_VARR(char);
/* Always the same size as stack.  Flag of that input operands of the
   corresponding insn are changed and a combination of the insn and
   setlocal can not be issued on the setlocal place. */
static VARR(char) *operands_changed_p;

#define CONCAT(a, b) a ## b

#define OPT_NAME(name) BIN(opt_ ## name)
#define SIR_NAME(name) BIN(sir_ ## name)
#define SIR_NAME2(name, suf) BIN(sir_ ## name ## suf)

static enum ruby_vminsn_type
get_sir_insn2_id(enum ruby_vminsn_type id, int local2_p, VALUE *val2, int local3_p, VALUE val3) {
#define SIR_INSN_ID_CASE(insn_name)					\
    case OPT_NAME(insn_name):                                           \
        if (local2_p && local3_p) return SIR_NAME2(insn_name, svv);	\
        if (local2_p) {							\
            if (val3 != Qnil) return SIR_NAME2(insn_name, svi);		\
            return SIR_NAME2(insn_name, svs);				\
        }								\
        if (local3_p) {							\
            if (*val2 != Qnil) return SIR_NAME2(insn_name, siv);	\
            return SIR_NAME2(insn_name, ssv);				\
        }								\
        if (val3 != Qnil) {						\
           *val2 = Qnil;						\
           return SIR_NAME2(insn_name, ssi);				\
        }								\
        if (*val2 != Qnil) return SIR_NAME2(insn_name, sis);		\
        return BIN(nop);

    switch (id) {
    SIR_INSN_ID_CASE(plus)
    SIR_INSN_ID_CASE(minus)
    SIR_INSN_ID_CASE(mult)
    SIR_INSN_ID_CASE(div)
    SIR_INSN_ID_CASE(mod)
    SIR_INSN_ID_CASE(eq)
    SIR_INSN_ID_CASE(neq)
    SIR_INSN_ID_CASE(lt)
    SIR_INSN_ID_CASE(gt)
    SIR_INSN_ID_CASE(le)
    SIR_INSN_ID_CASE(ge)
    SIR_INSN_ID_CASE(or)
    SIR_INSN_ID_CASE(and)
    SIR_INSN_ID_CASE(aref)
    default: return BIN(nop);
    }
}

static int
check_bop_redefined_p(enum ruby_basic_operators bop, int klass_flag, short *bop_mask) {
    if (!BASIC_OP_UNREDEFINED_P(bop, klass_flag)) return TRUE;
    bop_mask[bop] |= klass_flag;
    return FALSE;
}

static enum ruby_vminsn_type
get_sir_spec_insn2_id(enum ruby_vminsn_type id, int attr, short *bop_mask, int redefined_flag, int *exch_p) {
#define SIR_SPEC_INSN_ID_CASE0(bop, flag, insn_name, fname)                   					         \
    case OPT_NAME(insn_name): return check_bop_redefined_p(bop, flag, bop_mask) ? BIN(nop) : SIR_NAME2(fname, sss);      \
    case SIR_NAME2(insn_name, svv): return check_bop_redefined_p(bop, flag, bop_mask) ? BIN(nop) : SIR_NAME2(fname, svv);\
    case SIR_NAME2(insn_name, svi): return check_bop_redefined_p(bop, flag, bop_mask) ? BIN(nop) : SIR_NAME2(fname, svi);\
    case SIR_NAME2(insn_name, svs): return check_bop_redefined_p(bop, flag, bop_mask) ? BIN(nop) : SIR_NAME2(fname, svs);\
    case SIR_NAME2(insn_name, ssv): return check_bop_redefined_p(bop, flag, bop_mask) ? BIN(nop) : SIR_NAME2(fname, ssv);\
    case SIR_NAME2(insn_name, ssi): return check_bop_redefined_p(bop, flag, bop_mask) ? BIN(nop) : SIR_NAME2(fname, ssi);
#define SIR_SPEC_INSN_ID_CASE1(bop, flag, insn_name, fname)               					         \
    SIR_SPEC_INSN_ID_CASE0(bop, flag, insn_name, fname)                   					         \
    case SIR_NAME2(insn_name, siv): return check_bop_redefined_p(bop, flag, bop_mask) ? BIN(nop) : SIR_NAME2(fname, siv);\
    case SIR_NAME2(insn_name, sis): return check_bop_redefined_p(bop, flag, bop_mask) ? BIN(nop) : SIR_NAME2(fname, sis);
#define SIR_SPEC_INSN_ID_CASE2(bop, flag, insn_name, fname, exch_fname)               				  \
    SIR_SPEC_INSN_ID_CASE0(bop, flag, insn_name, fname)                						  \
    case SIR_NAME2(insn_name, siv):					  					  \
        *exch_p = TRUE; return check_bop_redefined_p(bop, flag, bop_mask) ? BIN(nop) : SIR_NAME2(exch_fname, svi);\
    case SIR_NAME2(insn_name, sis):							   		   	  \
        *exch_p = TRUE; return check_bop_redefined_p(bop, flag, bop_mask) ? BIN(nop) : SIR_NAME2(exch_fname, ssi);

    *exch_p = FALSE;
    if (attr == ATTR_ARRAY) {
        switch (id) {
        SIR_SPEC_INSN_ID_CASE0(BOP_AREF, ARRAY_REDEFINED_OP_FLAG, aref, iaref)
        default: return BIN(nop);
        }
    } else if (attr == ATTR_FIXNUM) {
        switch (id) {
        SIR_SPEC_INSN_ID_CASE1(BOP_PLUS, redefined_flag, plus, iplus)
        SIR_SPEC_INSN_ID_CASE1(BOP_MINUS, redefined_flag, minus, iminus)
        SIR_SPEC_INSN_ID_CASE1(BOP_MULT, redefined_flag, mult, imult)
        SIR_SPEC_INSN_ID_CASE1(BOP_DIV, redefined_flag, div, idiv)
        SIR_SPEC_INSN_ID_CASE1(BOP_MOD, redefined_flag, mod, imod)
        SIR_SPEC_INSN_ID_CASE2(BOP_OR, redefined_flag, or, ior, ior)
        SIR_SPEC_INSN_ID_CASE2(BOP_AND, redefined_flag, and, iand, iand)
        SIR_SPEC_INSN_ID_CASE2(BOP_EQ, redefined_flag, eq, ieq, ieq)
        SIR_SPEC_INSN_ID_CASE2(BOP_NEQ, redefined_flag, neq, ineq, ineq)
        SIR_SPEC_INSN_ID_CASE2(BOP_LT, redefined_flag, lt, ilt, ige)
        SIR_SPEC_INSN_ID_CASE2(BOP_GT, redefined_flag, gt, igt, ile)
        SIR_SPEC_INSN_ID_CASE2(BOP_LE, redefined_flag, le, ile, igt)
        SIR_SPEC_INSN_ID_CASE2(BOP_GE, redefined_flag, ge, ige, ilt)
        SIR_SPEC_INSN_ID_CASE0(BOP_AREF, ARRAY_REDEFINED_OP_FLAG, aref, iaref)
        default: return BIN(nop);
        }
    } else if (attr == ATTR_FLONUM) {
#ifdef FLONUM_INSNS
        switch (id) {
        SIR_SPEC_INSN_ID_CASE1(BOP_PLUS, redefined_flag, plus, fplus)
        SIR_SPEC_INSN_ID_CASE1(BOP_MINUS, redefined_flag, minus, fminus)
        SIR_SPEC_INSN_ID_CASE1(BOP_MULT, redefined_flag, mult, fmult)
        SIR_SPEC_INSN_ID_CASE1(BOP_DIV, redefined_flag, div, fdiv)
        SIR_SPEC_INSN_ID_CASE1(BOP_MOD, redefined_flag, mod, fmod)
        SIR_SPEC_INSN_ID_CASE1(BOP_EQ, redefined_flag, eq, feq)
        SIR_SPEC_INSN_ID_CASE1(BOP_NEQ, redefined_flag, neq, fneq)
        SIR_SPEC_INSN_ID_CASE1(BOP_LT, redefined_flag, lt, flt)
        SIR_SPEC_INSN_ID_CASE1(BOP_GT, redefined_flag, gt, fgt)
        SIR_SPEC_INSN_ID_CASE1(BOP_LE, redefined_flag, le, fle)
        SIR_SPEC_INSN_ID_CASE1(BOP_GE, redefined_flag, ge, fge)
        default: return BIN(nop);
        }
#endif
    }
    return BIN(nop);
}

static enum ruby_vminsn_type
get_sir_insn3_id(enum ruby_vminsn_type id, int local_p, int local2_p, int local3_p, VALUE val2) {
#define SIR_INSN3_ID_CASE(insn_name)					\
    case OPT_NAME(insn_name):                                   	\
    if (local_p) {							\
      if (local2_p && local3_p) return SIR_NAME2(insn_name, vvv);       \
      if (local3_p) {							\
        if (val2 == Qnil) return SIR_NAME2(insn_name, vsv);             \
        return SIR_NAME2(insn_name, viv);				\
      }                                                                 \
      if (local2_p) return SIR_NAME2(insn_name, vvs);                   \
      if (val2 != Qnil) return SIR_NAME2(insn_name, vis);		\
    } else {								\
      if (local2_p && local3_p) return SIR_NAME2(insn_name, svv);       \
      if (local3_p) {							\
        if (val2 == Qnil) return SIR_NAME2(insn_name, ssv);             \
        return SIR_NAME2(insn_name, siv);				\
      }                                                                 \
      if (local2_p) return SIR_NAME2(insn_name, svs);                   \
      if (val2 != Qnil) return SIR_NAME2(insn_name, sis);		\
    }									\
    return BIN(nop);

    switch (id) {
    SIR_INSN3_ID_CASE(aset)
    default: return BIN(nop);
    }
}

static enum ruby_vminsn_type
get_sir_spec_insn3_id(enum ruby_vminsn_type id, int attr1, int attr2, short *bop_mask) {
#define SIR_SPEC_INSN3_ID_CASE(bop, flag, insn_name, fname)                   					         \
    case OPT_NAME(insn_name): return check_bop_redefined_p(bop, flag, bop_mask) ? BIN(nop) : SIR_NAME2(fname, sss);	 \
    case SIR_NAME2(insn_name, svv): return check_bop_redefined_p(bop, flag, bop_mask) ? BIN(nop) : SIR_NAME2(fname, svv);\
    case SIR_NAME2(insn_name, siv): return check_bop_redefined_p(bop, flag, bop_mask) ? BIN(nop) : SIR_NAME2(fname, siv);\
    case SIR_NAME2(insn_name, svs): return check_bop_redefined_p(bop, flag, bop_mask) ? BIN(nop) : SIR_NAME2(fname, svs);\
    case SIR_NAME2(insn_name, ssv): return check_bop_redefined_p(bop, flag, bop_mask) ? BIN(nop) : SIR_NAME2(fname, ssv);\
    case SIR_NAME2(insn_name, sis): return check_bop_redefined_p(bop, flag, bop_mask) ? BIN(nop) : SIR_NAME2(fname, sis);\
    case SIR_NAME2(insn_name, vss): return check_bop_redefined_p(bop, flag, bop_mask) ? BIN(nop) : SIR_NAME2(fname, vss);\
    case SIR_NAME2(insn_name, vvv): return check_bop_redefined_p(bop, flag, bop_mask) ? BIN(nop) : SIR_NAME2(fname, vvv);\
    case SIR_NAME2(insn_name, viv): return check_bop_redefined_p(bop, flag, bop_mask) ? BIN(nop) : SIR_NAME2(fname, viv);\
    case SIR_NAME2(insn_name, vvs): return check_bop_redefined_p(bop, flag, bop_mask) ? BIN(nop) : SIR_NAME2(fname, vvs);\
    case SIR_NAME2(insn_name, vsv): return check_bop_redefined_p(bop, flag, bop_mask) ? BIN(nop) : SIR_NAME2(fname, vsv);\
    case SIR_NAME2(insn_name, vis): return check_bop_redefined_p(bop, flag, bop_mask) ? BIN(nop) : SIR_NAME2(fname, vis);

    if (attr1 == ATTR_ARRAY && attr2 == ATTR_FIXNUM) {
        switch (id) {
        SIR_SPEC_INSN3_ID_CASE(BOP_ASET, ARRAY_REDEFINED_OP_FLAG, aset, iaset)
        default: return BIN(nop);
        }
    }
    return BIN(nop);
}

static enum ruby_vminsn_type
get_var_res_sir_insn_id(enum ruby_vminsn_type id) {
#define VAR_RES_SIR_INSN_ID_CASE0(insn_name)				\
    case SIR_NAME2(insn_name, sss): return SIR_NAME2(insn_name, vss);	\
    case SIR_NAME2(insn_name, svv): return SIR_NAME2(insn_name, vvv);	\
    case SIR_NAME2(insn_name, svi): return SIR_NAME2(insn_name, vvi);	\
    case SIR_NAME2(insn_name, svs): return SIR_NAME2(insn_name, vvs);	\
    case SIR_NAME2(insn_name, ssv): return SIR_NAME2(insn_name, vsv);	\
    case SIR_NAME2(insn_name, ssi): return SIR_NAME2(insn_name, vsi);

#define VAR_RES_SIR_INSN_ID_CASE(insn_name)				\
    VAR_RES_SIR_INSN_ID_CASE0(insn_name)				\
    case SIR_NAME2(insn_name, siv): return SIR_NAME2(insn_name, viv);	\
    case SIR_NAME2(insn_name, sis): return SIR_NAME2(insn_name, vis);

    switch (id) {
    VAR_RES_SIR_INSN_ID_CASE(iplus)
    VAR_RES_SIR_INSN_ID_CASE(iminus)
    VAR_RES_SIR_INSN_ID_CASE(imult)
    VAR_RES_SIR_INSN_ID_CASE(idiv)
    VAR_RES_SIR_INSN_ID_CASE(imod)
    VAR_RES_SIR_INSN_ID_CASE0(ior)
    VAR_RES_SIR_INSN_ID_CASE0(iand)
    VAR_RES_SIR_INSN_ID_CASE0(ieq)
    VAR_RES_SIR_INSN_ID_CASE0(ineq)
    VAR_RES_SIR_INSN_ID_CASE0(ilt)
    VAR_RES_SIR_INSN_ID_CASE0(igt)
    VAR_RES_SIR_INSN_ID_CASE0(ile)
    VAR_RES_SIR_INSN_ID_CASE0(ige)
    VAR_RES_SIR_INSN_ID_CASE(feq)
    VAR_RES_SIR_INSN_ID_CASE(fneq)
    VAR_RES_SIR_INSN_ID_CASE(flt)
    VAR_RES_SIR_INSN_ID_CASE(fgt)
    VAR_RES_SIR_INSN_ID_CASE(fle)
    VAR_RES_SIR_INSN_ID_CASE(fge)
    default: return BIN(nop);
    }
}

static enum ruby_vminsn_type
get_branch_sir_insn_id(enum ruby_vminsn_type cmp_id) {
#define BRANCH_SIR_INSN_ID_CASE(cmp_name, br_name)			\
    case SIR_NAME2(cmp_name, sss): return SIR_NAME2(br_name, ss);	\
    case SIR_NAME2(cmp_name, svv): return SIR_NAME2(br_name, vv);	\
    case SIR_NAME2(cmp_name, svi): return SIR_NAME2(br_name, vi);	\
    case SIR_NAME2(cmp_name, svs): return SIR_NAME2(br_name, vs);	\
    case SIR_NAME2(cmp_name, ssv): return SIR_NAME2(br_name, sv);	\
    case SIR_NAME2(cmp_name, ssi): return SIR_NAME2(br_name, si);

    switch (cmp_id) {
    BRANCH_SIR_INSN_ID_CASE(ieq, ibeq)
    BRANCH_SIR_INSN_ID_CASE(ineq, ibneq)
    BRANCH_SIR_INSN_ID_CASE(ilt, iblt)
    BRANCH_SIR_INSN_ID_CASE(igt, ibgt)
    BRANCH_SIR_INSN_ID_CASE(ile, ible)
    BRANCH_SIR_INSN_ID_CASE(ige, ibge)
    default: return BIN(nop);
    }
}

static enum ruby_vminsn_type
get_sir_speculative_insn2_id(enum ruby_vminsn_type id, int attr, short *bop_mask, int *exch_p) {
#define SIR_SPECULATIVE_INSN_ID_CASE0(bop, flag, insn_name, fname)              				         \
    case OPT_NAME(insn_name): return check_bop_redefined_p(bop, flag, bop_mask) ? BIN(nop) : SIR_NAME2(fname, sss);      \
    case SIR_NAME2(insn_name, svv): return check_bop_redefined_p(bop, flag, bop_mask) ? BIN(nop) : SIR_NAME2(fname, svv);\
    case SIR_NAME2(insn_name, svi): return check_bop_redefined_p(bop, flag, bop_mask) ? BIN(nop) : SIR_NAME2(fname, svi);\
    case SIR_NAME2(insn_name, svs): return check_bop_redefined_p(bop, flag, bop_mask) ? BIN(nop) : SIR_NAME2(fname, svs);\
    case SIR_NAME2(insn_name, ssv): return check_bop_redefined_p(bop, flag, bop_mask) ? BIN(nop) : SIR_NAME2(fname, ssv);\
    case SIR_NAME2(insn_name, ssi): return check_bop_redefined_p(bop, flag, bop_mask) ? BIN(nop) : SIR_NAME2(fname, ssi);
#define SIR_SPECULATIVE_INSN_ID_CASE1(bop, flag, insn_name, fname)              				         \
    SIR_SPECULATIVE_INSN_ID_CASE0(bop, flag, insn_name, fname)                  				         \
    case SIR_NAME2(insn_name, siv): return check_bop_redefined_p(bop, flag, bop_mask) ? BIN(nop) : SIR_NAME2(fname, siv);\
    case SIR_NAME2(insn_name, sis): return check_bop_redefined_p(bop, flag, bop_mask) ? BIN(nop) : SIR_NAME2(fname, sis);
#define SIR_SPECULATIVE_INSN_ID_CASE2(bop, flag, insn_name, fname, exch_fname)         				  \
    SIR_SPECULATIVE_INSN_ID_CASE0(bop, flag, insn_name, fname)                					  \
    case SIR_NAME2(insn_name, siv):					  					  \
        *exch_p = TRUE; return check_bop_redefined_p(bop, flag, bop_mask) ? BIN(nop) : SIR_NAME2(exch_fname, svi);\
    case SIR_NAME2(insn_name, sis):							   		   	  \
        *exch_p = TRUE; return check_bop_redefined_p(bop, flag, bop_mask) ? BIN(nop) : SIR_NAME2(exch_fname, ssi);

    *exch_p = FALSE;
    if (attr == ATTR_ARRAY) {
        switch (id) {
        SIR_SPECULATIVE_INSN_ID_CASE1(BOP_AREF, ARRAY_REDEFINED_OP_FLAG, aref, siaref)
        default: return BIN(nop);
        }
    } else if (attr == ATTR_FIXNUM) {
        switch (id) {
        SIR_SPECULATIVE_INSN_ID_CASE1(BOP_PLUS, INTEGER_REDEFINED_OP_FLAG, plus, siplus)
        SIR_SPECULATIVE_INSN_ID_CASE1(BOP_MINUS, INTEGER_REDEFINED_OP_FLAG, minus, siminus)
        SIR_SPECULATIVE_INSN_ID_CASE1(BOP_MULT, INTEGER_REDEFINED_OP_FLAG, mult, simult)
        SIR_SPECULATIVE_INSN_ID_CASE1(BOP_DIV, INTEGER_REDEFINED_OP_FLAG, div, sidiv)
        SIR_SPECULATIVE_INSN_ID_CASE1(BOP_MOD, INTEGER_REDEFINED_OP_FLAG, mod, simod)
        SIR_SPECULATIVE_INSN_ID_CASE2(BOP_OR, INTEGER_REDEFINED_OP_FLAG, or, sior, sior)
        SIR_SPECULATIVE_INSN_ID_CASE2(BOP_AND, INTEGER_REDEFINED_OP_FLAG, and, siand, siand)
        SIR_SPECULATIVE_INSN_ID_CASE2(BOP_EQ, INTEGER_REDEFINED_OP_FLAG, eq, sieq, sieq)
        SIR_SPECULATIVE_INSN_ID_CASE2(BOP_NEQ, INTEGER_REDEFINED_OP_FLAG, neq, sineq, sineq)
        SIR_SPECULATIVE_INSN_ID_CASE2(BOP_LT, INTEGER_REDEFINED_OP_FLAG, lt, silt, sige)
        SIR_SPECULATIVE_INSN_ID_CASE2(BOP_GT, INTEGER_REDEFINED_OP_FLAG, gt, sigt, sile)
        SIR_SPECULATIVE_INSN_ID_CASE2(BOP_LE, INTEGER_REDEFINED_OP_FLAG, le, sile, sigt)
        SIR_SPECULATIVE_INSN_ID_CASE2(BOP_GE, INTEGER_REDEFINED_OP_FLAG, ge, sige, silt)
        default: return BIN(nop);
        }
    } else if (attr == ATTR_FLONUM) {
#ifdef FLONUM_INSNS
        switch (id) {
        SIR_SPECULATIVE_INSN_ID_CASE1(BOP_PLUS, FLOAT_REDEFINED_OP_FLAG, plus, sfplus)
        SIR_SPECULATIVE_INSN_ID_CASE1(BOP_MINUS, FLOAT_REDEFINED_OP_FLAG, minus, sfminus)
        SIR_SPECULATIVE_INSN_ID_CASE1(BOP_MULT, FLOAT_REDEFINED_OP_FLAG, mult, sfmult)
        SIR_SPECULATIVE_INSN_ID_CASE1(BOP_DIV, FLOAT_REDEFINED_OP_FLAG, div, sfdiv)
        SIR_SPECULATIVE_INSN_ID_CASE1(BOP_MOD, FLOAT_REDEFINED_OP_FLAG, mod, sfmod)
        SIR_SPECULATIVE_INSN_ID_CASE1(BOP_EQ, FLOAT_REDEFINED_OP_FLAG, eq, sfeq)
        SIR_SPECULATIVE_INSN_ID_CASE1(BOP_NEQ, FLOAT_REDEFINED_OP_FLAG, neq, sfneq)
        SIR_SPECULATIVE_INSN_ID_CASE1(BOP_LT, FLOAT_REDEFINED_OP_FLAG, lt, sflt)
        SIR_SPECULATIVE_INSN_ID_CASE1(BOP_GT, FLOAT_REDEFINED_OP_FLAG, gt, sfgt)
        SIR_SPECULATIVE_INSN_ID_CASE1(BOP_LE, FLOAT_REDEFINED_OP_FLAG, le, sfle)
        SIR_SPECULATIVE_INSN_ID_CASE1(BOP_GE, FLOAT_REDEFINED_OP_FLAG, ge, sfge)
        default: return BIN(nop);
        }
#endif
    }
    return BIN(nop);
}

static enum ruby_vminsn_type
get_sir_speculative_insn3_id(enum ruby_vminsn_type id, int attr1, int attr2, short *bop_mask) {
#define SIR_SPEC_INSN3_ID_CASE(bop, flag, insn_name, fname)                   					         \
    case OPT_NAME(insn_name): return check_bop_redefined_p(bop, flag, bop_mask) ? BIN(nop) : SIR_NAME2(fname, sss);	 \
    case SIR_NAME2(insn_name, svv): return check_bop_redefined_p(bop, flag, bop_mask) ? BIN(nop) : SIR_NAME2(fname, svv);\
    case SIR_NAME2(insn_name, siv): return check_bop_redefined_p(bop, flag, bop_mask) ? BIN(nop) : SIR_NAME2(fname, siv);\
    case SIR_NAME2(insn_name, svs): return check_bop_redefined_p(bop, flag, bop_mask) ? BIN(nop) : SIR_NAME2(fname, svs);\
    case SIR_NAME2(insn_name, ssv): return check_bop_redefined_p(bop, flag, bop_mask) ? BIN(nop) : SIR_NAME2(fname, ssv);\
    case SIR_NAME2(insn_name, sis): return check_bop_redefined_p(bop, flag, bop_mask) ? BIN(nop) : SIR_NAME2(fname, sis);\
    case SIR_NAME2(insn_name, vss): return check_bop_redefined_p(bop, flag, bop_mask) ? BIN(nop) : SIR_NAME2(fname, vss);\
    case SIR_NAME2(insn_name, vvv): return check_bop_redefined_p(bop, flag, bop_mask) ? BIN(nop) : SIR_NAME2(fname, vvv);\
    case SIR_NAME2(insn_name, viv): return check_bop_redefined_p(bop, flag, bop_mask) ? BIN(nop) : SIR_NAME2(fname, viv);\
    case SIR_NAME2(insn_name, vvs): return check_bop_redefined_p(bop, flag, bop_mask) ? BIN(nop) : SIR_NAME2(fname, vvs);\
    case SIR_NAME2(insn_name, vsv): return check_bop_redefined_p(bop, flag, bop_mask) ? BIN(nop) : SIR_NAME2(fname, vsv);\
    case SIR_NAME2(insn_name, vis): return check_bop_redefined_p(bop, flag, bop_mask) ? BIN(nop) : SIR_NAME2(fname, vis);

    if (attr1 == ATTR_ARRAY && attr2 == ATTR_FIXNUM) {
        switch (id) {
        SIR_SPEC_INSN3_ID_CASE(BOP_ASET, ARRAY_REDEFINED_OP_FLAG, aset, siaset)
        default: return BIN(nop);
        }
    }
    return BIN(nop);
}

static int
get_temp_num(rb_iseq_t *iseq, int n) { /* n is 0, 1, ... -- 0 corresponds stack bottom */
    return n + iseq->body->local_table_size;
}

static int
get_lvar_num(rb_iseq_t *iseq, VALUE op) {
    int idx = (int)op;
    /* make 1st arg having zero number: */
    if (idx < 0) idx = -idx;
    int res = (int)iseq->body->local_table_size - (idx - VM_ENV_DATA_SIZE) - 1;
    SIR_GEN_ASSERT(0 <= res && res < (int)iseq->body->local_table_size);
    return res;
}

static VARR(char) *attrs;
static VARR(char) *arg_attrs;

static void
check_final_attrs(int n_attrs, char *attrs) {
    assert(ATTR_END_MARKER == 0); /* to use strcmp/strlen functions */
    SIR_GEN_ASSERT(attrs == NULL
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
            SIR_GEN_ASSERT(VARR_GET(char, attributes, i) != ATTR_END_MARKER);
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
    SIR_GEN_ASSERT(len >= 0);
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
    SIR_GEN_ASSERT(stack_size >= argc);
    start = get_temp_num(iseq, stack_size - argc);
    last_i = -1;
    for (i = 0; i < argc && i + start < (int)VARR_LENGTH(char, attrs); i++)
        if (VARR_GET(char, attrs, i + start) != ATTR_UNKNOWN)
            last_i = i;
    if (last_i < 0) return NULL;
    call_attrs = SIR_ALLOC_N(char, last_i + 2);
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
                fprintf (stderr, " %d:%s", i,
                         (attrs[i] == ATTR_FIXNUM ? "fix" : attrs[i] == ATTR_FLONUM
                          ? "flo" : attrs[i] == ATTR_BOOL ? "bool" : "arr"));
    }
    fprintf(stderr, "\n");
}

typedef struct rb_iseq_bbv *rb_iseq_bbv_t;

DEF_VARR(rb_iseq_bbv_t);
static VARR(rb_iseq_bbv_t) *all_bbvs;

static int
get_bbv_nums(rb_iseq_bbv_t head) {
    int n = 0;
    rb_iseq_bbv_t curr;

    SIR_GEN_ASSERT(head->attrs[0] == ATTR_END_MARKER);
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
    SIR_GEN_ASSERT(FALSE);
    return NULL;
}

static rb_iseq_bbv_t
find_bbv(rb_iseq_bbv_t head, int env_p, int n_attrs, char *attrs) {
    rb_iseq_bbv_t curr;

    check_final_attrs(n_attrs, attrs);
    for (curr = head->next_bbv; curr != NULL; curr = curr->next_bbv) {
        if (curr->env_p != env_p)
            continue;
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

#define BBV_COUNT_TO_PROFILE_SPECIALIZE 3
#define BBV_COUNT_TO_JIT 10

static rb_iseq_bbv_t
create_bbv(rb_iseq_t *iseq, enum rb_bbv_type type, int index, int start, int bound,
           int env_p, int n_attrs, char *attrs) {
    rb_iseq_bbv_t bbv;

    check_final_attrs(n_attrs, attrs);
    bbv = (rb_iseq_bbv_t)SIR_ALLOC_N(char, sizeof(struct rb_iseq_bbv) + (attrs == NULL ? 0 : n_attrs - 1));
    bbv->spec_count = BBV_COUNT_TO_PROFILE_SPECIALIZE;
    bbv->jit_count = BBV_COUNT_TO_JIT;
    SIR_GEN_ASSERT(BBV_COUNT_TO_PROFILE_SPECIALIZE + 1 < BBV_COUNT_TO_JIT);
    bbv->iseq = iseq;
    bbv->cme = NULL;
    bbv->iseq_start = start;
    bbv->iseq_bound = bound;
    bbv->type = type;
    bbv->env_p = env_p;
    bbv->index = index;
    bbv->next_bbv = NULL;
    bbv->head_bbv = NULL;
    bbv->bbv_start = -1; /* flag of the code is not generated */
    bbv->start_stack_size = -1;
    bbv->in_edges = bbv->out_edges = NULL;
    bbv->call_elems = NULL;
    if (attrs == NULL) {
        bbv->attrs[0] = ATTR_END_MARKER;
    } else {
        memcpy(bbv->attrs, attrs, n_attrs);
    }
    memset(bbv->used_bops, 0, sizeof(bbv->used_bops));
    bbv->num = (int)VARR_LENGTH(rb_iseq_bbv_t, all_bbvs);
    VARR_PUSH(rb_iseq_bbv_t, all_bbvs, bbv);
    return bbv;
}

static rb_iseq_bbv_t
add_new_bbv(rb_iseq_t *iseq, rb_iseq_bbv_t head, int start, int bound, int env_p, int n_attrs, char *attrs) {
    rb_iseq_bbv_t bbv = create_bbv(iseq, head->type, head->index, start, bound, env_p, n_attrs, attrs);

    bbv->next_bbv = head->next_bbv;
    head->next_bbv = bbv;
    bbv->head_bbv = head;
    return bbv;
}

#define MAX_BBV_NUM 16

static rb_iseq_bbv_t
get_bbv(rb_iseq_t *iseq, enum rb_bbv_type type, int index, int start, int env_p, int n_attrs, char *attrs) {
    rb_iseq_bbv_t bbv, head_bbv = get_bbv_list_head(iseq, type, index, start);

    if ((bbv = find_bbv(head_bbv, env_p, n_attrs, attrs)) != NULL) return bbv;
    if (get_bbv_nums(head_bbv) >= MAX_BBV_NUM) return head_bbv;
    bbv = add_new_bbv(iseq, head_bbv, start, head_bbv->iseq_bound, env_p, n_attrs, attrs);
    return bbv;
}

static rb_iseq_bbv_t
get_bbv_by_head(rb_iseq_t *iseq, rb_iseq_bbv_t head, int env_p, int n_attrs, char *attrs) {
    rb_iseq_bbv_t bbv;

    SIR_GEN_ASSERT(head != NULL && head->head_bbv == NULL);
    if ((bbv = find_bbv(head, env_p, n_attrs, attrs)) != NULL) return bbv;
    if (get_bbv_nums(head) >= MAX_BBV_NUM) return head;
    bbv = add_new_bbv(iseq, head, head->iseq_start, head->iseq_bound, env_p, n_attrs, attrs);
    return bbv;
}


static int
non_sir_bb_end_p(enum ruby_vminsn_type insn_id) {
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
    struct rb_iseq_bbv_edge *e = (struct rb_iseq_bbv_edge *)SIR_ALLOC_N(struct rb_iseq_bbv_edge, 1);

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
        body->bbv_code_bound = (bbv_start + add_len) * 100 / 2;
        old_bbv_code = body->bbv_code;
        SIR_REALLOC_N(body->bbv_code, VALUE, body->bbv_code_bound);
        SIR_REALLOC_N(body->bbv_pos_map, int, body->bbv_code_bound);
        if (old_bbv_code != NULL && body->bbv_code != old_bbv_code) {
            fprintf(stderr, "==> Repositioning bbv_code\n");
            abort();
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

typedef struct rb_sir_send_info *send_info_t;
DEF_VARR(send_info_t);
static VARR(send_info_t) *iseq_send_info;

static struct rb_sir_send_info *
get_send_info(rb_iseq_bbv_t caller_bbv, rb_iseq_t *block_iseq, char *attrs,
              VALUE klass, rb_callable_method_entry_t *cme, int argc, ID mid) {
    struct rb_sir_send_info *si = SIR_ALLOC_N(struct rb_sir_send_info, 1);
    si->caller_bbv = caller_bbv;
    si->block_iseq = block_iseq;
    si->attrs = attrs;
    si->klass = klass;
    si->cme = cme;
    si->argc = argc;
    si->mid = mid;
    VARR_PUSH(send_info_t, iseq_send_info, si);
    RB_OBJ_WRITTEN(caller_bbv->iseq, Qundef, klass);
    RB_OBJ_WRITTEN(caller_bbv->iseq, Qundef, cme);
    return si;
}

static void
init_send_info(void) {
    VARR_CREATE(send_info_t, iseq_send_info, 512);
}

static void
finish_send_info(void) {
    VARR_DESTROY(send_info_t, iseq_send_info);
}

DEF_VARR(VALUE);
static VARR(VALUE) *bbv_encoded;

static VARR(int) *pos_map;

static void
update_pos_map(int pos, int orig_pos, int len) {
    int i;

    while ((int)VARR_LENGTH(int, pos_map) < pos + len)
        VARR_PUSH(int, pos_map, orig_pos < 0 ? 0 : -1);
    VARR_SET(int, pos_map, pos, orig_pos);
    /* increase original position as line number is calculated from stack pc - 1. */
    for (i = 1; i < len; i++)
        VARR_SET(int, pos_map, pos + i, orig_pos + 1);
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

    va_start(va, nops);
    start = (int)VARR_LENGTH(VALUE, bbv_encoded);
    VARR_PUSH(VALUE, bbv_encoded, (VALUE)insn_id);
    for (i = 0; i < nops; i++) {
        VARR_PUSH(VALUE, bbv_encoded, va_arg(va, VALUE));
    }
    va_end(va);
    update_pos_map(start, orig_pos, nops + 1);
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
    update_pos_map(start, orig_pos, nops + 1);
    return start;
}

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
    SIR_GEN_ASSERT(*pos >= 0);
    update_labeled(*pos);
    return ST_CONTINUE;
}

const void * const *sir_dispatch_table;

static void
bbv_encode(VALUE *bbv_encoded, int len) {
    int i;
    enum ruby_vminsn_type insn_id;

    for (i = 0; i < len; i += insn_len(insn_id)) {
	insn_id = (enum ruby_vminsn_type)bbv_encoded[i];
        bbv_encoded[i] = (VALUE)sir_dispatch_table[insn_id];
    }
}

static int
sir_insn_trace_update(const rb_iseq_t *iseq, VALUE *pc) {
    const struct rb_iseq_constant_body *const body = iseq->body;
    int stack_pos;
    const VALUE *stack_pc;
    rb_insn_data_t *e;

    e = rb_get_insn_data(pc);
    stack_pc = rb_iseq_encoded_pc(iseq, pc);
    VM_ASSERT(body->iseq_encoded <= stack_pc && stack_pc < body->iseq_encoded + body->iseq_size);
    switch (e->insn) {
    case BIN(sir_make_bbv):
    case BIN(sir_make_optpc_bbv):
    case BIN(sir_make_catch_bbv):
    case BIN(specialize_bbv):
    case BIN(sir_gen_bbv):
    case BIN(sir_gen_bbv2):
    case BIN(sir_gen_bbv_key):
    case BIN(sir_inspect_type):
    case BIN(sir_inspect_fixtype):
    case BIN(sir_inspect_flotype):
    case BIN(sir_inspect_stack_type):
    case BIN(sir_inspect_stack_fixtype):
    case BIN(sir_inspect_stack_flotype):
        /* we don't want to trace these insns */
        *pc = (VALUE)e->notrace_encoded_insn;
        break;
    default:
        stack_pos = (int)(stack_pc - body->iseq_encoded);
        *pc = (VALUE)(body->iseq_encoded_trace_p != NULL && body->iseq_encoded_trace_p[stack_pos]
                      ? e->trace_encoded_insn : e->notrace_encoded_insn);
        break;
    }
    return e->insn_len;
}

static void
sir_trace_update(const rb_iseq_t *iseq, int start, int bound)
{
    int pos;

    for (pos = start; pos < bound;)
        pos += sir_insn_trace_update(iseq, &iseq->body->bbv_code[pos]);
}

void
rb_sir_trace_update(const rb_iseq_t *iseq)
{
    sir_trace_update(iseq, 0, iseq->body->bbv_code_free);
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
	insn_id = rb_mirjit_insn_decode(&code[i]);
        if (insn_id == BIN(sir_gen_bbv) || insn_id == BIN(sir_gen_bbv2) || insn_id == BIN(sir_gen_bbv_key)) {
            rb_iseq_bbv_t bbv = (rb_iseq_bbv_t)code[i + 1];
            fprintf(stderr, "%s  # %04d: %s(BBV%d 0x%llx, %s)", prefix,
                    i, insn_name(insn_id), bbv->num, (unsigned long long)bbv, bbv->env_p ? "env" : "stack");
            print_attrs(bbv->attrs);
        }
        print_insn(iseq, insn_id, code, i, prefix);
        len = insn_len(insn_id);
        if (longer_paths_p) {
            if (follow_jumps_p && insn_id == BIN(jump) && (long)code[i + 1] > 0)
                i = i + (int)code[i + 1];
            else if (bound < 0 && non_sir_bb_end_p(insn_id))
                break;
        }
        i += len;
        if (bound >= 0 && i >= bound) break;
    }
}

void
rb_sir_print_bbv(rb_iseq_bbv_t bbv) {
    print_bb(bbv->iseq, bbv->iseq->body->bbv_code, bbv->bbv_start, bbv->bbv_bound, "  ");
}

static void
print_iseq_title(rb_iseq_t *iseq) {
    const rb_code_location_t *loc = &iseq->body->location.code_location;
    fprintf(stderr, "#<ISeq %s@%s:%d (%d,%d)-(%d,%d)>",
            RSTRING_PTR(iseq->body->location.label), RSTRING_PTR(rb_iseq_path(iseq)), loc->beg_pos.lineno,
            loc->beg_pos.lineno, loc->beg_pos.column, loc->end_pos.lineno, loc->end_pos.column);
}

void
rb_sir_stub_gen(rb_iseq_t *iseq) {
    RB_VM_LOCK_ENTER();
    int i, n, lev, curr_bb_start, pos, len, make_bbv_area_start;
    enum ruby_vminsn_type insn_id;
    rb_iseq_t *parent_iseq, *last_iseq;
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
        case BIN(setlocal_WC_1):
            lev = 1;
            goto setlocal;
        case BIN(setlocal):
            lev = (int)encoded[i + 2];
            if (lev == 0)
                break;
        setlocal:
            for (last_iseq = NULL, parent_iseq = iseq, n = 0;
                 n < lev;
                 n++, parent_iseq = (rb_iseq_t *)parent_iseq->body->parent_iseq)
                last_iseq = parent_iseq;
            if (last_iseq->body->parent_var_changed_p == NULL)
                last_iseq->body->parent_var_changed_p = SIR_ZALLOC_N(char, parent_iseq->body->local_table_size);
            SIR_GEN_ASSERT(encoded[i + 1] >= VM_ENV_DATA_SIZE);
            last_iseq->body->parent_var_changed_p[encoded[i + 1] - VM_ENV_DATA_SIZE] = TRUE;
            break;
        case BIN(jump):
        case BIN(branchif):
        case BIN(branchunless):
        case BIN(branchnil):
        case BIN(opt_getinlinecache):
            update_labeled(i + len);
            pos = i + len + (int)(OFFSET)encoded[i + 1]; /* dst */
            SIR_GEN_ASSERT(pos >= 0);
            update_labeled(pos);
            break;
        case BIN(opt_case_dispatch):
            update_labeled(i + len);
            pos = i + len + (int)(OFFSET)encoded[i + 2]; /* else_dst */
            SIR_GEN_ASSERT(pos >= 0);
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
            update_labeled((int)body->param.opt_table[i]);
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
                bbv = create_bbv(iseq, RB_BBV_REGULAR, 0, i, -1, TRUE, 0, NULL);
                VARR_PUSH(rb_iseq_bbv_t, bbvs, bbv);
            }
        }
    } else { /* create bbv at the **end** of the BB */
        curr_bb_start = 0;
        for (i = 0; i < (int)body->iseq_size; i += insn_len(insn_id)) {
            insn_id = rb_vm_insn_addr2insn((void *)encoded[i]);
            if (i != 0 && i < (int)VARR_LENGTH(char, labeled_p) && VARR_GET(char, labeled_p, i)) {
                bbv = create_bbv(iseq, RB_BBV_REGULAR, 0, curr_bb_start, i, TRUE, 0, NULL);
                VARR_PUSH(rb_iseq_bbv_t, bbvs, bbv);
                curr_bb_start = i;
            }
        }
        if (curr_bb_start != (int)body->iseq_size) {
            bbv = create_bbv(iseq, RB_BBV_REGULAR, 0, curr_bb_start, (int)body->iseq_size, TRUE, 0, NULL);
            VARR_PUSH(rb_iseq_bbv_t, bbvs, bbv);
        }
    }
    /* Add sir_make_xxx_bbv insns for all entry points and create bbvs for opt_table and catch_table: */
    VARR_TRUNC(VALUE, bbv_encoded, 0);
    VARR_TRUNC(int, pos_map, 0);
    add_insn(0, BIN(sir_make_bbv), 1, 0);
    if (body->param.opt_num == 0) {
        body->curr_param_opt_table = NULL;
    } else {
        body->curr_param_opt_table = SIR_ALLOC_N(VALUE, body->param.opt_num + 1);
        for (i = 0; i <= body->param.opt_num; i++) { /* optional parameter pc */
            pos = (int)body->param.opt_table[i];
            bbv = create_bbv(iseq, RB_BBV_OPT, i, pos, -1, TRUE, 0, NULL);
            VARR_PUSH (rb_iseq_bbv_t, bbvs, bbv);
            body->curr_param_opt_table[i] = VARR_LENGTH(VALUE, bbv_encoded);
            add_insn(pos, BIN(sir_make_optpc_bbv), 1, i);
        }
    }
    if (body->catch_table != NULL) {
        for (i = 0; i < (int)body->catch_table->size; i++) { /* exception continuation pc */
            pos = body->catch_table->entries[i].cont;
            bbv = create_bbv(iseq, RB_BBV_CATCH, i, pos, -1, TRUE, 0, NULL);
            VARR_PUSH(rb_iseq_bbv_t, bbvs, bbv);
            body->catch_table->entries[i].curr_cont = (unsigned)VARR_LENGTH(VALUE, bbv_encoded);
            add_insn(pos, BIN(sir_make_catch_bbv), 1, i);
        }
    }
    body->bbvs = SIR_ALLOC_N(rb_iseq_bbv_t, VARR_LENGTH(rb_iseq_bbv_t, bbvs));
    body->n_bbvs = (int)VARR_LENGTH(rb_iseq_bbv_t, bbvs);
    memcpy(body->bbvs, VARR_ADDR(rb_iseq_bbv_t, bbvs), body->n_bbvs * sizeof(rb_iseq_bbv_t));
    qsort(body->bbvs, body->n_bbvs, sizeof(rb_iseq_bbv_t), compare_bbvs);
    len = (int)VARR_LENGTH(VALUE, bbv_encoded);
    make_bbv_area_start = get_bbv_code_space(iseq, len);
    SIR_GEN_ASSERT(make_bbv_area_start == 0);
    SIR_GEN_ASSERT(len == (int)VARR_LENGTH(int, pos_map));
    memcpy(&body->bbv_code[make_bbv_area_start], VARR_ADDR(VALUE, bbv_encoded), len * sizeof(VALUE));
    VARR_TRUNC(VALUE, bbv_encoded, 0);
    memcpy(&body->bbv_pos_map[make_bbv_area_start],
           VARR_ADDR(int, pos_map), VARR_LENGTH(int, pos_map) * sizeof(int));
    VARR_TRUNC(int, pos_map, 0);
    body->iseq_code = body->iseq_start_pc = &body->bbv_code[make_bbv_area_start];
    bbv_encode(&body->bbv_code[make_bbv_area_start], len);
    int debug_level = rb_sir_debug_level();
    if (debug_level >= 1) {
        fprintf(stderr, "## Make BBV area of ");
        print_iseq_title(iseq);
        fprintf(stderr, "\n");
        if (debug_level >= 3) {
            print_bb(iseq, body->bbv_code, make_bbv_area_start, make_bbv_area_start + len, "  ");
            print_pos_map(iseq, make_bbv_area_start, make_bbv_area_start + len, FALSE);
        }
    }
    RB_VM_LOCK_LEAVE();
}

static void
nullify(int pos) {
    int i, len = insn_len((enum ruby_vminsn_type)VARR_GET(VALUE, bbv_encoded, pos));
    for (i = 0; i < len; i++)
        VARR_SET(VALUE, bbv_encoded, pos + i, BIN(nop));
}

static int
remove_nops(int *nops_num) {
    int curr_ind, new_curr_ind, last_insn_p, nel, len, last_sir_ind = 0;
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
        last_sir_ind = new_curr_ind;
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
    VARR_TRUNC(int, pos_map, last_sir_ind + 1);
    return last_sir_ind;
}

static int
catch_between_p(rb_iseq_t *iseq, int sir_producer_pos, int orig_consumer_pos) {
    int orig_producer_pos = VARR_GET(int, pos_map, sir_producer_pos);
    const struct rb_iseq_constant_body *const body = iseq->body;
    if (body->catch_table == NULL) return FALSE;
    for (int i = 0; i < (int)body->catch_table->size; i++) { /* mark exception continuation pc */
        const struct iseq_catch_table_entry *entry = UNALIGNED_MEMBER_PTR(body->catch_table, entries[i]);
        if (orig_producer_pos <= (int)entry->cont && (int)entry->cont <= orig_consumer_pos) return TRUE;
    }
    return FALSE;
}

static void
get_op(rb_iseq_t *iseq, int orig_insn_pos, int op_insn_pos, int *local_p, VALUE *local, VALUE *val) {
    *local_p = FALSE;
    *local = Qnil;
    if (val != NULL) *val = Qnil;
    if (op_insn_pos >= 0 && !catch_between_p(iseq, op_insn_pos, orig_insn_pos)) {
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
pos_in_catch_table_p(rb_iseq_t *iseq, int pos, int cont_only_p) {
    int i;
    const struct rb_iseq_constant_body *const body = iseq->body;

    if (body->catch_table != NULL) {
        for (i = 0; i < (int)body->catch_table->size; i++) {
            const struct iseq_catch_table_entry *entry =
                UNALIGNED_MEMBER_PTR(body->catch_table, entries[i]);
            if (!cont_only_p) {
                if (pos == (int)entry->start) return TRUE;
                if (pos == (int)entry->end) return TRUE;
            }
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
    char env_p;
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

    SIR_GEN_ASSERT(rb_vm_insn_decode(iseq->body->iseq_encoded[data->origin_case_start])
                   == BIN(opt_case_dispatch));
    dst = data->origin_case_start + len + (int)FIX2LONG(val);
    dst_bbv = get_bbv(iseq, RB_BBV_REGULAR, 0, dst, data->env_p,
                      (int)VARR_LENGTH(char, attrs), VARR_ADDR(char, attrs));
    disp = add_insn(dst, BIN(sir_gen_bbv_key), 3, (VALUE)dst_bbv, (VALUE)0, (VALUE)key);
    VARR_PUSH(rb_iseq_bbv_t, out_bbvs, dst_bbv);
    VARR_PUSH(int, out_gen_bbv_disps, disp);
    VARR_SET(VALUE, bbv_encoded, disp + 2, (VALUE)(data->case_start - disp - insn_len(BIN(sir_gen_bbv_key))));
    rb_hash_aset(data->map, key, INT2FIX(disp - (data->case_start + len)));
    return ST_CONTINUE;
}

static inline VALUE abs_op(VALUE v) { return (EPDISP)v < 0 ? (VALUE)-(EPDISP)v : v; }

static int
contains_input_lvar_p(int insn_pos, int lvar) {
    if (insn_pos < 0) return FALSE;

    enum ruby_vminsn_type insn_id = (enum ruby_vminsn_type)VARR_GET(VALUE, bbv_encoded, insn_pos);
    VALUE *operands = VARR_ADDR(VALUE, bbv_encoded) + insn_pos + 1;
    SIR_GEN_ASSERT(lvar >= 0);

#define SIR_SPEC_INSN_LVAR_CASE0(insn_name)                                   			\
    case SIR_NAME2(insn_name, svv):								\
    case SIR_NAME2(insn_name, vvv):								\
        return abs_op(operands[0]) == (VALUE)lvar || abs_op(operands[1]) == (VALUE)lvar; 	\
    case SIR_NAME2(insn_name, svi):								\
    case SIR_NAME2(insn_name, vvi):								\
    case SIR_NAME2(insn_name, svs):								\
    case SIR_NAME2(insn_name, vvs): return abs_op(operands[0]) == (VALUE)lvar;			\
    case SIR_NAME2(insn_name, ssv):								\
    case SIR_NAME2(insn_name, vsv): return abs_op(operands[1]) == (VALUE)lvar;

#define SIR_SPEC_INSN_LVAR_CASE(insn_name)                                   			\
    SIR_SPEC_INSN_LVAR_CASE0(insn_name)								\
    case SIR_NAME2(insn_name, siv):								\
    case SIR_NAME2(insn_name, viv): return abs_op(operands[1]) == (VALUE)lvar;

#define SIR_INSN_LVAR_CASE(insn_name)                            				\
    case SIR_NAME2(insn_name, svv):								\
        return abs_op(operands[0]) == (VALUE)lvar || abs_op(operands[1]) == (VALUE)lvar; 	\
    case SIR_NAME2(insn_name, svi):								\
    case SIR_NAME2(insn_name, svs): return abs_op(operands[0]) == (VALUE)lvar;			\
    case SIR_NAME2(insn_name, ssv):								\
    case SIR_NAME2(insn_name, siv): return abs_op(operands[1]) == (VALUE)lvar;

#define SIR_SPEC_FP_RES_INSN_LVAR_CASE(insn_name) SIR_INSN_LVAR_CASE(insn_name)

    switch (insn_id) {
    SIR_SPEC_INSN_LVAR_CASE(iplus)
    SIR_SPEC_INSN_LVAR_CASE(iminus)
    SIR_SPEC_INSN_LVAR_CASE(imult)
    SIR_SPEC_INSN_LVAR_CASE(idiv)
    SIR_SPEC_INSN_LVAR_CASE(imod)
    SIR_SPEC_INSN_LVAR_CASE0(ior)
    SIR_SPEC_INSN_LVAR_CASE0(iand)
    SIR_SPEC_INSN_LVAR_CASE0(ieq)
    SIR_SPEC_INSN_LVAR_CASE0(ineq)
    SIR_SPEC_INSN_LVAR_CASE0(ilt)
    SIR_SPEC_INSN_LVAR_CASE0(igt)
    SIR_SPEC_INSN_LVAR_CASE0(ile)
    SIR_SPEC_INSN_LVAR_CASE0(ige)
    SIR_SPEC_FP_RES_INSN_LVAR_CASE(fplus)
    SIR_SPEC_FP_RES_INSN_LVAR_CASE(fminus)
    SIR_SPEC_FP_RES_INSN_LVAR_CASE(fmult)
    SIR_SPEC_FP_RES_INSN_LVAR_CASE(fdiv)
    SIR_SPEC_FP_RES_INSN_LVAR_CASE(fmod)
    SIR_SPEC_INSN_LVAR_CASE(feq)
    SIR_SPEC_INSN_LVAR_CASE(fneq)
    SIR_SPEC_INSN_LVAR_CASE(flt)
    SIR_SPEC_INSN_LVAR_CASE(fgt)
    SIR_SPEC_INSN_LVAR_CASE(fle)
    SIR_SPEC_INSN_LVAR_CASE(fge)
    SIR_INSN_LVAR_CASE(plus)
    SIR_INSN_LVAR_CASE(minus)
    SIR_INSN_LVAR_CASE(mult)
    SIR_INSN_LVAR_CASE(div)
    SIR_INSN_LVAR_CASE(mod)
    SIR_INSN_LVAR_CASE(or)
    SIR_INSN_LVAR_CASE(and)
    SIR_INSN_LVAR_CASE(eq)
    SIR_INSN_LVAR_CASE(neq)
    SIR_INSN_LVAR_CASE(lt)
    SIR_INSN_LVAR_CASE(gt)
    SIR_INSN_LVAR_CASE(le)
    SIR_INSN_LVAR_CASE(ge)
    case BIN (getlocal):
        if (operands[1] != 0) return FALSE; /*op1==level*/
        /* fallthrough */
    case BIN(getlocal_WC_0):
        return (VALUE)lvar == operands[0];
    default: return FALSE;
    }
}

#define SIR_INSN2_IMM_CASES(name)                  \
        case SIR_NAME2(name, siv):                 \
        case SIR_NAME2(name, sis)

#define SIR_INSN2_IMM2_CASES(name)                 \
        case SIR_NAME2(name, svi):                 \
        case SIR_NAME2(name, ssi)

#define SIR_INSN2_NON_IMM_VS_CASE(name) case SIR_NAME2(name, svs)

#define SIR_INSN2_NON_IMM_NON_VS_CASES(name)       \
        case OPT_NAME(name):                       \
        case SIR_NAME2(name, svv):                 \
        case SIR_NAME2(name, ssv)

#define SIR_INSN2_NON_IMM_CASES(name)              \
        SIR_INSN2_NON_IMM_NON_VS_CASES(name):      \
        SIR_INSN2_NON_IMM_VS_CASE(name)

#define SIR_INSN2_CASES(name)                   \
    SIR_INSN2_IMM_CASES(name):                  \
    SIR_INSN2_NON_IMM_CASES(name)

#define SIR_INSN3_NON_IMM_CASES(name)              	\
        case OPT_NAME(name):                       	\
        case SIR_NAME2(name, svv):                 	\
        case SIR_NAME2(name, svs):                 	\
        case SIR_NAME2(name, ssv): op1_stack_p = TRUE;	\
        case SIR_NAME2(name, vss):                 	\
        case SIR_NAME2(name, vvv):                 	\
        case SIR_NAME2(name, vvs):                 	\
        case SIR_NAME2(name, vsv)

/* Generate non specialized (or non-speculative) SIR code for BBV
   which should be the head BBV (sometimes called the default BBV):
   Move producers to setlocals and to branch insns to make specialized
   code for other BBVs later. */
static void
gen_sir_from_stack_insns(rb_iseq_t *iseq, rb_iseq_bbv_t bbv) {
    enum ruby_vminsn_type insn_id = BIN(nop);
    enum ruby_vminsn_type new_insn_id, op_insn_id;
    int init_stack_size = bbv->start_stack_size;
    int i, n, last_pos, curr_orig_insn_pos, len, nops, inc, retn, br_p, dst2_p, stack_size;
    int op_insn_pos, op3_insn_pos, op2_insn_pos, local_p, local2_p, local3_p;
    int last_sir_ind, nops_num;
    int debug_level = rb_sir_debug_level();
    int disp = 0, disp2 = 0, else_disp = 0; /* for removing uninitialized warning */
    OFFSET dst = 0, dst2 = 0, else_dst = 0; /* for removing uninitialized warning */
    VALUE val, val2, val3, local, local2, local3, *operands, new_operands[10];
    int offset, offset2;
    struct rb_iseq_constant_body *body = iseq->body;
    rb_iseq_bbv_t dst_bbv, else_dst_bbv;

    SIR_GEN_ASSERT(bbv->head_bbv == NULL && bbv->attrs[0] == ATTR_END_MARKER && init_stack_size >= 0);
    if (debug_level >= 1) {
        fprintf(stderr, "\n## Generation of General SIR BBV%d 0x%llx (%s):",
                bbv->num, (unsigned long long)bbv, bbv->env_p ? "env" : "stack");
        print_attrs(bbv->attrs);
        fprintf(stderr, "Original BB of ");
        print_iseq_title(iseq);
        fprintf(stderr, "\n");
        if (debug_level >= 2)
            print_bb(iseq, body->iseq_encoded, bbv->iseq_start, bbv->iseq_bound, "  ");
    }
    SIR_GEN_ASSERT(bbv->bbv_start < 0);
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
    SIR_GEN_ASSERT(bbv->iseq_bound < 0 || bbv->iseq_start < bbv->iseq_bound);
    for (curr_orig_insn_pos = bbv->iseq_start;;) {
        last_sir_ind = (int)VARR_LENGTH(VALUE, bbv_encoded);
	insn_id = rb_vm_insn_addr2insn((void *)body->iseq_encoded[curr_orig_insn_pos]);
        len = insn_len(insn_id);
        operands = &body->iseq_encoded[curr_orig_insn_pos + 1];
        switch (insn_id) {
        case BIN(setn):
            stack_size = (int)VARR_LENGTH(int, stack);
            n = (int)operands[0];
            SIR_GEN_ASSERT(stack_size > n);
            for (i = 0; i <= n; i++) /* we can not use stack above n w/o change setn arg */
                VARR_SET(int, stack, stack_size - i - 1, -1); /* flag of unknown insn */
            add_insn_arr(curr_orig_insn_pos, insn_id, len - 1, operands);
            break;
        case BIN(topn):
            n = (int)operands[0];
            stack_size = (int)VARR_LENGTH(int, stack);
            SIR_GEN_ASSERT(stack_size > n);
            for (i = 0; i < n; i++) {
                VARR_SET(int, stack, stack_size - 1 - i, -1); /* flag of unknown insn */
            }
            VARR_PUSH(int, stack, -1); /* flag of unknown insn */
            VARR_PUSH(char, operands_changed_p, FALSE);
            add_insn_arr(curr_orig_insn_pos, insn_id, len - 1, operands);
            break;
        case BIN(dup):
            n = 1;
            goto dup;
        case BIN(dupn): /* ??? optimize: when values are only putobject|getlocal */
            n = (int)operands[0];
        dup:
            stack_size = (int)VARR_LENGTH(int, stack);
            SIR_GEN_ASSERT(stack_size >= n);
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
        case BIN(sir_make_bbv): // ???
        case BIN(sir_jump): /* nops not labeled by catch table will be removed */
            if (pos_in_catch_table_p(iseq, curr_orig_insn_pos, FALSE)) {
                last_sir_ind = add_insn(curr_orig_insn_pos, BIN(nop), 0);
                update_labeled(last_sir_ind);
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
            stack_size = (int)VARR_LENGTH(int, stack);
            SIR_GEN_ASSERT(stack_size >= 2);
            op3_insn_pos = VARR_LAST(int, stack);
            op2_insn_pos = VARR_GET(int, stack, stack_size - 2);
            get_op(iseq, curr_orig_insn_pos, op3_insn_pos, &local3_p, &local3, &val3);
            get_op(iseq, curr_orig_insn_pos, op2_insn_pos, &local2_p, &local2, &val2);
            new_insn_id = get_sir_insn2_id(insn_id, local2_p, &val2, local3_p, val3);
            SIR_GEN_ASSERT(val2 == Qnil || val3 == Qnil);
            if (new_insn_id == BIN(nop)) goto rest_cases;
            nops = (local2_p || val2 != Qnil  ? 1 : 0) + (local3_p || val3 != Qnil ? 1 : 0);
            SIR_GEN_ASSERT(nops >= 1);
            if (local2_p || val2 != Qnil) {
                new_operands[0] = local2_p ? (VALUE)-(EPDISP)local2 : val2;
                if (local3_p || val3 != Qnil)
                    new_operands[1] = local3_p ? (VALUE)-(EPDISP)local3 : val3;
            } else {
                SIR_GEN_ASSERT(local3_p || val3 != Qnil);
                new_operands[0] = local3_p ? (VALUE)-(EPDISP)local3 : val3;
            }
            if (nops + 2 == insn_len(new_insn_id)) { /* cd */
                new_operands[nops] = operands[0];
                nops++;
            } else { /* 2 cds */
                SIR_GEN_ASSERT(nops + 3 == insn_len(new_insn_id));
                new_operands[nops] = operands[0];
                new_operands[nops + 1] = operands[1];
                nops += 2;
            }
            last_sir_ind = add_insn_arr(curr_orig_insn_pos, new_insn_id, nops, new_operands);
            if (local2_p || val2 != Qnil)
                nullify(op2_insn_pos);
            if (local3_p || val3 != Qnil)
                nullify(op3_insn_pos);
            VARR_TRUNC(int, stack, stack_size - 2);
            VARR_TRUNC(char, operands_changed_p, stack_size - 2);
            goto push;
        case BIN(opt_aset):
            stack_size = (int)VARR_LENGTH(int, stack);
            SIR_GEN_ASSERT(stack_size >= 3);
            if (bbv->iseq_bound >= 0 && curr_orig_insn_pos + len >= bbv->iseq_bound) goto rest_cases;
            op_insn_pos = VARR_GET(int, stack, stack_size - 3);
            op2_insn_pos = VARR_GET(int, stack, stack_size - 2);
            op3_insn_pos = VARR_GET(int, stack, stack_size - 1);
            get_op(iseq, curr_orig_insn_pos, op_insn_pos, &local_p, &local, NULL);
            get_op(iseq, curr_orig_insn_pos, op2_insn_pos, &local2_p, &local2, &val2);
            get_op(iseq, curr_orig_insn_pos, op3_insn_pos, &local3_p, &local3, NULL);
            new_insn_id = get_sir_insn3_id(insn_id, local_p, local2_p, local3_p, val2);
            if (new_insn_id == BIN(nop)) goto rest_cases;
            nops = (local_p ? 1 : 0) + (local2_p || val2 != Qnil ? 1 : 0) + (local3_p ? 1 : 0);
            SIR_GEN_ASSERT(nops >= 1);
            if (local_p) {
                new_operands[0] = -(EPDISP)local;
                if (local2_p || val2 != Qnil) {
                    new_operands[1] = local2_p ? (VALUE)-(EPDISP)local2 : val2;
                    if (local3_p)
                        new_operands[2] = -(EPDISP)local3;
                } else if (local3_p) {
                    new_operands[1] = -(EPDISP)local3;
                }
            } else {
                if (local2_p || val2 != Qnil) {
                    new_operands[0] = local2_p ? (VALUE)-(EPDISP)local2 : val2;
                    if (local3_p)
                        new_operands[1] = (VALUE)-(EPDISP)local3;
                } else if (local3_p) {
                    new_operands[0] = (VALUE)-(EPDISP)local3;
                }
            }
            if (nops + 2 == insn_len (new_insn_id)) { /* cd */
                new_operands[nops] = operands[0];
                nops++;
            } else {
                SIR_GEN_ASSERT(nops + 1 == insn_len(new_insn_id));
            }
            last_sir_ind = add_insn_arr(curr_orig_insn_pos, new_insn_id, nops, new_operands);
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
                add_insn(curr_orig_insn_pos, BIN(sir_jump), 1, (VALUE)0);
            goto stack_update;
        case BIN(branchif):
        case BIN(branchunless):
            stack_size = (int)VARR_LENGTH(int, stack);
            if (stack_size != 0 && (op_insn_pos = VARR_LAST(int, stack)) >= 0
                && !catch_between_p(iseq, op_insn_pos, curr_orig_insn_pos)) {
                /* put compare producer adjacent to the branch for specialized insns: */
                op_insn_id = (enum ruby_vminsn_type)VARR_GET(VALUE, bbv_encoded, op_insn_pos);
                switch (op_insn_id) {
                SIR_INSN2_CASES(eq):
                SIR_INSN2_CASES(neq):
                SIR_INSN2_CASES(lt):
                SIR_INSN2_CASES(gt):
                SIR_INSN2_CASES(le):
                SIR_INSN2_CASES(ge):
                    add_insn_arr(VARR_GET(int, pos_map, op_insn_pos), op_insn_id, (int)insn_len(op_insn_id) - 1,
                                 VARR_ADDR(VALUE, bbv_encoded) + op_insn_pos + 1);
                    nullify(op_insn_pos);
                    break;
                default: break;
                }
            }
            add_insn(curr_orig_insn_pos, BIN(sir_branch), 4, (VALUE)0, (VALUE)0, (VALUE)0, (VALUE)0);
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
            add_insn(curr_orig_insn_pos, BIN(sir_branchnil), 2, (VALUE)0, (VALUE)0);
            goto stack_update;
        case BIN(opt_getinlinecache):
            br_p = dst2_p = TRUE;
            dst = curr_orig_insn_pos + len + (OFFSET)operands[0];
            dst2 = curr_orig_insn_pos + len;
            add_insn(curr_orig_insn_pos, BIN(sir_getinlinecache), 3, (VALUE)0, (VALUE)0, operands[1]);
            goto stack_update;
        case BIN(setlocal):
            if (operands[1] != 0) goto rest_cases;
            /* fallthrough */
        case BIN(setlocal_WC_0):
            stack_size = (int)VARR_LENGTH(int, stack);
            SIR_GEN_ASSERT(stack_size > 0);
            if ((op_insn_pos = VARR_LAST(int, stack)) < 0 || catch_between_p(iseq, op_insn_pos, curr_orig_insn_pos))
                goto change_setlocal0;
            if (VARR_LAST(char, operands_changed_p)) goto change_setlocal0;
            op_insn_id = (enum ruby_vminsn_type)VARR_GET(VALUE, bbv_encoded, op_insn_pos);
            new_insn_id = get_var_res_sir_insn_id(op_insn_id);
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
                    add_insn(curr_orig_insn_pos, BIN(sir_movi), 2, (VALUE)-(EPDISP)operands[0], val);
                    nullify(op_insn_pos);
                    goto change_setlocal;
                case BIN (getlocal):
                    if (VARR_GET(VALUE, bbv_encoded, op_insn_pos + 2) != 0) goto rest_cases; /*op1==level*/
                    /* fallthrough */
                case BIN(getlocal_WC_0):
                    add_insn(curr_orig_insn_pos, BIN(sir_mov), 2, -(EPDISP)operands[0],
                             -(EPDISP)VARR_GET(VALUE, bbv_encoded, op_insn_pos + 1));
                    nullify(op_insn_pos);
                    goto change_setlocal;
                SIR_INSN2_CASES(plus):
                SIR_INSN2_CASES(minus):
                SIR_INSN2_CASES(mult):
                SIR_INSN2_CASES(div):
                SIR_INSN2_CASES(mod):
                SIR_INSN2_CASES(or):
                SIR_INSN2_CASES(and):
                SIR_INSN2_CASES(aref):
                SIR_INSN2_CASES(eq):
                SIR_INSN2_CASES(neq):
                SIR_INSN2_CASES(lt):
                SIR_INSN2_CASES(gt):
                SIR_INSN2_CASES(le):
                SIR_INSN2_CASES(ge):
                    /* put producer adjacent to setlocal */
                    add_insn_arr(VARR_GET(int, pos_map, op_insn_pos),
                                 op_insn_id, (int)insn_len(op_insn_id) - 1,
                                 VARR_ADDR(VALUE, bbv_encoded) + op_insn_pos + 1);
                    nullify(op_insn_pos);
                    break;
                default: break;
                }
            }
        change_setlocal0:
            add_insn_arr(curr_orig_insn_pos, insn_id, len - 1, operands);
        change_setlocal:
            for (i = 0; i < stack_size - 1; i++) {
                if (contains_input_lvar_p(VARR_GET(int, stack, i), (int)operands[0]))
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
            VARR_PUSH(int, stack, last_sir_ind); /* insn producing temp */
            VARR_PUSH(char, operands_changed_p, FALSE);
            break;
        case BIN(opt_case_dispatch):
            br_p = TRUE;
            /* fall through */
        default:
        rest_cases:
            if (insn_id != BIN(sir_gen_bbv) && insn_id != BIN(sir_gen_bbv2))
                add_insn_arr(curr_orig_insn_pos, insn_id, len - 1, operands);
        stack_update:
            if (insn_id == BIN(send) || insn_id == BIN(opt_send_without_block) || insn_id == BIN(invokesuper)) {
                inc = (int)sp_inc_of_sendish(((CALL_DATA)operands[0])->ci);
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
                SIR_GEN_ASSERT(-inc <= (int)VARR_LENGTH(int, stack));
                VARR_TRUNC(int, stack, new_size);
                VARR_TRUNC(char, operands_changed_p, new_size);
            }
            retn = (int)insn_stack_retn(insn_id, operands);
            SIR_GEN_ASSERT(inc <= retn && (int)VARR_LENGTH(int, stack) >= retn);
            for (i = 0; i < retn; i++) {
                VARR_SET(int, stack, VARR_LENGTH(int, stack) - retn + i, -1);
            }
        }
        curr_orig_insn_pos += len;
        if (bbv->iseq_bound >= 0 && curr_orig_insn_pos >= bbv->iseq_bound) break;
        if (longer_paths_p && follow_jumps_p) {
            if (insn_id == BIN(jump) && (long)operands[0] > 0)
                curr_orig_insn_pos = (int)dst; /* follow jump destination */
            else if (non_sir_bb_end_p(insn_id))
                break;
        }
    }
    last_sir_ind = remove_nops(&nops_num);
    last_pos = VARR_LAST(int, pos_map);
    if (insn_id != BIN(leave) && insn_id != BIN(throw) && insn_id != BIN(opt_case_dispatch)) {
        if (!br_p) {
            last_sir_ind = add_insn(last_pos, BIN(sir_jump), 1, (VALUE)0);
            dst = insn_id == BIN(opt_case_dispatch) ? curr_orig_insn_pos : bbv->iseq_bound;
        }
        dst_bbv = get_bbv(iseq, RB_BBV_REGULAR, 0, (int)dst, bbv->env_p, 0, NULL);
        len = insn_len(BIN(sir_gen_bbv));
        disp = add_insn(last_pos, BIN(sir_gen_bbv), 2, (VALUE)dst_bbv, (VALUE)0);
        VARR_PUSH(rb_iseq_bbv_t, out_bbvs, dst_bbv);
        VARR_PUSH(int, out_gen_bbv_disps, disp);
        VARR_SET(VALUE, bbv_encoded, disp + 2, (VALUE)(last_sir_ind - (disp + len)));
        if (dst2_p) {
            len = insn_len(BIN(sir_gen_bbv2));
            dst_bbv = get_bbv(iseq, RB_BBV_REGULAR, 0, (int)dst2, bbv->env_p, 0, NULL);
            disp2 = add_insn(last_pos, BIN(sir_gen_bbv2), 2, (VALUE)dst_bbv, (VALUE)0);
            VARR_PUSH(rb_iseq_bbv_t, out_bbvs, dst_bbv);
            VARR_PUSH(int, out_gen_bbv_disps, disp2);
            VARR_SET(VALUE, bbv_encoded, disp2 + 2, (VALUE)(last_sir_ind - (disp2 + len)));
        }
    }
    if (insn_id == BIN(opt_case_dispatch)) { /* add sir_gen_bbv for cdhash and else_offset */
        struct cdhash_set_el data;

        else_dst = curr_orig_insn_pos + (OFFSET)operands[1]; /* end of opt_case_dispatch + else dst */
        else_dst_bbv = get_bbv(iseq, RB_BBV_REGULAR, 0, (int)else_dst, bbv->env_p, 0, NULL);
        else_disp = add_insn(last_pos, BIN(sir_gen_bbv2), 2, (VALUE)else_dst_bbv, (VALUE)0);
        VARR_PUSH(rb_iseq_bbv_t, out_bbvs, else_dst_bbv);
        VARR_PUSH(int, out_gen_bbv_disps, else_disp);
        len = insn_len(BIN(sir_gen_bbv2));
        VARR_SET(VALUE, bbv_encoded, else_disp + 2, (VALUE)(last_sir_ind - (else_disp + len)));
        VARR_SET(VALUE, bbv_encoded, last_sir_ind + 2, (VALUE)(else_disp - last_sir_ind - len));
        /* set up new hash and add insns sir_gen_bb_key */
        data.iseq = iseq;
        data.map = rb_hash_new();
        RHASH_TBL_RAW(data.map)->type = &cdhash_type;
        RB_OBJ_WRITTEN(iseq, Qundef, data.map);
        data.env_p = bbv->env_p;
        data.case_start = last_sir_ind;
        data.origin_case_start = curr_orig_insn_pos - insn_len(BIN(opt_case_dispatch));
        rb_hash_foreach(operands[0], set_case_hash_el, (VALUE)&data);
        VARR_SET(VALUE, bbv_encoded, last_sir_ind + 1, data.map);
    }
    add_insn(last_pos, BIN(nop), 0); /* to get position right after the last insn */
    len = (int)VARR_LENGTH(VALUE, bbv_encoded);
    bbv->bbv_start = get_bbv_code_space(iseq, len);
    bbv->bbv_bound = bbv->bbv_start + len;
    SIR_GEN_ASSERT(len == (int)VARR_LENGTH(int, pos_map));
    memcpy(&body->bbv_code[bbv->bbv_start], VARR_ADDR(VALUE, bbv_encoded), len * sizeof(VALUE));
    VARR_TRUNC(VALUE, bbv_encoded, 0);
    memcpy(&body->bbv_pos_map[bbv->bbv_start], VARR_ADDR(int, pos_map), VARR_LENGTH(int, pos_map) * sizeof(int));
    VARR_TRUNC(int, pos_map, 0);
    if (insn_id != BIN(leave) && insn_id != BIN(throw) && insn_id != BIN(opt_case_dispatch)) {
        insn_id = (enum ruby_vminsn_type)body->bbv_code[bbv->bbv_start + last_sir_ind];
        offset = last_sir_ind + insn_len(insn_id); /* offset of the branch end from zero */
        offset = disp - offset; /* offset relative to the 1st sir_gen_code */
        body->bbv_code[bbv->bbv_start + last_sir_ind + 1] = (VALUE)offset;
        if (dst2_p) {
            offset2 = last_sir_ind + insn_len(insn_id); /* offset of the branch end from zero */
            offset2 = disp2 - offset2;  /* offset relative to the 2nd sir_gen_code */
            body->bbv_code[bbv->bbv_start + last_sir_ind + 2] = (VALUE)offset2;
        }
    }
    SIR_GEN_ASSERT(VARR_LENGTH(rb_iseq_bbv_t, out_bbvs) == VARR_LENGTH(int, out_gen_bbv_disps));
    /* make no alloc between changing bbv_bound and encode: */
    bbv_encode(&body->bbv_code[bbv->bbv_start], len);
    for (i = 0; i < (int)VARR_LENGTH(rb_iseq_bbv_t, out_bbvs); i++)
        add_bbv_edge(bbv, VARR_GET(rb_iseq_bbv_t, out_bbvs, i), VARR_GET(int, out_gen_bbv_disps, i));
    sir_trace_update(iseq, bbv->bbv_start, bbv->bbv_start + len);
    if (debug_level >= 2) {
        fprintf(stderr, "## SIR BBV:\n");
        print_bb(iseq, body->bbv_code, bbv->bbv_start, bbv->bbv_bound, "  ");
        print_pos_map(iseq, bbv->bbv_start, bbv->bbv_bound, FALSE);
    }
}

static int
add_spec_insn_arr(int sir_pos, enum ruby_vminsn_type insn_id, int nops, VALUE *operands) {
    SIR_GEN_ASSERT(sir_pos >= 0);
    return add_insn_arr(-(sir_pos + 1), insn_id, nops, operands);
}

#define BRANCH_CASES(name)      \
    case SIR_NAME2(name, ss):	\
    case SIR_NAME2(name, sv):	\
    case SIR_NAME2(name, si):	\
    case SIR_NAME2(name, vs):	\
    case SIR_NAME2(name, vv):	\
    case SIR_NAME2(name, vi)

static void mark_bbv_change(rb_iseq_bbv_t bbv) {
    rb_iseq_t *iseq = bbv->iseq;
    const struct rb_iseq_constant_body *body = iseq->body;
    if (rb_mirjit_enabled_p()
        && rb_mirjit_get_pc_dispatch_addr(&body->bbv_code[bbv->bbv_start]) != NULL)
        rb_mirjit_mark_bbv_change(bbv);
}

static void
remove_out_edges(rb_iseq_bbv_t src_bbv) {
    rb_iseq_bbv_t bbv;
    struct rb_iseq_bbv_edge *out_e, *next_out_e, *in_e, *next_in_e, *prev_in_e;

    SIR_GEN_ASSERT(src_bbv->bbv_start >= 0);
    for (out_e = src_bbv->out_edges; out_e != NULL; out_e = next_out_e) {
        SIR_GEN_ASSERT(out_e->in == src_bbv);
        next_out_e = out_e->next_out_edge;
        bbv = out_e->out;
        for (prev_in_e = NULL, in_e = bbv->in_edges; in_e != NULL; in_e = next_in_e) {
            SIR_GEN_ASSERT(out_e->out == bbv);
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
        SIR_FREE((void *)out_e);
    }
    src_bbv->out_edges = NULL;
}

static void
redirect_src_bbv_dests(rb_iseq_bbv_t dst_bbv, int gen_p) {
    rb_iseq_bbv_t bbv;
    struct rb_iseq_bbv_edge *in_e;
    rb_iseq_t *iseq = dst_bbv->iseq;
    const struct rb_iseq_constant_body *body = iseq->body;
    int bbv_gen_pos, branch_pos, new_dest_pos;
    OFFSET branch_offset, offset;
    enum ruby_vminsn_type branch_id;
    int debug_level = rb_sir_debug_level();

    SIR_GEN_ASSERT(dst_bbv->bbv_start >= 0);
    for (in_e = dst_bbv->in_edges; in_e != NULL; in_e = in_e->next_in_edge) {
        SIR_GEN_ASSERT(in_e->out == dst_bbv);
        bbv = in_e->in;
        bbv_gen_pos = bbv->bbv_start + in_e->gen_bbv_disp; /* pos of sir_gen_bbvxxx */
        branch_offset = (OFFSET)body->bbv_code[bbv_gen_pos + 2];
        if (rb_vm_insn_decode(body->bbv_code[bbv_gen_pos]) == BIN(sir_gen_bbv)) {
            branch_pos = bbv_gen_pos + insn_len(BIN(sir_gen_bbv)) + (int)branch_offset;
        } else if (rb_vm_insn_decode(body->bbv_code[bbv_gen_pos]) == BIN(sir_gen_bbv2)) {
            branch_pos = bbv_gen_pos + insn_len(BIN(sir_gen_bbv)) + (int)branch_offset;
        } else {
            SIR_GEN_ASSERT(rb_vm_insn_decode(body->bbv_code[bbv_gen_pos]) == BIN(sir_gen_bbv_key));
            branch_pos = bbv_gen_pos + insn_len(BIN(sir_gen_bbv_key)) + (int)branch_offset;
        }
        branch_id = rb_vm_insn_decode(body->bbv_code[branch_pos]);
        new_dest_pos = (gen_p ? bbv_gen_pos : dst_bbv->bbv_start);
        offset = new_dest_pos - (branch_pos + insn_len(branch_id));
        if (debug_level >= 1) {
            fprintf(stderr, "--> Redirect dest in %04d %s to %d\n",
                    (int)branch_pos, insn_name(branch_id), new_dest_pos);
        }
        mark_bbv_change(bbv);
        if (rb_vm_insn_decode(body->bbv_code[bbv_gen_pos]) == BIN(sir_gen_bbv)) {
            body->bbv_code[branch_pos + 1] = offset;
        } else if (rb_vm_insn_decode(body->bbv_code[bbv_gen_pos]) == BIN(sir_gen_bbv2)) {
            body->bbv_code[branch_pos + 2] = offset;
        } else {
            SIR_GEN_ASSERT(branch_id == BIN(opt_case_dispatch));
            rb_hash_aset(body->bbv_code[branch_pos + 1] /* hash */,
                         body->bbv_code[bbv_gen_pos + 3], /* key */
                         INT2FIX(offset));
        }
    }
}

#define SIR_INSN2_SPEC_STACK_CASES0(name)           \
    case SIR_NAME2(name, svv):                      \
    case SIR_NAME2(name, svs):                      \
    case SIR_NAME2(name, ssv):                      \
    case SIR_NAME2(name, sss):                      \
    case SIR_NAME2(name, svi):                      \
    case SIR_NAME2(name, ssi)

#define SIR_INSN2_SPEC_STACK_CASES(name)         \
    SIR_INSN2_SPEC_STACK_CASES0(name):		 \
    case SIR_NAME2(name, siv):                   \
    case SIR_NAME2(name, sis)

#define SIR_INSN2_SPEC_LVAR_CASES0(name)            \
    case SIR_NAME2(name, vvv):                      \
    case SIR_NAME2(name, vvs):                      \
    case SIR_NAME2(name, vsv):                      \
    case SIR_NAME2(name, vss):                      \
    case SIR_NAME2(name, vvi):                      \
    case SIR_NAME2(name, vsi)

#define SIR_INSN2_SPEC_LVAR_CASES(name)          \
    SIR_INSN2_SPEC_LVAR_CASES0(name):         	 \
    case SIR_NAME2(name, viv):                   \
    case SIR_NAME2(name, vis)


static int change_to_cfunc_send(VALUE *call_pc, rb_iseq_t *caller_iseq);

VALUE (*sir_fixnum_ntimes_func)(VALUE);
VALUE (*sir_range_each_func)(VALUE);

extern bool rb_simple_iseq_p(const rb_iseq_t *iseq);

static void
update_attrs(rb_iseq_t *block_iseq) {
    if (block_iseq == NULL || block_iseq->body->parent_var_changed_p == NULL)
        return;
    for (size_t i = 0; i < VARR_LENGTH(char, attrs); i++)
        if (VARR_GET(char, attrs, i) != ATTR_UNKNOWN && block_iseq->body->parent_var_changed_p[i]) {
            VARR_SET(char, attrs, i, ATTR_UNKNOWN);
            if (rb_sir_debug_level() >= 1)
                fprintf(stderr, "--> making unknown attr for local %lu as changed in nested block\n",
                        (long unsigned)i);
        }
}

static inline int
fp_num_p(VALUE val) {
    return FLONUM_P(val) || (!RB_SPECIAL_CONST_P(val) && RBASIC_CLASS(val) == rb_cFloat);
}

static void get_insn2_attrs(rb_iseq_t *iseq, int nops, int imm_p, int imm2_p, int vs_p,
                            VALUE *operands, int stack_size, int *op1_attr, int *op2_attr) {
    if (nops == 0) {
        *op1_attr = get_attr(get_temp_num(iseq, stack_size - 1));
        *op2_attr = get_attr(get_temp_num(iseq, stack_size - 2));
    } else if (nops == 1) {
        if (imm_p || imm2_p) {
            *op1_attr = (FIXNUM_P(operands[0]) ? ATTR_FIXNUM
                         : FLONUM_P(operands[0]) ? ATTR_FLONUM
                         : operands[0] == Qtrue || operands[0] == Qfalse ? ATTR_BOOL
                         : !SPECIAL_CONST_P(operands[0]) && RBASIC_CLASS(operands[0]) == rb_cArray
                         ? ATTR_ARRAY : ATTR_UNKNOWN);
            *op2_attr = get_attr(get_temp_num(iseq, stack_size - 1));
        } else { /* op attrs are interchangable here for result type calculation */
            *op1_attr = get_attr(get_lvar_num(iseq, operands[0]));
            *op2_attr = get_attr(get_temp_num(iseq, stack_size - 1));
        }
        if (imm2_p || (!imm_p && !vs_p)) {
            int temp = *op1_attr;
            *op1_attr = *op2_attr;
            *op2_attr = temp;
        }
    } else {
        if (imm_p) {
            *op1_attr = (FIXNUM_P(operands[0]) ? ATTR_FIXNUM
                         : FLONUM_P(operands[0]) ? ATTR_FLONUM
                         : operands[0] == Qtrue || operands[0] == Qfalse ? ATTR_BOOL
                         : !SPECIAL_CONST_P(operands[0]) && RBASIC_CLASS(operands[0]) == rb_cArray
                         ? ATTR_ARRAY : ATTR_UNKNOWN);
        } else {
            *op1_attr = get_attr(get_lvar_num(iseq, operands[0]));
        }
        if (imm2_p) {
            *op2_attr = (FIXNUM_P(operands[1]) ? ATTR_FIXNUM
                         : FLONUM_P(operands[1]) ? ATTR_FLONUM
                         : operands[1] == Qtrue || operands[1] == Qfalse ? ATTR_BOOL
                         : !SPECIAL_CONST_P(operands[1]) && RBASIC_CLASS(operands[1]) == rb_cArray
                         ? ATTR_ARRAY : ATTR_UNKNOWN);
        } else {
            *op2_attr = get_attr(get_lvar_num(iseq, operands[1]));
        }
    }
}

static int get_res_attr(int aref_p, int imm_p, int imm2_p, int op1_attr, int op2_attr, int *flag) {
    int attr;
    *flag = INTEGER_REDEFINED_OP_FLAG;
    attr = ATTR_UNKNOWN;
    if (aref_p) {
        if (op1_attr == ATTR_ARRAY && op2_attr == ATTR_FIXNUM) {
            attr = ATTR_ARRAY;
            *flag = ARRAY_REDEFINED_OP_FLAG;
        }
    } else if (imm_p && op1_attr == ATTR_FIXNUM && op2_attr == ATTR_FLONUM) {
        attr = ATTR_FLONUM;
        *flag = INTEGER_REDEFINED_OP_FLAG;
    } else if (imm2_p && op2_attr == ATTR_FIXNUM && op1_attr == ATTR_FLONUM) {
        attr = ATTR_FLONUM;
        *flag = FLOAT_REDEFINED_OP_FLAG;
    } else if (op1_attr == op2_attr) {
        attr = op1_attr;
        *flag = op1_attr == ATTR_FLONUM ? FLOAT_REDEFINED_OP_FLAG : INTEGER_REDEFINED_OP_FLAG;
    }
    return attr;
}

static void get_insn3_attrs(rb_iseq_t *iseq, enum ruby_vminsn_type insn_id, int nops, VALUE *operands, int stack_size,
                            int *v_p, int *v2_p, int *imm2_p, int *op1_attr, int *op2_attr, int *op3_attr) {
    *v_p = *v2_p = *imm2_p = FALSE;
    switch (insn_id) {
    case SIR_NAME2(aset, sis):
        *op1_attr = get_attr(get_temp_num(iseq, stack_size - 2));
        *op2_attr = (FIXNUM_P(operands[0]) ? ATTR_FIXNUM
                    : FLONUM_P(operands[0]) ? ATTR_FLONUM : ATTR_UNKNOWN);
        *op3_attr = get_attr(get_temp_num(iseq, stack_size - 1));
        *imm2_p = TRUE;
        break;
    case SIR_NAME2(aset, siv):
        *op1_attr = get_attr(get_temp_num(iseq, stack_size - 1));
        *op2_attr = (FIXNUM_P(operands[0]) ? ATTR_FIXNUM
                    : FLONUM_P(operands[0]) ? ATTR_FLONUM : ATTR_UNKNOWN);
        *op3_attr = get_attr(get_lvar_num(iseq, operands[1]));
        *imm2_p = TRUE;
        break;
    case SIR_NAME2(aset, viv):
        *op1_attr = get_attr(get_lvar_num(iseq, operands[0]));
        *op2_attr = (FIXNUM_P(operands[1]) ? ATTR_FIXNUM
                    : FLONUM_P(operands[1]) ? ATTR_FLONUM : ATTR_UNKNOWN);
        *op3_attr = get_attr(get_lvar_num(iseq, operands[2]));
        *v_p = *imm2_p = TRUE;
        break;
    case SIR_NAME2(aset, vis):
        *op1_attr = get_attr(get_lvar_num(iseq, operands[0]));
        *op2_attr = (FIXNUM_P(operands[1]) ? ATTR_FIXNUM
                    : FLONUM_P(operands[1]) ? ATTR_FLONUM : ATTR_UNKNOWN);
        *op3_attr = get_attr(get_temp_num(iseq, stack_size - 1));
        *v_p = *imm2_p = TRUE;
        break;
    case OPT_NAME(aset):
        *op1_attr = get_attr(get_temp_num(iseq, stack_size - 3));
        *op2_attr = get_attr(get_temp_num(iseq, stack_size - 2));
        *op3_attr = get_attr(get_temp_num(iseq, stack_size - 1));
        break;
    case SIR_NAME2(aset, svv):
        *op1_attr = get_attr(get_temp_num(iseq, stack_size - 1));
        *op2_attr = get_attr(get_lvar_num(iseq, operands[0]));
        *op3_attr = get_attr(get_lvar_num(iseq, operands[1]));
        *v2_p = TRUE;
        break;
    case SIR_NAME2(aset, svs):
        *op1_attr = get_attr(get_temp_num(iseq, stack_size - 2));
        *op2_attr = get_attr(get_lvar_num(iseq, operands[0]));
        *op3_attr = get_attr(get_temp_num(iseq, stack_size - 1));
        *v2_p = TRUE;
        break;
    case SIR_NAME2(aset, ssv):
        *op1_attr = get_attr(get_temp_num(iseq, stack_size - 2));
        *op2_attr = get_attr(get_temp_num(iseq, stack_size - 1));
        *op3_attr = get_attr(get_lvar_num(iseq, operands[0]));
        break;
    case SIR_NAME2(aset, vss):
        *op1_attr = get_attr(get_lvar_num(iseq, operands[0]));
        *op2_attr = get_attr(get_temp_num(iseq, stack_size - 2));
        *op3_attr = get_attr(get_temp_num(iseq, stack_size - 1));
        *v_p = TRUE;
        break;
    case SIR_NAME2(aset, vvv):
        *op1_attr = get_attr(get_lvar_num(iseq, operands[0]));
        *op2_attr = get_attr(get_lvar_num(iseq, operands[1]));
        *op3_attr = get_attr(get_lvar_num(iseq, operands[2]));
        *v_p = *v2_p = TRUE;
        break;
    case SIR_NAME2(aset, vvs):
        *op1_attr = get_attr(get_lvar_num(iseq, operands[0]));
        *op2_attr = get_attr(get_lvar_num(iseq, operands[1]));
        *op3_attr = get_attr(get_temp_num(iseq, stack_size - 1));
        *v_p = *v2_p = TRUE;
        break;
    case SIR_NAME2(aset, vsv):
        *op1_attr = get_attr(get_lvar_num(iseq, operands[0]));
        *op2_attr = get_attr(get_temp_num(iseq, stack_size - 1));
        *op3_attr = get_attr(get_lvar_num(iseq, operands[1]));
        *v_p = TRUE;
        break;
    default:
        SIR_GEN_ASSERT(FALSE);
    }
}

/* Make type-specialized SIR from general SIR or make speculative
   type-specialized SIR based on profile (profile_p) caused by
   specialize_bbv execution. Return pos in new bbv code to continue
   after profile specialization caused by specialize_bbv execution,
   return -1 otherwise. */
static int
gen_specialized_sir(rb_iseq_t *iseq, rb_iseq_bbv_t bbv, int profile_p) {
    enum ruby_vminsn_type insn_id = BIN(nop), new_insn_id, prev_new_insn_id, branch_id;
    int init_stack_size = bbv->start_stack_size;
    int i, n, curr_sir_insn_pos, len, nops, inc, retn, stack_size;
    int stack_insn_pos, orig_pos, new_insn_pos, prev_new_insn_pos, branch_pos, prev_new_insn_nops;
    int sir_branch_offset_add, attr, inspect_attr1, inspect_attr2, op1_attr, op2_attr, op3_attr;
    int flag, imm_p, imm2_p, vs_p, v_p, v2_p, exch_p, exch2_p, div_p, bool_p, aref_p, aset_p, neq_p;
    int debug_level = rb_sir_debug_level();
    int disp, nops_num;
    VALUE val, *operands, new_operands[10];
    lindex_t op;
    int offset, specialize_bbv_pos = -1;
    struct rb_iseq_constant_body *body = iseq->body;
    rb_iseq_bbv_t head_bbv = bbv->head_bbv, orig_bbv, dst_bbv, old_dst_bbv, body_bbv;
    int add_bbv_cont_p, mirjit_enabled_p = rb_mirjit_enabled_p();

    mirjit_enabled_p = mirjit_enabled_p && profile_p;
    SIR_GEN_ASSERT(profile_p || bbv->bbv_start < 0);
    if (head_bbv == NULL)
        head_bbv = bbv;
    SIR_GEN_ASSERT(init_stack_size >= 0 && head_bbv->start_stack_size == init_stack_size);
    orig_bbv = profile_p ? bbv : head_bbv;
    if (debug_level >= 1) {
        fprintf(stderr, "\n## Making %s Specialized BBV%d 0x%llx (%s):",
                profile_p ? "Profile" : "Type", bbv->num, (unsigned long long)bbv, bbv->env_p ? "env" : "stack");
        print_attrs(bbv->attrs);
        fprintf(stderr, "Original SIR BBV%d of ", orig_bbv->num);
        print_iseq_title(iseq);
        fprintf(stderr, "\n");
        if (debug_level >= 2)
            print_bb(iseq, body->bbv_code, orig_bbv->bbv_start, orig_bbv->bbv_bound, "  ");
    }
    VARR_TRUNC(VALUE, bbv_encoded, 0);
    VARR_TRUNC(send_info_t, iseq_send_info, 0);
    copy_attrs(bbv->attrs);
    VARR_TRUNC(int, pos_map, 0);
    VARR_TRUNC(char, labeled_p, 0);
    VARR_TRUNC(rb_iseq_bbv_t, out_bbvs, 0);
    VARR_TRUNC(int, out_gen_bbv_disps, 0);
    stack_size = init_stack_size;
    new_insn_pos = -1;
    sir_branch_offset_add = 0;
    if (mirjit_enabled_p) {
        add_insn_arr(bbv->iseq_start, BIN(sir_bbv_start), 1, (VALUE *)&bbv);
        bbv->jit_count = BBV_COUNT_TO_JIT;
    }
    for (curr_sir_insn_pos = orig_bbv->bbv_start;
         curr_sir_insn_pos < orig_bbv->bbv_bound;
         curr_sir_insn_pos += len) {
        prev_new_insn_pos = new_insn_pos;
        new_insn_pos = (int)VARR_LENGTH(VALUE, bbv_encoded);
      again:
	insn_id = rb_vm_insn_addr2insn((void *)body->bbv_code[curr_sir_insn_pos]);
        len = insn_len(insn_id);
        operands = &body->bbv_code[curr_sir_insn_pos + 1];
        aref_p = aset_p = neq_p = imm_p = imm2_p = vs_p = bool_p = FALSE; /* immediate operand and unknown result */
        orig_pos = curr_sir_insn_pos;
        if (profile_p) {
            orig_pos = body->bbv_pos_map[curr_sir_insn_pos];
            if (orig_pos < 0)
                orig_pos = -orig_pos - 1;
        }
        stack_insn_pos = body->bbv_pos_map[curr_sir_insn_pos];
        if (profile_p) {
            SIR_GEN_ASSERT(stack_insn_pos < 0);
            stack_insn_pos = body->bbv_pos_map[-stack_insn_pos - 1];
        }
        SIR_GEN_ASSERT(stack_insn_pos >= 0);
        if (pos_in_catch_table_p(iseq, stack_insn_pos, TRUE)) /* clear all attrs: ??? improve */
            for (int i = 0; i < stack_size; i++) {
                int attr_num = get_temp_num(iseq, i);
                if (attr_num < (int)VARR_LENGTH(char, attrs))
                    VARR_SET(char, attrs, attr_num, ATTR_UNKNOWN);
            }
        add_bbv_cont_p = FALSE;
        switch (insn_id) {
        case BIN(sir_inspect_type):
            SIR_GEN_ASSERT(profile_p);
            setup_attr(get_lvar_num(iseq, operands[0]), ATTR_UNKNOWN);
            new_insn_pos = prev_new_insn_pos;
            break;/* it was not changed -- remove it */
        case BIN(sir_inspect_stack_type):
            SIR_GEN_ASSERT(profile_p);
            setup_attr(get_temp_num(iseq, stack_size - (int)operands[0] - 1), ATTR_UNKNOWN);
            new_insn_pos = prev_new_insn_pos;
            break;/* it was not changed -- remove it */
        case BIN(sir_inspect_fixtype):
        case BIN(sir_inspect_stack_fixtype):
            inspect_attr1 = ATTR_FIXNUM;
            goto speculative;
        case BIN(sir_inspect_flotype):
        case BIN(sir_inspect_stack_flotype):
            inspect_attr1 = ATTR_FLONUM;
            goto speculative;
        case BIN(sir_inspect_array):
        case BIN(sir_inspect_stack_array):
            inspect_attr1 = ATTR_ARRAY;
        speculative:
            curr_sir_insn_pos += len;
            SIR_GEN_ASSERT(curr_sir_insn_pos < orig_bbv->bbv_bound);
            insn_id = rb_vm_insn_addr2insn((void *)body->bbv_code[curr_sir_insn_pos]);
            len = insn_len(insn_id);
            switch (insn_id) {
            case BIN(sir_inspect_fixtype):
            case BIN(sir_inspect_stack_fixtype): {
                curr_sir_insn_pos += len;
                SIR_GEN_ASSERT(curr_sir_insn_pos < orig_bbv->bbv_bound);
                if (inspect_attr1 != ATTR_FIXNUM && inspect_attr1 != ATTR_ARRAY) goto again;
                inspect_attr2 = ATTR_FIXNUM;
                insn_id = rb_vm_insn_addr2insn((void *)body->bbv_code[curr_sir_insn_pos]);
                len = insn_len(insn_id);
                break;
            }
            case BIN(sir_inspect_flotype):
            case BIN(sir_inspect_stack_flotype): {
                curr_sir_insn_pos += len;
                SIR_GEN_ASSERT(curr_sir_insn_pos < orig_bbv->bbv_bound);
                if (inspect_attr1 != ATTR_FLONUM) goto again;
                inspect_attr2 = ATTR_FLONUM;
                insn_id = rb_vm_insn_addr2insn((void *)body->bbv_code[curr_sir_insn_pos]);
                len = insn_len(insn_id);
                break;
            }
            default:
                inspect_attr2 = ATTR_UNKNOWN;
                break;
            }
            div_p = imm_p = imm2_p = FALSE;
            operands = &body->bbv_code[curr_sir_insn_pos + 1];
            nops = len - 2; /* exclude cd */
            switch (insn_id) {
            SIR_INSN2_IMM_CASES(neq):
                nops--;
                /* fall through */
            SIR_INSN2_IMM_CASES(eq):
            SIR_INSN2_IMM_CASES(lt):
            SIR_INSN2_IMM_CASES(gt):
            SIR_INSN2_IMM_CASES(le):
            SIR_INSN2_IMM_CASES(ge):
                bool_p = TRUE;
                goto imm;
            SIR_INSN2_IMM_CASES(aref): /* imm is array */
                if (inspect_attr1 != ATTR_FIXNUM || SPECIAL_CONST_P(operands[0]) || RBASIC_CLASS(operands[0]) != rb_cArray)
                    goto again;
                inspect_attr1 = ATTR_ARRAY;
                inspect_attr2 = ATTR_FIXNUM;
                imm_p = aref_p = TRUE;
                break;
            SIR_INSN2_IMM_CASES(or):
            SIR_INSN2_IMM_CASES(and):
                if (inspect_attr1 != ATTR_FIXNUM) goto again;
                /* fall through */
            SIR_INSN2_IMM_CASES(plus):
            SIR_INSN2_IMM_CASES(minus):
            SIR_INSN2_IMM_CASES(mult):
            SIR_INSN2_IMM_CASES(div):
            SIR_INSN2_IMM_CASES(mod):
            imm:
                SIR_GEN_ASSERT(inspect_attr2 == ATTR_UNKNOWN);
                imm_p = TRUE;
                inspect_attr2 = inspect_attr1;
                inspect_attr1 = FIXNUM_P(operands[0]) ? ATTR_FIXNUM : fp_num_p(operands[0]) ? ATTR_FLONUM : ATTR_UNKNOWN;
                if (inspect_attr1 != inspect_attr2) goto again;
                break;
            SIR_INSN2_IMM2_CASES(div):
            SIR_INSN2_IMM2_CASES(mod):
                div_p = TRUE;
                goto imm2;
            SIR_INSN2_IMM2_CASES(neq):
                nops--;
                /* fall through */
            SIR_INSN2_IMM2_CASES(eq):
            SIR_INSN2_IMM2_CASES(lt):
            SIR_INSN2_IMM2_CASES(gt):
            SIR_INSN2_IMM2_CASES(le):
            SIR_INSN2_IMM2_CASES(ge):
                bool_p = TRUE;
                goto imm2;
            SIR_INSN2_IMM2_CASES(aref): /* imm is index */
                if (inspect_attr1 != ATTR_ARRAY) goto again;
                if (!FIXNUM_P(operands[nops == 1 ? 0 : 1])) goto again;
                inspect_attr2 = ATTR_FIXNUM;
                imm2_p = aref_p = TRUE;
                break;
            SIR_INSN2_IMM2_CASES(or):
            SIR_INSN2_IMM2_CASES(and):
                if (inspect_attr1 != ATTR_FIXNUM) goto again;
                /* fall through */
            SIR_INSN2_IMM2_CASES(plus):
            SIR_INSN2_IMM2_CASES(minus):
            SIR_INSN2_IMM2_CASES(mult):
            imm2:
                imm2_p = TRUE;
                SIR_GEN_ASSERT(inspect_attr2 == ATTR_UNKNOWN);
                inspect_attr2 = (FIXNUM_P(operands[nops == 1 ? 0 : 1])
                                 ? ATTR_FIXNUM : fp_num_p(operands[nops == 1 ? 0 : 1]) ? ATTR_FLONUM : ATTR_UNKNOWN);
                if (inspect_attr1 != inspect_attr2) goto again;
                if (div_p && inspect_attr1 == ATTR_FIXNUM && FIX2LONG(operands[nops == 1 ? 0 : 1]) == 0) goto again;
                break;
            SIR_INSN2_NON_IMM_VS_CASE(neq):
                vs_p = TRUE;
                /* fall through */
            SIR_INSN2_NON_IMM_NON_VS_CASES(neq):
                nops--;
                bool_p = TRUE;
                goto non_imm;
            SIR_INSN2_NON_IMM_VS_CASE(eq):
            SIR_INSN2_NON_IMM_VS_CASE(lt):
            SIR_INSN2_NON_IMM_VS_CASE(gt):
            SIR_INSN2_NON_IMM_VS_CASE(le):
            SIR_INSN2_NON_IMM_VS_CASE(ge):
                vs_p = TRUE;
                /* fall through */
            SIR_INSN2_NON_IMM_NON_VS_CASES(eq):
            SIR_INSN2_NON_IMM_NON_VS_CASES(lt):
            SIR_INSN2_NON_IMM_NON_VS_CASES(gt):
            SIR_INSN2_NON_IMM_NON_VS_CASES(le):
            SIR_INSN2_NON_IMM_NON_VS_CASES(ge):
                bool_p = TRUE;
                goto non_imm;
            SIR_INSN2_NON_IMM_VS_CASE(aref):
                vs_p = TRUE;
                /* fall through */
            SIR_INSN2_NON_IMM_NON_VS_CASES(aref):
                if (inspect_attr1 != ATTR_ARRAY || inspect_attr2 != ATTR_FIXNUM) goto again;
                aref_p = TRUE;
                break;
            SIR_INSN2_NON_IMM_VS_CASE(or):
            SIR_INSN2_NON_IMM_VS_CASE(and):
                vs_p = TRUE;
                /* fall through */
            SIR_INSN2_NON_IMM_NON_VS_CASES(or):
            SIR_INSN2_NON_IMM_NON_VS_CASES(and):
                if (inspect_attr1 != ATTR_FIXNUM || inspect_attr2 != ATTR_FIXNUM) goto again;
                break;
            SIR_INSN2_NON_IMM_VS_CASE(plus):
            SIR_INSN2_NON_IMM_VS_CASE(minus):
            SIR_INSN2_NON_IMM_VS_CASE(mult):
            SIR_INSN2_NON_IMM_VS_CASE(div):
            SIR_INSN2_NON_IMM_VS_CASE(mod):
                vs_p = TRUE;
                /* fall through */
            SIR_INSN2_NON_IMM_NON_VS_CASES(plus):
            SIR_INSN2_NON_IMM_NON_VS_CASES(minus):
            SIR_INSN2_NON_IMM_NON_VS_CASES(mult):
            SIR_INSN2_NON_IMM_NON_VS_CASES(div):
            SIR_INSN2_NON_IMM_NON_VS_CASES(mod):
            non_imm:
                if (!(inspect_attr1 == ATTR_FIXNUM && inspect_attr2 == ATTR_FIXNUM)
                    && !(inspect_attr1 == ATTR_FLONUM && inspect_attr2 == ATTR_FLONUM)) goto again;
                break;
            case SIR_NAME2(aset, viv):
            case SIR_NAME2(aset, vis):
                v_p = TRUE;
                /* fall through */
            case SIR_NAME2(aset, sis):
            case SIR_NAME2(aset, siv):
                SIR_GEN_ASSERT(inspect_attr2 == ATTR_UNKNOWN);
                inspect_attr2 = (FIXNUM_P(operands[v_p ? 1 : 0]) ? ATTR_FIXNUM : ATTR_UNKNOWN);
                /* fall through */
            case OPT_NAME(aset):
            case SIR_NAME2(aset, svv):
            case SIR_NAME2(aset, svs):
            case SIR_NAME2(aset, ssv):
            case SIR_NAME2(aset, vss):
            case SIR_NAME2(aset, vvv):
            case SIR_NAME2(aset, vvs):
            case SIR_NAME2(aset, vsv):
                if (inspect_attr1 != ATTR_ARRAY || inspect_attr2 != ATTR_FIXNUM) goto again;
                aset_p = TRUE;
                new_insn_id = get_sir_speculative_insn3_id(insn_id, inspect_attr1, inspect_attr2, bbv->used_bops);
                SIR_GEN_ASSERT(new_insn_id != BIN(nop));
                get_insn3_attrs(iseq, insn_id, nops, operands, stack_size,
                                &v_p, &v2_p, &imm2_p, &op1_attr, &op2_attr, &op3_attr);
                if (get_sir_spec_insn3_id(insn_id, op1_attr, op2_attr, bbv->used_bops) != BIN(nop))
                    goto again; /* process for non-speculative type specialization */
                /* setup var attrs from speculative insn */
                if (v_p) setup_attr(get_lvar_num(iseq, operands[0]), inspect_attr1);
                if (v2_p) setup_attr(get_lvar_num(iseq, operands[v_p ? 1 : 0]), inspect_attr2);
                for (int i = 0; i < nops; i++) new_operands[i] = operands[i];
                goto add_speculative;
            default: goto again;
            }
            SIR_GEN_ASSERT(len >= 2);
            attr = inspect_attr1;
            new_insn_id = get_sir_speculative_insn2_id(insn_id, attr, bbv->used_bops, &exch_p);
            if (new_insn_id == BIN(nop)) goto again;
            get_insn2_attrs(iseq, nops, imm_p, imm2_p, vs_p, operands, stack_size, &op1_attr, &op2_attr);
            op1_attr = get_res_attr(FALSE, imm_p, imm2_p, op1_attr, op2_attr, &flag);
            if (get_sir_spec_insn2_id(insn_id, op1_attr, bbv->used_bops, flag, &exch2_p) != BIN(nop))
                goto again; /* process for non-speculative type specialization */
            /* setup var attrs from speculative insn */
            if (imm_p) {
                if (nops >= 2) setup_attr(get_lvar_num(iseq, operands[1]), inspect_attr2);
            } else if (imm2_p) {
                if (nops >= 2) setup_attr(get_lvar_num(iseq, operands[0]), inspect_attr1);
            } else if (nops == 1) {
                setup_attr(get_lvar_num(iseq, operands[0]), vs_p ? inspect_attr1 : inspect_attr2);
            } else if (nops >= 2) {
                setup_attr(get_lvar_num(iseq, operands[0]), inspect_attr1);
                setup_attr(get_lvar_num(iseq, operands[1]), inspect_attr2);
            }
            if (nops == 1) {
                new_operands[0] = operands[0];
                if ((imm_p || imm2_p) && attr == ATTR_FLONUM)
                    new_operands[0] = sir_dbl2val(RFLOAT_VALUE(new_operands[0]));
            } else if (nops == 2) {
                if (exch_p) {
                    SIR_GEN_ASSERT(attr == ATTR_FIXNUM);
                    new_operands[0] = operands[1];
                    new_operands[1] = operands[0];
                } else {
                    new_operands[0] = operands[0];
                    new_operands[1] = operands[1];
                    if (attr == ATTR_FLONUM) {
                        if (imm_p)
                            new_operands[0] = sir_dbl2val(RFLOAT_VALUE(new_operands[0]));
                        else if (imm2_p)
                            new_operands[1] = sir_dbl2val(RFLOAT_VALUE(new_operands[1]));
                    }
                }
            }
        add_speculative:
            add_spec_insn_arr(orig_pos, new_insn_id, nops, new_operands); /* excluding cd */
            inc = insn_stack_increase(new_insn_id, new_operands);
            stack_size += inc - 1;
            if (aref_p || aset_p) attr = ATTR_UNKNOWN;
            if (bool_p) attr = ATTR_BOOL;
            goto push;
        case BIN(swap):
            attr = get_attr(get_temp_num(iseq, stack_size - 1));
            SIR_GEN_ASSERT(stack_size >= 2);
            setup_attr(get_temp_num(iseq, stack_size - 1), get_attr(get_temp_num(iseq, stack_size - 2)));
            setup_attr(get_temp_num(iseq, stack_size - 2), attr);
            add_spec_insn_arr(orig_pos, insn_id, len - 1, operands);
            break;
        case BIN(setn):
            n = (int)operands[0];
            SIR_GEN_ASSERT(stack_size > n);
            setup_attr(get_temp_num(iseq, stack_size - n - 1), get_attr(get_temp_num(iseq, stack_size - 1)));
            add_spec_insn_arr(orig_pos, insn_id, len - 1, operands);
            break;
        case BIN(dup):
            n = 1;
            goto dup;
        case BIN(dupn): /* ??? optimize: when values are only putobject|getlocal */
            n = (int)operands[0];
        dup:
            SIR_GEN_ASSERT(stack_size >= n);
            for (i = 0; i < n; i++)
                setup_attr(get_temp_num(iseq, stack_size + i), get_attr(get_temp_num(iseq, stack_size - 1 - i)));
            stack_size += n;
            add_spec_insn_arr(orig_pos, insn_id, len - 1, operands);
            break;
        case BIN(nop):
            update_labeled(new_insn_pos);
            goto rest_cases;
        SIR_INSN2_NON_IMM_VS_CASE(neq):
            vs_p = TRUE;
            /* fall through */
        SIR_INSN2_NON_IMM_NON_VS_CASES(neq):
            neq_p = bool_p = TRUE;
            goto insn_op2;
        SIR_INSN2_IMM_CASES(neq):
            neq_p = TRUE;
            /* fall through */
        SIR_INSN2_IMM_CASES(eq):
        SIR_INSN2_IMM_CASES(lt):
        SIR_INSN2_IMM_CASES(gt):
        SIR_INSN2_IMM_CASES(le):
        SIR_INSN2_IMM_CASES(ge):
            bool_p = imm_p = TRUE;
            goto insn_op2;
        SIR_INSN2_IMM_CASES(aref):
            aref_p = imm_p = TRUE;
            goto insn_op2;
        SIR_INSN2_IMM2_CASES(neq):
            neq_p = TRUE;
            /* fall through */
        SIR_INSN2_IMM2_CASES(eq):
        SIR_INSN2_IMM2_CASES(lt):
        SIR_INSN2_IMM2_CASES(gt):
        SIR_INSN2_IMM2_CASES(le):
        SIR_INSN2_IMM2_CASES(ge):
            bool_p = imm2_p = TRUE;
            goto insn_op2;
        SIR_INSN2_IMM2_CASES(aref):
            aref_p = imm2_p = TRUE;
            goto insn_op2;
        SIR_INSN2_NON_IMM_VS_CASE(eq):
        SIR_INSN2_NON_IMM_VS_CASE(lt):
        SIR_INSN2_NON_IMM_VS_CASE(gt):
        SIR_INSN2_NON_IMM_VS_CASE(le):
        SIR_INSN2_NON_IMM_VS_CASE(ge):
            vs_p = TRUE;
            /* fall through */
        SIR_INSN2_NON_IMM_NON_VS_CASES(eq):
        SIR_INSN2_NON_IMM_NON_VS_CASES(lt):
        SIR_INSN2_NON_IMM_NON_VS_CASES(gt):
        SIR_INSN2_NON_IMM_NON_VS_CASES(le):
        SIR_INSN2_NON_IMM_NON_VS_CASES(ge):
            bool_p = TRUE;
            goto insn_op2;
        SIR_INSN2_NON_IMM_VS_CASE(aref):
            vs_p = TRUE;
            /* fall through */
        SIR_INSN2_NON_IMM_NON_VS_CASES(aref):
            aref_p = TRUE;
            goto insn_op2;
        SIR_INSN2_IMM_CASES(plus):
        SIR_INSN2_IMM_CASES(minus):
        SIR_INSN2_IMM_CASES(mult):
        SIR_INSN2_IMM_CASES(div):
        SIR_INSN2_IMM_CASES(mod):
        SIR_INSN2_IMM_CASES(or):
        SIR_INSN2_IMM_CASES(and):
            imm_p = TRUE;
            goto insn_op2;
        SIR_INSN2_IMM2_CASES(plus):
        SIR_INSN2_IMM2_CASES(minus):
        SIR_INSN2_IMM2_CASES(mult):
        SIR_INSN2_IMM2_CASES(div):
        SIR_INSN2_IMM2_CASES(mod):
        SIR_INSN2_IMM2_CASES(or):
        SIR_INSN2_IMM2_CASES(and):
            imm2_p = TRUE;
            goto insn_op2;
        SIR_INSN2_NON_IMM_VS_CASE(plus):
        SIR_INSN2_NON_IMM_VS_CASE(minus):
        SIR_INSN2_NON_IMM_VS_CASE(mult):
        SIR_INSN2_NON_IMM_VS_CASE(div):
        SIR_INSN2_NON_IMM_VS_CASE(mod):
            vs_p = TRUE;
            /* fall through */
        SIR_INSN2_NON_IMM_NON_VS_CASES(plus):
        SIR_INSN2_NON_IMM_NON_VS_CASES(minus):
        SIR_INSN2_NON_IMM_NON_VS_CASES(mult):
        SIR_INSN2_NON_IMM_NON_VS_CASES(div):
        SIR_INSN2_NON_IMM_NON_VS_CASES(mod):
        SIR_INSN2_NON_IMM_NON_VS_CASES(or):
        SIR_INSN2_NON_IMM_NON_VS_CASES(and):
        insn_op2:
            SIR_GEN_ASSERT(!imm_p || !imm2_p);
            nops = len - (neq_p ? 3 : 2); /* insn operands w/o cd(s) */
            SIR_GEN_ASSERT(nops <= 2 && stack_size + nops >= 2);
            get_insn2_attrs(iseq, nops, imm_p, imm2_p, vs_p, operands, stack_size, &op1_attr, &op2_attr);
            attr = get_res_attr(aref_p, imm_p, imm2_p, op1_attr, op2_attr, &flag);
            new_insn_id = get_sir_spec_insn2_id(insn_id, attr, bbv->used_bops, flag, &exch_p);
            if (new_insn_id == BIN(nop)) {
                if (profile_p) goto rest_cases;
                if (imm_p && (!FIXNUM_P(operands[0]) && !fp_num_p(operands[0]))) goto rest_cases;
                if (imm2_p && (!FIXNUM_P(operands[nops == 1 ? 0 : 1])
                               && !fp_num_p(operands[nops == 1 ? 0 : 1]))) goto rest_cases;
                /* Always generate inspection for 1st and then 2nd operand: */
                if (nops == 0) {
                    new_operands[0] = 1;
                    add_spec_insn_arr(orig_pos, BIN(sir_inspect_stack_type), 1, new_operands);
                    new_operands[0] = 0;
                    add_spec_insn_arr(orig_pos, BIN(sir_inspect_stack_type), 1, new_operands);
                } else if (nops == 1) {
                    if (vs_p) {
                        new_operands[0] = operands[0];
                        add_spec_insn_arr(orig_pos, BIN(sir_inspect_type), 1, new_operands);
                        new_operands[0] = 0;
                        add_spec_insn_arr(orig_pos, BIN(sir_inspect_stack_type), 1, new_operands);
                    } else {
                        new_operands[0] = 0;
                        add_spec_insn_arr(orig_pos, BIN(sir_inspect_stack_type), 1, new_operands);
                        if (!imm_p && !imm2_p) {
                            new_operands[0] = operands[0];
                            add_spec_insn_arr(orig_pos, BIN(sir_inspect_type), 1, new_operands);
                        }
                    }
                } else {
                    SIR_GEN_ASSERT(nops == 2);
                    if (imm_p || imm2_p) {
                        new_operands[0] = imm2_p ? operands[0] : operands[1];
                        add_spec_insn_arr(orig_pos, BIN(sir_inspect_type), 1, new_operands);
                    } else {
                        new_operands[0] = operands[0];
                        add_spec_insn_arr(orig_pos, BIN(sir_inspect_type), 1, new_operands);
                        new_operands[0] = operands[1];
                        add_spec_insn_arr(orig_pos, BIN(sir_inspect_type), 1, new_operands);
                    }
                }
                goto rest_cases;
            }
            stack_size -= (2 - nops);
            if (nops == 2 && exch_p) {
                SIR_GEN_ASSERT(attr != ATTR_FLONUM);
                new_operands[0] = operands[1];
                new_operands[1] = operands[0];
                add_spec_insn_arr(orig_pos, new_insn_id, 2, new_operands);
            } else if (attr == ATTR_FLONUM && imm_p) {
                new_operands[0] = sir_dbl2val(FIXNUM_P(operands[0])
                                              ? (double)FIX2LONG(operands[0]) : RFLOAT_VALUE(operands[0]));
                if (nops == 2) new_operands[1] = operands[1];
                add_spec_insn_arr(orig_pos, new_insn_id, nops, new_operands);
            } else if (attr == ATTR_FLONUM && imm2_p) {
                if (nops == 2) new_operands[0] = operands[0];
                new_operands[nops - 1] = sir_dbl2val(FIXNUM_P(operands[nops - 1])
                                                     ? (double)FIX2LONG(operands[nops - 1])
                                                     : RFLOAT_VALUE(operands[nops - 1]));
                add_spec_insn_arr(orig_pos, new_insn_id, nops, new_operands);
            } else {
                add_spec_insn_arr(orig_pos, new_insn_id, nops, operands);
            }
            if (aref_p) attr = ATTR_UNKNOWN;
            if (bool_p) attr = ATTR_BOOL;
            goto push;
        case SIR_NAME2(aset, sis):
        case SIR_NAME2(aset, siv):
        case SIR_NAME2(aset, viv):
        case SIR_NAME2(aset, vis):
        case OPT_NAME(aset):
        case SIR_NAME2(aset, svv):
        case SIR_NAME2(aset, svs):
        case SIR_NAME2(aset, ssv):
        case SIR_NAME2(aset, vss):
        case SIR_NAME2(aset, vvv):
        case SIR_NAME2(aset, vvs):
        case SIR_NAME2(aset, vsv):
            get_insn3_attrs(iseq, insn_id, nops, operands, stack_size,
                            &v_p, &v2_p, &imm2_p, &op1_attr, &op2_attr, &op3_attr);
            nops = len - 2; /* insn operands w/o cd */
            SIR_GEN_ASSERT(nops <= 3 && stack_size + nops >= 3);
            new_insn_id = get_sir_spec_insn3_id(insn_id, op1_attr, op2_attr, bbv->used_bops);
            if (new_insn_id == BIN(nop)) {
                if (profile_p) goto rest_cases;
                if (imm2_p && !FIXNUM_P(operands[v_p ? 1 : 0])) goto rest_cases;
                if (nops == 0) { /* sss */
                    new_operands[0] = 1;
                    add_spec_insn_arr(orig_pos, BIN(sir_inspect_stack_type), 1, new_operands);
                    new_operands[0] = 0;
                    add_spec_insn_arr(orig_pos, BIN(sir_inspect_stack_type), 1, new_operands);
                } else if (nops == 1) {
                    if (v_p) { /* vss */
                        new_operands[0] = operands[0];
                        add_spec_insn_arr(orig_pos, BIN(sir_inspect_type), 1, new_operands);
                    } else { /* sis|svs|ssv */
                        new_operands[0] = 1;
                        add_spec_insn_arr(orig_pos, BIN(sir_inspect_stack_type), 1, new_operands);
                    }
                    if (v2_p) { /* svs */
                        new_operands[0] = operands[0];
                        add_spec_insn_arr(orig_pos, BIN(sir_inspect_type), 1, new_operands);
                    } else if (!imm2_p) { /* vss|ssv */
                        new_operands[0] = v_p ? 1 : 0;
                        add_spec_insn_arr(orig_pos, BIN(sir_inspect_stack_type), 1, new_operands);
                    }
                } else if (nops == 2) {
                    SIR_GEN_ASSERT(nops == 2);
                    if (v_p) { /* vvs | vis | vsv */
                        new_operands[0] = operands[0];
                        add_spec_insn_arr(orig_pos, BIN(sir_inspect_type), 1, new_operands);
                    } else { /* svv | siv */
                        new_operands[0] = 0;
                        add_spec_insn_arr(orig_pos, BIN(sir_inspect_stack_type), 1, new_operands);
                    }
                    if (v2_p) { /* vvs | svv */
                        new_operands[0] = operands[v_p ? 1 : 0];
                        add_spec_insn_arr(orig_pos, BIN(sir_inspect_type), 1, new_operands);
                    } else if (!imm2_p) { /* vsv */
                        new_operands[0] = 0;
                        add_spec_insn_arr(orig_pos, BIN(sir_inspect_stack_type), 1, new_operands);
                    }
                } else {
                    SIR_GEN_ASSERT(nops == 3 && !imm2_p && v_p && v2_p);
                    new_operands[0] = operands[0];
                    add_spec_insn_arr(orig_pos, BIN(sir_inspect_type), 1, new_operands);
                    new_operands[0] = operands[1];
                    add_spec_insn_arr(orig_pos, BIN(sir_inspect_type), 1, new_operands);
                }
                goto rest_cases;
            }
            attr = op3_attr;
            stack_size -= (3 - nops);
            add_spec_insn_arr(orig_pos, new_insn_id, nops, operands);
            goto push;
        SIR_INSN2_SPEC_STACK_CASES(iplus):
        SIR_INSN2_SPEC_STACK_CASES(iminus):
        SIR_INSN2_SPEC_STACK_CASES(imult):
        SIR_INSN2_SPEC_STACK_CASES(idiv):
        SIR_INSN2_SPEC_STACK_CASES(imod):
        SIR_INSN2_SPEC_STACK_CASES0(ior):
        SIR_INSN2_SPEC_STACK_CASES0(iand):
            inc = insn_stack_increase(insn_id, operands);
            stack_size += inc - 1;
            attr = ATTR_FIXNUM; /* attr of temp */
            goto add_and_push;
        SIR_INSN2_SPEC_LVAR_CASES(iplus):
        SIR_INSN2_SPEC_LVAR_CASES(iminus):
        SIR_INSN2_SPEC_LVAR_CASES(imult):
        SIR_INSN2_SPEC_LVAR_CASES(idiv):
        SIR_INSN2_SPEC_LVAR_CASES(imod):
        SIR_INSN2_SPEC_LVAR_CASES0(ior):
        SIR_INSN2_SPEC_LVAR_CASES0(iand):
            setup_attr(get_lvar_num(iseq, operands[0]), ATTR_FIXNUM); /* attr of lvar */
            goto rest_cases;
        SIR_INSN2_SPEC_STACK_CASES(fplus):
        SIR_INSN2_SPEC_STACK_CASES(fminus):
        SIR_INSN2_SPEC_STACK_CASES(fmult):
        SIR_INSN2_SPEC_STACK_CASES(fdiv):
        SIR_INSN2_SPEC_STACK_CASES(fmod):
            inc = insn_stack_increase(insn_id, operands);
            stack_size += inc - 1;
            attr = ATTR_FLONUM; /* attr of temp */
            goto add_and_push;
        SIR_INSN2_SPEC_STACK_CASES(feq):
        SIR_INSN2_SPEC_STACK_CASES(fneq):
        SIR_INSN2_SPEC_STACK_CASES(flt):
        SIR_INSN2_SPEC_STACK_CASES(fgt):
        SIR_INSN2_SPEC_STACK_CASES(fle):
        SIR_INSN2_SPEC_STACK_CASES(fge):
            inc = insn_stack_increase(insn_id, operands);
            stack_size += inc - 1;
            attr = ATTR_BOOL; /* attr of temp */
            goto add_and_push;
        SIR_INSN2_SPEC_LVAR_CASES(feq):
        SIR_INSN2_SPEC_LVAR_CASES(fneq):
        SIR_INSN2_SPEC_LVAR_CASES(flt):
        SIR_INSN2_SPEC_LVAR_CASES(fgt):
        SIR_INSN2_SPEC_LVAR_CASES(fle):
        SIR_INSN2_SPEC_LVAR_CASES(fge):
            setup_attr(get_lvar_num(iseq, operands[0]), ATTR_BOOL); /* attr of lvar */
            goto rest_cases;
        case BIN(sir_getinlinecache):
            if (profile_p) {
                struct iseq_inline_constant_cache_entry *ice = ((IC)operands[2])->entry;
                if (ice && ice->ic_cref == NULL) insn_id = BIN(sir_spec_getinlinecache);
            }
        case BIN(sir_spec_getinlinecache):
        case BIN(sir_jump):
        BRANCH_CASES(ibeq):
        BRANCH_CASES(ibneq):
        BRANCH_CASES(iblt):
        BRANCH_CASES(ibgt):
        BRANCH_CASES(ible):
        BRANCH_CASES(ibge):
            if (!profile_p) {
                new_operands[0] = (VALUE)bbv;
                add_spec_insn_arr(orig_pos, BIN(specialize_bbv), 1, new_operands);
            }
            goto rest_cases;
        case BIN(sir_branch):
            if (!profile_p) {
                new_operands[0] = (VALUE)bbv;
                add_spec_insn_arr(orig_pos, BIN(specialize_bbv), 1, new_operands);
            }
            if (prev_new_insn_pos < 0 || specialize_bbv_pos >= 0) goto rest_cases;
            if (pos_in_catch_table_p(iseq, stack_insn_pos, TRUE)) goto rest_cases;
            prev_new_insn_id = (enum ruby_vminsn_type)VARR_GET(VALUE, bbv_encoded, prev_new_insn_pos);
            new_insn_id = get_branch_sir_insn_id(prev_new_insn_id);
            if (new_insn_id == BIN(nop)) goto rest_cases;
            nops = insn_len(new_insn_id) - 1;
            new_operands[0] = operands[0];
            new_operands[1] = operands[1];
            prev_new_insn_nops = insn_len(prev_new_insn_id) - 1;
            SIR_GEN_ASSERT(prev_new_insn_nops <= 2);
            if (prev_new_insn_nops >= 1)
                new_operands[nops - 1] = VARR_GET(VALUE, bbv_encoded, prev_new_insn_pos + prev_new_insn_nops);
            if (prev_new_insn_nops >= 2)
                new_operands[nops - 2] = VARR_GET(VALUE, bbv_encoded, prev_new_insn_pos + prev_new_insn_nops - 1);
            add_spec_insn_arr(orig_pos, new_insn_id, nops, new_operands);
            nullify(prev_new_insn_pos);
            sir_branch_offset_add = len - insn_len(new_insn_id);
            goto stack_update;
        case BIN(setlocal):
            if (operands[1] != 0) goto rest_cases;
            /* fallthrough */
        case BIN(setlocal_WC_0):
        case BIN(sir_setlocal):
            SIR_GEN_ASSERT(stack_size > 0);
            attr = get_attr(get_temp_num(iseq, stack_size - 1));
            if ((bbv->env_p && attr != ATTR_FIXNUM && attr != ATTR_BOOL)
                || pos_in_catch_table_p(iseq, stack_insn_pos, TRUE)) {
                SIR_GEN_ASSERT(insn_id != BIN(sir_setlocal));
                setup_attr(get_lvar_num(iseq, operands[0]), attr); /* attr of temp */
                goto rest_cases;
            }
            op = operands[0];
            setup_attr(get_lvar_num(iseq, op), attr); /* attr of temp */
            new_operands[0] = (insn_id == BIN(sir_setlocal) ? operands[0] : (VALUE)-(EPDISP)operands[0]);
            if (prev_new_insn_pos < 0) goto sir_setlocal;
            prev_new_insn_id = (enum ruby_vminsn_type)VARR_GET(VALUE, bbv_encoded, prev_new_insn_pos);
            new_insn_id = get_var_res_sir_insn_id(prev_new_insn_id);
            if (new_insn_id == BIN(nop)) goto sir_setlocal;
            nops = insn_len(new_insn_id) - 1;
            SIR_GEN_ASSERT(nops <= 3); /* longest variant is vvv or vvi */
            if (nops > 1)
                new_operands[1] = VARR_GET(VALUE, bbv_encoded, prev_new_insn_pos + 1); /* ops[0] */
            if (nops > 2)
                new_operands[2] = VARR_GET(VALUE, bbv_encoded, prev_new_insn_pos + 2); /* ops[1] */
            add_insn_arr(VARR_GET(int, pos_map, prev_new_insn_pos), new_insn_id, nops, new_operands);
            nullify(prev_new_insn_pos);
            goto stack_update;
        sir_setlocal:
            add_spec_insn_arr(orig_pos, BIN(sir_setlocal), 1, new_operands);
            goto stack_update;
        case BIN(getinstancevariable): /* to do specialized insn at the execution */
            SIR_GEN_ASSERT(len == 3);
            if (profile_p) goto rest_cases;
            add_spec_insn_arr(orig_pos, BIN(sir_make_getivar), 2, operands);
            goto stack_update;
        case BIN(setinstancevariable): /* to do specialized insn at the execution */
            SIR_GEN_ASSERT(len == 3);
            if (profile_p) goto rest_cases;
            add_spec_insn_arr(orig_pos, BIN(sir_make_setivar), 2, operands);
            goto stack_update;
        case BIN(opt_send_without_block): {
            CALL_DATA cd = (CALL_DATA)operands[0];
            int argc = vm_ci_argc(cd->ci);
            unsigned int flags = vm_ci_flag(cd->ci);

            SIR_GEN_ASSERT(len == 2 && (flags & VM_CALL_ARGS_BLOCKARG) == 0);
            if (profile_p || IS_ARGS_SPLAT(cd->ci) || IS_ARGS_KW_OR_KW_SPLAT(cd->ci)
                || (flags & VM_CALL_TAILCALL) != 0) goto rest_cases;
            new_operands[0] = operands[0];
            new_operands[1] = (VALUE)get_send_info(bbv, NULL, get_call_attrs(iseq, argc, stack_size),
                                                   Qundef, NULL, argc, vm_ci_mid(cd->ci));
            add_spec_insn_arr(orig_pos, BIN(sir_make_send), 2, new_operands);
            goto stack_update;
        }
        case BIN(send): {
            CALL_DATA cd = (CALL_DATA)operands[0];
            int argc = vm_ci_argc(cd->ci);
            unsigned int flags = vm_ci_flag(cd->ci);

            SIR_GEN_ASSERT(len == 3);
            update_attrs((rb_iseq_t *)operands[1]);
            if (profile_p || IS_ARGS_SPLAT(cd->ci) || IS_ARGS_KW_OR_KW_SPLAT(cd->ci)
                || (flags & VM_CALL_TAILCALL) != 0 || (flags & VM_CALL_ARGS_BLOCKARG) != 0) goto rest_cases;
            new_operands[0] = operands[0];
            new_operands[1] = (VALUE)get_send_info(bbv, (rb_iseq_t *)operands[1], get_call_attrs(iseq, argc, stack_size),
                                                   Qundef, NULL, argc, vm_ci_mid(cd->ci));
            add_spec_insn_arr(orig_pos, BIN(sir_make_send), 2, new_operands);
            goto stack_update;
        }
        case BIN(invokesuper): {
            update_attrs((rb_iseq_t *)operands[1]);
            goto rest_cases;
        }
        case BIN(sir_igetivar):
            SIR_GEN_ASSERT(profile_p);
            attr = ATTR_FIXNUM;
            goto add_and_push;
        case BIN(sir_send_without_block):
            SIR_GEN_ASSERT(profile_p);
            new_operands[0] = operands[0];
            add_spec_insn_arr(orig_pos, BIN(opt_send_without_block), 1, new_operands);
            goto stack_update;
        case BIN(sir_getivar):
        case BIN(sir_setivar):
        case BIN(sir_send_ivget):
        case BIN(sir_send_ivset):
            SIR_GEN_ASSERT(profile_p);
            goto rest_cases;
        case BIN(sir_make_send): {
            struct rb_sir_send_info *si = (struct rb_sir_send_info *)operands[1];

            SIR_GEN_ASSERT(profile_p);
            update_attrs(si->block_iseq);
            /* sir_make_send is not changed if we call cfunc and leave it by a throw
               (e.g. by break in a nested block) -- specialize it here: */
            if (change_to_cfunc_send(&body->bbv_code[curr_sir_insn_pos], iseq)) goto again;
            goto rest_cases;
        }
        case BIN(sir_cfunc_send): {
            SEND_INFO si = (SEND_INFO)operands[0];
            rb_iseq_t *block_iseq = (rb_iseq_t *)operands[1];
            const struct rb_callable_method_entry_struct *cme = si->cme;
            const rb_method_cfunc_t *cfunc;
            rb_iseq_bbv_t block_bbv;
            char temp_attrs[3];
            OFFSET disp;
            int (*start_func)(VALUE, VALUE *);
            int (*arg_func)(VALUE, VALUE, VALUE *);
            VALUE (*cont_func)(VALUE, VALUE);

            if (block_iseq == NULL) goto rest_cases;
            update_attrs(block_iseq);
            SIR_GEN_ASSERT(cme != NULL && cme->def->type == VM_METHOD_TYPE_CFUNC && profile_p);
            if (!rb_simple_iseq_p(block_iseq)) goto rest_cases;
            cfunc = UNALIGNED_MEMBER_PTR(cme->def, body.cfunc);
            SIR_GEN_ASSERT(stack_size >= 1);
            if (!sir_get_iter_funcs(cfunc->func, &start_func, &arg_func, &cont_func, temp_attrs)) goto rest_cases;
            new_operands[0] = (lindex_t)start_func;
            add_spec_insn_arr(orig_pos, BIN(sir_iter_start), 1, new_operands);
            disp = 0;
            if (mirjit_enabled_p)
                disp = insn_len(BIN(sir_bbv_cont));
            new_operands[0] = (OFFSET)insn_len(BIN(sir_iter_cont)) + disp; /* to iter_finish */
            temp_attrs[1] = ATTR_END_MARKER;
            block_bbv = get_bbv(block_iseq, RB_BBV_REGULAR, 0, 0, FALSE,
                                temp_attrs[0] == ATTR_UNKNOWN ? 0 : 2,
                                temp_attrs[0] == ATTR_UNKNOWN ? NULL : temp_attrs);
            block_bbv->cme = cme;
            if (block_bbv->bbv_start < 0)
                VARR_PUSH(rb_iseq_bbv_t, bbvs, block_bbv);
            /* if ntimes is redefined the caller block is already recorded by cfunc */
            new_operands[1] = (lindex_t)block_bbv;
            new_operands[2] = (lindex_t)arg_func;
            add_spec_insn_arr(orig_pos, BIN(sir_iter_body), 3, new_operands);
            disp = 0;
#ifdef MIRJIT_ITER_INSNS
            if (mirjit_enabled_p) {
                add_spec_insn_arr(orig_pos, BIN(sir_bbv_cont), 1, (VALUE *)&bbv);
                disp = insn_len(BIN(sir_bbv_cont));
            }
#endif
            /* to int_iter */
            new_operands[0] = -(OFFSET)(disp + insn_len(BIN(sir_iter_body)) + insn_len(BIN(sir_iter_cont)));
            new_operands[1] = (lindex_t)cont_func;
            add_spec_insn_arr(orig_pos, BIN(sir_iter_cont), 2, new_operands);
            if (mirjit_enabled_p) {
                add_spec_insn_arr(orig_pos, BIN(sir_bbv_cont), 1, (VALUE *)&bbv);
                disp = insn_len(BIN(sir_bbv_cont));
            }
            goto stack_update;
        }
        case BIN(sir_iter_body): {
            SIR_GEN_ASSERT(profile_p);
            body_bbv = (rb_iseq_bbv_t)operands[1];
            update_attrs(body_bbv->iseq);
            goto rest_cases;
        }
        case BIN(sir_iseq_send_without_block): {
            /* attrs can be changed so the target bbv can */
            int argc = ((SEND_INFO)operands[0])->argc;

            if (mirjit_enabled_p) add_bbv_cont_p = TRUE;
            SIR_GEN_ASSERT(profile_p);
            VARR_TRUNC(char, arg_attrs, 0);
            for (i = 0; i < argc; i++)
                VARR_PUSH(char, arg_attrs, get_attr(get_temp_num(iseq, stack_size - argc + i)));
            compact_attrs(arg_attrs);
            old_dst_bbv = (rb_iseq_bbv_t)operands[1];
            dst_bbv = get_bbv(old_dst_bbv->iseq, old_dst_bbv->type, old_dst_bbv->index, old_dst_bbv->iseq_start,
                              old_dst_bbv->env_p, (int)VARR_LENGTH(char, arg_attrs), VARR_ADDR(char, arg_attrs));
            SIR_GEN_ASSERT(len == 3);
            if (dst_bbv != old_dst_bbv) {
                if (debug_level >= 1)
                    fprintf(stderr, "--> Changing sir_iseq_send_without_block from BBVx%llx to BBVx%llx\n",
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
        case BIN(duparray):
            attr = ATTR_ARRAY;
            goto add_and_push;
        case BIN(newarray):
        case BIN(newarraykwsplat): {
            rb_num_t num = (rb_num_t)operands[0];
            int new_size = stack_size - (int)num;
            SIR_GEN_ASSERT(new_size >= 0);
            stack_size = new_size;
            truncate_attrs(get_temp_num(iseq, new_size));
            attr = ATTR_ARRAY;
            goto add_and_push;
        }
        case BIN(splatarray):
            SIR_GEN_ASSERT(stack_size > 0);
            stack_size--;
            truncate_attrs(get_temp_num(iseq, stack_size));
            attr = ATTR_ARRAY;
            goto add_and_push;
        case BIN(concatarray):
            SIR_GEN_ASSERT(stack_size > 1);
            stack_size -= 2;
            truncate_attrs(get_temp_num(iseq, stack_size));
            attr = ATTR_ARRAY;
            goto add_and_push;
        case BIN(sir_mov):
            val = operands[1];
            attr = get_attr(get_lvar_num(iseq, operands[1]));
            setup_attr(get_lvar_num(iseq, operands[0]), attr);
            add_spec_insn_arr(orig_pos, insn_id, len - 1, operands);
            break;
        case BIN(sir_movi):
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
        case BIN(sir_make_bbv):
            SIR_GEN_ASSERT(FALSE); /* cannot occur in SIR */
            break;
        case BIN(opt_case_dispatch):
            if (!profile_p) {
                new_operands[0] = (VALUE)bbv;
                add_spec_insn_arr(orig_pos, BIN(specialize_bbv), 1, new_operands);
            }
            SIR_GEN_ASSERT(len == 3);
            new_operands[0] = rb_hash_new(); /* to duplicate */
            RHASH_TBL_RAW(new_operands[0])->type = &cdhash_type;
            RB_OBJ_WRITTEN(iseq, Qundef, new_operands[0]);
            new_operands[1] = operands[1];
            add_spec_insn_arr(orig_pos, insn_id, 2, new_operands);
            goto stack_update;
        case BIN(sir_gen_bbv):
        case BIN(sir_gen_bbv2):
        case BIN(sir_gen_bbv_key):
            compact_attrs(attrs);
            old_dst_bbv = (rb_iseq_bbv_t)operands[0];
            dst_bbv = get_bbv_by_head(iseq, old_dst_bbv->head_bbv == NULL ? old_dst_bbv : old_dst_bbv->head_bbv,
                                      bbv->env_p, (int)VARR_LENGTH(char, attrs), VARR_ADDR(char, attrs));
            new_operands[0] = (VALUE)dst_bbv;
            new_operands[1] = (VALUE)((OFFSET)operands[1] + sir_branch_offset_add); /* offset of the branch */
            /* Set up offset in the branch as it can be redirected from sir_gen_bbv */
            branch_pos = (int)VARR_LENGTH(VALUE, bbv_encoded) + len + (int)new_operands[1];
            branch_id = (enum ruby_vminsn_type)VARR_GET(VALUE, bbv_encoded, branch_pos);
            offset = (int)VARR_LENGTH(VALUE, bbv_encoded) - branch_pos - insn_len(branch_id);
            if (insn_id == BIN(sir_gen_bbv)) {
                VARR_SET(VALUE, bbv_encoded, branch_pos + 1, (VALUE)(OFFSET)offset);
            } else if (insn_id == BIN(sir_gen_bbv2)) {
                VARR_SET(VALUE, bbv_encoded, branch_pos + 2, (VALUE)(OFFSET)offset);
            } else {
                SIR_GEN_ASSERT(branch_id == BIN(opt_case_dispatch));
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
            new_insn_pos = prev_new_insn_pos; // keeping it stack inconsistency
            // crash because icmpsxx;branch combining without popping val made by the icmp
            break; /* remove */
        default:
        rest_cases:
            add_spec_insn_arr(orig_pos, insn_id, len - 1, operands);
            if (add_bbv_cont_p)
                add_spec_insn_arr(orig_pos, BIN(sir_bbv_cont), 1, (VALUE *)&bbv);
        stack_update:
            if (insn_id == BIN(send) || insn_id == BIN(opt_send_without_block) || insn_id == BIN(invokesuper)
                || insn_id == BIN(sir_send_without_block)) {
                inc = (int)sp_inc_of_sendish(((CALL_DATA)operands[0])->ci);
            } else if (insn_id == BIN(sir_iseq_send_without_block) || insn_id == BIN(sir_cfunc_send)) {
                inc = (int)sp_inc_of_sendinfo((struct rb_sir_send_info *)operands[0]);
            } else {
                inc = insn_stack_increase(insn_id, operands);
            }
            if (inc >= 0) {
                for (i = 0; i < inc; i++)
                    setup_attr(get_temp_num(iseq, stack_size + i), ATTR_UNKNOWN);
                stack_size += inc;
            } else {
                int new_size = stack_size + inc;
                SIR_GEN_ASSERT(-inc <= stack_size);
                stack_size = new_size;
                truncate_attrs(get_temp_num(iseq, new_size));
            }
            retn = (int)insn_stack_retn(insn_id, operands);
            SIR_GEN_ASSERT(inc <= retn && stack_size >= retn);
            for (i = 0; i < retn; i++) {
                setup_attr(get_temp_num(iseq, stack_size - retn + i), ATTR_UNKNOWN);
            }
        }
    }
    if (profile_p && specialize_bbv_pos < 0)
        /* We can not find specialize_bbv as we use bbv code
           specialized in another ractor -- don't add a new code */
        return -1;
    remove_nops(&nops_num); // ??? modify out_gen_bbv_disps ... */
    len = (int)VARR_LENGTH(VALUE, bbv_encoded);
    bbv->bbv_start = get_bbv_code_space(iseq, len);
    bbv->bbv_bound = bbv->bbv_start + len;
    bbv_encode(VARR_ADDR(VALUE, bbv_encoded), len);
    memcpy(&body->bbv_code[bbv->bbv_start], VARR_ADDR(VALUE, bbv_encoded), len * sizeof(VALUE));
    VARR_TRUNC(VALUE, bbv_encoded, 0);
    memcpy(&body->bbv_pos_map[bbv->bbv_start], VARR_ADDR(int, pos_map),
           VARR_LENGTH(int, pos_map) * sizeof(int));
    VARR_TRUNC(int, pos_map, 0);
    if (body->send_info == NULL && VARR_LENGTH(send_info_t, iseq_send_info) != 0) {
        SIR_GEN_ASSERT(!profile_p);
        VARR_PUSH(send_info_t, iseq_send_info, NULL); /* end marker */
        body->send_info = SIR_ALLOC_N(struct rb_sir_send_info, VARR_LENGTH(send_info_t, iseq_send_info));
        memcpy(body->send_info, VARR_ADDR(send_info_t, iseq_send_info),
               VARR_LENGTH(send_info_t, iseq_send_info) * sizeof(struct rb_sir_send_info *));
    }
    compact_attrs(attrs);
    if (profile_p) { /* attrs can change -- remove targets and change source bbv destinations: */
        remove_out_edges(bbv);
        redirect_src_bbv_dests(bbv, FALSE);
    }
    SIR_GEN_ASSERT(bbv->out_edges == NULL);
    SIR_GEN_ASSERT(VARR_LENGTH(rb_iseq_bbv_t, out_bbvs) == VARR_LENGTH(int, out_gen_bbv_disps));
    for (i = 0; i < (int)VARR_LENGTH(rb_iseq_bbv_t, out_bbvs); i++)
        add_bbv_edge(bbv, VARR_GET(rb_iseq_bbv_t, out_bbvs, i), VARR_GET(int, out_gen_bbv_disps, i) - nops_num);
    sir_trace_update(iseq, bbv->bbv_start, bbv->bbv_start + len);
    if (debug_level >= 2) {
        fprintf(stderr, "## %s specialized BBV%d 0x%llx (%s):", profile_p ? "Profile" : "Type",
                bbv->num, (unsigned long long)bbv, bbv->env_p ? "env" : "stack");
        print_attrs(bbv->attrs);
        print_bb(iseq, body->bbv_code, bbv->bbv_start, bbv->bbv_bound, "  ");
        print_pos_map(iseq, bbv->bbv_start, bbv->bbv_bound, TRUE);
    }
    return specialize_bbv_pos < 0 ? -1 : bbv->bbv_start + specialize_bbv_pos - nops_num;
}

static void
change_insn(const rb_iseq_t *iseq, VALUE *pc, enum ruby_vminsn_type new_insn_id) {
    *pc = (VALUE)sir_dispatch_table[new_insn_id];
    sir_insn_trace_update(iseq, pc);
}

void
rb_sir_inspect_type(rb_control_frame_t *cfp, VALUE val, int stack_p) {
    int insn_id, new_insn_id;
    int fixtype_p = FIXNUM_P(val);
    int array_p = !SPECIAL_CONST_P(val) && RBASIC_CLASS(val) == rb_cArray;
#ifdef FLONUM_INSNS
    int flotype_p = FLONUM_P(val) || (!RB_SPECIAL_CONST_P(val) && RBASIC_CLASS(val) == rb_cFloat);
#else
    int flotype_p = FALSE;
#endif
    if ((!fixtype_p && (rb_vm_insn_decode(*cfp->pc) == BIN(sir_inspect_fixtype)
                        || rb_vm_insn_decode(*cfp->pc) == BIN(sir_inspect_stack_fixtype)))
#ifdef FLONUM_INSNS
        || (!flotype_p && (rb_vm_insn_decode(*cfp->pc) == BIN(sir_inspect_flotype)
                           || rb_vm_insn_decode(*cfp->pc) == BIN(sir_inspect_stack_flotype)))
#endif
        || (!array_p && (rb_vm_insn_decode(*cfp->pc) == BIN(sir_inspect_array)
                         || rb_vm_insn_decode(*cfp->pc) == BIN(sir_inspect_stack_array)))) {
        RB_VM_LOCK_ENTER();
        /* check that it was not already changed, e.g. in another Ractor */
        if ((insn_id = rb_vm_insn_decode(*cfp->pc)) != BIN(nop2)) {
            VALUE *insn_pc = (VALUE *)cfp->pc;
            int debug_level = rb_sir_debug_level();
            if (debug_level >= 1)
                fprintf(stderr, "--> Changing %s to %4ld nop2\n",
                        insn_name(insn_id), insn_pc - cfp->iseq->body->bbv_code);
            change_insn(cfp->iseq, insn_pc, BIN(nop2));
        }
        RB_VM_LOCK_LEAVE();
    } else if (fixtype_p || flotype_p || array_p) {
        if (rb_vm_insn_decode(*cfp->pc) == BIN(sir_inspect_type)
            || rb_vm_insn_decode(*cfp->pc) == BIN(sir_inspect_stack_type)) {
            VALUE *insn_pc = (VALUE *)cfp->pc;
            int debug_level = rb_sir_debug_level();
            RB_VM_LOCK_ENTER();
            if (stack_p) {
                new_insn_id = (fixtype_p ? BIN(sir_inspect_stack_fixtype)
                               : array_p ? BIN(sir_inspect_stack_array) : BIN(sir_inspect_stack_flotype));
                /* check that it was not already changed, e.g. in another Ractor */
                if (rb_vm_insn_decode(*cfp->pc) == BIN(sir_inspect_stack_type)) {
                    if (debug_level >= 1)
                        fprintf(stderr, "--> Changing sir_inspect_stack_type to %4ld %s\n",
                                insn_pc - cfp->iseq->body->bbv_code, insn_name(new_insn_id));
                    change_insn(cfp->iseq, insn_pc, new_insn_id);
                }
            } else if (rb_vm_insn_decode(*cfp->pc) == BIN(sir_inspect_type)) {
                new_insn_id = (fixtype_p ? BIN(sir_inspect_fixtype)
                               : array_p ? BIN(sir_inspect_array) : BIN(sir_inspect_flotype));
                if (debug_level >= 1)
                    fprintf(stderr, "--> Changing sir_inspect_type to %4ld %s\n",
                            insn_pc - cfp->iseq->body->bbv_code, insn_name(new_insn_id));
                change_insn(cfp->iseq, insn_pc, new_insn_id);
            }
            RB_VM_LOCK_LEAVE();
        }
    }
}

static void
gen_sir_bbv_code(rb_iseq_t *iseq, rb_iseq_bbv_t bbv, int init_stack_size) {
    SIR_GEN_ASSERT(bbv->bbv_start < 0 && init_stack_size >= 0
                   && (bbv->start_stack_size < 0 || bbv->start_stack_size == init_stack_size));
    bbv->start_stack_size = init_stack_size;
    if (bbv->head_bbv == NULL) { /* head bbv */
        gen_sir_from_stack_insns(iseq, bbv);
        return;
    }
    if (bbv->head_bbv->bbv_start < 0) {
        if (bbv->head_bbv->start_stack_size < 0)
            bbv->head_bbv->start_stack_size = init_stack_size;
        /* we generate specialized SIR code from non-speculative SIR code */
        gen_sir_from_stack_insns(iseq, bbv->head_bbv);
    }
    gen_specialized_sir(iseq, bbv, FALSE); /* type specialization of BBV */
}

VALUE *
rb_sir_make_bbv(rb_control_frame_t *cfp, int iseq_bbv_start) { /* called from make bbv area only */
    rb_iseq_t *iseq = (rb_iseq_t *)cfp->iseq;
    struct rb_iseq_constant_body *body = iseq->body;
    rb_iseq_bbv_t bbv;
    VALUE *start_pc;

    RB_VM_LOCK_ENTER();
    bbv = get_bbv((rb_iseq_t *)iseq, RB_BBV_REGULAR, 0, iseq_bbv_start, TRUE, 0, NULL);
    if (bbv->bbv_start < 0)
        gen_sir_bbv_code((rb_iseq_t *)iseq, bbv, (int)(cfp->sp - cfp->__bp__));
    if (iseq_bbv_start == 0)
        body->iseq_start_pc = body->bbv_code + bbv->bbv_start;
    start_pc = body->bbv_code + bbv->bbv_start;
    RB_VM_LOCK_LEAVE();
    return start_pc;
}

VALUE *
rb_sir_make_optpc_bbv(rb_control_frame_t *cfp, int opt) { /* called from make bbv area only */
    rb_iseq_t *iseq = (rb_iseq_t *)cfp->iseq;
    struct rb_iseq_constant_body *body = iseq->body;
    int iseq_bbv_start = (int)body->param.opt_table[opt];
    rb_iseq_bbv_t bbv;
    VALUE *start_pc;

    RB_VM_LOCK_ENTER();
    bbv = get_bbv((rb_iseq_t *)iseq, RB_BBV_OPT, opt, iseq_bbv_start, TRUE, 0, NULL);
    if (bbv->bbv_start < 0)
        gen_sir_bbv_code((rb_iseq_t *)iseq, bbv, (int)(cfp->sp - cfp->__bp__));
    SIR_GEN_ASSERT(body->bbv_code + bbv->bbv_start >= body->iseq_start_pc);
    body->curr_param_opt_table[opt] = body->bbv_code + bbv->bbv_start - body->iseq_start_pc;
    start_pc = body->bbv_code + bbv->bbv_start;
    RB_VM_LOCK_LEAVE();
    return start_pc;
}

VALUE *
rb_sir_make_catch_bbv(rb_control_frame_t *cfp, int entry_ind) { /* called from make bbv area only */
    rb_iseq_t *iseq = (rb_iseq_t *)cfp->iseq;
    struct rb_iseq_constant_body *body = iseq->body;
    int iseq_bbv_start = body->catch_table->entries[entry_ind].cont;
    rb_iseq_bbv_t bbv;
    VALUE *start_pc;

    RB_VM_LOCK_ENTER();
    bbv = get_bbv((rb_iseq_t *)iseq, RB_BBV_CATCH, entry_ind, iseq_bbv_start, TRUE, 0, NULL);
    if (bbv->bbv_start < 0)
        gen_sir_bbv_code((rb_iseq_t *)iseq, bbv, (int)(cfp->sp - cfp->__bp__));
    body->catch_table->entries[entry_ind].curr_cont = bbv->bbv_start;
    start_pc = body->bbv_code + bbv->bbv_start;
    RB_VM_LOCK_LEAVE();
    return start_pc;
}

VALUE *
rb_sir_gen_bbv(rb_control_frame_t *cfp, rb_iseq_bbv_t bbv, int branch_offset, int first_p) {
    VALUE *start_pc;

    RB_VM_LOCK_ENTER();
    rb_iseq_bbv_t src_bbv = NULL;
    struct rb_iseq_constant_body *body = cfp->iseq->body;
    int gen_bbv_insn_pos = (int)(cfp->pc - body->bbv_code); /* position of sir_gen_bbv end */
    enum ruby_vminsn_type branch_id = rb_vm_insn_decode(body->bbv_code[gen_bbv_insn_pos + branch_offset]);
    int branch_len = insn_len(branch_id), sir_gen_bbv_len = insn_len(BIN(sir_gen_bbv));
    VALUE *offset_ref, new_offset;
    struct rb_iseq_bbv_edge *in_e;

    SIR_GEN_ASSERT(gen_bbv_insn_pos >= 0 && gen_bbv_insn_pos <= body->bbv_code_bound
                   && insn_len(BIN(sir_gen_bbv)) == insn_len(BIN(sir_gen_bbv2)));
    gen_bbv_insn_pos -= sir_gen_bbv_len;
    if (bbv->bbv_start < 0) /* can be generated by another sir_gen_bbv insn */
        gen_sir_bbv_code((rb_iseq_t *)cfp->iseq, bbv, (int)(cfp->sp - cfp->__bp__));
    /* redirect hash from sir_gen_bbv[2] to the generated bbv start */
    offset_ref = &body->bbv_code[gen_bbv_insn_pos + sir_gen_bbv_len + branch_offset + (first_p ? 1 : 2)];
    new_offset = bbv->bbv_start - (gen_bbv_insn_pos + sir_gen_bbv_len + branch_offset + branch_len);
    *offset_ref = new_offset;
    start_pc = body->bbv_code + bbv->bbv_start;
    for (in_e = bbv->in_edges; in_e != NULL; in_e = in_e->next_in_edge) {
        src_bbv = in_e->in;
        if (src_bbv->bbv_start <= gen_bbv_insn_pos && gen_bbv_insn_pos < src_bbv->bbv_bound) break;
    }
    if (in_e != NULL)
        mark_bbv_change(src_bbv);
    RB_VM_LOCK_LEAVE();
    return start_pc;
}

VALUE *
rb_sir_gen_bbv_key(rb_control_frame_t *cfp, rb_iseq_bbv_t bbv, int case_offset, VALUE key) {
    VALUE *start_pc;

    RB_VM_LOCK_ENTER();
    struct rb_iseq_constant_body *body = cfp->iseq->body;
    int gen_bbv_insn_pos = (int)(cfp->pc - body->bbv_code); /* position of sir_gen_code end */
    enum ruby_vminsn_type branch_id = rb_vm_insn_decode(body->bbv_code[gen_bbv_insn_pos + case_offset]);
    int branch_len = insn_len(branch_id), sir_gen_bbv_len = insn_len(BIN(sir_gen_bbv));
    CDHASH hash;

    hash = body->bbv_code[gen_bbv_insn_pos + case_offset + 1];
    SIR_GEN_ASSERT(branch_id == BIN(opt_case_dispatch)
                   && gen_bbv_insn_pos >= 0 && gen_bbv_insn_pos <= body->bbv_code_bound);
    gen_bbv_insn_pos -= sir_gen_bbv_len;
    if (bbv->bbv_start < 0) /* can be generated by another sir_gen_bbv insn */
        gen_sir_bbv_code((rb_iseq_t *)cfp->iseq, bbv, (int)(cfp->sp - cfp->__bp__));
    /* redirect hash from sir_gen_bbv_key to the generated bbv start */
    rb_hash_aset(hash, key, INT2FIX(bbv->bbv_start
                                    - (gen_bbv_insn_pos + sir_gen_bbv_len + case_offset + branch_len)));
    start_pc = body->bbv_code + bbv->bbv_start;
    RB_VM_LOCK_LEAVE();
    return start_pc;
}

static st_table *call_elems_tab, *cme_call_elems_tab;

static void remove_call_elem_from_bbv_set(struct rb_sir_call_elem *elem) {
    if (0)
        fprintf (stderr, "deleting 0x%lx (bbv=0x%lx, cme=0x%lx) from bbv set\n",
                 (long)elem, (long)elem->bbv, (long)elem->cme);
    if (elem->next_bbv_call_elem != NULL)
        elem->next_bbv_call_elem->prev_bbv_call_elem = elem->prev_bbv_call_elem;
    if (elem->prev_bbv_call_elem != NULL) {
        elem->prev_bbv_call_elem->next_bbv_call_elem = elem->next_bbv_call_elem;
    } else { /* it is a head */
        elem->bbv->call_elems = elem->next_bbv_call_elem;
    }
    st_data_t key = (st_data_t)elem;
    int exist_p = rb_st_delete(call_elems_tab, &key, NULL);
    SIR_GEN_ASSERT(exist_p);
}

static int
replace_call_elem(st_data_t *key, st_data_t *value, st_data_t arg, int existing) {
    SIR_GEN_ASSERT(existing);
    *value = arg;
    return ST_CONTINUE;
}

static void remove_call_elem_from_cme_set(struct rb_sir_call_elem *elem) {
    if (0)
        fprintf (stderr, "deleting 0x%lx (bbv=0x%lx, cme=0x%lx) from cme set\n",
                 (long)elem, (long)elem->bbv, (long)elem->cme);
    if (elem->next_cme_call_elem != NULL)
        elem->next_cme_call_elem->prev_cme_call_elem = elem->prev_cme_call_elem;
    if (elem->prev_cme_call_elem != NULL) {
        elem->prev_cme_call_elem->next_cme_call_elem = elem->next_cme_call_elem;
    } else if (elem->next_cme_call_elem != NULL) { /* it is a head */
        int exist_p = rb_st_update(cme_call_elems_tab, (st_data_t)elem->cme,
                                   replace_call_elem, (st_data_t)elem->next_cme_call_elem);
        SIR_GEN_ASSERT(exist_p);
    } else { /* head of one element list */
        st_data_t cme = (st_data_t)elem->cme;
        int exist_p = rb_st_delete(cme_call_elems_tab, &cme, NULL);
        SIR_GEN_ASSERT(exist_p);
    }
}

static void delete_bbv_call_elems(struct rb_iseq_bbv *bbv, const rb_callable_method_entry_t *except_cme) {
    struct rb_sir_call_elem *call_elem, *next_call_elem;
    for (call_elem = bbv->call_elems; call_elem != NULL; call_elem = next_call_elem) {
        next_call_elem = call_elem->next_bbv_call_elem;
        if (call_elem->cme == except_cme) continue;
        remove_call_elem_from_cme_set(call_elem);
        remove_call_elem_from_bbv_set(call_elem);
        SIR_FREE((void *)call_elem);
    }
    bbv->call_elems = NULL;
}

static struct rb_sir_call_elem *
find_cme_call_elem_list_head(const rb_callable_method_entry_t *cme) {
    st_data_t call_elem;
    if (!rb_st_lookup(cme_call_elems_tab, (st_data_t)cme, &call_elem)) return NULL;
    return (struct rb_sir_call_elem *)call_elem;
}

static void
add_new_call_elem(struct rb_sir_call_elem *call_elem) {
    int found_p;
    struct rb_sir_call_elem *found_elem;
    struct rb_iseq_bbv *bbv = call_elem->bbv;
    const rb_callable_method_entry_t *cme = call_elem->cme;
    st_data_t value;

    call_elem->prev_cme_call_elem = call_elem->prev_bbv_call_elem = NULL;
    if (0)
        fprintf (stderr, "inserting 0x%lx (bbv=0x%lx, cme=0x%lx)\n",
                 (long)call_elem, (long)bbv, (long)cme);
    found_p = rb_st_lookup(cme_call_elems_tab, (st_data_t)cme, &value);
    found_elem = found_p ? (struct rb_sir_call_elem *)value : NULL;
    call_elem->next_cme_call_elem = found_elem;
    if (found_p)
        found_elem->prev_cme_call_elem = call_elem;
    found_p = st_insert(cme_call_elems_tab, (st_data_t)cme, (st_data_t)call_elem);
    call_elem->next_bbv_call_elem = bbv->call_elems;
    if (bbv->call_elems != NULL)
        bbv->call_elems->prev_bbv_call_elem = call_elem;
    bbv->call_elems = call_elem;
    rb_st_insert(call_elems_tab, (st_data_t)call_elem, (st_data_t)call_elem);
}

static void
add_call_elem(struct rb_iseq_bbv *bbv, const rb_callable_method_entry_t *cme) {
    struct rb_sir_call_elem call_elem_s, *call_elem;

    call_elem_s.cme = cme;
    call_elem_s.bbv = bbv;
    if (rb_st_lookup(call_elems_tab, (st_data_t)&call_elem_s, NULL)) return;
    call_elem = SIR_ALLOC_N(struct rb_sir_call_elem, 1);
    call_elem->bbv = bbv;
    call_elem->cme = cme;
    add_new_call_elem(call_elem);
}

static int
call_elem_cmp(st_data_t lhs, st_data_t rhs) {
    const struct rb_sir_call_elem *e1 = (struct rb_sir_call_elem *)lhs;
    const struct rb_sir_call_elem *e2 = (struct rb_sir_call_elem *)rhs;
    return e1->cme == e2->cme && e1->bbv == e2->bbv ? 0 : 1;
}

static st_index_t
call_elem_hash(st_data_t arg) {
    const struct rb_sir_call_elem *e = (struct rb_sir_call_elem *)arg;
    st_index_t h =  st_hash_start(42);
    h = st_hash_uint(h, (st_index_t)e->cme);
    h = st_hash_uint(h, (st_index_t)e->bbv);
    return st_hash_end(h);
}

static const struct st_hash_type call_elem_hashtype = {
    call_elem_cmp,
    call_elem_hash,
};

static void
init_call_elems(void) {
    cme_call_elems_tab = rb_st_init_numtable();
    call_elems_tab = rb_st_init_table(&call_elem_hashtype);
}

static void
finish_call_elems(void) {
    rb_st_free_table(cme_call_elems_tab);
    rb_st_free_table(call_elems_tab);
}

static void
set_iseq_start(rb_iseq_bbv_t bbv) {
    rb_iseq_t *iseq = bbv->iseq;
    struct rb_iseq_constant_body *body = iseq->body;

    if (bbv->type == RB_BBV_REGULAR) {
        if (bbv->iseq_start == 0)
            body->iseq_start_pc = body->bbv_code + bbv->bbv_start;
    } else if (bbv->type == RB_BBV_OPT) {
        SIR_GEN_ASSERT(body->iseq_start_pc != NULL);
        body->curr_param_opt_table[bbv->index] = bbv->bbv_start + body->bbv_code - body->iseq_start_pc;
    } else {
        SIR_GEN_ASSERT(bbv->type == RB_BBV_CATCH);
        body->catch_table->entries[bbv->index].curr_cont = bbv->bbv_start;
    }
}

static void
invalidate_bbv_code(rb_iseq_bbv_t bbv, const rb_callable_method_entry_t *except_cme) {
    rb_iseq_t *iseq = bbv->iseq;
    struct rb_iseq_constant_body *body = iseq->body;
    int enter_bbv_p;
    const rb_execution_context_t *ec;
    rb_control_frame_t *cfp;
    int debug_level = rb_sir_debug_level();

    if (bbv->bbv_start < 0) return; /* already invalidated */
    if (debug_level >= 1) {
        fprintf (stderr, "## Invalidating BBV%d 0x%llx (%s) of ",
                 bbv->num, (unsigned long long)bbv, bbv->env_p ? "env" : "stack");
        print_iseq_title(bbv->iseq);
        fprintf(stderr, "\n");
        if (debug_level >= 2)
            print_bb(bbv->iseq, bbv->iseq->body->bbv_code, bbv->bbv_start, bbv->bbv_bound, "  ");
    }
    ec = GET_EC();
    for (cfp = ec->cfp; (void *)cfp < (void *)(ec->vm_stack + ec->vm_stack_size); cfp++) {
        if (cfp->iseq != NULL && cfp->iseq->body->bbv_code != NULL && cfp->iseq == bbv->iseq
            && cfp->iseq->body->bbv_code + bbv->bbv_start <= cfp->pc
            && cfp->pc < cfp->iseq->body->bbv_code + bbv->bbv_bound) { /* switch given bbv pc to safe code one */
            cfp->pc = rb_sir_get_safe_pc(cfp, cfp->pc);
        }
    }
    enter_bbv_p = bbv->type != RB_BBV_REGULAR || bbv->iseq_start == 0;
    if (bbv->type == RB_BBV_REGULAR && enter_bbv_p)
        body->iseq_start_pc = NULL; /* to check that we process BBV_OPT after BBV_REGULAR entrance */
    remove_out_edges(bbv);
    redirect_src_bbv_dests(bbv, TRUE); /* redirect branches in src bbvs to corresponding sir_gen_bbvxxx */
    bbv->bbv_start = -1; /* mark bbv as needed for new code */
    delete_bbv_call_elems(bbv, except_cme);
    if (!enter_bbv_p) {
        memset(bbv->used_bops, 0, sizeof(bbv->used_bops));
    } else {
        /* We need to generate code for iseq enter BBV here as we need
           already code in sir_iseq_send_without_block.  We could do
           this sir_iseq_send_without_block but we assume the insn
           execution is much more frequent than BOP invalidating. */
        gen_sir_bbv_code((rb_iseq_t *)iseq, bbv, 0);
        set_iseq_start(bbv);
    }
}

void
rb_sir_bop_redefined(enum ruby_basic_operators bop, int klass_flag) { // ???
    rb_iseq_bbv_t bbv;
    int i;

    if (!rb_sir_enabled_p()) return;
    RB_VM_LOCK_ENTER();
    rb_vm_barrier (); /* the change can be non-atomic */
    for (i = 0; i < (int)VARR_LENGTH(rb_iseq_bbv_t, all_bbvs); i++) {
        bbv = VARR_GET(rb_iseq_bbv_t, all_bbvs, i);
        if (bbv != NULL && (bbv->used_bops[bop] & klass_flag) != 0)
            invalidate_bbv_code(bbv, NULL);
    }
    RB_VM_LOCK_LEAVE();
}

int
rb_sir_singleton_method_defined(VALUE old_class, ID id) {
    const rb_execution_context_t *ec;
    rb_control_frame_t *cfp;

    if (!rb_sir_enabled_p() || old_class != rb_cArray || (id != idAREF && id != idASET)) return FALSE;
    RB_VM_LOCK_ENTER();
    rb_vm_barrier (); /* the change can be non-atomic */
    ec = GET_EC();
    for (cfp = ec->cfp; (void *)cfp < (void *)(ec->vm_stack + ec->vm_stack_size); cfp++) {
        if (cfp->iseq != NULL && cfp->iseq->body->bbv_code != NULL) { /* switch given bbv pc to safe code one */
            cfp->pc = rb_sir_get_safe_pc(cfp, cfp->pc);
        }
    }
    RB_VM_LOCK_LEAVE();
    return TRUE;
}

void
rb_sir_cme_invalidate(const rb_callable_method_entry_t *cme) {
    struct rb_sir_call_elem *call_elem, *next_call_elem;
    if (cme_call_elems_tab == NULL)
        return; /* we did not initialize it yet */
    if (!rb_sir_enabled_p()) return;
    RB_VM_LOCK_ENTER();
    rb_vm_barrier (); /* the change can be non-atomic */
    for (call_elem = find_cme_call_elem_list_head(cme); call_elem != NULL; call_elem = next_call_elem) {
        next_call_elem = call_elem->next_cme_call_elem;
        invalidate_bbv_code(call_elem->bbv, cme);
    }
    RB_VM_LOCK_LEAVE();
}

void
rb_sir_trace_invalidate(void) {
    rb_iseq_bbv_t bbv;
    int i;

    if (!rb_sir_enabled_p()) return;
    RB_VM_LOCK_ENTER();
    rb_vm_barrier (); /* the change can be non-atomic */
    for (i = 0; i < (int)VARR_LENGTH(rb_iseq_bbv_t, all_bbvs); i++) {
        bbv = VARR_GET(rb_iseq_bbv_t, all_bbvs, i);
        if (bbv == NULL || bbv->head_bbv == NULL) continue;
        invalidate_bbv_code(bbv, NULL);
    }
    RB_VM_LOCK_LEAVE();
}

VALUE *
rb_sir_specialize_bbv(rb_control_frame_t *cfp, rb_iseq_bbv_t bbv) {
    rb_iseq_t *iseq = (rb_iseq_t *)cfp->iseq;
    struct rb_iseq_constant_body *body = iseq->body;
    rb_iseq_bbv_t called_bbv;
    int bbv_cont_pos;
    VALUE *cont_pc;

    RB_VM_LOCK_ENTER();
    /* specialize curr bbv by profile: */
    VARR_TRUNC(rb_iseq_bbv_t, bbvs, 0);
    bbv_cont_pos = gen_specialized_sir(bbv->iseq, bbv, TRUE);
    if (bbv_cont_pos < 0) {
        /* already changed in another ractor */
        cont_pc = (VALUE*)cfp->pc + insn_len(BIN(specialize_bbv));
    } else {
        assert(rb_vm_insn_decode(*cfp->pc) == BIN(specialize_bbv));
        if (VARR_LENGTH(rb_iseq_bbv_t, bbvs) != 0) {
            int debug_level = rb_sir_debug_level();
            if (debug_level >= 1)
                fprintf(stderr, "<<< Start of further type specialization of called BBVs\n");
            /* Here we can reuse globals.  Remember bbvs is updated only for
               profile_p and sir_gen_bbv_code never means profile_p. */
            while (VARR_LENGTH(rb_iseq_bbv_t, bbvs) != 0) {
                called_bbv = VARR_POP(rb_iseq_bbv_t, bbvs);
                if (called_bbv->bbv_start < 0) /* bbv can occur more one time in bbvs */
                    gen_sir_bbv_code(called_bbv->iseq, called_bbv, 0);
            }
            if (debug_level >= 1)
                fprintf(stderr, ">>> End of further type specialization of called BBVs\n");
        }
        set_iseq_start(bbv);
        cont_pc = body->bbv_code + bbv_cont_pos;
    }
    SIR_GEN_ASSERT(insn_len(BIN(specialize_bbv)) == insn_len(BIN(nop2)));
    *(VALUE *)cfp->pc = (VALUE)sir_dispatch_table[BIN(nop2)]; /* to exclude repeated specialization */
    RB_VM_LOCK_LEAVE();
    if (bbv_cont_pos >= 0 && rb_mirjit_enabled_p())
        rb_sir_change_bbv(bbv);
    return cont_pc;
}

void
rb_sir_change_getivar(VALUE self, rb_control_frame_t *cfp, IVC ic, VALUE val) {
    VALUE *insn_pc = (VALUE *)cfp->pc - insn_len(BIN(sir_make_getivar));
    int debug_level = rb_sir_debug_level(), fixnum_p = FIXNUM_P(val);

    if (RB_TYPE_P(self, T_OBJECT)) {
        RB_VM_LOCK_ENTER();
        if (debug_level >= 1)
            fprintf(stderr, "--> Changing sir_make_getivar to %4ld %s\n",
                    insn_pc - cfp->iseq->body->bbv_code, fixnum_p ? "sir_igetivar" : "sir_getivar");
        struct rb_iv_index_tbl_entry *entry = ((IVC)insn_pc[2])->entry;
        *insn_pc = (VALUE)sir_dispatch_table[fixnum_p ? BIN(sir_igetivar) : BIN(sir_getivar)];
        insn_pc[1] = entry == NULL ? 0 : entry->index;
        insn_pc[2] = entry == NULL ? 0 : entry->class_serial;
        RB_VM_LOCK_LEAVE();
        return;
    }
    RB_VM_LOCK_ENTER();
    if (debug_level >= 1)
        fprintf(stderr, "--> Changing sir_make_getivar to getinstancevariable\n");
    change_insn(cfp->iseq, insn_pc, BIN(getinstancevariable));
    RB_VM_LOCK_LEAVE();
}

void
rb_sir_change_setivar(VALUE self, rb_control_frame_t *cfp, IVC ic) {
    VALUE *insn_pc = (VALUE *)cfp->pc - insn_len(BIN(sir_make_setivar));
    int debug_level = rb_sir_debug_level();

    SIR_GEN_ASSERT(ic->entry != NULL);
    if (RB_TYPE_P(self, T_OBJECT)) {
        RB_VM_LOCK_ENTER();
        if (debug_level >= 1)
            fprintf(stderr, "--> Changing sir_make_setivar to sir_setivar\n");
        struct rb_iv_index_tbl_entry *entry = ((IVC)insn_pc[2])->entry;
        change_insn(cfp->iseq, insn_pc, BIN(sir_setivar));
        insn_pc[1] = entry == NULL ? 0 : entry->index;
        insn_pc[2] = entry == NULL ? 0 : entry->class_serial;
        RB_VM_LOCK_LEAVE();
        return;
    }
    RB_VM_LOCK_ENTER();
    if (debug_level >= 1)
        fprintf(stderr, "--> Changing sir_make_set_ivar to setinstancevariable\n");
    change_insn(cfp->iseq, insn_pc, BIN(setinstancevariable));
    RB_VM_LOCK_LEAVE();
}

extern bool rb_iseq_only_optparam_p(const rb_iseq_t *iseq);

static int
change_send_without_block(const rb_execution_context_t *ec, rb_control_frame_t *caller_cfp,
                          CALL_DATA cd, struct rb_sir_send_info *si) {
    VALUE *call_pc = (VALUE *)caller_cfp->pc - insn_len(BIN(sir_make_send));
    VALUE recv;
    const struct rb_callinfo *ci = cd->ci;
    int argc = vm_ci_argc(ci);
    const struct rb_callcache *cc = cd->cc;
    const rb_callable_method_entry_t *cme = vm_cc_cme(cc);
    rb_iseq_bbv_t bbv;
    rb_iseq_t *iseq;
    int debug_level = rb_sir_debug_level();

    if (cme != NULL) {
        recv = *caller_cfp->sp;
        if (cme->def->type == VM_METHOD_TYPE_ISEQ
            && (iseq = (rb_iseq_t *)def_iseq_ptr(cme->def)) != NULL
            && (rb_simple_iseq_p(iseq) || rb_iseq_only_optparam_p(iseq))) {
            struct rb_iseq_constant_body *body = iseq->body;
            const int lead_num = body->param.lead_num;
            const int opt = argc - lead_num;

            SIR_GEN_ASSERT(0 <= opt && opt <= body->param.opt_num);
            if (debug_level >= 1) {
                fprintf(stderr,
                        "--> Changing sir_make_send to sir_iseq_send_without_block: argc=%d",
                        argc);
                print_attrs(si->attrs);
            }
            if (body->param.opt_table == NULL)
                bbv = get_bbv(iseq, RB_BBV_REGULAR, 0, 0, FALSE,
                              si->attrs == NULL ? 0 : (int)strlen(si->attrs) + 1, si->attrs);
            else
                bbv = get_bbv(iseq, RB_BBV_OPT, opt, (int)body->param.opt_table[opt], FALSE,
                              si->attrs == NULL ? 0 : (int)strlen(si->attrs) + 1, si->attrs);
            if (bbv->bbv_start < 0)
                gen_sir_bbv_code(iseq, bbv, 0);
            SIR_GEN_ASSERT(bbv->iseq == iseq);
            ec->cfp->pc = &body->bbv_code[bbv->bbv_start]; /* to start with BBV code */
            /* Change sir_make_send(cd,send_info) to sir_iseq_send_without_block(si, bbv) */
            change_insn(caller_cfp->iseq, call_pc, BIN(sir_iseq_send_without_block));
            si->klass = vm_cc_class(cc);
            si->cme = cme;
            call_pc[1] = (VALUE)si;
            call_pc[2] = (VALUE)bbv;
            add_call_elem(si->caller_bbv, vm_cc_cme(cd->cc));
            RB_OBJ_WRITTEN(caller_cfp->iseq, Qundef, cme);
            RB_OBJ_WRITTEN(caller_cfp->iseq, Qundef, si->klass);
        } else if (cme->def->type == VM_METHOD_TYPE_IVAR
                   && (vm_ci_flag(ci) & (VM_CALL_ARGS_SPLAT | VM_CALL_KW_SPLAT)) == 0
                   && RB_TYPE_P(recv, T_OBJECT)
                   && (ruby_vm_event_flags & (RUBY_EVENT_CALL | RUBY_EVENT_RETURN)) == 0) {
            rb_check_arity(argc, 0, 0);
            if (vm_cc_attr_index(cc) <= 0)
                return TRUE; /* do nothing -- no attr index yet */
            SIR_GEN_ASSERT(vm_cc_attr_index(cc) > 0);
            if (debug_level >= 1)
                fprintf(stderr, "--> Changing sir_make_send to sir_send_ivget\n");
            change_insn(caller_cfp->iseq, call_pc, BIN(sir_send_ivget));
            call_pc[1] = (VALUE)cc->klass;
            call_pc[2] = (VALUE)vm_cc_attr_index(cc) - 1;
            RB_OBJ_WRITTEN(caller_cfp->iseq, Qundef, cc->klass);
        } else if (cme->def->type == VM_METHOD_TYPE_ATTRSET
                   && (vm_ci_flag(ci) & (VM_CALL_ARGS_SPLAT | VM_CALL_KW_SPLAT | VM_CALL_KWARG)) == 0
                   && RB_TYPE_P(recv, T_OBJECT)
                   && (ruby_vm_event_flags & (RUBY_EVENT_CALL | RUBY_EVENT_RETURN)) == 0) {
            rb_check_arity(argc, 1, 1);
            if (vm_cc_attr_index(cc) <= 0)
                return TRUE; /* do nothing -- no attr index yet */
            if (debug_level >= 1)
                fprintf(stderr, "--> Changing sir_make_send to sir_send_ivset\n");
            change_insn(caller_cfp->iseq, call_pc, BIN(sir_send_ivset));
            call_pc[1] = (VALUE)cc->klass;
            call_pc[2] = (VALUE)vm_cc_attr_index(cc) - 1;
            RB_OBJ_WRITTEN(caller_cfp->iseq, Qundef, cc->klass);
        } else {
            return FALSE;
        }
        return TRUE;
    }
    return FALSE;
}

static int
change_to_cfunc_send(VALUE *call_pc, rb_iseq_t *caller_iseq) {
    if (*call_pc != (VALUE)sir_dispatch_table[BIN(sir_make_send)])
        return *call_pc == (VALUE)sir_dispatch_table[BIN(sir_cfunc_send)];
    SIR_GEN_ASSERT(*call_pc == (VALUE)sir_dispatch_table[BIN(sir_make_send)]);
    CALL_DATA cd = (CALL_DATA)call_pc[1];
    struct rb_sir_send_info *si = (void *)call_pc[2];
    const struct rb_callinfo *ci = cd->ci;
    int argc = vm_ci_argc(ci);
    const struct rb_callcache *cc = cd->cc;
    const rb_callable_method_entry_t *cme = vm_cc_cme(cc);
    int debug_level = rb_sir_debug_level();
    MAYBE_UNUSED (const rb_method_cfunc_t *cfunc);

    if (cme == NULL || cme->def->type != VM_METHOD_TYPE_CFUNC) return FALSE;
    cfunc = UNALIGNED_MEMBER_PTR(cme->def, body.cfunc);
#if 0
    if (argc != cfunc->argc) return FALSE;
#endif
    if (debug_level >= 1) {
        fprintf(stderr, "--> Changing sir_make_send to sir_cfunc_send: argc=%d\n", argc);
    }
    /* Change sir_make_send(cd,send_info) to sir_cfunc_send(si, iseq) */
    change_insn(caller_iseq, call_pc, BIN(sir_cfunc_send));
    si->klass = vm_cc_class(cc);
    si->cme = cme;
    call_pc[1] = (VALUE)si;
    call_pc[2] = (VALUE)si->block_iseq;
    add_call_elem(si->caller_bbv, cme);
    RB_OBJ_WRITTEN(caller_iseq, Qundef, cme);
    RB_OBJ_WRITTEN(caller_iseq, Qundef, si->klass);
    return TRUE;
}

void
rb_sir_change_send(const rb_execution_context_t *ec, rb_control_frame_t *caller_cfp,
                   CALL_DATA cd, struct rb_sir_send_info *si) {
    VALUE *call_pc = (VALUE *)caller_cfp->pc - insn_len(BIN(sir_make_send));
    int no_opt_p = TRUE;

    RB_VM_LOCK_ENTER();
    rb_vm_barrier (); /* the change can be non-atomic */
    if (si->block_iseq == NULL) no_opt_p = !change_send_without_block(ec, caller_cfp, cd, si);
    if (no_opt_p) no_opt_p = !change_to_cfunc_send(call_pc, (rb_iseq_t *)caller_cfp->iseq);
    if (no_opt_p) {
        if (rb_sir_debug_level() >= 1) {
            fprintf(stderr,
                    "--> Changing sir_make_send to sir_send_without_block: argc=%d",
                    vm_ci_argc(cd->ci));
            print_attrs(si->attrs);
        }
        if (si->block_iseq == NULL) {
            change_insn(caller_cfp->iseq, call_pc, BIN(sir_send_without_block));
        } else {
            call_pc[2] = (VALUE)si->block_iseq;
            change_insn(caller_cfp->iseq, call_pc, BIN(send));
        }
    }
    RB_VM_LOCK_LEAVE();
}

void
rb_sir_change_bbv(rb_iseq_bbv_t bbv)
{
#ifdef HAVE_LIBMIR
    rb_iseq_t *iseq = bbv->iseq;
    struct rb_iseq_constant_body *body = iseq->body;
    int debug_level = rb_mirjit_debug_level();
    if (debug_level >= 1) {
        fprintf(stderr, "\n## JITting BBV%d 0x%llx:", bbv->num, (unsigned long long)bbv);
        print_attrs(bbv->attrs);
        print_iseq_title(iseq);
        fprintf(stderr, "\n");
        if (debug_level >= 2)
            print_bb(iseq, body->bbv_code, bbv->bbv_start, bbv->bbv_bound, "  ");
    }
    RB_VM_LOCK_ENTER();
    rb_jit_bbv(bbv);
    RB_VM_LOCK_LEAVE();
#endif
}

const VALUE *
rb_sir_trace_stack_insn_pc(rb_execution_context_t *ec, rb_control_frame_t *reg_cfp) {
    const rb_iseq_t *iseq = reg_cfp->iseq;
    struct rb_iseq_constant_body *body = iseq->body;
    const VALUE *pc = rb_cfp_iseq_encoded_pc(reg_cfp);
    enum ruby_vminsn_type insn_id, new_insn_id;

    SIR_GEN_ASSERT(body->iseq_encoded <= pc && pc < body->iseq_encoded + body->iseq_size);
    if (body->iseq_encoded_trace_p[pc - body->iseq_encoded]) return pc;
    /* slow path : */
    RB_VM_LOCK_ENTER();
    insn_id = rb_vm_insn_addr2opcode(pc);
    if (insn_id >= VM_INSTRUCTION_SIZE / 2) { /* can be become non-trace in another ractor */
        new_insn_id = insn_id - VM_INSTRUCTION_SIZE / 2;
        if (rb_sir_debug_level() >= 1) {
            fprintf(stderr, "--> Changing %04d %s to %s\n", (int)(reg_cfp->pc - body->bbv_code),
                    insn_name(insn_id), insn_name(new_insn_id));
        }
        *(VALUE *)reg_cfp->pc = (VALUE)sir_dispatch_table[new_insn_id];
    }
    RB_VM_LOCK_LEAVE();
    return NULL;
}

static sir_inline const VALUE *
get_safe_pc(const rb_control_frame_t *cfp, const VALUE *pc) {
    const struct rb_iseq_constant_body *body = cfp->iseq->body;
    SIR_GEN_ASSERT(body->bbv_code <= pc && pc < body->bbv_code + body->bbv_code_free);
    int pos = (int)(pc - body->bbv_code);
    SIR_GEN_ASSERT(pos < body->bbv_code_free);
    int safe_pos = -body->bbv_pos_map[pos] - 1;
    SIR_GEN_ASSERT(safe_pos >= 0 && safe_pos < body->bbv_code_free);
    return body->bbv_code + safe_pos;
}

const VALUE *
rb_sir_get_safe_pc(const rb_control_frame_t *cfp, const VALUE *pc) {
    return get_safe_pc(cfp, pc);
}

const VALUE *
rb_sir_get_jump_safe_pc(const rb_control_frame_t *cfp, const VALUE *pc) {
#ifdef SIR_PROFILE
    enum ruby_vminsn_type insn_id = rb_vm_insn_addr2insn((void *)*pc);
    rb_sir_cancel[insn_id]++;
    if (rb_sir_debug_p())
        fprintf (stderr, "canceling %04ld: %s\n", (long)(pc - cfp->iseq->body->bbv_code), insn_name(insn_id));
#endif
    return get_safe_pc(cfp, pc);
}

void
rb_sir_iseq_free(struct rb_iseq_constant_body *body) {
    rb_iseq_bbv_t bbv, curr_bbv, next_bbv;
    int i, bbv_num;

    for (i = 0; i < body->n_bbvs; i++) {
        bbv = body->bbvs[i];
        for (curr_bbv = bbv; curr_bbv != NULL; curr_bbv = next_bbv) {
            next_bbv = curr_bbv->next_bbv;
            bbv_num = curr_bbv->num;
            /* ??? free attrs in sir_make_send */
            delete_bbv_call_elems(curr_bbv, NULL);
            SIR_FREE((void *)curr_bbv);
            VARR_SET(rb_iseq_bbv_t, all_bbvs, bbv_num, NULL);
        }
    }
    if (body->curr_param_opt_table != NULL && body->curr_param_opt_table != body->param.opt_table)
        SIR_FREE((void *)body->curr_param_opt_table);
    SIR_FREE((void *)body->bbvs);
    SIR_FREE((void *)body->bbv_code);
    SIR_FREE((void *)body->bbv_pos_map);
    if (body->send_info != NULL) {
        for (size_t i = 0; ((void **)body->send_info)[i] != NULL; i++)
            SIR_FREE(((void **)body->send_info)[i]);
        SIR_FREE(body->send_info);
    }
}

void
rb_sir_iseq_update_references(const struct rb_iseq_constant_body *body) { /* process bbv specific data but bbv_code */
    struct rb_sir_call_elem *call_elem;

    SIR_GEN_ASSERT(body != NULL);
    for (int n = 0; n < body->n_bbvs; n++) {
        for (rb_iseq_bbv_t curr = body->bbvs[n]; curr != NULL; curr = curr->next_bbv) {
            curr->iseq = (struct rb_iseq_struct *)rb_gc_location((VALUE)curr->iseq);
            for (call_elem = curr->call_elems; call_elem != NULL; call_elem = call_elem->next_bbv_call_elem) {
                rb_callable_method_entry_t *new_cme = (rb_callable_method_entry_t *)rb_gc_location((VALUE)call_elem->cme);
                if (call_elem->cme != new_cme) {
                    remove_call_elem_from_cme_set(call_elem);
                    remove_call_elem_from_bbv_set(call_elem);
                    call_elem->cme = new_cme;
                    add_new_call_elem(call_elem);
                }
            }
        }
    }
    send_info_t si, *send_info = body->send_info;
    if (send_info == NULL) return;
    for (size_t i = 0; (si = send_info[i]) != NULL; i++) {
        if (si->block_iseq) si->block_iseq = (struct rb_iseq_struct *)rb_gc_location((VALUE)si->block_iseq);
        if (si->klass != Qundef) si->klass = rb_gc_location(si->klass);
        if (si->cme) si->cme = (rb_callable_method_entry_t *)rb_gc_location((VALUE)si->cme);
    }
}

void
rb_sir_iseq_data_mark(const struct rb_iseq_constant_body *body) { /* process bbv specific data but bbv_code */
    struct rb_sir_call_elem *call_elem;

    SIR_GEN_ASSERT(VARR_LENGTH(VALUE, bbv_encoded) == 0);
    SIR_GEN_ASSERT(body != NULL);
    for (int n = 0; n < body->n_bbvs; n++) {
        for (rb_iseq_bbv_t curr = body->bbvs[n]; curr != NULL; curr = curr->next_bbv) {
            for (call_elem = curr->call_elems; call_elem != NULL; call_elem = call_elem->next_bbv_call_elem) {
                rb_gc_mark_movable((VALUE)call_elem->cme);
            }
        }
    }
    send_info_t si, *send_info = body->send_info;
    if (send_info == NULL) return;
    for (size_t i = 0; (si = send_info[i]) != NULL; i++) {
        if (RTEST((VALUE)si->block_iseq)) rb_gc_mark_movable((VALUE)si->block_iseq);
        if (si->klass != Qundef) rb_gc_mark_movable(si->klass);
        if (si->cme) rb_gc_mark_movable((VALUE)si->cme);
    }
}

/* Initiate stack insns to SIR generator.  Return FALSE if we failed
   to do this.  */
int
rb_sir_gen_init(void) {
    if (RUBY_SETJMP(sir_gen_jump_buf) != 0)
	return FALSE;
    sir_dispatch_table = rb_vm_get_insns_address_table();
    VARR_CREATE(int, stack, 0);
    VARR_CREATE(char, operands_changed_p, 0);
    VARR_CREATE(char, attrs, 0);
    VARR_CREATE(char, arg_attrs, 0);
    VARR_CREATE(char, labeled_p, 0);
    VARR_CREATE(int, pos_map, 0);
    VARR_CREATE(VALUE, bbv_encoded, 0);
    VARR_CREATE(rb_iseq_bbv_t, all_bbvs, 0);
    VARR_CREATE(rb_iseq_bbv_t, bbvs, 0);
    VARR_CREATE(rb_iseq_bbv_t, out_bbvs, 0);
    VARR_CREATE(int, out_gen_bbv_disps, 0);
    init_send_info();
    init_call_elems();
    return TRUE;
}

#ifdef SIR_PROFILE
long rb_sir_profile[VM_INSTRUCTION_SIZE];
long rb_sir_cancel[VM_INSTRUCTION_SIZE];
struct profile {int insn; long count;};

static int
profile_cmp(const void *e1, const void *e2) {
    const struct profile *p1 = e1, *p2 = e2;
    if (p1->count < p2->count) return 1;
    if (p1->count > p2->count) return -1;
    return 0;
}
#endif

static void print_bb_at_end(rb_iseq_bbv_t bbv) {
    print_iseq_title(bbv->iseq);
    fprintf(stderr, "SIR BBV%d 0x%llx (%s):",
            bbv->num, (unsigned long long)bbv, bbv->env_p ? "env" : "stack");
    print_attrs(bbv->attrs);
    fprintf (stderr, "  in:");
    for (struct rb_iseq_bbv_edge *in_e = bbv->in_edges; in_e != NULL; in_e = in_e->next_in_edge) {
        fprintf(stderr, "%4d(0x%-16llx)", in_e->in->num, (unsigned long long)in_e->in);
    }
    fprintf (stderr, "\n  out:");
    for (struct rb_iseq_bbv_edge *out_e = bbv->out_edges; out_e != NULL; out_e = out_e->next_out_edge) {
        fprintf(stderr, "%4d(0x%-16llx)", out_e->out->num, (unsigned long long)out_e->out);
    }
    fprintf (stderr, "\n");
}

/* Finish stack insns to SIR generator.  */
void
rb_sir_gen_finish(void) {
    if (rb_sir_enabled_p()) {
#ifdef SIR_PROFILE
        struct profile profile[VM_INSTRUCTION_SIZE];
        for (int i = 0; i < VM_INSTRUCTION_SIZE; i++) {
            profile[i].insn = i;
            profile[i].count = rb_sir_profile[i];
        }
        qsort(profile, VM_INSTRUCTION_SIZE, sizeof(struct profile), profile_cmp);
        if (profile[0].count != 0)
            fprintf(stderr, "+++++++++++SIR INSN PROFILE++++++++++\n");
        for (int i = 0; i < VM_INSTRUCTION_SIZE; i++) {
            if (profile[i].count == 0) break;
            if (rb_sir_cancel[profile[i].insn] == 0)
                fprintf(stderr, "%-25ld:%s\n", profile[i].count, insn_name(profile[i].insn));
            else
                fprintf(stderr, "%-12ld !%-11ld:%s\n",
                        profile[i].count, rb_sir_cancel[profile[i].insn], insn_name(profile[i].insn));
        }
#endif
        if (rb_sir_debug_level() >= 3) {
            for (size_t i = 0; i < VARR_LENGTH(rb_iseq_bbv_t, all_bbvs); i++) {
                rb_iseq_bbv_t bbv = VARR_GET(rb_iseq_bbv_t, all_bbvs, i);
                if (bbv == NULL || (bbv->in_edges == NULL && bbv->out_edges == NULL)) continue;
                print_bb_at_end(bbv);
            }
        }
    }
    VARR_DESTROY(int, stack);
    VARR_DESTROY(char, operands_changed_p);
    VARR_DESTROY(char, attrs);
    VARR_DESTROY(char, arg_attrs);
    VARR_DESTROY(char, labeled_p);
    VARR_DESTROY(int, pos_map);
    VARR_DESTROY(VALUE, bbv_encoded);
    VARR_DESTROY(rb_iseq_bbv_t, all_bbvs);
    VARR_DESTROY(rb_iseq_bbv_t, bbvs);
    VARR_DESTROY(rb_iseq_bbv_t, out_bbvs);
    VARR_DESTROY(int, out_gen_bbv_disps);
    finish_send_info();
    finish_call_elems();
}
