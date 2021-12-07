/**********************************************************************

  rtl_exec.c - Code for executing RTL (Register Transfer Language)
               insns in the VM interpreter

  Copyright (C) 2021 Vladimir Makarov vmakarov@redhat.com

**********************************************************************/

/* Each RTL insn from insns.def has a corresponding function with
   prefix vm_ which is responsible for executing all or the most insn
   code.  The functions are declared as static inline to exclude an
   overhead for the function call.  Such approach is to simplify
   implementation of interpreter.

   We have many different specialized move insns to improve the
   interpreter performance.  We would not need such number of the
   insns for JIT only as the used C compiler can generate specialized
   insns versions itself.  */

#if 1
#define RTL_EXEC_ASSERT(expr)
#else
#define RTL_EXEC_ASSERT(expr) RUBY_ASSERT_ALWAYS(expr)
#endif

#if defined(__GNUC__)
#define rtl_inline inline __attribute__ ((__always_inline__))
#else
#define rtl_inline inline
#endif

/* Macros used to translate insn name into the corresponding function
   name.  */
#define RTL_CONCAT(a, b) a ## b
#define RTL_FUNC_NAME(insn_name) RTL_CONCAT(vm_, insn_name)

#define RTL_FUNCS2(iname, fname)			                                        \
    static rtl_inline VALUE RTL_CONCAT(iname, svv)(VALUE v1, VALUE v2) { return fname(v1, v2); }\
    static rtl_inline VALUE RTL_CONCAT(iname, svi)(VALUE v1, VALUE v2) { return fname(v1, v2); }\
    static rtl_inline VALUE RTL_CONCAT(iname, svs)(VALUE v1, VALUE v2) { return fname(v1, v2); }\
    static rtl_inline VALUE RTL_CONCAT(iname, ssv)(VALUE v1, VALUE v2) { return fname(v1, v2); }\
    static rtl_inline VALUE RTL_CONCAT(iname, ssi)(VALUE v1, VALUE v2) { return fname(v1, v2); }\

RTL_FUNCS2(vm_opt_plus, vm_opt_plus)
RTL_FUNCS2(vm_opt_minus, vm_opt_minus)
RTL_FUNCS2(vm_opt_mult, vm_opt_mult)
RTL_FUNCS2(vm_opt_div, vm_opt_div)
RTL_FUNCS2(vm_opt_mod, vm_opt_mod)
RTL_FUNCS2(vm_opt_or, vm_opt_or)
RTL_FUNCS2(vm_opt_and, vm_opt_and)
RTL_FUNCS2(vm_opt_lt, vm_opt_lt)
RTL_FUNCS2(vm_opt_gt, vm_opt_gt)
RTL_FUNCS2(vm_opt_le, vm_opt_le)
RTL_FUNCS2(vm_opt_ge, vm_opt_ge)
RTL_FUNCS2(vm_opt_aref, vm_opt_aref)

#define RTL_FUNCS(iname) RTL_FUNCS2(iname, iname) \
    static rtl_inline VALUE RTL_CONCAT(iname, vvv)(VALUE v1, VALUE v2) { return iname(v1, v2); }\
    static rtl_inline VALUE RTL_CONCAT(iname, vvi)(VALUE v1, VALUE v2) { return iname(v1, v2); }\
    static rtl_inline VALUE RTL_CONCAT(iname, vvs)(VALUE v1, VALUE v2) { return iname(v1, v2); }\
    static rtl_inline VALUE RTL_CONCAT(iname, vsv)(VALUE v1, VALUE v2) { return iname(v1, v2); }\
    static rtl_inline VALUE RTL_CONCAT(iname, vsi)(VALUE v1, VALUE v2) { return iname(v1, v2); }\
    static rtl_inline VALUE RTL_CONCAT(iname, vss)(VALUE v1, VALUE v2) { return iname(v1, v2); }\
    static rtl_inline VALUE RTL_CONCAT(iname, sss)(VALUE v1, VALUE v2) { return iname(v1, v2); }\


static rtl_inline void RTL_CONCAT(push, vv)(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2) {
    INC_SP(2); TOPN(1) = v1; TOPN(0) = v2;
}
static rtl_inline void RTL_CONCAT(push, vi)(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2) {
    INC_SP(2); TOPN(1) = v1; TOPN(0) = v2;
}
static rtl_inline void RTL_CONCAT(push, vs)(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2) {
    PUSH(v2); TOPN(1) = v1;
}
static rtl_inline void RTL_CONCAT(push, sv)(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2) {
    PUSH(v2);
}
static rtl_inline void RTL_CONCAT(push, si)(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2) {
    PUSH(v2);
}
static rtl_inline void RTL_CONCAT(push, ss)(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2) {
}

static rtl_inline void RTL_CONCAT(push, svv)(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2, VALUE v3) {
    INC_SP(2); TOPN(1) = v2; TOPN(0) = v3;
}
static rtl_inline void RTL_CONCAT(push, siv)(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2, VALUE v3) {
    INC_SP(2); TOPN(1) = v2; TOPN(0) = v3;
}
static rtl_inline void RTL_CONCAT(push, svs)(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2, VALUE v3) {
    INC_SP(1); TOPN(1) = v2; TOPN(0) = v3;
}
static rtl_inline void RTL_CONCAT(push, ssv)(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2, VALUE v3) {
    PUSH(v2);
}
static rtl_inline void RTL_CONCAT(push, sis)(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2, VALUE v3) {
    PUSH(v2);
}
static rtl_inline void RTL_CONCAT(push, sss)(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2, VALUE v3) {
}

static rtl_inline void RTL_CONCAT(push, vvv)(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2, VALUE v3) {
    INC_SP(3); TOPN(2) = v1; TOPN(1) = v2; TOPN(0) = v3;
}
static rtl_inline void RTL_CONCAT(push, viv)(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2, VALUE v3) {
    INC_SP(3); TOPN(2) = v1; TOPN(1) = v2; TOPN(0) = v3;
}
static rtl_inline void RTL_CONCAT(push, vvs)(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2, VALUE v3) {
    INC_SP(2); TOPN(2) = v1; TOPN(1) = v2; TOPN(0) = v3;
}
static rtl_inline void RTL_CONCAT(push, vsv)(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2, VALUE v3) {
    INC_SP(2); TOPN(2) = v1; TOPN(1) = v2; TOPN(0) = v3;
}
static rtl_inline void RTL_CONCAT(push, vis)(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2, VALUE v3) {
    INC_SP(2); TOPN(2) = v1; TOPN(1) = v2; TOPN(0) = v3;
}
static rtl_inline void RTL_CONCAT(push, vss)(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2, VALUE v3) {
    INC_SP(1); TOPN(2) = v1; TOPN(1) = v2; TOPN(0) = v3;
}

#define RTL_FUNCS3(iname)                                           \
    static rtl_inline VALUE RTL_CONCAT(iname, svv)(VALUE v0, VALUE v1, VALUE v2) { return iname(v0, v1, v2); }\
    static rtl_inline VALUE RTL_CONCAT(iname, siv)(VALUE v0, VALUE v1, VALUE v2) { return iname(v0, v1, v2); }\
    static rtl_inline VALUE RTL_CONCAT(iname, svs)(VALUE v0, VALUE v1, VALUE v2) { return iname(v0, v1, v2); }\
    static rtl_inline VALUE RTL_CONCAT(iname, ssv)(VALUE v0, VALUE v1, VALUE v2) { return iname(v0, v1, v2); }\
    static rtl_inline VALUE RTL_CONCAT(iname, sis)(VALUE v0, VALUE v1, VALUE v2) { return iname(v0, v1, v2); }\
    static rtl_inline VALUE RTL_CONCAT(iname, sss)(VALUE v0, VALUE v1, VALUE v2) { return iname(v0, v1, v2); }\
    static rtl_inline VALUE RTL_CONCAT(iname, vvv)(VALUE v0, VALUE v1, VALUE v2) { return iname(v0, v1, v2); }\
    static rtl_inline VALUE RTL_CONCAT(iname, viv)(VALUE v0, VALUE v1, VALUE v2) { return iname(v0, v1, v2); }\
    static rtl_inline VALUE RTL_CONCAT(iname, vvs)(VALUE v0, VALUE v1, VALUE v2) { return iname(v0, v1, v2); }\
    static rtl_inline VALUE RTL_CONCAT(iname, vsv)(VALUE v0, VALUE v1, VALUE v2) { return iname(v0, v1, v2); }\
    static rtl_inline VALUE RTL_CONCAT(iname, vis)(VALUE v0, VALUE v1, VALUE v2) { return iname(v0, v1, v2); }\
    static rtl_inline VALUE RTL_CONCAT(iname, vss)(VALUE v0, VALUE v1, VALUE v2) { return iname(v0, v1, v2); }

static rtl_inline VALUE vm_opt_iplus(VALUE v1, VALUE v2) { return rb_fix_plus_fix(v1, v2); }
static rtl_inline VALUE vm_opt_iminus(VALUE v1, VALUE v2) { return rb_fix_minus_fix(v1, v2); }
static rtl_inline VALUE vm_opt_imult(VALUE v1, VALUE v2) { return rb_fix_mul_fix(v1, v2); }
static rtl_inline VALUE vm_opt_idiv(VALUE v1, VALUE v2) { return rb_fix_div_fix(v1, v2); }
static rtl_inline VALUE vm_opt_imod(VALUE v1, VALUE v2) { return rb_fix_mod_fix(v1, v2); }
static rtl_inline VALUE vm_opt_ior(VALUE v1, VALUE v2) { return v1 | v2; }
static rtl_inline VALUE vm_opt_iand(VALUE v1, VALUE v2) { return (v1 & v2) | 1; }
static rtl_inline VALUE vm_opt_ieq(VALUE v1, VALUE v2) { return RBOOL(v1 == v2); }
static rtl_inline VALUE vm_opt_ineq(VALUE v1, VALUE v2) { return RBOOL(v1 != v2); }
static rtl_inline VALUE vm_opt_ilt(VALUE v1, VALUE v2) { return RBOOL((SIGNED_VALUE)v1 < (SIGNED_VALUE)v2); }
static rtl_inline VALUE vm_opt_igt(VALUE v1, VALUE v2) { return RBOOL((SIGNED_VALUE)v1 > (SIGNED_VALUE)v2); }
static rtl_inline VALUE vm_opt_ile(VALUE v1, VALUE v2) { return RBOOL((SIGNED_VALUE)v1 <= (SIGNED_VALUE)v2); }
static rtl_inline VALUE vm_opt_ige(VALUE v1, VALUE v2) { return RBOOL((SIGNED_VALUE)v1 > (SIGNED_VALUE)v2); }

RTL_FUNCS(vm_opt_iplus)
RTL_FUNCS(vm_opt_iminus)
RTL_FUNCS(vm_opt_imult)
RTL_FUNCS(vm_opt_idiv)
RTL_FUNCS(vm_opt_imod)
RTL_FUNCS(vm_opt_ior)
RTL_FUNCS(vm_opt_iand)
RTL_FUNCS(vm_opt_ieq)
RTL_FUNCS(vm_opt_ineq)
RTL_FUNCS(vm_opt_ilt)
RTL_FUNCS(vm_opt_igt)
RTL_FUNCS(vm_opt_ile)
RTL_FUNCS(vm_opt_ige)

static rtl_inline VALUE vm_opt_fplus(VALUE v1, VALUE v2) { return DBL2NUM(RFLOAT_VALUE(v1) + RFLOAT_VALUE(v2)); }
static rtl_inline VALUE vm_opt_fminus(VALUE v1, VALUE v2) { return DBL2NUM(RFLOAT_VALUE(v1) - RFLOAT_VALUE(v2)); }
static rtl_inline VALUE vm_opt_fmult(VALUE v1, VALUE v2) { return DBL2NUM(RFLOAT_VALUE(v1) * RFLOAT_VALUE(v2)); }
static rtl_inline VALUE vm_opt_fdiv(VALUE v1, VALUE v2) { return rb_flo_div_flo(v1, v2); }
static rtl_inline VALUE vm_opt_fmod(VALUE v1, VALUE v2) { return DBL2NUM(ruby_float_mod(RFLOAT_VALUE(v1), RFLOAT_VALUE(v2))); }
static rtl_inline VALUE vm_opt_feq(VALUE v1, VALUE v2) { return RBOOL(v1 == v2); }
static rtl_inline VALUE vm_opt_fneq(VALUE v1, VALUE v2) { return RBOOL(v1 != v2); }
static rtl_inline VALUE vm_opt_flt(VALUE v1, VALUE v2) { return RBOOL(RFLOAT_VALUE(v1) < RFLOAT_VALUE(v2)); }
static rtl_inline VALUE vm_opt_fgt(VALUE v1, VALUE v2) { return RBOOL(RFLOAT_VALUE(v1) > RFLOAT_VALUE(v2)); }
static rtl_inline VALUE vm_opt_fle(VALUE v1, VALUE v2) { return RBOOL(RFLOAT_VALUE(v1) <= RFLOAT_VALUE(v2)); }
static rtl_inline VALUE vm_opt_fge(VALUE v1, VALUE v2) { return RBOOL(RFLOAT_VALUE(v1) >= RFLOAT_VALUE(v2)); }

RTL_FUNCS(vm_opt_fplus)
RTL_FUNCS(vm_opt_fminus)
RTL_FUNCS(vm_opt_fmult)
RTL_FUNCS(vm_opt_fdiv)
RTL_FUNCS(vm_opt_fmod)
RTL_FUNCS(vm_opt_feq)
RTL_FUNCS(vm_opt_fneq)
RTL_FUNCS(vm_opt_flt)
RTL_FUNCS(vm_opt_fgt)
RTL_FUNCS(vm_opt_fle)
RTL_FUNCS(vm_opt_fge)

static rtl_inline VALUE vm_opt_iaref(VALUE v1, VALUE v2) {
    if (SPECIAL_CONST_P(v1) || RBASIC_CLASS(v1) != rb_cArray) return Qundef;
    return rb_ary_entry_internal(v1, FIX2LONG(v2));
}
RTL_FUNCS(vm_opt_iaref)

static rtl_inline VALUE vm_opt_iaset(VALUE arr, VALUE ind, VALUE set) {
    if (SPECIAL_CONST_P(arr) || RBASIC_CLASS(arr) != rb_cArray) return Qundef;
    rb_ary_store(arr, FIX2LONG(ind), set);
    return set;
}
RTL_FUNCS3(vm_opt_iaset)

RTL_FUNCS3(vm_opt_aset)

#define RTL_BR_FUNCS(iname, func_name)                              \
    static rtl_inline VALUE RTL_CONCAT(iname, ss)(VALUE v1, VALUE v2) { return func_name(v1, v2); }\
    static rtl_inline VALUE RTL_CONCAT(iname, vv)(VALUE v1, VALUE v2) { return func_name(v1, v2); }\
    static rtl_inline VALUE RTL_CONCAT(iname, vi)(VALUE v1, VALUE v2) { return func_name(v1, v2); }\
    static rtl_inline VALUE RTL_CONCAT(iname, vs)(VALUE v1, VALUE v2) { return func_name(v1, v2); }\
    static rtl_inline VALUE RTL_CONCAT(iname, sv)(VALUE v1, VALUE v2) { return func_name(v1, v2); }\
    static rtl_inline VALUE RTL_CONCAT(iname, si)(VALUE v1, VALUE v2) { return func_name(v1, v2); }

RTL_BR_FUNCS(vm_opt_ibeq, vm_opt_ieq)
RTL_BR_FUNCS(vm_opt_ibneq, vm_opt_ineq)
RTL_BR_FUNCS(vm_opt_iblt, vm_opt_ilt)
RTL_BR_FUNCS(vm_opt_ibgt, vm_opt_igt)
RTL_BR_FUNCS(vm_opt_ible, vm_opt_ile)
RTL_BR_FUNCS(vm_opt_ibge, vm_opt_ige)

RTL_BR_FUNCS(vm_opt_fbeq, vm_opt_feq)
RTL_BR_FUNCS(vm_opt_fbneq, vm_opt_fneq)
RTL_BR_FUNCS(vm_opt_fblt, vm_opt_flt)
RTL_BR_FUNCS(vm_opt_fbgt, vm_opt_fgt)
RTL_BR_FUNCS(vm_opt_fble, vm_opt_fle)
RTL_BR_FUNCS(vm_opt_fbge, vm_opt_fge)

#define RTL_EXEC_INSN2(iname, push_fname, res, v1, v2)		\
    if ((res = RTL_FUNC_NAME(iname)(v1, v2)) == Qundef) {       \
        ABS_JUMP(rb_rtl_get_stack_pc(GET_CFP(), reg_pc));	\
        push_fname(GET_CFP(), v1, v2);				\
        END_INSN(iname);					\
    }

#define RTL_EXEC_INSN3(iname, push_fname, v1, v2, v3)		\
    if (RTL_FUNC_NAME(iname)(v1, v2, v3) == Qundef) {   	\
        ABS_JUMP(rb_rtl_get_stack_pc(GET_CFP(), reg_pc));	\
        push_fname(GET_CFP(), v1, v2, v3);			\
        END_INSN(iname);					\
    }

#define CHECK_FIXNUM(iname, push_fname, res, v1, v2)    	\
    if (!FIXNUM_P(res)) {      					\
        ABS_JUMP(rb_rtl_get_stack_pc(GET_CFP(), reg_pc));	\
        push_fname(GET_CFP(), v1, v2);				\
        END_INSN(iname);                                \
    }

#define CHECK_ZERO(iname, push_fname, v1, v2)   		\
    if (v2 == LONG2FIX(0)) {					\
        ABS_JUMP(rb_rtl_get_stack_pc(GET_CFP(), reg_pc));	\
        push_fname(GET_CFP(), v1, v2);				\
        END_INSN(iname); 					\
    }

static rtl_inline const VALUE *
rb_rtl_get_stack_pc(const rb_control_frame_t *cfp, const VALUE *pc) {
    const struct rb_iseq_constant_body *body = cfp->iseq->body;
    VM_ASSERT(cfp->pc == pc && body->bbv_code <= pc && pc < body->bbv_code + body->bbv_code_size);
    int pos = pc - body->bbv_code;
    int stack_pos = body->bbv_pos_map[pos];
    return body->iseq_encoded + stack_pos;
}
