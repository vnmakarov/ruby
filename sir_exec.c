/**********************************************************************

  rtl_exec.c - Code for executing RTL (Register Transfer Language)
               insns in the VM interpreter

  Copyright (C) 2022 Vladimir Makarov vmakarov@redhat.com

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
#define rtl_noinline __attribute__ ((__noinline__))
#else
#define rtl_inline inline
#define rtl_noinline
#endif

/* Macros used to translate insn name into the corresponding function
   name.  */
#define RTL_CONCAT(a, b) a ## b
#define RTL_SUBST_CONCAT(a, b) RTL_CONCAT(a, b)
#define RTL_FUNC_NAME(insn_name) RTL_CONCAT(vm_, insn_name)

#define RTL_FUNC0_NAME(n, s) vm_opt_ ## n ## s
#define RTL_FUNCS0(iname, fname)                                              \
    static rtl_inline void RTL_FUNC0_NAME(iname, svv)(VALUE *res, VALUE v1, VALUE v2) { *res = fname(v1, v2); }\
    static rtl_inline void RTL_FUNC0_NAME(iname, svi)(VALUE *res, VALUE v1, VALUE v2) { *res = fname(v1, v2); }\
    static rtl_inline void RTL_FUNC0_NAME(iname, siv)(VALUE *res, VALUE v1, VALUE v2) { *res = fname(v1, v2); }\
    static rtl_inline void RTL_FUNC0_NAME(iname, svs)(VALUE *res, VALUE v1, VALUE v2) { *res = fname(v1, v2); }\
    static rtl_inline void RTL_FUNC0_NAME(iname, ssv)(VALUE *res, VALUE v1, VALUE v2) { *res = fname(v1, v2); }\
    static rtl_inline void RTL_FUNC0_NAME(iname, ssi)(VALUE *res, VALUE v1, VALUE v2) { *res = fname(v1, v2); }\
    static rtl_inline void RTL_FUNC0_NAME(iname, sis)(VALUE *res, VALUE v1, VALUE v2) { *res = fname(v1, v2); }

RTL_FUNCS0(plus, vm_opt_plus)
RTL_FUNCS0(minus, vm_opt_minus)
RTL_FUNCS0(mult, vm_opt_mult)
RTL_FUNCS0(div, vm_opt_div)
RTL_FUNCS0(mod, vm_opt_mod)
RTL_FUNCS0(or, vm_opt_or)
RTL_FUNCS0(and, vm_opt_and)
RTL_FUNCS0(lt, vm_opt_lt)
RTL_FUNCS0(gt, vm_opt_gt)
RTL_FUNCS0(le, vm_opt_le)
RTL_FUNCS0(ge, vm_opt_ge)
RTL_FUNCS0(aref,vm_opt_aref)

#define RTL_FUNCS(iname)										   \
    static rtl_inline void RTL_CONCAT(iname, svv)(VALUE *res, VALUE v1, VALUE v2) { *res = iname(v1, v2); }\
    static rtl_inline void RTL_CONCAT(iname, svi)(VALUE *res, VALUE v1, VALUE v2) { *res = iname(v1, v2); }\
    static rtl_inline void RTL_CONCAT(iname, siv)(VALUE *res, VALUE v1, VALUE v2) { *res = iname(v1, v2); }\
    static rtl_inline void RTL_CONCAT(iname, svs)(VALUE *res, VALUE v1, VALUE v2) { *res = iname(v1, v2); }\
    static rtl_inline void RTL_CONCAT(iname, ssv)(VALUE *res, VALUE v1, VALUE v2) { *res = iname(v1, v2); }\
    static rtl_inline void RTL_CONCAT(iname, ssi)(VALUE *res, VALUE v1, VALUE v2) { *res = iname(v1, v2); }\
    static rtl_inline void RTL_CONCAT(iname, sis)(VALUE *res, VALUE v1, VALUE v2) { *res = iname(v1, v2); }\
    static rtl_inline void RTL_CONCAT(iname, vvv)(VALUE *res, VALUE v1, VALUE v2) { *res = iname(v1, v2); }\
    static rtl_inline void RTL_CONCAT(iname, vvi)(VALUE *res, VALUE v1, VALUE v2) { *res = iname(v1, v2); }\
    static rtl_inline void RTL_CONCAT(iname, viv)(VALUE *res, VALUE v1, VALUE v2) { *res = iname(v1, v2); }\
    static rtl_inline void RTL_CONCAT(iname, vvs)(VALUE *res, VALUE v1, VALUE v2) { *res = iname(v1, v2); }\
    static rtl_inline void RTL_CONCAT(iname, vsv)(VALUE *res, VALUE v1, VALUE v2) { *res = iname(v1, v2); }\
    static rtl_inline void RTL_CONCAT(iname, vsi)(VALUE *res, VALUE v1, VALUE v2) { *res = iname(v1, v2); }\
    static rtl_inline void RTL_CONCAT(iname, vis)(VALUE *res, VALUE v1, VALUE v2) { *res = iname(v1, v2); }\
    static rtl_inline void RTL_CONCAT(iname, vss)(VALUE *res, VALUE v1, VALUE v2) { *res = iname(v1, v2); }\
    static rtl_inline void RTL_CONCAT(iname, sss)(VALUE *res, VALUE v1, VALUE v2) { *res = iname(v1, v2); }\


static rtl_inline void pushvv(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2) {INC_SP(2); TOPN(1) = v1; TOPN(0) = v2;}
static rtl_inline void pushvi(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2) {INC_SP(2); TOPN(1) = v1; TOPN(0) = v2;}
static rtl_inline void pushiv(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2) {INC_SP(2); TOPN(1) = v1; TOPN(0) = v2;}
static rtl_inline void pushvs(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2) {INC_SP(1); TOPN(0) = v2; TOPN(1) = v1;}
static rtl_inline void pushsv(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2) {INC_SP(1); TOPN(0) = v2;}
static rtl_inline void pushsi(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2) {INC_SP(1); TOPN(0) = v2;}
static rtl_inline void pushis(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2) {INC_SP(1); TOPN(0) = v2; TOPN(1) = v1;}
static rtl_inline void pushss(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2) {}

static rtl_inline void pushsvv(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2, VALUE v3) {
    INC_SP(2); TOPN(1) = v2; TOPN(0) = v3;
}
static rtl_inline void pushsiv(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2, VALUE v3) {
    INC_SP(2); TOPN(1) = v2; TOPN(0) = v3;
}
static rtl_inline void pushsvs(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2, VALUE v3) {
    INC_SP(1); TOPN(1) = v2; TOPN(0) = v3;
}
static rtl_inline void pushssv(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2, VALUE v3) {
    INC_SP(1); TOPN(0) = v2;
}
static rtl_inline void pushsis(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2, VALUE v3) {
    INC_SP(1); TOPN(1) = v2; TOPN(0) = v3;
}
static rtl_inline void pushsss(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2, VALUE v3) {
}

static rtl_inline void pushvvv(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2, VALUE v3) {
    INC_SP(3); TOPN(2) = v1; TOPN(1) = v2; TOPN(0) = v3;
}
static rtl_inline void pushviv(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2, VALUE v3) {
    INC_SP(3); TOPN(2) = v1; TOPN(1) = v2; TOPN(0) = v3;
}
static rtl_inline void pushvvs(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2, VALUE v3) {
    INC_SP(2); TOPN(2) = v1; TOPN(1) = v2; TOPN(0) = v3;
}
static rtl_inline void pushvsv(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2, VALUE v3) {
    INC_SP(2); TOPN(2) = v1; TOPN(1) = v2; TOPN(0) = v3;
}
static rtl_inline void pushvis(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2, VALUE v3) {
    INC_SP(2); TOPN(2) = v1; TOPN(1) = v2; TOPN(0) = v3;
}
static rtl_inline void pushvss(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2, VALUE v3) {
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

#if !__has_builtin(__builtin_add_overflow)
#define vm_opt_iplus(res, v1, v2, ov_label)                      \
    {                                                            \
        long r = FIX2LONG(v1) + FIX2LONG(v2);                    \
        if (RB_FIXABLE(r)) {                                     \
            *res = RB_LONG2FIX(r);                               \
        } else {                                                 \
            *res = rb_int2big(r);                                \
            goto RTL_CONCAT(exit_opt_, ov_label);                \
        }                                                        \
    }
#else
#define vm_opt_iplus(res, v1, v2, ov_label)                     \
    {								\
        long r;							\
        if (__builtin_add_overflow((long)v1, (long)v2-1, &r)) { \
            *res = rb_int2big(rb_overflowed_fix_to_int(r));     \
            goto RTL_CONCAT(exit_opt_, ov_label);               \
        }                                                       \
        *res = r;						\
    }
#endif

#define vm_opt_iplussss(res, v1, v2) vm_opt_iplus(res, v1, v2, iplussss)
#define vm_opt_iplusssv(res, v1, v2) vm_opt_iplus(res, v1, v2, iplusssv)
#define vm_opt_iplussvs(res, v1, v2) vm_opt_iplus(res, v1, v2, iplussvs)
#define vm_opt_iplussvv(res, v1, v2) vm_opt_iplus(res, v1, v2, iplussvv)
#define vm_opt_iplusssi(res, v1, v2) vm_opt_iplus(res, v1, v2, iplusssi)
#define vm_opt_iplussis(res, v1, v2) vm_opt_iplus(res, v1, v2, iplussis)
#define vm_opt_iplussvi(res, v1, v2) vm_opt_iplus(res, v1, v2, iplussvi)
#define vm_opt_iplussiv(res, v1, v2) vm_opt_iplus(res, v1, v2, iplussiv)
#define vm_opt_iplusvss(res, v1, v2) vm_opt_iplus(res, v1, v2, iplusvss)
#define vm_opt_iplusvsv(res, v1, v2) vm_opt_iplus(res, v1, v2, iplusvsv)
#define vm_opt_iplusvvs(res, v1, v2) vm_opt_iplus(res, v1, v2, iplusvvs)
#define vm_opt_iplusvvv(res, v1, v2) vm_opt_iplus(res, v1, v2, iplusvvv)
#define vm_opt_iplusvsi(res, v1, v2) vm_opt_iplus(res, v1, v2, iplusvsi)
#define vm_opt_iplusvis(res, v1, v2) vm_opt_iplus(res, v1, v2, iplusvis)
#define vm_opt_iplusvvi(res, v1, v2) vm_opt_iplus(res, v1, v2, iplusvvi)
#define vm_opt_iplusviv(res, v1, v2) vm_opt_iplus(res, v1, v2, iplusviv)

#if !__has_builtin(__builtin_sub_overflow)
#define vm_opt_iminus(res, v1, v2, ov_label)                     \
    {                                                            \
        long r = FIX2LONG(v1) - FIX2LONG(v2);                    \
        if (RB_FIXABLE(r)) {                                     \
            *res = RB_LONG2FIX(r);                               \
        } else {                                                 \
            *res = rb_int2big(r);                                \
            goto RTL_CONCAT(exit_opt_, ov_label);                \
        }                                                        \
    }
#else
#define vm_opt_iminus(res, v1, v2, ov_label)                    \
    {								\
        long r;			                                \
        if (__builtin_sub_overflow((long)v1, (long)v2-1, &r)) { \
            *res = rb_int2big(rb_overflowed_fix_to_int(r));     \
            goto RTL_CONCAT(exit_opt_, ov_label);               \
        }                                                       \
        *res = r;						\
    }
#endif

#define vm_opt_iminussss(res, v1, v2) vm_opt_iminus(res, v1, v2, iminussss)
#define vm_opt_iminusssv(res, v1, v2) vm_opt_iminus(res, v1, v2, iminusssv)
#define vm_opt_iminussvs(res, v1, v2) vm_opt_iminus(res, v1, v2, iminussvs)
#define vm_opt_iminussvv(res, v1, v2) vm_opt_iminus(res, v1, v2, iminussvv)
#define vm_opt_iminusssi(res, v1, v2) vm_opt_iminus(res, v1, v2, iminusssi)
#define vm_opt_iminussis(res, v1, v2) vm_opt_iminus(res, v1, v2, iminussis)
#define vm_opt_iminussvi(res, v1, v2) vm_opt_iminus(res, v1, v2, iminussvi)
#define vm_opt_iminussiv(res, v1, v2) vm_opt_iminus(res, v1, v2, iminussiv)
#define vm_opt_iminusvss(res, v1, v2) vm_opt_iminus(res, v1, v2, iminusvss)
#define vm_opt_iminusvsv(res, v1, v2) vm_opt_iminus(res, v1, v2, iminusvsv)
#define vm_opt_iminusvvs(res, v1, v2) vm_opt_iminus(res, v1, v2, iminusvvs)
#define vm_opt_iminusvvv(res, v1, v2) vm_opt_iminus(res, v1, v2, iminusvvv)
#define vm_opt_iminusvsi(res, v1, v2) vm_opt_iminus(res, v1, v2, iminusvsi)
#define vm_opt_iminusvis(res, v1, v2) vm_opt_iminus(res, v1, v2, iminusvis)
#define vm_opt_iminusvvi(res, v1, v2) vm_opt_iminus(res, v1, v2, iminusvvi)
#define vm_opt_iminusviv(res, v1, v2) vm_opt_iminus(res, v1, v2, iminusviv)

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

static rtl_inline VALUE rtl_fplus(DBL v1, DBL v2) { return DBL2NUM(v1 + v2); }
static rtl_inline VALUE rtl_fminus(DBL v1, DBL v2) { return DBL2NUM(v1 - v2); }
static rtl_inline VALUE rtl_fmult(DBL v1, DBL v2) { return DBL2NUM(v1 * v2); }
static rtl_inline VALUE rtl_fdiv(DBL v1, DBL v2) { return DBL2NUM(ruby_float_div(v1, v2)); }
static rtl_inline VALUE rtl_fmod(DBL v1, DBL v2) { return DBL2NUM(ruby_float_mod(v1, v2)); }
static rtl_inline VALUE rtl_feq(DBL v1, DBL v2) { return RBOOL(v1 == v2); }
static rtl_inline VALUE rtl_fneq(DBL v1, DBL v2) { return RBOOL(v1 != v2); }
static rtl_inline VALUE rtl_flt(DBL v1, DBL v2) { return RBOOL(v1 < v2); }
static rtl_inline VALUE rtl_fgt(DBL v1, DBL v2) { return RBOOL(v1 > v2); }
static rtl_inline VALUE rtl_fle(DBL v1, DBL v2) { return RBOOL(v1 <= v2); }
static rtl_inline VALUE rtl_fge(DBL v1, DBL v2) { return RBOOL(v1 >= v2); }

#define RTL_FLO_FUNCS2(iname1, iname2, fname)				\
 static rtl_inline VALUE iname1(VALUE v1, VALUE v2) { return fname(RFLOAT_VALUE(v1), RFLOAT_VALUE(v2)); } \
 static rtl_inline VALUE iname2(VALUE v1, DBL imm) { return fname(RFLOAT_VALUE(v1), imm); }

#define RTL_FLO_FUNCS3(iname1, iname2, iname3, fname)				\
 RTL_FLO_FUNCS2(iname1, iname2, fname)				\
 static rtl_inline VALUE iname3(DBL imm, VALUE v2) { return fname(imm, RFLOAT_VALUE(v2)); }

RTL_FLO_FUNCS2(vm_opt_fplus, vm_opt_fplusf, rtl_fplus)
RTL_FLO_FUNCS3(vm_opt_fminus, vm_opt_fminusf, vm_opt_frminusf, rtl_fminus)
RTL_FLO_FUNCS2(vm_opt_fmult, vm_opt_fmultf, rtl_fmult)
RTL_FLO_FUNCS3(vm_opt_fdiv, vm_opt_fdivf, vm_opt_frdivf, rtl_fdiv)
RTL_FLO_FUNCS3(vm_opt_fmod, vm_opt_fmodf, vm_opt_frmodf, rtl_fmod)

RTL_FLO_FUNCS2(vm_opt_feq, vm_opt_feqf, rtl_feq)
RTL_FLO_FUNCS2(vm_opt_fneq, vm_opt_fneqf, rtl_fneq)
RTL_FLO_FUNCS3(vm_opt_flt, vm_opt_fltf, vm_opt_frltf, rtl_flt)
RTL_FLO_FUNCS3(vm_opt_fgt, vm_opt_fgtf, vm_opt_frgtf, rtl_fgt)
RTL_FLO_FUNCS3(vm_opt_fle, vm_opt_flef, vm_opt_frlef, rtl_fle)
RTL_FLO_FUNCS3(vm_opt_fge, vm_opt_fgef, vm_opt_frgef, rtl_fge)

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
    static rtl_inline void RTL_CONCAT(iname, ss)(VALUE *res, VALUE v1, VALUE v2) { *res = func_name(v1, v2); }\
    static rtl_inline void RTL_CONCAT(iname, vv)(VALUE *res, VALUE v1, VALUE v2) { *res = func_name(v1, v2); }\
    static rtl_inline void RTL_CONCAT(iname, vi)(VALUE *res, VALUE v1, VALUE v2) { *res = func_name(v1, v2); }\
    static rtl_inline void RTL_CONCAT(iname, vs)(VALUE *res, VALUE v1, VALUE v2) { *res = func_name(v1, v2); }\
    static rtl_inline void RTL_CONCAT(iname, sv)(VALUE *res, VALUE v1, VALUE v2) { *res = func_name(v1, v2); }\
    static rtl_inline void RTL_CONCAT(iname, si)(VALUE *res, VALUE v1, VALUE v2) { *res = func_name(v1, v2); }

RTL_BR_FUNCS(vm_opt_ibeq, vm_opt_ieq)
RTL_BR_FUNCS(vm_opt_ibneq, vm_opt_ineq)
RTL_BR_FUNCS(vm_opt_iblt, vm_opt_ilt)
RTL_BR_FUNCS(vm_opt_ibgt, vm_opt_igt)
RTL_BR_FUNCS(vm_opt_ible, vm_opt_ile)
RTL_BR_FUNCS(vm_opt_ibge, vm_opt_ige)

#ifdef FLONUM_INSNS
RTL_BR_FUNCS(vm_opt_fbeq, vm_opt_feq)
RTL_BR_FUNCS(vm_opt_fbneq, vm_opt_fneq)
RTL_BR_FUNCS(vm_opt_fblt, vm_opt_flt)
RTL_BR_FUNCS(vm_opt_fbgt, vm_opt_fgt)
RTL_BR_FUNCS(vm_opt_fble, vm_opt_fle)
RTL_BR_FUNCS(vm_opt_fbge, vm_opt_fge)
#endif

#define CHECK_RES_FUNC_NAME(insn_name) RTL_CONCAT(result_check_, insn_name)

#define RTL_RES_CHECK_FUNCS2(iname, fname)			                                        		\
    static rtl_inline VALUE CHECK_RES_FUNC_NAME(RTL_CONCAT(iname, svv))(VALUE res) { return fname(res); }\
    static rtl_inline VALUE CHECK_RES_FUNC_NAME(RTL_CONCAT(iname, svi))(VALUE res) { return fname(res); }\
    static rtl_inline VALUE CHECK_RES_FUNC_NAME(RTL_CONCAT(iname, siv))(VALUE res) { return fname(res); }\
    static rtl_inline VALUE CHECK_RES_FUNC_NAME(RTL_CONCAT(iname, svs))(VALUE res) { return fname(res); }\
    static rtl_inline VALUE CHECK_RES_FUNC_NAME(RTL_CONCAT(iname, ssv))(VALUE res) { return fname(res); }\
    static rtl_inline VALUE CHECK_RES_FUNC_NAME(RTL_CONCAT(iname, sss))(VALUE res) { return fname(res); }\
    static rtl_inline VALUE CHECK_RES_FUNC_NAME(RTL_CONCAT(iname, ssi))(VALUE res) { return fname(res); }\
    static rtl_inline VALUE CHECK_RES_FUNC_NAME(RTL_CONCAT(iname, sis))(VALUE res) { return fname(res); }\
    static rtl_inline VALUE CHECK_RES_FUNC_NAME(RTL_CONCAT(iname, vvv))(VALUE res) { return fname(res); }\
    static rtl_inline VALUE CHECK_RES_FUNC_NAME(RTL_CONCAT(iname, vvi))(VALUE res) { return fname(res); }\
    static rtl_inline VALUE CHECK_RES_FUNC_NAME(RTL_CONCAT(iname, viv))(VALUE res) { return fname(res); }\
    static rtl_inline VALUE CHECK_RES_FUNC_NAME(RTL_CONCAT(iname, vvs))(VALUE res) { return fname(res); }\
    static rtl_inline VALUE CHECK_RES_FUNC_NAME(RTL_CONCAT(iname, vsv))(VALUE res) { return fname(res); }\
    static rtl_inline VALUE CHECK_RES_FUNC_NAME(RTL_CONCAT(iname, vss))(VALUE res) { return fname(res); }\
    static rtl_inline VALUE CHECK_RES_FUNC_NAME(RTL_CONCAT(iname, vsi))(VALUE res) { return fname(res); }\
    static rtl_inline VALUE CHECK_RES_FUNC_NAME(RTL_CONCAT(iname, vis))(VALUE res) { return fname(res); }

static rtl_inline int check_res_fixnum(VALUE res) { return !FIXNUM_P(res); }
static rtl_inline int check_res_flonum(VALUE res) { return !FLONUM_P(res); }
static rtl_inline int check_res_undef(VALUE res) { return res == Qundef; }
static rtl_inline int check_res_nothing(VALUE res) { return FALSE; }

RTL_RES_CHECK_FUNCS2(opt_iplus, check_res_nothing)
RTL_RES_CHECK_FUNCS2(opt_iminus, check_res_nothing)
RTL_RES_CHECK_FUNCS2(opt_imult, check_res_fixnum)
RTL_RES_CHECK_FUNCS2(opt_idiv, check_res_fixnum)
RTL_RES_CHECK_FUNCS2(opt_imod, check_res_fixnum)
RTL_RES_CHECK_FUNCS2(opt_ior, check_res_nothing)
RTL_RES_CHECK_FUNCS2(opt_iand, check_res_nothing)
RTL_RES_CHECK_FUNCS2(opt_ieq, check_res_nothing)
RTL_RES_CHECK_FUNCS2(opt_ineq, check_res_nothing)
RTL_RES_CHECK_FUNCS2(opt_ilt, check_res_nothing)
RTL_RES_CHECK_FUNCS2(opt_igt, check_res_nothing)
RTL_RES_CHECK_FUNCS2(opt_ile, check_res_nothing)
RTL_RES_CHECK_FUNCS2(opt_ige, check_res_nothing)
#ifdef FLONUM_INSNS
RTL_RES_CHECK_FUNCS2(opt_fplus, check_res_flonum)
RTL_RES_CHECK_FUNCS2(opt_fminus, check_res_flonum)
RTL_RES_CHECK_FUNCS2(opt_fmult, check_res_flonum)
RTL_RES_CHECK_FUNCS2(opt_fdiv, check_res_flonum)
RTL_RES_CHECK_FUNCS2(opt_fmod, check_res_flonum)
RTL_RES_CHECK_FUNCS2(opt_feq, check_res_nothing)
RTL_RES_CHECK_FUNCS2(opt_fneq, check_res_nothing)
RTL_RES_CHECK_FUNCS2(opt_flt, check_res_nothing)
RTL_RES_CHECK_FUNCS2(opt_fgt, check_res_nothing)
RTL_RES_CHECK_FUNCS2(opt_fle, check_res_nothing)
RTL_RES_CHECK_FUNCS2(opt_fge, check_res_nothing)
#endif
RTL_RES_CHECK_FUNCS2(opt_iaref, check_res_undef)

#define CHECK_FUNC_NAME(insn_name) RTL_CONCAT(input_check_, insn_name)

#define RTL_CHECK_FUNCS2(iname, fname)			                                        		\
    static rtl_inline VALUE CHECK_FUNC_NAME(RTL_CONCAT(iname, svv))(VALUE v1, VALUE v2) { return fname(v1, v2); }\
    static rtl_inline VALUE CHECK_FUNC_NAME(RTL_CONCAT(iname, svi))(VALUE v1, VALUE v2) { return fname(v1, v2); }\
    static rtl_inline VALUE CHECK_FUNC_NAME(RTL_CONCAT(iname, siv))(VALUE v1, VALUE v2) { return fname(v1, v2); }\
    static rtl_inline VALUE CHECK_FUNC_NAME(RTL_CONCAT(iname, svs))(VALUE v1, VALUE v2) { return fname(v1, v2); }\
    static rtl_inline VALUE CHECK_FUNC_NAME(RTL_CONCAT(iname, ssv))(VALUE v1, VALUE v2) { return fname(v1, v2); }\
    static rtl_inline VALUE CHECK_FUNC_NAME(RTL_CONCAT(iname, sss))(VALUE v1, VALUE v2) { return fname(v1, v2); }\
    static rtl_inline VALUE CHECK_FUNC_NAME(RTL_CONCAT(iname, ssi))(VALUE v1, VALUE v2) { return fname(v1, v2); }\
    static rtl_inline VALUE CHECK_FUNC_NAME(RTL_CONCAT(iname, sis))(VALUE v1, VALUE v2) { return fname(v1, v2); }\
    static rtl_inline VALUE CHECK_FUNC_NAME(RTL_CONCAT(iname, vvv))(VALUE v1, VALUE v2) { return fname(v1, v2); }\
    static rtl_inline VALUE CHECK_FUNC_NAME(RTL_CONCAT(iname, vvi))(VALUE v1, VALUE v2) { return fname(v1, v2); }\
    static rtl_inline VALUE CHECK_FUNC_NAME(RTL_CONCAT(iname, viv))(VALUE v1, VALUE v2) { return fname(v1, v2); }\
    static rtl_inline VALUE CHECK_FUNC_NAME(RTL_CONCAT(iname, vvs))(VALUE v1, VALUE v2) { return fname(v1, v2); }\
    static rtl_inline VALUE CHECK_FUNC_NAME(RTL_CONCAT(iname, vsv))(VALUE v1, VALUE v2) { return fname(v1, v2); }\
    static rtl_inline VALUE CHECK_FUNC_NAME(RTL_CONCAT(iname, vss))(VALUE v1, VALUE v2) { return fname(v1, v2); }\
    static rtl_inline VALUE CHECK_FUNC_NAME(RTL_CONCAT(iname, vsi))(VALUE v1, VALUE v2) { return fname(v1, v2); }\
    static rtl_inline VALUE CHECK_FUNC_NAME(RTL_CONCAT(iname, vis))(VALUE v1, VALUE v2) { return fname(v1, v2); }

static rtl_inline int check_zero(VALUE v1, VALUE v2) { return v2 == LONG2FIX(0); }
static rtl_inline int check_nothing(VALUE v1, VALUE v2) { return FALSE; }

RTL_CHECK_FUNCS2(opt_iplus, check_nothing)
RTL_CHECK_FUNCS2(opt_iminus, check_nothing)
RTL_CHECK_FUNCS2(opt_imult, check_nothing)
RTL_CHECK_FUNCS2(opt_idiv, check_zero)
RTL_CHECK_FUNCS2(opt_imod, check_zero)
RTL_CHECK_FUNCS2(opt_ior, check_nothing)
RTL_CHECK_FUNCS2(opt_iand, check_nothing)
RTL_CHECK_FUNCS2(opt_ieq, check_nothing)
RTL_CHECK_FUNCS2(opt_ineq, check_nothing)
RTL_CHECK_FUNCS2(opt_ilt, check_nothing)
RTL_CHECK_FUNCS2(opt_igt, check_nothing)
RTL_CHECK_FUNCS2(opt_ile, check_nothing)
RTL_CHECK_FUNCS2(opt_ige, check_nothing)
#ifdef FLONUM_INSNS
RTL_CHECK_FUNCS2(opt_fplus, check_nothing)
RTL_CHECK_FUNCS2(opt_fminus, check_nothing)
RTL_CHECK_FUNCS2(opt_fmult, check_nothing)
RTL_CHECK_FUNCS2(opt_fdiv, check_nothing)
RTL_CHECK_FUNCS2(opt_fmod, check_nothing)
RTL_CHECK_FUNCS2(opt_feq, check_nothing)
RTL_CHECK_FUNCS2(opt_fneq, check_nothing)
RTL_CHECK_FUNCS2(opt_flt, check_nothing)
RTL_CHECK_FUNCS2(opt_fgt, check_nothing)
RTL_CHECK_FUNCS2(opt_fle, check_nothing)
RTL_CHECK_FUNCS2(opt_fge, check_nothing)
#endif
RTL_CHECK_FUNCS2(opt_iaref, check_nothing)

#define CHECK_INPUTS(iname, push_fname, v1, v2)                 \
    if (UNLIKELY(CHECK_FUNC_NAME(iname)(v1, v2))) {             \
        ABS_JUMP(rb_rtl_get_safe_pc(GET_CFP(), reg_pc));	\
        END_INSN(iname); 					\
    }

static int
rtl_fixnum_times_start(VALUE recv, VALUE *index) {
    if (!FIXNUM_P(recv)) return FALSE;
    *index = LONG2FIX(0);
    return TRUE;
}

static VALUE
rtl_fixnum_times_cont(VALUE recv, VALUE index) {
    return index + 2; /* index is fixnum */
}

static int
rtl_fixnum_times_body(VALUE recv, VALUE index, VALUE *arg) {
    *arg = index;
    return index < recv;
}

#include "internal/range.h"

static int
rtl_range_each_start(VALUE recv, VALUE *index) {
    if (!rb_obj_is_kind_of(recv, rb_cRange)) return FALSE;
    *index = RANGE_BEG(recv);
    if (!FIXNUM_P(*index) || !FIXNUM_P(RANGE_END(recv))) return FALSE;
    return TRUE;
}

static VALUE
rtl_range_each_cont(VALUE recv, VALUE index) {
    return index + 2;
}

static int
rtl_range_each_body(VALUE recv, VALUE index, VALUE *arg) {
    *arg = index;
    if (RTEST(RANGE_EXCL(recv)) ? index < RANGE_END(recv) : index <= RANGE_END(recv))
        return TRUE;
    *arg = recv;
    return FALSE;
}

static int
rtl_arr_each_start(VALUE recv, VALUE *index) {
    if (!RB_TYPE_P(recv, T_ARRAY)) return FALSE;
    *index = LONG2FIX(0);
    return TRUE;
}

static VALUE
rtl_arr_each_cont(VALUE recv, VALUE index) {
    return index + 2;
}

static int
rtl_arr_each_body(VALUE recv, VALUE index, VALUE *arg) {
    long i = FIX2LONG(index);

    if (i < RARRAY_LEN(recv)) {
        *arg = RARRAY_AREF(recv, i);
        return TRUE;
    }
    *arg = recv;
    return FALSE;
}

int
rtl_get_iter_funcs(void *cfunc, int (**start_func)(VALUE, VALUE *), int (**arg_func)(VALUE, VALUE, VALUE *),
                   VALUE (**cont_func)(VALUE, VALUE), char *attr) {
    if (cfunc == rtl_fixnum_ntimes_func) {
	*start_func = rtl_fixnum_times_start;
        *arg_func = rtl_fixnum_times_body;
        *cont_func = rtl_fixnum_times_cont;
        *attr = ATTR_FIXNUM;
    } else if (cfunc == rtl_range_each_func) {
	*start_func = rtl_range_each_start;
        *arg_func = rtl_range_each_body;
        *cont_func = rtl_range_each_cont;
        *attr = ATTR_FIXNUM;
    } else if (cfunc == rb_ary_each) {
	*start_func = rtl_arr_each_start;
        *arg_func = rtl_arr_each_body;
        *cont_func = rtl_arr_each_cont;
        *attr = ATTR_UNKNOWN;
    } else {
	return FALSE;
    }
    return TRUE;
}
