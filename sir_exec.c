/**********************************************************************

  sir_exec.c - Code for executing SIR (Specialized Internal Representation)
               insns in the VM interpreter

  Copyright (C) 2022 Vladimir Makarov vmakarov@redhat.com

**********************************************************************/

/* Each SIR insn from insns.def has a corresponding function with
   prefix vm_ which is responsible for executing all or the most insn
   code.  The functions are declared as static inline to exclude an
   overhead for the function call.  Such approach is to simplify
   implementation of interpreter.

   We have many different specialized move insns to improve the
   interpreter performance.  We would not need such number of the
   insns for JIT only as the used C compiler can generate specialized
   insns versions itself.  */

#if 1
#define SIR_EXEC_ASSERT(expr)
#else
#define SIR_EXEC_ASSERT(expr) RUBY_ASSERT_ALWAYS(expr)
#endif

#if defined(__GNUC__)
#define sir_inline inline __attribute__ ((__always_inline__))
#define sir_noinline __attribute__ ((__noinline__))
#else
#define sir_inline inline
#define sir_noinline
#endif

/* Macros used to translate insn name into the corresponding function
   name.  */
#define SIR_CONCAT(a, b) a ## b
#define SIR_SUBST_CONCAT(a, b) SIR_CONCAT(a, b)
#define SIR_FUNC_NAME(insn_name) SIR_CONCAT(vm_, insn_name)

#define SIR_FUNC0_NAME(n, s) vm_sir_ ## n ## s
#define SIR_FUNCS0(iname, fname)                                              \
    static sir_inline void SIR_FUNC0_NAME(iname, svv)(VALUE *res, VALUE v1, VALUE v2) { *res = fname(v1, v2); }\
    static sir_inline void SIR_FUNC0_NAME(iname, svi)(VALUE *res, VALUE v1, VALUE v2) { *res = fname(v1, v2); }\
    static sir_inline void SIR_FUNC0_NAME(iname, siv)(VALUE *res, VALUE v1, VALUE v2) { *res = fname(v1, v2); }\
    static sir_inline void SIR_FUNC0_NAME(iname, svs)(VALUE *res, VALUE v1, VALUE v2) { *res = fname(v1, v2); }\
    static sir_inline void SIR_FUNC0_NAME(iname, ssv)(VALUE *res, VALUE v1, VALUE v2) { *res = fname(v1, v2); }\
    static sir_inline void SIR_FUNC0_NAME(iname, ssi)(VALUE *res, VALUE v1, VALUE v2) { *res = fname(v1, v2); }\
    static sir_inline void SIR_FUNC0_NAME(iname, sis)(VALUE *res, VALUE v1, VALUE v2) { *res = fname(v1, v2); }

SIR_FUNCS0(plus, vm_opt_plus)
SIR_FUNCS0(minus, vm_opt_minus)
SIR_FUNCS0(mult, vm_opt_mult)
SIR_FUNCS0(div, vm_opt_div)
SIR_FUNCS0(mod, vm_opt_mod)
SIR_FUNCS0(or, vm_opt_or)
SIR_FUNCS0(and, vm_opt_and)
SIR_FUNCS0(lt, vm_opt_lt)
SIR_FUNCS0(gt, vm_opt_gt)
SIR_FUNCS0(le, vm_opt_le)
SIR_FUNCS0(ge, vm_opt_ge)
SIR_FUNCS0(aref,vm_opt_aref)

#define SIR_FUNCS(iname)										   \
    static sir_inline void SIR_CONCAT(iname, svv)(VALUE *res, VALUE v1, VALUE v2) { *res = iname(v1, v2); }\
    static sir_inline void SIR_CONCAT(iname, svi)(VALUE *res, VALUE v1, VALUE v2) { *res = iname(v1, v2); }\
    static sir_inline void SIR_CONCAT(iname, siv)(VALUE *res, VALUE v1, VALUE v2) { *res = iname(v1, v2); }\
    static sir_inline void SIR_CONCAT(iname, svs)(VALUE *res, VALUE v1, VALUE v2) { *res = iname(v1, v2); }\
    static sir_inline void SIR_CONCAT(iname, ssv)(VALUE *res, VALUE v1, VALUE v2) { *res = iname(v1, v2); }\
    static sir_inline void SIR_CONCAT(iname, ssi)(VALUE *res, VALUE v1, VALUE v2) { *res = iname(v1, v2); }\
    static sir_inline void SIR_CONCAT(iname, sis)(VALUE *res, VALUE v1, VALUE v2) { *res = iname(v1, v2); }\
    static sir_inline void SIR_CONCAT(iname, vvv)(VALUE *res, VALUE v1, VALUE v2) { *res = iname(v1, v2); }\
    static sir_inline void SIR_CONCAT(iname, vvi)(VALUE *res, VALUE v1, VALUE v2) { *res = iname(v1, v2); }\
    static sir_inline void SIR_CONCAT(iname, viv)(VALUE *res, VALUE v1, VALUE v2) { *res = iname(v1, v2); }\
    static sir_inline void SIR_CONCAT(iname, vvs)(VALUE *res, VALUE v1, VALUE v2) { *res = iname(v1, v2); }\
    static sir_inline void SIR_CONCAT(iname, vsv)(VALUE *res, VALUE v1, VALUE v2) { *res = iname(v1, v2); }\
    static sir_inline void SIR_CONCAT(iname, vsi)(VALUE *res, VALUE v1, VALUE v2) { *res = iname(v1, v2); }\
    static sir_inline void SIR_CONCAT(iname, vis)(VALUE *res, VALUE v1, VALUE v2) { *res = iname(v1, v2); }\
    static sir_inline void SIR_CONCAT(iname, vss)(VALUE *res, VALUE v1, VALUE v2) { *res = iname(v1, v2); }\
    static sir_inline void SIR_CONCAT(iname, sss)(VALUE *res, VALUE v1, VALUE v2) { *res = iname(v1, v2); }\


static sir_inline void pushvv(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2) {INC_SP(2); TOPN(1) = v1; TOPN(0) = v2;}
static sir_inline void pushvi(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2) {INC_SP(2); TOPN(1) = v1; TOPN(0) = v2;}
static sir_inline void pushiv(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2) {INC_SP(2); TOPN(1) = v1; TOPN(0) = v2;}
static sir_inline void pushvs(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2) {INC_SP(1); TOPN(0) = v2; TOPN(1) = v1;}
static sir_inline void pushsv(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2) {INC_SP(1); TOPN(0) = v2;}
static sir_inline void pushsi(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2) {INC_SP(1); TOPN(0) = v2;}
static sir_inline void pushis(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2) {INC_SP(1); TOPN(0) = v2; TOPN(1) = v1;}
static sir_inline void pushss(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2) {}

static sir_inline void pushsvv(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2, VALUE v3) {
    INC_SP(2); TOPN(1) = v2; TOPN(0) = v3;
}
static sir_inline void pushsiv(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2, VALUE v3) {
    INC_SP(2); TOPN(1) = v2; TOPN(0) = v3;
}
static sir_inline void pushsvs(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2, VALUE v3) {
    INC_SP(1); TOPN(1) = v2; TOPN(0) = v3;
}
static sir_inline void pushssv(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2, VALUE v3) {
    INC_SP(1); TOPN(0) = v2;
}
static sir_inline void pushsis(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2, VALUE v3) {
    INC_SP(1); TOPN(1) = v2; TOPN(0) = v3;
}
static sir_inline void pushsss(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2, VALUE v3) {
}

static sir_inline void pushvvv(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2, VALUE v3) {
    INC_SP(3); TOPN(2) = v1; TOPN(1) = v2; TOPN(0) = v3;
}
static sir_inline void pushviv(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2, VALUE v3) {
    INC_SP(3); TOPN(2) = v1; TOPN(1) = v2; TOPN(0) = v3;
}
static sir_inline void pushvvs(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2, VALUE v3) {
    INC_SP(2); TOPN(2) = v1; TOPN(1) = v2; TOPN(0) = v3;
}
static sir_inline void pushvsv(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2, VALUE v3) {
    INC_SP(2); TOPN(2) = v1; TOPN(1) = v2; TOPN(0) = v3;
}
static sir_inline void pushvis(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2, VALUE v3) {
    INC_SP(2); TOPN(2) = v1; TOPN(1) = v2; TOPN(0) = v3;
}
static sir_inline void pushvss(rb_control_frame_t *reg_cfp, VALUE v1, VALUE v2, VALUE v3) {
    INC_SP(1); TOPN(2) = v1; TOPN(1) = v2; TOPN(0) = v3;
}

#define SIR_FUNCS3(iname)                                           \
    static sir_inline VALUE SIR_CONCAT(iname, svv)(VALUE v0, VALUE v1, VALUE v2) { return iname(v0, v1, v2); }\
    static sir_inline VALUE SIR_CONCAT(iname, siv)(VALUE v0, VALUE v1, VALUE v2) { return iname(v0, v1, v2); }\
    static sir_inline VALUE SIR_CONCAT(iname, svs)(VALUE v0, VALUE v1, VALUE v2) { return iname(v0, v1, v2); }\
    static sir_inline VALUE SIR_CONCAT(iname, ssv)(VALUE v0, VALUE v1, VALUE v2) { return iname(v0, v1, v2); }\
    static sir_inline VALUE SIR_CONCAT(iname, sis)(VALUE v0, VALUE v1, VALUE v2) { return iname(v0, v1, v2); }\
    static sir_inline VALUE SIR_CONCAT(iname, sss)(VALUE v0, VALUE v1, VALUE v2) { return iname(v0, v1, v2); }\
    static sir_inline VALUE SIR_CONCAT(iname, vvv)(VALUE v0, VALUE v1, VALUE v2) { return iname(v0, v1, v2); }\
    static sir_inline VALUE SIR_CONCAT(iname, viv)(VALUE v0, VALUE v1, VALUE v2) { return iname(v0, v1, v2); }\
    static sir_inline VALUE SIR_CONCAT(iname, vvs)(VALUE v0, VALUE v1, VALUE v2) { return iname(v0, v1, v2); }\
    static sir_inline VALUE SIR_CONCAT(iname, vsv)(VALUE v0, VALUE v1, VALUE v2) { return iname(v0, v1, v2); }\
    static sir_inline VALUE SIR_CONCAT(iname, vis)(VALUE v0, VALUE v1, VALUE v2) { return iname(v0, v1, v2); }\
    static sir_inline VALUE SIR_CONCAT(iname, vss)(VALUE v0, VALUE v1, VALUE v2) { return iname(v0, v1, v2); }

static sir_inline VALUE vm_sir_imult(VALUE v1, VALUE v2) { return rb_fix_mul_fix(v1, v2); }
static sir_inline VALUE vm_sir_idiv(VALUE v1, VALUE v2) { return rb_fix_div_fix(v1, v2); }
static sir_inline VALUE vm_sir_imod(VALUE v1, VALUE v2) { return rb_fix_mod_fix(v1, v2); }
static sir_inline VALUE vm_sir_ior(VALUE v1, VALUE v2) { return v1 | v2; }
static sir_inline VALUE vm_sir_iand(VALUE v1, VALUE v2) { return (v1 & v2) | 1; }
static sir_inline VALUE vm_sir_ieq(VALUE v1, VALUE v2) { return RBOOL(v1 == v2); }
static sir_inline VALUE vm_sir_ineq(VALUE v1, VALUE v2) { return RBOOL(v1 != v2); }
static sir_inline VALUE vm_sir_ilt(VALUE v1, VALUE v2) { return RBOOL((SIGNED_VALUE)v1 < (SIGNED_VALUE)v2); }
static sir_inline VALUE vm_sir_igt(VALUE v1, VALUE v2) { return RBOOL((SIGNED_VALUE)v1 > (SIGNED_VALUE)v2); }
static sir_inline VALUE vm_sir_ile(VALUE v1, VALUE v2) { return RBOOL((SIGNED_VALUE)v1 <= (SIGNED_VALUE)v2); }
static sir_inline VALUE vm_sir_ige(VALUE v1, VALUE v2) { return RBOOL((SIGNED_VALUE)v1 > (SIGNED_VALUE)v2); }

#if !__has_builtin(__builtin_add_overflow)
#define vm_sir_iplus(res, v1, v2, ov_label)                      \
    {                                                            \
        long r = FIX2LONG(v1) + FIX2LONG(v2);                    \
        if (RB_FIXABLE(r)) {                                     \
            *res = RB_LONG2FIX(r);                               \
        } else {                                                 \
            *res = rb_int2big(r);                                \
            goto SIR_CONCAT(exit_sir_, ov_label);                \
        }                                                        \
    }
#else
#define vm_sir_iplus(res, v1, v2, ov_label)                     \
    {								\
        long r;							\
        if (__builtin_add_overflow((long)v1, (long)v2-1, &r)) { \
            *res = rb_int2big(rb_overflowed_fix_to_int(r));     \
            goto SIR_CONCAT(exit_sir_, ov_label);               \
        }                                                       \
        *res = r;						\
    }
#endif

#define vm_sir_iplussss(res, v1, v2) vm_sir_iplus(res, v1, v2, iplussss)
#define vm_sir_iplusssv(res, v1, v2) vm_sir_iplus(res, v1, v2, iplusssv)
#define vm_sir_iplussvs(res, v1, v2) vm_sir_iplus(res, v1, v2, iplussvs)
#define vm_sir_iplussvv(res, v1, v2) vm_sir_iplus(res, v1, v2, iplussvv)
#define vm_sir_iplusssi(res, v1, v2) vm_sir_iplus(res, v1, v2, iplusssi)
#define vm_sir_iplussis(res, v1, v2) vm_sir_iplus(res, v1, v2, iplussis)
#define vm_sir_iplussvi(res, v1, v2) vm_sir_iplus(res, v1, v2, iplussvi)
#define vm_sir_iplussiv(res, v1, v2) vm_sir_iplus(res, v1, v2, iplussiv)
#define vm_sir_iplusvss(res, v1, v2) vm_sir_iplus(res, v1, v2, iplusvss)
#define vm_sir_iplusvsv(res, v1, v2) vm_sir_iplus(res, v1, v2, iplusvsv)
#define vm_sir_iplusvvs(res, v1, v2) vm_sir_iplus(res, v1, v2, iplusvvs)
#define vm_sir_iplusvvv(res, v1, v2) vm_sir_iplus(res, v1, v2, iplusvvv)
#define vm_sir_iplusvsi(res, v1, v2) vm_sir_iplus(res, v1, v2, iplusvsi)
#define vm_sir_iplusvis(res, v1, v2) vm_sir_iplus(res, v1, v2, iplusvis)
#define vm_sir_iplusvvi(res, v1, v2) vm_sir_iplus(res, v1, v2, iplusvvi)
#define vm_sir_iplusviv(res, v1, v2) vm_sir_iplus(res, v1, v2, iplusviv)

#if !__has_builtin(__builtin_sub_overflow)
#define vm_sir_iminus(res, v1, v2, ov_label)                     \
    {                                                            \
        long r = FIX2LONG(v1) - FIX2LONG(v2);                    \
        if (RB_FIXABLE(r)) {                                     \
            *res = RB_LONG2FIX(r);                               \
        } else {                                                 \
            *res = rb_int2big(r);                                \
            goto SIR_CONCAT(exit_sir_, ov_label);                \
        }                                                        \
    }
#else
#define vm_sir_iminus(res, v1, v2, ov_label)                    \
    {								\
        long r;			                                \
        if (__builtin_sub_overflow((long)v1, (long)v2-1, &r)) { \
            *res = rb_int2big(rb_overflowed_fix_to_int(r));     \
            goto SIR_CONCAT(exit_sir_, ov_label);               \
        }                                                       \
        *res = r;						\
    }
#endif

#define vm_sir_iminussss(res, v1, v2) vm_sir_iminus(res, v1, v2, iminussss)
#define vm_sir_iminusssv(res, v1, v2) vm_sir_iminus(res, v1, v2, iminusssv)
#define vm_sir_iminussvs(res, v1, v2) vm_sir_iminus(res, v1, v2, iminussvs)
#define vm_sir_iminussvv(res, v1, v2) vm_sir_iminus(res, v1, v2, iminussvv)
#define vm_sir_iminusssi(res, v1, v2) vm_sir_iminus(res, v1, v2, iminusssi)
#define vm_sir_iminussis(res, v1, v2) vm_sir_iminus(res, v1, v2, iminussis)
#define vm_sir_iminussvi(res, v1, v2) vm_sir_iminus(res, v1, v2, iminussvi)
#define vm_sir_iminussiv(res, v1, v2) vm_sir_iminus(res, v1, v2, iminussiv)
#define vm_sir_iminusvss(res, v1, v2) vm_sir_iminus(res, v1, v2, iminusvss)
#define vm_sir_iminusvsv(res, v1, v2) vm_sir_iminus(res, v1, v2, iminusvsv)
#define vm_sir_iminusvvs(res, v1, v2) vm_sir_iminus(res, v1, v2, iminusvvs)
#define vm_sir_iminusvvv(res, v1, v2) vm_sir_iminus(res, v1, v2, iminusvvv)
#define vm_sir_iminusvsi(res, v1, v2) vm_sir_iminus(res, v1, v2, iminusvsi)
#define vm_sir_iminusvis(res, v1, v2) vm_sir_iminus(res, v1, v2, iminusvis)
#define vm_sir_iminusvvi(res, v1, v2) vm_sir_iminus(res, v1, v2, iminusvvi)
#define vm_sir_iminusviv(res, v1, v2) vm_sir_iminus(res, v1, v2, iminusviv)

SIR_FUNCS(vm_sir_imult)
SIR_FUNCS(vm_sir_idiv)
SIR_FUNCS(vm_sir_imod)
SIR_FUNCS(vm_sir_ior)
SIR_FUNCS(vm_sir_iand)
SIR_FUNCS(vm_sir_ieq)
SIR_FUNCS(vm_sir_ineq)
SIR_FUNCS(vm_sir_ilt)
SIR_FUNCS(vm_sir_igt)
SIR_FUNCS(vm_sir_ile)
SIR_FUNCS(vm_sir_ige)

static sir_inline VALUE sir_fplus(DBL v1, DBL v2) { return DBL2NUM(v1 + v2); }
static sir_inline VALUE sir_fminus(DBL v1, DBL v2) { return DBL2NUM(v1 - v2); }
static sir_inline VALUE sir_fmult(DBL v1, DBL v2) { return DBL2NUM(v1 * v2); }
static sir_inline VALUE sir_fdiv(DBL v1, DBL v2) { return DBL2NUM(ruby_float_div(v1, v2)); }
static sir_inline VALUE sir_fmod(DBL v1, DBL v2) { return DBL2NUM(ruby_float_mod(v1, v2)); }
static sir_inline VALUE sir_feq(DBL v1, DBL v2) { return RBOOL(v1 == v2); }
static sir_inline VALUE sir_fneq(DBL v1, DBL v2) { return RBOOL(v1 != v2); }
static sir_inline VALUE sir_flt(DBL v1, DBL v2) { return RBOOL(v1 < v2); }
static sir_inline VALUE sir_fgt(DBL v1, DBL v2) { return RBOOL(v1 > v2); }
static sir_inline VALUE sir_fle(DBL v1, DBL v2) { return RBOOL(v1 <= v2); }
static sir_inline VALUE sir_fge(DBL v1, DBL v2) { return RBOOL(v1 >= v2); }

#define SIR_FLO_FUNCS2(iname1, iname2, fname)				\
 static sir_inline VALUE iname1(VALUE v1, VALUE v2) { return fname(RFLOAT_VALUE(v1), RFLOAT_VALUE(v2)); } \
 static sir_inline VALUE iname2(VALUE v1, DBL imm) { return fname(RFLOAT_VALUE(v1), imm); }

#define SIR_FLO_FUNCS3(iname1, iname2, iname3, fname)				\
 SIR_FLO_FUNCS2(iname1, iname2, fname)				\
 static sir_inline VALUE iname3(DBL imm, VALUE v2) { return fname(imm, RFLOAT_VALUE(v2)); }

SIR_FLO_FUNCS2(vm_sir_fplus, vm_sir_fplusf, sir_fplus)
SIR_FLO_FUNCS3(vm_sir_fminus, vm_sir_fminusf, vm_sir_frminusf, sir_fminus)
SIR_FLO_FUNCS2(vm_sir_fmult, vm_sir_fmultf, sir_fmult)
SIR_FLO_FUNCS3(vm_sir_fdiv, vm_sir_fdivf, vm_sir_frdivf, sir_fdiv)
SIR_FLO_FUNCS3(vm_sir_fmod, vm_sir_fmodf, vm_sir_frmodf, sir_fmod)

SIR_FLO_FUNCS2(vm_sir_feq, vm_sir_feqf, sir_feq)
SIR_FLO_FUNCS2(vm_sir_fneq, vm_sir_fneqf, sir_fneq)
SIR_FLO_FUNCS3(vm_sir_flt, vm_sir_fltf, vm_sir_frltf, sir_flt)
SIR_FLO_FUNCS3(vm_sir_fgt, vm_sir_fgtf, vm_sir_frgtf, sir_fgt)
SIR_FLO_FUNCS3(vm_sir_fle, vm_sir_flef, vm_sir_frlef, sir_fle)
SIR_FLO_FUNCS3(vm_sir_fge, vm_sir_fgef, vm_sir_frgef, sir_fge)

static sir_inline VALUE vm_sir_iaref(VALUE v1, VALUE v2) {
    if (SPECIAL_CONST_P(v1) || RBASIC_CLASS(v1) != rb_cArray) return Qundef;
    return rb_ary_entry_internal(v1, FIX2LONG(v2));
}
SIR_FUNCS(vm_sir_iaref)

static sir_inline VALUE vm_sir_iaset(VALUE arr, VALUE ind, VALUE set) {
    if (SPECIAL_CONST_P(arr) || RBASIC_CLASS(arr) != rb_cArray) return Qundef;
    rb_ary_store(arr, FIX2LONG(ind), set);
    return set;
}
SIR_FUNCS3(vm_sir_iaset)

SIR_FUNCS3(vm_opt_aset)

#define SIR_BR_FUNCS(iname, func_name)                              \
    static sir_inline void SIR_CONCAT(iname, ss)(VALUE *res, VALUE v1, VALUE v2) { *res = func_name(v1, v2); }\
    static sir_inline void SIR_CONCAT(iname, vv)(VALUE *res, VALUE v1, VALUE v2) { *res = func_name(v1, v2); }\
    static sir_inline void SIR_CONCAT(iname, vi)(VALUE *res, VALUE v1, VALUE v2) { *res = func_name(v1, v2); }\
    static sir_inline void SIR_CONCAT(iname, vs)(VALUE *res, VALUE v1, VALUE v2) { *res = func_name(v1, v2); }\
    static sir_inline void SIR_CONCAT(iname, sv)(VALUE *res, VALUE v1, VALUE v2) { *res = func_name(v1, v2); }\
    static sir_inline void SIR_CONCAT(iname, si)(VALUE *res, VALUE v1, VALUE v2) { *res = func_name(v1, v2); }

SIR_BR_FUNCS(vm_sir_ibeq, vm_sir_ieq)
SIR_BR_FUNCS(vm_sir_ibneq, vm_sir_ineq)
SIR_BR_FUNCS(vm_sir_iblt, vm_sir_ilt)
SIR_BR_FUNCS(vm_sir_ibgt, vm_sir_igt)
SIR_BR_FUNCS(vm_sir_ible, vm_sir_ile)
SIR_BR_FUNCS(vm_sir_ibge, vm_sir_ige)

#ifdef FLONUM_INSNS
SIR_BR_FUNCS(vm_sir_fbeq, vm_sir_feq)
SIR_BR_FUNCS(vm_sir_fbneq, vm_sir_fneq)
SIR_BR_FUNCS(vm_sir_fblt, vm_sir_flt)
SIR_BR_FUNCS(vm_sir_fbgt, vm_sir_fgt)
SIR_BR_FUNCS(vm_sir_fble, vm_sir_fle)
SIR_BR_FUNCS(vm_sir_fbge, vm_sir_fge)
#endif

#define CHECK_RES_FUNC_NAME(insn_name) SIR_CONCAT(result_check_, insn_name)

#define SIR_RES_CHECK_FUNCS2(iname, fname)			                                        		\
    static sir_inline VALUE CHECK_RES_FUNC_NAME(SIR_CONCAT(iname, svv))(VALUE res) { return fname(res); }\
    static sir_inline VALUE CHECK_RES_FUNC_NAME(SIR_CONCAT(iname, svi))(VALUE res) { return fname(res); }\
    static sir_inline VALUE CHECK_RES_FUNC_NAME(SIR_CONCAT(iname, siv))(VALUE res) { return fname(res); }\
    static sir_inline VALUE CHECK_RES_FUNC_NAME(SIR_CONCAT(iname, svs))(VALUE res) { return fname(res); }\
    static sir_inline VALUE CHECK_RES_FUNC_NAME(SIR_CONCAT(iname, ssv))(VALUE res) { return fname(res); }\
    static sir_inline VALUE CHECK_RES_FUNC_NAME(SIR_CONCAT(iname, sss))(VALUE res) { return fname(res); }\
    static sir_inline VALUE CHECK_RES_FUNC_NAME(SIR_CONCAT(iname, ssi))(VALUE res) { return fname(res); }\
    static sir_inline VALUE CHECK_RES_FUNC_NAME(SIR_CONCAT(iname, sis))(VALUE res) { return fname(res); }\
    static sir_inline VALUE CHECK_RES_FUNC_NAME(SIR_CONCAT(iname, vvv))(VALUE res) { return fname(res); }\
    static sir_inline VALUE CHECK_RES_FUNC_NAME(SIR_CONCAT(iname, vvi))(VALUE res) { return fname(res); }\
    static sir_inline VALUE CHECK_RES_FUNC_NAME(SIR_CONCAT(iname, viv))(VALUE res) { return fname(res); }\
    static sir_inline VALUE CHECK_RES_FUNC_NAME(SIR_CONCAT(iname, vvs))(VALUE res) { return fname(res); }\
    static sir_inline VALUE CHECK_RES_FUNC_NAME(SIR_CONCAT(iname, vsv))(VALUE res) { return fname(res); }\
    static sir_inline VALUE CHECK_RES_FUNC_NAME(SIR_CONCAT(iname, vss))(VALUE res) { return fname(res); }\
    static sir_inline VALUE CHECK_RES_FUNC_NAME(SIR_CONCAT(iname, vsi))(VALUE res) { return fname(res); }\
    static sir_inline VALUE CHECK_RES_FUNC_NAME(SIR_CONCAT(iname, vis))(VALUE res) { return fname(res); }

static sir_inline int check_res_fixnum(VALUE res) { return !FIXNUM_P(res); }
static sir_inline int check_res_flonum(VALUE res) { return !FLONUM_P(res); }
static sir_inline int check_res_undef(VALUE res) { return res == Qundef; }
static sir_inline int check_res_nothing(VALUE res) { return FALSE; }

SIR_RES_CHECK_FUNCS2(sir_iplus, check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_iminus, check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_imult, check_res_fixnum)
SIR_RES_CHECK_FUNCS2(sir_idiv, check_res_fixnum)
SIR_RES_CHECK_FUNCS2(sir_imod, check_res_fixnum)
SIR_RES_CHECK_FUNCS2(sir_ior, check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_iand, check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_ieq, check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_ineq, check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_ilt, check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_igt, check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_ile, check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_ige, check_res_nothing)
#ifdef FLONUM_INSNS
SIR_RES_CHECK_FUNCS2(sir_fplus, check_res_flonum)
SIR_RES_CHECK_FUNCS2(sir_fminus, check_res_flonum)
SIR_RES_CHECK_FUNCS2(sir_fmult, check_res_flonum)
SIR_RES_CHECK_FUNCS2(sir_fdiv, check_res_flonum)
SIR_RES_CHECK_FUNCS2(sir_fmod, check_res_flonum)
SIR_RES_CHECK_FUNCS2(sir_feq, check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_fneq, check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_flt, check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_fgt, check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_fle, check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_fge, check_res_nothing)
#endif
SIR_RES_CHECK_FUNCS2(sir_iaref, check_res_undef)

#define CHECK_FUNC_NAME(insn_name) SIR_CONCAT(input_check_, insn_name)

#define SIR_CHECK_FUNCS2(iname, fname)			                                        		\
    static sir_inline VALUE CHECK_FUNC_NAME(SIR_CONCAT(iname, svv))(VALUE v1, VALUE v2) { return fname(v1, v2); }\
    static sir_inline VALUE CHECK_FUNC_NAME(SIR_CONCAT(iname, svi))(VALUE v1, VALUE v2) { return fname(v1, v2); }\
    static sir_inline VALUE CHECK_FUNC_NAME(SIR_CONCAT(iname, siv))(VALUE v1, VALUE v2) { return fname(v1, v2); }\
    static sir_inline VALUE CHECK_FUNC_NAME(SIR_CONCAT(iname, svs))(VALUE v1, VALUE v2) { return fname(v1, v2); }\
    static sir_inline VALUE CHECK_FUNC_NAME(SIR_CONCAT(iname, ssv))(VALUE v1, VALUE v2) { return fname(v1, v2); }\
    static sir_inline VALUE CHECK_FUNC_NAME(SIR_CONCAT(iname, sss))(VALUE v1, VALUE v2) { return fname(v1, v2); }\
    static sir_inline VALUE CHECK_FUNC_NAME(SIR_CONCAT(iname, ssi))(VALUE v1, VALUE v2) { return fname(v1, v2); }\
    static sir_inline VALUE CHECK_FUNC_NAME(SIR_CONCAT(iname, sis))(VALUE v1, VALUE v2) { return fname(v1, v2); }\
    static sir_inline VALUE CHECK_FUNC_NAME(SIR_CONCAT(iname, vvv))(VALUE v1, VALUE v2) { return fname(v1, v2); }\
    static sir_inline VALUE CHECK_FUNC_NAME(SIR_CONCAT(iname, vvi))(VALUE v1, VALUE v2) { return fname(v1, v2); }\
    static sir_inline VALUE CHECK_FUNC_NAME(SIR_CONCAT(iname, viv))(VALUE v1, VALUE v2) { return fname(v1, v2); }\
    static sir_inline VALUE CHECK_FUNC_NAME(SIR_CONCAT(iname, vvs))(VALUE v1, VALUE v2) { return fname(v1, v2); }\
    static sir_inline VALUE CHECK_FUNC_NAME(SIR_CONCAT(iname, vsv))(VALUE v1, VALUE v2) { return fname(v1, v2); }\
    static sir_inline VALUE CHECK_FUNC_NAME(SIR_CONCAT(iname, vss))(VALUE v1, VALUE v2) { return fname(v1, v2); }\
    static sir_inline VALUE CHECK_FUNC_NAME(SIR_CONCAT(iname, vsi))(VALUE v1, VALUE v2) { return fname(v1, v2); }\
    static sir_inline VALUE CHECK_FUNC_NAME(SIR_CONCAT(iname, vis))(VALUE v1, VALUE v2) { return fname(v1, v2); }

static sir_inline int check_zero(VALUE v1, VALUE v2) { return v2 == LONG2FIX(0); }
static sir_inline int check_nothing(VALUE v1, VALUE v2) { return FALSE; }

SIR_CHECK_FUNCS2(sir_iplus, check_nothing)
SIR_CHECK_FUNCS2(sir_iminus, check_nothing)
SIR_CHECK_FUNCS2(sir_imult, check_nothing)
SIR_CHECK_FUNCS2(sir_idiv, check_zero)
SIR_CHECK_FUNCS2(sir_imod, check_zero)
SIR_CHECK_FUNCS2(sir_ior, check_nothing)
SIR_CHECK_FUNCS2(sir_iand, check_nothing)
SIR_CHECK_FUNCS2(sir_ieq, check_nothing)
SIR_CHECK_FUNCS2(sir_ineq, check_nothing)
SIR_CHECK_FUNCS2(sir_ilt, check_nothing)
SIR_CHECK_FUNCS2(sir_igt, check_nothing)
SIR_CHECK_FUNCS2(sir_ile, check_nothing)
SIR_CHECK_FUNCS2(sir_ige, check_nothing)
#ifdef FLONUM_INSNS
SIR_CHECK_FUNCS2(sir_fplus, check_nothing)
SIR_CHECK_FUNCS2(sir_fminus, check_nothing)
SIR_CHECK_FUNCS2(sir_fmult, check_nothing)
SIR_CHECK_FUNCS2(sir_fdiv, check_nothing)
SIR_CHECK_FUNCS2(sir_fmod, check_nothing)
SIR_CHECK_FUNCS2(sir_feq, check_nothing)
SIR_CHECK_FUNCS2(sir_fneq, check_nothing)
SIR_CHECK_FUNCS2(sir_flt, check_nothing)
SIR_CHECK_FUNCS2(sir_fgt, check_nothing)
SIR_CHECK_FUNCS2(sir_fle, check_nothing)
SIR_CHECK_FUNCS2(sir_fge, check_nothing)
#endif
SIR_CHECK_FUNCS2(sir_iaref, check_nothing)

#define CHECK_INPUTS(iname, push_fname, v1, v2)                 \
    if (UNLIKELY(CHECK_FUNC_NAME(iname)(v1, v2))) {             \
        ABS_JUMP(rb_sir_get_safe_pc(GET_CFP(), reg_pc));	\
        END_INSN(iname); 					\
    }

static int
sir_fixnum_times_start(VALUE recv, VALUE *index) {
    if (!FIXNUM_P(recv)) return FALSE;
    *index = LONG2FIX(0);
    return TRUE;
}

static VALUE
sir_fixnum_times_cont(VALUE recv, VALUE index) {
    return index + 2; /* index is fixnum */
}

static int
sir_fixnum_times_body(VALUE recv, VALUE index, VALUE *arg) {
    *arg = index;
    return index < recv;
}

#include "internal/range.h"

static int
sir_range_each_start(VALUE recv, VALUE *index) {
    if (!rb_obj_is_kind_of(recv, rb_cRange)) return FALSE;
    *index = RANGE_BEG(recv);
    if (!FIXNUM_P(*index) || !FIXNUM_P(RANGE_END(recv))) return FALSE;
    return TRUE;
}

static VALUE
sir_range_each_cont(VALUE recv, VALUE index) {
    return index + 2;
}

static int
sir_range_each_body(VALUE recv, VALUE index, VALUE *arg) {
    *arg = index;
    if (RTEST(RANGE_EXCL(recv)) ? index < RANGE_END(recv) : index <= RANGE_END(recv))
        return TRUE;
    *arg = recv;
    return FALSE;
}

static int
sir_arr_each_start(VALUE recv, VALUE *index) {
    if (!RB_TYPE_P(recv, T_ARRAY)) return FALSE;
    *index = LONG2FIX(0);
    return TRUE;
}

static VALUE
sir_arr_each_cont(VALUE recv, VALUE index) {
    return index + 2;
}

static int
sir_arr_each_body(VALUE recv, VALUE index, VALUE *arg) {
    long i = FIX2LONG(index);

    if (i < RARRAY_LEN(recv)) {
        *arg = RARRAY_AREF(recv, i);
        return TRUE;
    }
    *arg = recv;
    return FALSE;
}

int
sir_get_iter_funcs(void *cfunc, int (**start_func)(VALUE, VALUE *), int (**arg_func)(VALUE, VALUE, VALUE *),
                   VALUE (**cont_func)(VALUE, VALUE), char *attr) {
    if (cfunc == sir_fixnum_ntimes_func) {
	*start_func = sir_fixnum_times_start;
        *arg_func = sir_fixnum_times_body;
        *cont_func = sir_fixnum_times_cont;
        *attr = ATTR_FIXNUM;
    } else if (cfunc == sir_range_each_func) {
	*start_func = sir_range_each_start;
        *arg_func = sir_range_each_body;
        *cont_func = sir_range_each_cont;
        *attr = ATTR_FIXNUM;
    } else if (cfunc == rb_ary_each) {
	*start_func = sir_arr_each_start;
        *arg_func = sir_arr_each_body;
        *cont_func = sir_arr_each_cont;
        *attr = ATTR_UNKNOWN;
    } else {
	return FALSE;
    }
    return TRUE;
}
