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

#ifndef SIR_SPEC_CHECK
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

static sir_inline void
sir_stack_write(const VALUE *ep, int idx, VALUE val) {
    SIR_EXEC_ASSERT((ep[VM_ENV_DATA_INDEX_FLAGS] & VM_ENV_FLAG_WB_REQUIRED) == 0);
    VM_STACK_ENV_WRITE(ep, (int)idx, val);
}

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
SIR_FUNCS0(aref, vm_opt_aref)

#define SIR_SFUNCS(iname)										   \
    static sir_inline void SIR_CONCAT(iname, svv)(VALUE *res, VALUE v1, VALUE v2) { *res = iname(v1, v2); }\
    static sir_inline void SIR_CONCAT(iname, svi)(VALUE *res, VALUE v1, VALUE v2) { *res = iname(v1, v2); }\
    static sir_inline void SIR_CONCAT(iname, siv)(VALUE *res, VALUE v1, VALUE v2) { *res = iname(v1, v2); }\
    static sir_inline void SIR_CONCAT(iname, svs)(VALUE *res, VALUE v1, VALUE v2) { *res = iname(v1, v2); }\
    static sir_inline void SIR_CONCAT(iname, ssv)(VALUE *res, VALUE v1, VALUE v2) { *res = iname(v1, v2); }\
    static sir_inline void SIR_CONCAT(iname, ssi)(VALUE *res, VALUE v1, VALUE v2) { *res = iname(v1, v2); }\
    static sir_inline void SIR_CONCAT(iname, sis)(VALUE *res, VALUE v1, VALUE v2) { *res = iname(v1, v2); }\
    static sir_inline void SIR_CONCAT(iname, sss)(VALUE *res, VALUE v1, VALUE v2) { *res = iname(v1, v2); }

#define SIR_FUNCS(iname) SIR_SFUNCS(iname)								   \
    static sir_inline void SIR_CONCAT(iname, vvv)(VALUE *res, VALUE v1, VALUE v2) { *res = iname(v1, v2); }\
    static sir_inline void SIR_CONCAT(iname, vvi)(VALUE *res, VALUE v1, VALUE v2) { *res = iname(v1, v2); }\
    static sir_inline void SIR_CONCAT(iname, viv)(VALUE *res, VALUE v1, VALUE v2) { *res = iname(v1, v2); }\
    static sir_inline void SIR_CONCAT(iname, vvs)(VALUE *res, VALUE v1, VALUE v2) { *res = iname(v1, v2); }\
    static sir_inline void SIR_CONCAT(iname, vsv)(VALUE *res, VALUE v1, VALUE v2) { *res = iname(v1, v2); }\
    static sir_inline void SIR_CONCAT(iname, vsi)(VALUE *res, VALUE v1, VALUE v2) { *res = iname(v1, v2); }\
    static sir_inline void SIR_CONCAT(iname, vis)(VALUE *res, VALUE v1, VALUE v2) { *res = iname(v1, v2); }\
    static sir_inline void SIR_CONCAT(iname, vss)(VALUE *res, VALUE v1, VALUE v2) { *res = iname(v1, v2); }

#define pushvv(v1, v2) do {INC_SP(2); TOPN(1) = v1; TOPN(0) = v2;} while (0)
#define pushvi(v1, v2) do {INC_SP(2); TOPN(1) = v1; TOPN(0) = v2;} while (0)
#define pushiv(v1, v2) do {INC_SP(2); TOPN(1) = v1; TOPN(0) = v2;} while (0)
#define pushvs(v1, v2) do {INC_SP(1); TOPN(0) = v2; TOPN(1) = v1;} while (0)
#define pushsv(v1, v2) do {INC_SP(1); TOPN(0) = v2;} while (0)
#define pushsi(v1, v2) do {INC_SP(1); TOPN(0) = v2;} while (0)
#define pushis(v1, v2) do {INC_SP(1); TOPN(0) = v2; TOPN(1) = v1;} while (0)
#define pushss(v1, v2)

#define pushsvv(v1, v2, v3) do {INC_SP(2); TOPN(1) = v2; TOPN(0) = v3;} while (0)
#define pushsiv(v1, v2, v3) do {INC_SP(2); TOPN(1) = v2; TOPN(0) = v3;} while (0)
#define pushsvs(v1, v2, v3) do {INC_SP(1); TOPN(1) = v2; TOPN(0) = v3;} while (0)
#define pushssv(v1, v2, v3) do {INC_SP(1); TOPN(0) = v3;} while (0)
#define pushsis(v1, v2, v3) do {INC_SP(1); TOPN(1) = v2; TOPN(0) = v3;} while (0)
#define pushsss(v1, v2, v3)

#define pushvvv(v1, v2, v3) do {INC_SP(3); TOPN(2) = v1; TOPN(1) = v2; TOPN(0) = v3;} while (0)
#define pushviv(v1, v2, v3) do {INC_SP(3); TOPN(2) = v1; TOPN(1) = v2; TOPN(0) = v3;} while (0)
#define pushvvs(v1, v2, v3) do {INC_SP(2); TOPN(2) = v1; TOPN(1) = v2; TOPN(0) = v3;} while (0)
#define pushvsv(v1, v2, v3) do {INC_SP(2); TOPN(2) = v1; TOPN(1) = v2; TOPN(0) = v3;} while (0)
#define pushvis(v1, v2, v3) do {INC_SP(2); TOPN(2) = v1; TOPN(1) = v2; TOPN(0) = v3;} while (0)
#define pushvss(v1, v2, v3) do {INC_SP(1); TOPN(2) = v1; TOPN(1) = v2; TOPN(0) = v3;} while (0)

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
static sir_inline VALUE vm_sir_ige(VALUE v1, VALUE v2) { return RBOOL((SIGNED_VALUE)v1 >= (SIGNED_VALUE)v2); }

static sir_inline VALUE vm_sir_simult(VALUE v1, VALUE v2) { return rb_fix_mul_fix(v1, v2); }
static sir_inline VALUE vm_sir_sidiv(VALUE v1, VALUE v2) { return rb_fix_div_fix(v1, v2); }
static sir_inline VALUE vm_sir_simod(VALUE v1, VALUE v2) { return rb_fix_mod_fix(v1, v2); }
static sir_inline VALUE vm_sir_sior(VALUE v1, VALUE v2) { return v1 | v2; }
static sir_inline VALUE vm_sir_siand(VALUE v1, VALUE v2) { return (v1 & v2) | 1; }
static sir_inline VALUE vm_sir_sieq(VALUE v1, VALUE v2) { return RBOOL(v1 == v2); }
static sir_inline VALUE vm_sir_sineq(VALUE v1, VALUE v2) { return RBOOL(v1 != v2); }
static sir_inline VALUE vm_sir_silt(VALUE v1, VALUE v2) { return RBOOL((SIGNED_VALUE)v1 < (SIGNED_VALUE)v2); }
static sir_inline VALUE vm_sir_sigt(VALUE v1, VALUE v2) { return RBOOL((SIGNED_VALUE)v1 > (SIGNED_VALUE)v2); }
static sir_inline VALUE vm_sir_sile(VALUE v1, VALUE v2) { return RBOOL((SIGNED_VALUE)v1 <= (SIGNED_VALUE)v2); }
static sir_inline VALUE vm_sir_sige(VALUE v1, VALUE v2) { return RBOOL((SIGNED_VALUE)v1 >= (SIGNED_VALUE)v2); }

#if !__has_builtin(__builtin_add_overflow)
#define vm_sir_iplus(res, v1, v2)                                \
    {                                                            \
        long r = FIX2LONG(v1) + FIX2LONG(v2);                    \
        if (RB_FIXABLE(r)) {                                     \
            *res = RB_LONG2FIX(r);                               \
        } else {                                                 \
            goto safe_insn_restart;		                 \
        }                                                        \
    }
#else
#define vm_sir_iplus(res, v1, v2)                     			                \
    {											\
        long r;										\
        if (__builtin_expect(__builtin_add_overflow((long)v1, (long)v2-1, &r), 0))	\
            goto safe_insn_restart;                             			\
        *res = r;									\
    }
#endif

#define vm_sir_iplussss vm_sir_iplus
#define vm_sir_iplusssv vm_sir_iplus
#define vm_sir_iplussvs vm_sir_iplus
#define vm_sir_iplussvv vm_sir_iplus
#define vm_sir_iplusssi vm_sir_iplus
#define vm_sir_iplussis vm_sir_iplus
#define vm_sir_iplussvi vm_sir_iplus
#define vm_sir_iplussiv vm_sir_iplus
#define vm_sir_iplusvss vm_sir_iplus
#define vm_sir_iplusvsv vm_sir_iplus
#define vm_sir_iplusvvs vm_sir_iplus
#define vm_sir_iplusvvv vm_sir_iplus
#define vm_sir_iplusvsi vm_sir_iplus
#define vm_sir_iplusvis vm_sir_iplus
#define vm_sir_iplusvvi vm_sir_iplus
#define vm_sir_iplusviv vm_sir_iplus

#define vm_sir_siplussss vm_sir_iplus
#define vm_sir_siplusssv vm_sir_iplus
#define vm_sir_siplussvs vm_sir_iplus
#define vm_sir_siplussvv vm_sir_iplus
#define vm_sir_siplusssi vm_sir_iplus
#define vm_sir_siplussis vm_sir_iplus
#define vm_sir_siplussvi vm_sir_iplus
#define vm_sir_siplussiv vm_sir_iplus

#if !__has_builtin(__builtin_sub_overflow) || defined(MIRJIT)
#define vm_sir_iminus(res, v1, v2)                               \
    {                                                            \
        long r = FIX2LONG(v1) - FIX2LONG(v2);                    \
        if (RB_FIXABLE(r)) {                                     \
            *res = RB_LONG2FIX(r);                               \
        } else {                                                 \
            goto safe_insn_restart;		                 \
        }                                                        \
    }
#else
#define vm_sir_iminus(res, v1, v2)                              \
    {								\
        long r;			                                \
        if (__builtin_sub_overflow((long)v1, (long)v2-1, &r))	\
            goto safe_insn_restart;                             \
        *res = r;						\
    }
#endif

#define vm_sir_iminussss vm_sir_iminus
#define vm_sir_iminusssv vm_sir_iminus
#define vm_sir_iminussvs vm_sir_iminus
#define vm_sir_iminussvv vm_sir_iminus
#define vm_sir_iminusssi vm_sir_iminus
#define vm_sir_iminussis vm_sir_iminus
#define vm_sir_iminussvi vm_sir_iminus
#define vm_sir_iminussiv vm_sir_iminus
#define vm_sir_iminusvss vm_sir_iminus
#define vm_sir_iminusvsv vm_sir_iminus
#define vm_sir_iminusvvs vm_sir_iminus
#define vm_sir_iminusvvv vm_sir_iminus
#define vm_sir_iminusvsi vm_sir_iminus
#define vm_sir_iminusvis vm_sir_iminus
#define vm_sir_iminusvvi vm_sir_iminus
#define vm_sir_iminusviv vm_sir_iminus

#define vm_sir_siminussss vm_sir_iminus
#define vm_sir_siminusssv vm_sir_iminus
#define vm_sir_siminussvs vm_sir_iminus
#define vm_sir_siminussvv vm_sir_iminus
#define vm_sir_siminusssi vm_sir_iminus
#define vm_sir_siminussis vm_sir_iminus
#define vm_sir_siminussvi vm_sir_iminus
#define vm_sir_siminussiv vm_sir_iminus

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

SIR_FUNCS(vm_sir_simult)
SIR_FUNCS(vm_sir_sidiv)
SIR_FUNCS(vm_sir_simod)
SIR_FUNCS(vm_sir_sior)
SIR_FUNCS(vm_sir_siand)
SIR_FUNCS(vm_sir_sieq)
SIR_FUNCS(vm_sir_sineq)
SIR_FUNCS(vm_sir_silt)
SIR_FUNCS(vm_sir_sigt)
SIR_FUNCS(vm_sir_sile)
SIR_FUNCS(vm_sir_sige)

static sir_inline double vm_sir_float(VALUE v) {
    if (RB_LIKELY(FLONUM_P(v))) return RFLOAT_VALUE(v);
    SIR_EXEC_ASSERT(!SPECIAL_CONST_P(v) && RBASIC_CLASS(v) == rb_cFloat);
    return RFLOAT(v)->float_value;
}

static sir_inline VALUE vm_sir_fplus(VALUE v1, VALUE v2) { return DBL2NUM(vm_sir_float(v1) + vm_sir_float(v2)); }
static sir_inline VALUE vm_sir_fminus(VALUE v1, VALUE v2) { return DBL2NUM(vm_sir_float(v1) - vm_sir_float(v2)); }
static sir_inline VALUE vm_sir_fmult(VALUE v1, VALUE v2) { return DBL2NUM(vm_sir_float(v1) * vm_sir_float(v2)); }
static sir_inline VALUE vm_sir_fdiv(VALUE v1, VALUE v2) {
    return DBL2NUM(ruby_float_div(vm_sir_float(v1), vm_sir_float(v2)));
}
static sir_inline VALUE vm_sir_fmod(VALUE v1, VALUE v2) {
    return DBL2NUM(ruby_float_mod(vm_sir_float(v1), vm_sir_float(v2)));
}
static sir_inline VALUE vm_sir_feq(VALUE v1, VALUE v2) { return RBOOL(vm_sir_float(v1) == vm_sir_float(v2)); }
static sir_inline VALUE vm_sir_fneq(VALUE v1, VALUE v2) { return RBOOL(vm_sir_float(v1) != vm_sir_float(v2)); }
static sir_inline VALUE vm_sir_flt(VALUE v1, VALUE v2) { return RBOOL(vm_sir_float(v1) < vm_sir_float(v2)); }
static sir_inline VALUE vm_sir_fgt(VALUE v1, VALUE v2) { return RBOOL(vm_sir_float(v1) > vm_sir_float(v2)); }
static sir_inline VALUE vm_sir_fle(VALUE v1, VALUE v2) { return RBOOL(vm_sir_float(v1) <= vm_sir_float(v2)); }
static sir_inline VALUE vm_sir_fge(VALUE v1, VALUE v2) { return RBOOL(vm_sir_float(v1) >= vm_sir_float(v2)); }

static sir_inline VALUE vm_sir_fplusi(VALUE v1, DBL imm) { return DBL2NUM(vm_sir_float(v1) + imm); }
static sir_inline VALUE vm_sir_fminusi(VALUE v1, DBL imm) { return DBL2NUM(vm_sir_float(v1) - imm); }
static sir_inline VALUE vm_sir_fmulti(VALUE v1, DBL imm) { return DBL2NUM(vm_sir_float(v1) * imm); }
static sir_inline VALUE vm_sir_fdivi(VALUE v1, DBL imm) { return DBL2NUM(ruby_float_div(vm_sir_float(v1), imm)); }
static sir_inline VALUE vm_sir_fmodi(VALUE v1, DBL imm) { return DBL2NUM(ruby_float_mod(vm_sir_float(v1), imm)); }
static sir_inline VALUE vm_sir_feqi(VALUE v1, DBL imm) { return RBOOL(vm_sir_float(v1) == imm); }
static sir_inline VALUE vm_sir_fneqi(VALUE v1, DBL imm) { return RBOOL(vm_sir_float(v1) != imm); }
static sir_inline VALUE vm_sir_flti(VALUE v1, DBL imm) { return RBOOL(vm_sir_float(v1) < imm); }
static sir_inline VALUE vm_sir_fgti(VALUE v1, DBL imm) { return RBOOL(vm_sir_float(v1) > imm); }
static sir_inline VALUE vm_sir_flei(VALUE v1, DBL imm) { return RBOOL(vm_sir_float(v1) <= imm); }
static sir_inline VALUE vm_sir_fgei(VALUE v1, DBL imm) { return RBOOL(vm_sir_float(v1) >= imm); }

static sir_inline VALUE vm_sir_fplusri(DBL imm, VALUE v2) { return DBL2NUM(imm + vm_sir_float(v2)); }
static sir_inline VALUE vm_sir_fminusri(DBL imm, VALUE v2) { return DBL2NUM(imm - vm_sir_float(v2)); }
static sir_inline VALUE vm_sir_fmultri(DBL imm, VALUE v2) { return DBL2NUM(imm * vm_sir_float(v2)); }
static sir_inline VALUE vm_sir_fdivri(DBL imm, VALUE v2) { return DBL2NUM(ruby_float_div(imm, vm_sir_float(v2))); }
static sir_inline VALUE vm_sir_fmodri(DBL imm, VALUE v2) { return DBL2NUM(ruby_float_mod(imm, vm_sir_float(v2))); }
static sir_inline VALUE vm_sir_feqri(DBL imm, VALUE v2) { return RBOOL(imm == vm_sir_float(v2)); }
static sir_inline VALUE vm_sir_fneqri(DBL imm, VALUE v2) { return RBOOL(imm != vm_sir_float(v2)); }
static sir_inline VALUE vm_sir_fltri(DBL imm, VALUE v2) { return RBOOL(imm < vm_sir_float(v2)); }
static sir_inline VALUE vm_sir_fgtri(DBL imm, VALUE v2) { return RBOOL(imm > vm_sir_float(v2)); }
static sir_inline VALUE vm_sir_fleri(DBL imm, VALUE v2) { return RBOOL(imm <= vm_sir_float(v2)); }
static sir_inline VALUE vm_sir_fgeri(DBL imm, VALUE v2) { return RBOOL(imm >= vm_sir_float(v2)); }

#define SIR_SFUNCSF(iname, name, namei, nameri)                                        			   \
    static sir_inline void SIR_CONCAT(iname, svv)(VALUE *res, VALUE v1, VALUE v2) { *res = name(v1, v2); }  \
    static sir_inline void SIR_CONCAT(iname, svi)(VALUE *res, VALUE v1, DBL imm) { *res = namei(v1, imm); } \
    static sir_inline void SIR_CONCAT(iname, siv)(VALUE *res, DBL imm, VALUE v2) { *res = nameri(imm, v2); }\
    static sir_inline void SIR_CONCAT(iname, svs)(VALUE *res, VALUE v1, VALUE v2) { *res = name(v1, v2); }  \
    static sir_inline void SIR_CONCAT(iname, ssv)(VALUE *res, VALUE v1, VALUE v2) { *res = name(v1, v2); }  \
    static sir_inline void SIR_CONCAT(iname, ssi)(VALUE *res, VALUE v1, DBL imm) { *res = namei(v1, imm); } \
    static sir_inline void SIR_CONCAT(iname, sis)(VALUE *res, DBL imm, VALUE v2) { *res = nameri(imm, v2); }\
    static sir_inline void SIR_CONCAT(iname, sss)(VALUE *res, VALUE v1, VALUE v2) { *res = name(v1, v2); }

#define SIR_FUNCSF(name, namei, nameri) SIR_SFUNCSF(name, name, namei, nameri) \
    static sir_inline void SIR_CONCAT(name, vvv)(VALUE *res, VALUE v1, VALUE v2) { *res = name(v1, v2); }  \
    static sir_inline void SIR_CONCAT(name, vvi)(VALUE *res, VALUE v1, DBL imm) { *res = namei(v1, imm); } \
    static sir_inline void SIR_CONCAT(name, viv)(VALUE *res, DBL imm, VALUE v2) { *res = nameri(imm, v2); }\
    static sir_inline void SIR_CONCAT(name, vvs)(VALUE *res, VALUE v1, VALUE v2) { *res = name(v1, v2); }  \
    static sir_inline void SIR_CONCAT(name, vsv)(VALUE *res, VALUE v1, VALUE v2) { *res = name(v1, v2); }  \
    static sir_inline void SIR_CONCAT(name, vsi)(VALUE *res, VALUE v1, DBL imm) { *res = namei(v1, imm); } \
    static sir_inline void SIR_CONCAT(name, vis)(VALUE *res, DBL imm, VALUE v2) { *res = nameri(imm, v2); } \
    static sir_inline void SIR_CONCAT(name, vss)(VALUE *res, VALUE v1, VALUE v2) { *res = name(v1, v2); }

SIR_SFUNCSF(vm_sir_fplus, vm_sir_fplus, vm_sir_fplusi, vm_sir_fplusri)
SIR_SFUNCSF(vm_sir_fminus, vm_sir_fminus, vm_sir_fminusi, vm_sir_fminusri)
SIR_SFUNCSF(vm_sir_fmult, vm_sir_fmult, vm_sir_fmulti, vm_sir_fmultri)
SIR_SFUNCSF(vm_sir_fdiv, vm_sir_fdiv, vm_sir_fdivi, vm_sir_fdivri)
SIR_SFUNCSF(vm_sir_fmod, vm_sir_fmod, vm_sir_fmodi, vm_sir_fmodri)
SIR_FUNCSF(vm_sir_feq, vm_sir_feqi, vm_sir_feqri)
SIR_FUNCSF(vm_sir_fneq, vm_sir_fneqi, vm_sir_fneqri)
SIR_FUNCSF(vm_sir_flt, vm_sir_flti, vm_sir_fltri)
SIR_FUNCSF(vm_sir_fgt, vm_sir_fgti, vm_sir_fgtri)
SIR_FUNCSF(vm_sir_fle, vm_sir_flei, vm_sir_fleri)
SIR_FUNCSF(vm_sir_fge, vm_sir_fgei, vm_sir_fgeri)

SIR_SFUNCSF(vm_sir_sfplus, vm_sir_fplus, vm_sir_fplusi, vm_sir_fplusri)
SIR_SFUNCSF(vm_sir_sfminus, vm_sir_fminus, vm_sir_fminusi, vm_sir_fminusri)
SIR_SFUNCSF(vm_sir_sfmult, vm_sir_fmult, vm_sir_fmulti, vm_sir_fmultri)
SIR_SFUNCSF(vm_sir_sfdiv, vm_sir_fdiv, vm_sir_fdivi, vm_sir_fdivri)
SIR_SFUNCSF(vm_sir_sfmod, vm_sir_fmod, vm_sir_fmodi, vm_sir_fmodri)
SIR_SFUNCSF(vm_sir_sfeq, vm_sir_feq, vm_sir_feqi, vm_sir_feqri)
SIR_SFUNCSF(vm_sir_sfneq, vm_sir_fneq, vm_sir_fneqi, vm_sir_fneqri)
SIR_SFUNCSF(vm_sir_sflt, vm_sir_flt, vm_sir_flti, vm_sir_fltri)
SIR_SFUNCSF(vm_sir_sfgt, vm_sir_fgt, vm_sir_fgti, vm_sir_fgtri)
SIR_SFUNCSF(vm_sir_sfle, vm_sir_fle, vm_sir_flei, vm_sir_fleri)
SIR_SFUNCSF(vm_sir_sfge, vm_sir_fge, vm_sir_fgei, vm_sir_fgeri)

static sir_inline VALUE rb_sir_ary_entry(VALUE ary, long idx) {
    long len;
    const VALUE *ptr;
    if (RB_FL_ANY_RAW(ary, RARRAY_EMBED_FLAG)) {
        len = RARRAY_EMBED_LEN(ary);
        ptr = RARRAY(ary)->as.ary;
    } else {
        len = RARRAY(ary)->as.heap.len;
        ptr = RARRAY(ary)->as.heap.ptr;
    }
    if (idx < 0) {
	idx += len;
	if (RB_UNLIKELY(idx < 0)) return Qnil;
    } else if (RB_UNLIKELY(idx >= len)) {
        return Qnil;
    }
    return ptr[idx];
}

static sir_inline VALUE vm_sir_iaref(VALUE v1, VALUE v2) { return rb_sir_ary_entry(v1, FIX2LONG(v2)); }
static sir_inline VALUE vm_sir_siaref(VALUE v1, VALUE v2) { return rb_sir_ary_entry(v1, FIX2LONG(v2)); }
SIR_SFUNCS(vm_sir_iaref)
SIR_SFUNCS(vm_sir_siaref)

static sir_inline int rb_sir_ary_store(VALUE ary, long idx, VALUE val) {
    if (RB_UNLIKELY(FL_TEST_RAW((ary), ELTS_SHARED) != 0)) {
        rb_ary_cancel_sharing(ary);
    }
    long len;
    VALUE *ptr;
    if (RB_FL_ANY_RAW(ary, RARRAY_EMBED_FLAG)) {
        len = RARRAY_EMBED_LEN(ary);
        ptr = (VALUE *)RARRAY(ary)->as.ary;
    } else {
        len = RARRAY(ary)->as.heap.len;
        ptr = (VALUE *)RARRAY(ary)->as.heap.ptr;
    }
    if (idx < 0) {
	idx += len;
	if (RB_UNLIKELY(idx < 0)) return TRUE;
    } else if (RB_UNLIKELY(idx >= len)) {
        return TRUE;
    }
    RB_OBJ_WRITE(ary, &ptr[idx], val);
    return FALSE;
}

static sir_inline VALUE vm_sir_iaset(VALUE arr, VALUE ind, VALUE set) {
    if (SPECIAL_CONST_P(arr) || RBASIC_CLASS(arr) != rb_cArray) return Qundef;
    return rb_sir_ary_store(arr, FIX2LONG(ind), set);
}

SIR_FUNCS3(vm_opt_aset)

static sir_inline int vm_sir_ibeq(VALUE v1, VALUE v2) { return v1 == v2; }
static sir_inline int vm_sir_ibneq(VALUE v1, VALUE v2) { return v1 != v2; }
static sir_inline int vm_sir_iblt(VALUE v1, VALUE v2) { return (SIGNED_VALUE)v1 < (SIGNED_VALUE)v2; }
static sir_inline int vm_sir_ibgt(VALUE v1, VALUE v2) { return (SIGNED_VALUE)v1 > (SIGNED_VALUE)v2; }
static sir_inline int vm_sir_ible(VALUE v1, VALUE v2) { return (SIGNED_VALUE)v1 <= (SIGNED_VALUE)v2; }
static sir_inline int vm_sir_ibge(VALUE v1, VALUE v2) { return (SIGNED_VALUE)v1 >= (SIGNED_VALUE)v2; }

#define SIR_BR_FUNCS(iname)                                  \
    static sir_inline int SIR_CONCAT(iname, ss)(VALUE v1, VALUE v2) { return iname(v1, v2); }\
    static sir_inline int SIR_CONCAT(iname, vv)(VALUE v1, VALUE v2) { return iname(v1, v2); }\
    static sir_inline int SIR_CONCAT(iname, vi)(VALUE v1, VALUE v2) { return iname(v1, v2); }\
    static sir_inline int SIR_CONCAT(iname, vs)(VALUE v1, VALUE v2) { return iname(v1, v2); }\
    static sir_inline int SIR_CONCAT(iname, sv)(VALUE v1, VALUE v2) { return iname(v1, v2); }\
    static sir_inline int SIR_CONCAT(iname, si)(VALUE v1, VALUE v2) { return iname(v1, v2); }

SIR_BR_FUNCS(vm_sir_ibeq)
SIR_BR_FUNCS(vm_sir_ibneq)
SIR_BR_FUNCS(vm_sir_iblt)
SIR_BR_FUNCS(vm_sir_ibgt)
SIR_BR_FUNCS(vm_sir_ible)
SIR_BR_FUNCS(vm_sir_ibge)

#define CHECK_RES_FUNC_NAME(insn_name) SIR_CONCAT(insn_name, _result_check)

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

static sir_inline int sir_check_res_fixnum(VALUE res) { return !FIXNUM_P(res); }
static sir_inline int sir_check_res_flonum(VALUE res) { return !FLONUM_P(res); }
static sir_inline int sir_check_res_nothing(VALUE res) { return FALSE; }

SIR_RES_CHECK_FUNCS2(sir_iplus, sir_check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_iminus, sir_check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_imult, sir_check_res_fixnum)
SIR_RES_CHECK_FUNCS2(sir_idiv, sir_check_res_fixnum)
SIR_RES_CHECK_FUNCS2(sir_imod, sir_check_res_fixnum)
SIR_RES_CHECK_FUNCS2(sir_ior, sir_check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_iand, sir_check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_iaref, sir_check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_ieq, sir_check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_ineq, sir_check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_ilt, sir_check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_igt, sir_check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_ile, sir_check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_ige, sir_check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_fplus, sir_check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_fminus, sir_check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_fmult, sir_check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_fdiv, sir_check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_fmod, sir_check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_feq, sir_check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_fneq, sir_check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_flt, sir_check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_fgt, sir_check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_fle, sir_check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_fge, sir_check_res_nothing)

SIR_RES_CHECK_FUNCS2(sir_siplus, sir_check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_siminus, sir_check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_simult, sir_check_res_fixnum)
SIR_RES_CHECK_FUNCS2(sir_sidiv, sir_check_res_fixnum)
SIR_RES_CHECK_FUNCS2(sir_simod, sir_check_res_fixnum)
SIR_RES_CHECK_FUNCS2(sir_sior, sir_check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_siand, sir_check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_siaref, sir_check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_sieq, sir_check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_sineq, sir_check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_silt, sir_check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_sigt, sir_check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_sile, sir_check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_sige, sir_check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_sfplus, sir_check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_sfminus, sir_check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_sfmult, sir_check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_sfdiv, sir_check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_sfmod, sir_check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_sfeq, sir_check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_sfneq, sir_check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_sflt, sir_check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_sfgt, sir_check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_sfle, sir_check_res_nothing)
SIR_RES_CHECK_FUNCS2(sir_sfge, sir_check_res_nothing)

#define CHECK_FUNC_NAME(insn_name) SIR_CONCAT(sir_input_check_, insn_name)

#define SIR_CHECK_VFUNCS2(iname, fname, bop, flag)                                       			 \
    static sir_inline VALUE CHECK_FUNC_NAME(SIR_CONCAT(iname, svv))(VALUE v1, VALUE v2) { return fname(v1, v2, bop, flag); }\
    static sir_inline VALUE CHECK_FUNC_NAME(SIR_CONCAT(iname, svs))(VALUE v1, VALUE v2) { return fname(v1, v2, bop, flag); }\
    static sir_inline VALUE CHECK_FUNC_NAME(SIR_CONCAT(iname, ssv))(VALUE v1, VALUE v2) { return fname(v1, v2, bop, flag); }\
    static sir_inline VALUE CHECK_FUNC_NAME(SIR_CONCAT(iname, sss))(VALUE v1, VALUE v2) { return fname(v1, v2, bop, flag); }\
    static sir_inline VALUE CHECK_FUNC_NAME(SIR_CONCAT(iname, vvv))(VALUE v1, VALUE v2) { return fname(v1, v2, bop, flag); }\
    static sir_inline VALUE CHECK_FUNC_NAME(SIR_CONCAT(iname, vvs))(VALUE v1, VALUE v2) { return fname(v1, v2, bop, flag); }\
    static sir_inline VALUE CHECK_FUNC_NAME(SIR_CONCAT(iname, vsv))(VALUE v1, VALUE v2) { return fname(v1, v2, bop, flag); }\
    static sir_inline VALUE CHECK_FUNC_NAME(SIR_CONCAT(iname, vss))(VALUE v1, VALUE v2) { return fname(v1, v2, bop, flag); }

#define SIR_CHECK_FUNCS2(iname, fname, bop, flag) SIR_CHECK_VFUNCS2(iname, fname, bop, flag) \
    static sir_inline VALUE CHECK_FUNC_NAME(SIR_CONCAT(iname, svi))(VALUE v1, VALUE v2) { return fname(v1, v2, bop, flag); }\
    static sir_inline VALUE CHECK_FUNC_NAME(SIR_CONCAT(iname, siv))(VALUE v1, VALUE v2) { return fname(v1, v2, bop, flag); }\
    static sir_inline VALUE CHECK_FUNC_NAME(SIR_CONCAT(iname, ssi))(VALUE v1, VALUE v2) { return fname(v1, v2, bop, flag); }\
    static sir_inline VALUE CHECK_FUNC_NAME(SIR_CONCAT(iname, sis))(VALUE v1, VALUE v2) { return fname(v1, v2, bop, flag); }\
    static sir_inline VALUE CHECK_FUNC_NAME(SIR_CONCAT(iname, vvi))(VALUE v1, VALUE v2) { return fname(v1, v2, bop, flag); }\
    static sir_inline VALUE CHECK_FUNC_NAME(SIR_CONCAT(iname, viv))(VALUE v1, VALUE v2) { return fname(v1, v2, bop, flag); }\
    static sir_inline VALUE CHECK_FUNC_NAME(SIR_CONCAT(iname, vsi))(VALUE v1, VALUE v2) { return fname(v1, v2, bop, flag); }\
    static sir_inline VALUE CHECK_FUNC_NAME(SIR_CONCAT(iname, vis))(VALUE v1, VALUE v2) { return fname(v1, v2, bop, flag); }

#define SIR_CHECK_IFUNCS2(iname, fname, fname1, fname2, bop, flag) SIR_CHECK_VFUNCS2(iname, fname, bop, flag) \
    static sir_inline VALUE CHECK_FUNC_NAME(SIR_CONCAT(iname, svi))(VALUE v1, VALUE v2) { return fname1(v1, bop, flag); }\
    static sir_inline VALUE CHECK_FUNC_NAME(SIR_CONCAT(iname, siv))(VALUE v1, VALUE v2) { return fname2(v2, bop, flag); }\
    static sir_inline VALUE CHECK_FUNC_NAME(SIR_CONCAT(iname, ssi))(VALUE v1, VALUE v2) { return fname1(v1, bop, flag); }\
    static sir_inline VALUE CHECK_FUNC_NAME(SIR_CONCAT(iname, sis))(VALUE v1, VALUE v2) { return fname2(v2, bop, flag); }\
    static sir_inline VALUE CHECK_FUNC_NAME(SIR_CONCAT(iname, vvi))(VALUE v1, VALUE v2) { return fname1(v1, bop, flag); }\
    static sir_inline VALUE CHECK_FUNC_NAME(SIR_CONCAT(iname, viv))(VALUE v1, VALUE v2) { return fname2(v2, bop, flag); }\
    static sir_inline VALUE CHECK_FUNC_NAME(SIR_CONCAT(iname, vsi))(VALUE v1, VALUE v2) { return fname1(v1, bop, flag); }\
    static sir_inline VALUE CHECK_FUNC_NAME(SIR_CONCAT(iname, vis))(VALUE v1, VALUE v2) { return fname2(v2, bop, flag); }

#define SIR_CHECK_DFUNCS2(iname, fname, fname1, fname2, bop, flag) SIR_CHECK_VFUNCS2(iname, fname, bop, flag) \
    static sir_inline VALUE CHECK_FUNC_NAME(SIR_CONCAT(iname, svi))(VALUE v1, DBL v2) { return fname1(v1, bop, flag); }\
    static sir_inline VALUE CHECK_FUNC_NAME(SIR_CONCAT(iname, siv))(DBL v1, VALUE v2) { return fname2(v2, bop, flag); }\
    static sir_inline VALUE CHECK_FUNC_NAME(SIR_CONCAT(iname, ssi))(VALUE v1, DBL v2) { return fname1(v1, bop, flag); }\
    static sir_inline VALUE CHECK_FUNC_NAME(SIR_CONCAT(iname, sis))(DBL v1, VALUE v2) { return fname2(v2, bop, flag); }\
    static sir_inline VALUE CHECK_FUNC_NAME(SIR_CONCAT(iname, vvi))(VALUE v1, DBL v2) { return fname1(v1, bop, flag); }\
    static sir_inline VALUE CHECK_FUNC_NAME(SIR_CONCAT(iname, viv))(DBL v1, VALUE v2) { return fname2(v2, bop, flag); }\
    static sir_inline VALUE CHECK_FUNC_NAME(SIR_CONCAT(iname, vsi))(VALUE v1, DBL v2) { return fname1(v1, bop, flag); }\
    static sir_inline VALUE CHECK_FUNC_NAME(SIR_CONCAT(iname, vis))(DBL v1, VALUE v2) { return fname2(v2, bop, flag); }

static sir_inline int sir_check_fix_nothing(VALUE v1, VALUE v2, enum ruby_basic_operators bop, int klass_flag) {
    SIR_EXEC_ASSERT(FIXNUM_2_P(v1, v2) && BASIC_OP_UNREDEFINED_P(bop, klass_flag));
    return FALSE;
}
static sir_inline int sir_check_fixnum_zero(VALUE v1, VALUE v2, enum ruby_basic_operators bop, int klass_flag) {
    SIR_EXEC_ASSERT(FIXNUM_2_P(v1, v2) && BASIC_OP_UNREDEFINED_P(bop, klass_flag));
    return v2 == LONG2FIX(0);
}
static sir_inline int sir_check_nothing(VALUE v1, VALUE v2, enum ruby_basic_operators bop, int klass_flag) {
    SIR_EXEC_ASSERT(BASIC_OP_UNREDEFINED_P(bop, klass_flag));
    return FALSE;
}
static sir_inline int sir_check_nothing1(VALUE v, enum ruby_basic_operators bop, int klass_flag) {
    SIR_EXEC_ASSERT(BASIC_OP_UNREDEFINED_P(bop, klass_flag));
    return FALSE;
}
static sir_inline int sir_check_fix(VALUE v1, VALUE v2, enum ruby_basic_operators bop, int klass_flag) {
    SIR_EXEC_ASSERT(BASIC_OP_UNREDEFINED_P(bop, klass_flag));
    return !FIXNUM_2_P(v1, v2);
}
static sir_inline int sir_check_fix1(VALUE v, enum ruby_basic_operators bop, int klass_flag) {
    SIR_EXEC_ASSERT(BASIC_OP_UNREDEFINED_P(bop, klass_flag));
    return !FIXNUM_P(v);
}
static sir_inline int sir_check_nonzero_fix(VALUE v1, VALUE v2, enum ruby_basic_operators bop, int klass_flag) {
    SIR_EXEC_ASSERT(BASIC_OP_UNREDEFINED_P(bop, klass_flag));
    return !FIXNUM_2_P(v1, v2) || v2 == LONG2FIX(0);
}
static sir_inline int sir_check_nonzero_fix1(VALUE v, enum ruby_basic_operators bop, int klass_flag) {
    SIR_EXEC_ASSERT(BASIC_OP_UNREDEFINED_P(bop, klass_flag));
    return !FIXNUM_P(v) || v == LONG2FIX(0);
}
static sir_inline int sir_fp_p(VALUE v) { return FLONUM_P(v) || (!SPECIAL_CONST_P(v) && RBASIC_CLASS(v) == rb_cFloat);}
static sir_inline int sir_check_fp(VALUE v1, VALUE v2, enum ruby_basic_operators bop, int klass_flag) {
    SIR_EXEC_ASSERT(BASIC_OP_UNREDEFINED_P(bop, klass_flag));
    return !sir_fp_p(v1) || !sir_fp_p(v2);
}
static sir_inline int sir_check_fp1(VALUE v, enum ruby_basic_operators bop, int klass_flag) {
    SIR_EXEC_ASSERT(BASIC_OP_UNREDEFINED_P(bop, klass_flag));
    return !sir_fp_p(v);
}
static sir_inline int sir_check_aref(VALUE v1, VALUE v2, enum ruby_basic_operators bop, int klass_flag) {
    SIR_EXEC_ASSERT(!SPECIAL_CONST_P(v1) && RBASIC_CLASS(v1) == rb_cArray
                    && FIXNUM_P(v2) && BASIC_OP_UNREDEFINED_P(bop, klass_flag));
    return FALSE;
}
static sir_inline int sir_check_ary(VALUE ary) {
    return SPECIAL_CONST_P(ary) || RBASIC_CLASS(ary) != rb_cArray;
}
static sir_inline int sir_check_aref1(VALUE v, enum ruby_basic_operators bop, int klass_flag) {
    SIR_EXEC_ASSERT(BASIC_OP_UNREDEFINED_P(bop, klass_flag));
    return sir_check_ary(v);;
}
static sir_inline int sir_check_saref(VALUE v1, VALUE v2, enum ruby_basic_operators bop, int klass_flag) {
    SIR_EXEC_ASSERT(BASIC_OP_UNREDEFINED_P(bop, klass_flag));
    return !FIXNUM_P(v2) || sir_check_ary(v1);
}
static sir_inline int sir_check_aset(VALUE ary, VALUE ind, enum ruby_basic_operators bop, int klass_flag) {
    SIR_EXEC_ASSERT(FIXNUM_P(ind) && !SPECIAL_CONST_P(ary) && RBASIC_CLASS(ary) == rb_cArray
                    && BASIC_OP_UNREDEFINED_P(bop, klass_flag));
    return RB_OBJ_FROZEN(ary);
}
static sir_inline int sir_check_aset1(VALUE v, enum ruby_basic_operators bop, int klass_flag) {
    SIR_EXEC_ASSERT(BASIC_OP_UNREDEFINED_P(bop, klass_flag));
    return sir_check_ary(v) || RB_OBJ_FROZEN(v);
}
static sir_inline int sir_check_saset(VALUE v1, VALUE v2, enum ruby_basic_operators bop, int klass_flag) {
    SIR_EXEC_ASSERT(BASIC_OP_UNREDEFINED_P(bop, klass_flag));
    return !FIXNUM_P(v2) || sir_check_ary(v1) || RB_OBJ_FROZEN(v1);
}

SIR_CHECK_FUNCS2(sir_iplus, sir_check_fix_nothing, BOP_PLUS, INTEGER_REDEFINED_OP_FLAG)
SIR_CHECK_FUNCS2(sir_iminus, sir_check_fix_nothing, BOP_MINUS, INTEGER_REDEFINED_OP_FLAG)
SIR_CHECK_FUNCS2(sir_imult, sir_check_fix_nothing, BOP_MULT, INTEGER_REDEFINED_OP_FLAG)
SIR_CHECK_FUNCS2(sir_idiv, sir_check_fixnum_zero, BOP_DIV, INTEGER_REDEFINED_OP_FLAG)
SIR_CHECK_FUNCS2(sir_imod, sir_check_fixnum_zero, BOP_MOD, INTEGER_REDEFINED_OP_FLAG)
SIR_CHECK_FUNCS2(sir_ior, sir_check_fix_nothing, BOP_OR, INTEGER_REDEFINED_OP_FLAG)
SIR_CHECK_FUNCS2(sir_iand, sir_check_fix_nothing, BOP_AND, INTEGER_REDEFINED_OP_FLAG)
SIR_CHECK_FUNCS2(sir_iaref, sir_check_aref, BOP_AREF, ARRAY_REDEFINED_OP_FLAG)
SIR_CHECK_FUNCS2(sir_iaset, sir_check_aset, BOP_ASET, ARRAY_REDEFINED_OP_FLAG)
SIR_CHECK_FUNCS2(sir_ieq, sir_check_fix_nothing, BOP_EQ, INTEGER_REDEFINED_OP_FLAG)
SIR_CHECK_FUNCS2(sir_ineq, sir_check_fix_nothing, BOP_NEQ, INTEGER_REDEFINED_OP_FLAG)
SIR_CHECK_FUNCS2(sir_ilt, sir_check_fix_nothing, BOP_LT, INTEGER_REDEFINED_OP_FLAG)
SIR_CHECK_FUNCS2(sir_igt, sir_check_fix_nothing, BOP_GT, INTEGER_REDEFINED_OP_FLAG)
SIR_CHECK_FUNCS2(sir_ile, sir_check_fix_nothing, BOP_LE, INTEGER_REDEFINED_OP_FLAG)
SIR_CHECK_FUNCS2(sir_ige, sir_check_fix_nothing, BOP_GE, INTEGER_REDEFINED_OP_FLAG)
SIR_CHECK_DFUNCS2(sir_fplus, sir_check_nothing, sir_check_nothing1, sir_check_nothing1, BOP_PLUS, FLOAT_REDEFINED_OP_FLAG)
SIR_CHECK_DFUNCS2(sir_fminus, sir_check_nothing, sir_check_nothing1, sir_check_nothing1, BOP_MINUS, FLOAT_REDEFINED_OP_FLAG)
SIR_CHECK_DFUNCS2(sir_fmult, sir_check_nothing, sir_check_nothing1, sir_check_nothing1, BOP_MULT, FLOAT_REDEFINED_OP_FLAG)
SIR_CHECK_DFUNCS2(sir_fdiv, sir_check_nothing, sir_check_nothing1, sir_check_nothing1, BOP_DIV, FLOAT_REDEFINED_OP_FLAG)
SIR_CHECK_DFUNCS2(sir_fmod, sir_check_nothing, sir_check_nothing1, sir_check_nothing1, BOP_MOD, FLOAT_REDEFINED_OP_FLAG)
SIR_CHECK_DFUNCS2(sir_feq, sir_check_nothing, sir_check_nothing1, sir_check_nothing1, BOP_EQ, FLOAT_REDEFINED_OP_FLAG)
SIR_CHECK_DFUNCS2(sir_fneq, sir_check_nothing, sir_check_nothing1, sir_check_nothing1, BOP_NEQ, FLOAT_REDEFINED_OP_FLAG)
SIR_CHECK_DFUNCS2(sir_flt, sir_check_nothing, sir_check_nothing1, sir_check_nothing1, BOP_LT, FLOAT_REDEFINED_OP_FLAG)
SIR_CHECK_DFUNCS2(sir_fgt, sir_check_nothing, sir_check_nothing1, sir_check_nothing1, BOP_GT, FLOAT_REDEFINED_OP_FLAG)
SIR_CHECK_DFUNCS2(sir_fle, sir_check_nothing, sir_check_nothing1, sir_check_nothing1, BOP_LE, FLOAT_REDEFINED_OP_FLAG)
SIR_CHECK_DFUNCS2(sir_fge, sir_check_nothing, sir_check_nothing1, sir_check_nothing1, BOP_GE, FLOAT_REDEFINED_OP_FLAG)

SIR_CHECK_IFUNCS2(sir_siplus, sir_check_fix, sir_check_fix1, sir_check_fix1, BOP_PLUS, INTEGER_REDEFINED_OP_FLAG)
SIR_CHECK_IFUNCS2(sir_siminus, sir_check_fix, sir_check_fix1, sir_check_fix1, BOP_MINUS, INTEGER_REDEFINED_OP_FLAG)
SIR_CHECK_IFUNCS2(sir_simult, sir_check_fix, sir_check_fix1, sir_check_fix1, BOP_MULT, INTEGER_REDEFINED_OP_FLAG)
SIR_CHECK_IFUNCS2(sir_sidiv, sir_check_nonzero_fix, sir_check_fix1, sir_check_nonzero_fix1, BOP_DIV, INTEGER_REDEFINED_OP_FLAG)
SIR_CHECK_IFUNCS2(sir_simod, sir_check_nonzero_fix, sir_check_fix1, sir_check_nonzero_fix1, BOP_MOD, INTEGER_REDEFINED_OP_FLAG)
SIR_CHECK_IFUNCS2(sir_sior, sir_check_fix, sir_check_fix1, sir_check_fix1, BOP_OR, INTEGER_REDEFINED_OP_FLAG)
SIR_CHECK_IFUNCS2(sir_siand, sir_check_fix, sir_check_fix1, sir_check_fix1, BOP_AND, INTEGER_REDEFINED_OP_FLAG)
SIR_CHECK_IFUNCS2(sir_siaref, sir_check_saref, sir_check_aref1, sir_check_fix1, BOP_AREF, ARRAY_REDEFINED_OP_FLAG)
SIR_CHECK_IFUNCS2(sir_siaset, sir_check_saset, sir_check_aset1, sir_check_fix1, BOP_ASET, ARRAY_REDEFINED_OP_FLAG)
SIR_CHECK_IFUNCS2(sir_sieq, sir_check_fix, sir_check_fix1, sir_check_fix1, BOP_EQ, INTEGER_REDEFINED_OP_FLAG)
SIR_CHECK_IFUNCS2(sir_sineq, sir_check_fix, sir_check_fix1, sir_check_fix1, BOP_NEQ, INTEGER_REDEFINED_OP_FLAG)
SIR_CHECK_IFUNCS2(sir_silt, sir_check_fix, sir_check_fix1, sir_check_fix1, BOP_LT, INTEGER_REDEFINED_OP_FLAG)
SIR_CHECK_IFUNCS2(sir_sigt, sir_check_fix, sir_check_fix1, sir_check_fix1, BOP_GT, INTEGER_REDEFINED_OP_FLAG)
SIR_CHECK_IFUNCS2(sir_sile, sir_check_fix, sir_check_fix1, sir_check_fix1, BOP_LE, INTEGER_REDEFINED_OP_FLAG)
SIR_CHECK_IFUNCS2(sir_sige, sir_check_fix, sir_check_fix1, sir_check_fix1, BOP_GE, INTEGER_REDEFINED_OP_FLAG)

SIR_CHECK_DFUNCS2(sir_sfplus, sir_check_fp, sir_check_fp1, sir_check_fp1, BOP_PLUS, FLOAT_REDEFINED_OP_FLAG)
SIR_CHECK_DFUNCS2(sir_sfminus, sir_check_fp, sir_check_fp1, sir_check_fp1, BOP_MINUS, FLOAT_REDEFINED_OP_FLAG)
SIR_CHECK_DFUNCS2(sir_sfmult, sir_check_fp, sir_check_fp1, sir_check_fp1, BOP_MULT, FLOAT_REDEFINED_OP_FLAG)
SIR_CHECK_DFUNCS2(sir_sfdiv, sir_check_fp, sir_check_fp1, sir_check_fp1, BOP_DIV, FLOAT_REDEFINED_OP_FLAG)
SIR_CHECK_DFUNCS2(sir_sfmod, sir_check_fp, sir_check_fp1, sir_check_fp1, BOP_MOD, FLOAT_REDEFINED_OP_FLAG)
SIR_CHECK_DFUNCS2(sir_sfeq, sir_check_fp, sir_check_fp1, sir_check_fp1, BOP_EQ, FLOAT_REDEFINED_OP_FLAG)
SIR_CHECK_DFUNCS2(sir_sfneq, sir_check_fp, sir_check_fp1, sir_check_fp1, BOP_NEQ, FLOAT_REDEFINED_OP_FLAG)
SIR_CHECK_DFUNCS2(sir_sflt, sir_check_fp, sir_check_fp1, sir_check_fp1, BOP_LT, FLOAT_REDEFINED_OP_FLAG)
SIR_CHECK_DFUNCS2(sir_sfgt, sir_check_fp, sir_check_fp1, sir_check_fp1, BOP_GT, FLOAT_REDEFINED_OP_FLAG)
SIR_CHECK_DFUNCS2(sir_sfle, sir_check_fp, sir_check_fp1, sir_check_fp1, BOP_LE, FLOAT_REDEFINED_OP_FLAG)
SIR_CHECK_DFUNCS2(sir_sfge, sir_check_fp, sir_check_fp1, sir_check_fp1, BOP_GE, FLOAT_REDEFINED_OP_FLAG)

#define CHECK_INPUTS(iname, v1, v2)		                \
    if (UNLIKELY(CHECK_FUNC_NAME(iname)(v1, v2))) {             \
        ABS_SAFE_JUMP(GET_CFP(), reg_pc);			\
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
    long lim = FIX2LONG(RANGE_END(recv));
    if (!RTEST(RANGE_EXCL((recv)))) lim++;
    *arg = index;
    if (FIX2LONG(index) < lim)
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

static SIR_UNUSED void
mirjit_push_frame(rb_execution_context_t *ec, const rb_iseq_t *iseq, VALUE type, VALUE self, VALUE specval,
                  VALUE cref_or_me, const VALUE *pc, VALUE *sp, int local_size, int stack_max)
{
    rb_control_frame_t *const cfp = RUBY_VM_NEXT_CONTROL_FRAME(ec->cfp);

    /* check stack overflow */
//    CHECK_VM_STACK_OVERFLOW0(cfp, sp, local_size + stack_max);

    /* setup vm value stack */
    /* initialize local variables */
    for (int i=0; i < local_size; i++) *sp++ = Qnil;

    /* setup ep with managing data */
    *sp++ = cref_or_me; /* ep[-2] / Qnil or T_IMEMO(cref) or T_IMEMO(ment) */
    *sp++ = specval     /* ep[-1] / block handler or prev env ptr */;
    *sp++ = type;       /* ep[-0] / ENV_FLAGS */

    /* setup new frame */
    cfp->pc = pc;
    cfp->sp = sp;
    cfp->iseq = iseq;
    cfp->self = self;
    cfp->block_code = NULL;
    cfp->__bp__ = sp;
    cfp->jit_return = NULL;

    ec->cfp = cfp;
}

void
mirjit_opt_send_without_block(rb_execution_context_t *ec, rb_control_frame_t *cfp, VALUE *pc, CALL_DATA cd) {
    // ???
}
