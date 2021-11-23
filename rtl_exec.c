/**********************************************************************

  rtl_exec.c - Code for executing RTL (Register Transfer Language) insns

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

/* Macros used to translate insn name into the corresponding function
   name.  */
#define RTL_CONCAT(a, b) a ## b
#define RTL_FUNC_NAME(insn_name) RTL_CONCAT(vm_, insn_name)

#define RTL_FUNCS(insn_name)                                           \
    static do_inline VALUE RTL_CONCAT(insn_name, svv)(VALUE v1, VALUE v2) { return insn_name(v1, v2); }\
    static do_inline VALUE RTL_CONCAT(insn_name, svi)(VALUE v1, VALUE v2) { return insn_name(v1, v2); }\
    static do_inline VALUE RTL_CONCAT(insn_name, svs)(VALUE v1, VALUE v2) { return insn_name(v1, v2); }\
    static do_inline VALUE RTL_CONCAT(insn_name, ssv)(VALUE v1, VALUE v2) { return insn_name(v1, v2); }\
    static do_inline VALUE RTL_CONCAT(insn_name, ssi)(VALUE v1, VALUE v2) { return insn_name(v1, v2); }\
    static do_inline VALUE RTL_CONCAT(insn_name, sss)(VALUE v1, VALUE v2) { return insn_name(v1, v2); }\
    static do_inline VALUE RTL_CONCAT(insn_name, vvv)(VALUE v1, VALUE v2) { return insn_name(v1, v2); }\
    static do_inline VALUE RTL_CONCAT(insn_name, vvi)(VALUE v1, VALUE v2) { return insn_name(v1, v2); }\
    static do_inline VALUE RTL_CONCAT(insn_name, vvs)(VALUE v1, VALUE v2) { return insn_name(v1, v2); }\
    static do_inline VALUE RTL_CONCAT(insn_name, vsv)(VALUE v1, VALUE v2) { return insn_name(v1, v2); }\
    static do_inline VALUE RTL_CONCAT(insn_name, vsi)(VALUE v1, VALUE v2) { return insn_name(v1, v2); }\
    static do_inline VALUE RTL_CONCAT(insn_name, vss)(VALUE v1, VALUE v2) { return insn_name(v1, v2); }

#define RTL_FUNCS3(insn_name)                                           \
    static do_inline void RTL_CONCAT(insn_name, svv)(VALUE v0, VALUE v1, VALUE v2) { insn_name(v0, v1, v2); }\
    static do_inline void RTL_CONCAT(insn_name, siv)(VALUE v0, VALUE v1, VALUE v2) { insn_name(v0, v1, v2); }\
    static do_inline void RTL_CONCAT(insn_name, svs)(VALUE v0, VALUE v1, VALUE v2) { insn_name(v0, v1, v2); }\
    static do_inline void RTL_CONCAT(insn_name, ssv)(VALUE v0, VALUE v1, VALUE v2) { insn_name(v0, v1, v2); }\
    static do_inline void RTL_CONCAT(insn_name, sis)(VALUE v0, VALUE v1, VALUE v2) { insn_name(v0, v1, v2); }\
    static do_inline void RTL_CONCAT(insn_name, sss)(VALUE v0, VALUE v1, VALUE v2) { insn_name(v0, v1, v2); }\
    static do_inline void RTL_CONCAT(insn_name, vvv)(VALUE v0, VALUE v1, VALUE v2) { insn_name(v0, v1, v2); }\
    static do_inline void RTL_CONCAT(insn_name, viv)(VALUE v0, VALUE v1, VALUE v2) { insn_name(v0, v1, v2); }\
    static do_inline void RTL_CONCAT(insn_name, vvs)(VALUE v0, VALUE v1, VALUE v2) { insn_name(v0, v1, v2); }\
    static do_inline void RTL_CONCAT(insn_name, vsv)(VALUE v0, VALUE v1, VALUE v2) { insn_name(v0, v1, v2); }\
    static do_inline void RTL_CONCAT(insn_name, vis)(VALUE v0, VALUE v1, VALUE v2) { insn_name(v0, v1, v2); }\
    static do_inline void RTL_CONCAT(insn_name, vss)(VALUE v0, VALUE v1, VALUE v2) { insn_name(v0, v1, v2); }

#define RTL_BR_FUNCS(insn_name, func_name)                              \
    static do_inline VALUE RTL_CONCAT(insn_name, ss)(VALUE v1, VALUE v2) { return func_name(v1, v2); }\
    static do_inline VALUE RTL_CONCAT(insn_name, vv)(VALUE v1, VALUE v2) { return func_name(v1, v2); }\
    static do_inline VALUE RTL_CONCAT(insn_name, vi)(VALUE v1, VALUE v2) { return func_name(v1, v2); }\
    static do_inline VALUE RTL_CONCAT(insn_name, vs)(VALUE v1, VALUE v2) { return func_name(v1, v2); }\
    static do_inline VALUE RTL_CONCAT(insn_name, sv)(VALUE v1, VALUE v2) { return func_name(v1, v2); }\
    static do_inline VALUE RTL_CONCAT(insn_name, si)(VALUE v1, VALUE v2) { return func_name(v1, v2); }

static do_inline VALUE vm_opt_iplus(VALUE v1, VALUE v2) { return rb_fix_plus_fix(v1, v2); }
static do_inline VALUE vm_opt_iminus(VALUE v1, VALUE v2) { return rb_fix_minus_fix(v1, v2); }
static do_inline VALUE vm_opt_imult(VALUE v1, VALUE v2) { return rb_fix_mul_fix(v1, v2); }
static do_inline VALUE vm_opt_idiv(VALUE v1, VALUE v2) { return rb_fix_div_fix(v1, v2); } /* ??? FIX2LONG(v2) == 0 */
static do_inline VALUE vm_opt_imod(VALUE v1, VALUE v2) { return rb_fix_mod_fix(v1, v2); } /* ??? FIX2LONG(v2) == 0 */
static do_inline VALUE vm_opt_ior(VALUE v1, VALUE v2) { return v1 | v2; }
static do_inline VALUE vm_opt_iand(VALUE v1, VALUE v2) { return (v1 & v2) | 1; }
static do_inline VALUE vm_opt_ieq(VALUE v1, VALUE v2) { return RBOOL(v1 == v2); }
static do_inline VALUE vm_opt_ineq(VALUE v1, VALUE v2) { return RBOOL(v1 != v2); }
static do_inline VALUE vm_opt_ilt(VALUE v1, VALUE v2) { return RBOOL((SIGNED_VALUE)v1 < (SIGNED_VALUE)v2); }
static do_inline VALUE vm_opt_igt(VALUE v1, VALUE v2) { return RBOOL((SIGNED_VALUE)v1 > (SIGNED_VALUE)v2); }
static do_inline VALUE vm_opt_ile(VALUE v1, VALUE v2) { return RBOOL((SIGNED_VALUE)v1 <= (SIGNED_VALUE)v2); }
static do_inline VALUE vm_opt_ige(VALUE v1, VALUE v2) { return RBOOL((SIGNED_VALUE)v1 > (SIGNED_VALUE)v2); }

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

static do_inline VALUE vm_opt_fplus(VALUE v1, VALUE v2) { return DBL2NUM(RFLOAT_VALUE(v1) + RFLOAT_VALUE(v2)); }
static do_inline VALUE vm_opt_fminus(VALUE v1, VALUE v2) { return DBL2NUM(RFLOAT_VALUE(v1) - RFLOAT_VALUE(v2)); }
static do_inline VALUE vm_opt_fmult(VALUE v1, VALUE v2) { return DBL2NUM(RFLOAT_VALUE(v1) * RFLOAT_VALUE(v2)); }
static do_inline VALUE vm_opt_fdiv(VALUE v1, VALUE v2) { return rb_flo_div_flo(v1, v2); }
static do_inline VALUE vm_opt_fmod(VALUE v1, VALUE v2) { return DBL2NUM(ruby_float_mod(RFLOAT_VALUE(v1), RFLOAT_VALUE(v2))); }
static do_inline VALUE vm_opt_feq(VALUE v1, VALUE v2) { return RBOOL(v1 == v2); }
static do_inline VALUE vm_opt_fneq(VALUE v1, VALUE v2) { return RBOOL(v1 != v2); }
static do_inline VALUE vm_opt_flt(VALUE v1, VALUE v2) { return RBOOL(RFLOAT_VALUE(v1) < RFLOAT_VALUE(v2)); }
static do_inline VALUE vm_opt_fgt(VALUE v1, VALUE v2) { return RBOOL(RFLOAT_VALUE(v1) > RFLOAT_VALUE(v2)); }
static do_inline VALUE vm_opt_fle(VALUE v1, VALUE v2) { return RBOOL(RFLOAT_VALUE(v1) <= RFLOAT_VALUE(v2)); }
static do_inline VALUE vm_opt_fge(VALUE v1, VALUE v2) { return RBOOL(RFLOAT_VALUE(v1) >= RFLOAT_VALUE(v2)); }

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

static do_inline VALUE vm_opt_iaref(VALUE v1, VALUE v2) { return rb_ary_entry_internal(v1, FIX2LONG(v2)); }
RTL_FUNCS(vm_opt_iaref)

static do_inline void vm_opt_iaset(VALUE arr, VALUE ind, VALUE set) { rb_ary_store(arr, FIX2LONG(ind), set); }
RTL_FUNCS3(vm_opt_iaset)

#define RTL_BR_FUNCS(insn_name, func_name)                              \
    static do_inline VALUE RTL_CONCAT(insn_name, ss)(VALUE v1, VALUE v2) { return func_name(v1, v2); }\
    static do_inline VALUE RTL_CONCAT(insn_name, vv)(VALUE v1, VALUE v2) { return func_name(v1, v2); }\
    static do_inline VALUE RTL_CONCAT(insn_name, vi)(VALUE v1, VALUE v2) { return func_name(v1, v2); }\
    static do_inline VALUE RTL_CONCAT(insn_name, vs)(VALUE v1, VALUE v2) { return func_name(v1, v2); }\
    static do_inline VALUE RTL_CONCAT(insn_name, sv)(VALUE v1, VALUE v2) { return func_name(v1, v2); }\
    static do_inline VALUE RTL_CONCAT(insn_name, si)(VALUE v1, VALUE v2) { return func_name(v1, v2); }

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
