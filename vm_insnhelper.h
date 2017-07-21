/**********************************************************************

  insnhelper.h - helper macros to implement each instructions

  $Author$
  created at: 04/01/01 15:50:34 JST

  Copyright (C) 2004-2007 Koichi Sasada

**********************************************************************/

#ifndef RUBY_INSNHELPER_H
#define RUBY_INSNHELPER_H

extern VALUE ruby_vm_const_missing_count;

#if VM_COLLECT_USAGE_DETAILS
#define COLLECT_USAGE_INSN(insn)           vm_collect_usage_insn(insn)
#define COLLECT_USAGE_OPERAND(insn, n, op) vm_collect_usage_operand((insn), (n), ((VALUE)(op)))

#define COLLECT_USAGE_REGISTER(reg, s)     vm_collect_usage_register((reg), (s))
#else
#if MJIT_INSN_STATISTICS
#define COLLECT_USAGE_INSN(insn)	   byte_code_insns_num++
#else
#define COLLECT_USAGE_INSN(insn)		/* none */
#endif
#define COLLECT_USAGE_OPERAND(insn, n, op)	/* none */
#define COLLECT_USAGE_REGISTER(reg, s)		/* none */
#endif

/**********************************************************/
/* deal with stack                                        */
/**********************************************************/

#define PUSH(x) (SET_SV(x), INC_SP(1))
#define TOPN(n) (*(GET_SP()-(n)-1))
#define POPN(n) (DEC_SP(n))
#define POP()   (DEC_SP(1))
#define STACK_ADDR_FROM_TOP(n) (GET_SP()-(n))

#define GET_TOS()  (tos)	/* dummy */

/**********************************************************/
/* deal with registers                                    */
/**********************************************************/

#define VM_REG_CFP (reg_cfp)
#define VM_REG_PC  (VM_REG_CFP->pc)
#define VM_REG_SP  (VM_REG_CFP->sp)
#define VM_REG_BP  (VM_REG_CFP->bp)
#define VM_REG_EP  (VM_REG_CFP->ep)

#define RESTORE_REGS() do { \
    VM_REG_CFP = th->cfp; \
} while (0)

#define REG_A   reg_a
#define REG_B   reg_b

enum vm_regan_regtype {
    VM_REGAN_PC = 0,
    VM_REGAN_SP = 1,
    VM_REGAN_EP = 2,
    VM_REGAN_CFP = 3,
    VM_REGAN_SELF = 4,
    VM_REGAN_ISEQ = 5,
};
enum vm_regan_acttype {
    VM_REGAN_ACT_GET = 0,
    VM_REGAN_ACT_SET = 1,
};

#if VM_COLLECT_USAGE_DETAILS
#define COLLECT_USAGE_REGISTER_HELPER(a, b, v) \
  (COLLECT_USAGE_REGISTER((VM_REGAN_##a), (VM_REGAN_ACT_##b)), (v))
#else
#define COLLECT_USAGE_REGISTER_HELPER(a, b, v) (v)
#endif

/* PC */
#define GET_PC()           (COLLECT_USAGE_REGISTER_HELPER(PC, GET, VM_REG_PC))
#define SET_PC(x)          (VM_REG_PC = (COLLECT_USAGE_REGISTER_HELPER(PC, SET, (x))))
#define GET_CURRENT_INSN() (*GET_PC())
#define GET_OPERAND(n)     (GET_PC()[(n)])
#define ADD_PC(n)          (SET_PC(VM_REG_PC + (n)))
#define JUMP(dst)          (VM_REG_PC += (dst))

/* frame pointer, environment pointer */
#define GET_CFP()  (COLLECT_USAGE_REGISTER_HELPER(CFP, GET, VM_REG_CFP))
#define GET_EP()   (COLLECT_USAGE_REGISTER_HELPER(EP, GET, VM_REG_EP))
#define SET_EP(x)  (VM_REG_EP = (COLLECT_USAGE_REGISTER_HELPER(EP, SET, (x))))
#define GET_LEP()  (VM_EP_LEP(GET_EP()))
#define GET_BP()   (COLLECT_USAGE_REGISTER_HELPER(SP, GET, REG_BP))

/* SP */
#define GET_SP()   (COLLECT_USAGE_REGISTER_HELPER(SP, GET, VM_REG_SP))
#define SET_SP(x)  (VM_REG_SP  = (COLLECT_USAGE_REGISTER_HELPER(SP, SET, (x))))
#define INC_SP(x)  (VM_REG_SP += (COLLECT_USAGE_REGISTER_HELPER(SP, SET, (x))))
#define DEC_SP(x)  (VM_REG_SP -= (COLLECT_USAGE_REGISTER_HELPER(SP, SET, (x))))
#define SET_SV(x)  (*GET_SP() = (x))
  /* set current stack value as x */

#define GET_SP_COUNT() (VM_REG_SP - th->stack)

/* instruction sequence C struct */
#define GET_ISEQ() (GET_CFP()->iseq)

/**********************************************************/
/* deal with variables                                    */
/**********************************************************/

#define GET_PREV_EP(ep)                ((VALUE *)((ep)[VM_ENV_DATA_INDEX_SPECVAL] & ~0x03))

#define GET_GLOBAL(entry)       rb_gvar_get((struct rb_global_entry*)(entry))
#define SET_GLOBAL(entry, val)  rb_gvar_set((struct rb_global_entry*)(entry), (val))

#define GET_CONST_INLINE_CACHE(dst) ((IC) * (GET_PC() + (dst) + 2))

/**********************************************************/
/* deal with values                                       */
/**********************************************************/

#define GET_SELF() (COLLECT_USAGE_REGISTER_HELPER(SELF, GET, GET_CFP()->self))

/**********************************************************/
/* deal with control flow 2: method/iterator              */
/**********************************************************/

#define CALL_METHOD(calling, cd) do { \
    VALUE v = ((cd)->call_cache.call)(th, GET_CFP(), (calling), &(cd)->call_info, &(cd)->call_cache); \
    if (v == Qundef && (v = mjit_execute_iseq(th)) == Qundef) {	\
	RESTORE_REGS(); \
	set_default_sp(reg_cfp, reg_cfp->bp);		\
	NEXT_INSN(); \
    } \
    else { \
        set_default_sp(reg_cfp, reg_cfp->bp);		\
        *get_temp_addr(reg_cfp, (cd)->call_start) = v;	\
    } \
} while (0)

/* set fastpath when cached method is *NOT* protected
 * because inline method cache does not care about receiver.
 */

#ifndef OPT_CALL_FASTPATH
#define OPT_CALL_FASTPATH 1
#endif

#if OPT_CALL_FASTPATH
#define CI_SET_FASTPATH(cc, func, enabled) do { \
    if (LIKELY(enabled)) ((cc)->call = (func)); \
} while (0)
#else
#define CI_SET_FASTPATH(ci, func, enabled) /* do nothing */
#endif

#define GET_BLOCK_HANDLER() (GET_LEP()[VM_ENV_DATA_INDEX_SPECVAL])

/**********************************************************/
/* deal with control flow 3: exception                    */
/**********************************************************/


/**********************************************************/
/* others                                                 */
/**********************************************************/

/* optimize insn */
#define FIXNUM_2_P(a, b) ((a) & (b) & 1)
#if USE_FLONUM
#define FLONUM_2_P(a, b) (((((a)^2) | ((b)^2)) & 3) == 0) /* (FLONUM_P(a) && FLONUM_P(b)) */
#else
#define FLONUM_2_P(a, b) 0
#endif

#ifndef USE_IC_FOR_SPECIALIZED_METHOD
#define USE_IC_FOR_SPECIALIZED_METHOD 1
#endif

#define VM_ATOMIC_SET(var, val) ATOMIC_SET(var, val)
#define VM_ATOMIC_INC(var) ATOMIC_INC(var)

#define NEXT_CLASS_SERIAL() (VM_ATOMIC_INC(ruby_vm_class_serial), ruby_vm_class_serial)
#define GET_GLOBAL_METHOD_STATE() (ruby_vm_global_method_state)
#define INC_GLOBAL_METHOD_STATE() (VM_ATOMIC_INC(ruby_vm_global_method_state))
#define GET_GLOBAL_CONSTANT_STATE() (ruby_vm_global_constant_state)
#define INC_GLOBAL_CONSTANT_STATE() (VM_ATOMIC_INC(ruby_vm_global_constant_state))

extern VALUE make_no_method_exception(VALUE exc, VALUE format, VALUE obj,
				      int argc, const VALUE *argv, int priv);

static do_inline struct vm_throw_data *
THROW_DATA_NEW(VALUE val, const rb_control_frame_t *cf, VALUE st)
{
    return (struct vm_throw_data *)rb_imemo_new(imemo_throw_data, val, (VALUE)cf, st, 0);
}

static do_inline void
THROW_DATA_CATCH_FRAME_SET(struct vm_throw_data *obj, const rb_control_frame_t *cfp)
{
    obj->catch_frame = cfp;
}

static do_inline void
THROW_DATA_STATE_SET(struct vm_throw_data *obj, int st)
{
    obj->throw_state = (VALUE)st;
}

static do_inline VALUE
THROW_DATA_VAL(const struct vm_throw_data *obj)
{
    return obj->throw_obj;
}

static do_inline const rb_control_frame_t *
THROW_DATA_CATCH_FRAME(const struct vm_throw_data *obj)
{
    return obj->catch_frame;
}

static do_inline int
THROW_DATA_STATE(const struct vm_throw_data *obj)
{
    return (int)obj->throw_state;
}

extern rb_cref_t *rb_vm_get_cref(const VALUE *ep);

extern const rb_cref_t *vm_get_const_key_cref(const VALUE *ep);

extern VALUE lep_svar_get(rb_thread_t *th, const VALUE *lep, rb_num_t key);
extern void lep_svar_set(rb_thread_t *th, const VALUE *lep, rb_num_t key, VALUE val);

extern VALUE vm_defined(rb_thread_t *th, rb_control_frame_t *reg_cfp, rb_num_t op_type, VALUE obj, VALUE needstr, VALUE v);
extern VALUE vm_invoke_block(rb_thread_t *th, rb_control_frame_t *reg_cfp, struct rb_calling_info *calling, const struct rb_call_info *ci);
extern void vm_search_super_method(rb_thread_t *th, rb_control_frame_t *reg_cfp,
				   struct rb_calling_info *calling, struct rb_call_info *ci, struct rb_call_cache *cc);

extern VALUE check_match(VALUE pattern, VALUE target, enum vm_check_match_type type);
extern VALUE vm_throw(rb_thread_t *th, rb_control_frame_t *reg_cfp, rb_num_t throw_state, VALUE throwobj);
extern VALUE vm_search_const_defined_class(const VALUE cbase, ID id);

#if VM_CHECK_MODE > 0
extern void vm_check_frame(VALUE type, VALUE specval, VALUE cref_or_me, const rb_iseq_t *iseq);
#else
#define vm_check_frame(a, b, c, d)
#endif

extern void vm_stackoverflow(void);

extern rb_cref_t *vm_cref_push(rb_thread_t *th, VALUE klass, const VALUE *ep, int pushed_by_eval);
extern VALUE vm_once_exec(VALUE iseq);
extern VALUE vm_once_clear(VALUE data);
extern VALUE vm_exec(rb_thread_t *th, int no_mjit_p);

extern VALUE *vm_exec_insn_address_table;

static inline void
vm_change_insn(rb_iseq_t *iseq, VALUE *pc, int insn_id) {
    VALUE addr = vm_exec_insn_address_table[insn_id];
    
    if (mjit_init_p && *pc != addr)
        mjit_change_iseq(iseq, FALSE);
    *pc = addr;
}

#endif /* RUBY_INSNHELPER_H */
