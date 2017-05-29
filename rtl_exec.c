/**********************************************************************

  rtl_exec.c - Code for executing RTL (Register Transfer Language) insns

  Copyright (C) 2017 Vladimir Makarov vmakarov@redhat.com

**********************************************************************/

/* Each RTL insn from insns.def has a corresponding function with
   suffix _f which is responsible for executing all or the most insn
   code.  The functions are declared as static inline to exclude an
   overhead for the function call.  Such approach is to simplify
   implementation of interpreter and MJIT compiler.  MJIT compiler
   mostly translates each insn into the corresponding C function call.

   We have many different specialized move insns to improve the
   interpreter performance.  We would not need such number of the
   insns for JIT only as the used C compiler can generate specialized
   insns versions itself.  */

/* Macros used to translate insn name into the corresponding function
   name.  */
#define RTL_CONCAT(a, b) a ## b
#define RTL_FUNC_NAME(insn_name) RTL_CONCAT(insn_name, _f)

/* Return BP value of CFP.  If we compiled JIT code with speculation
   that EP == BP, we use EP.  This can decrease a register pressure in
   the JIT code.  */
#define RTL_GET_BP(cfp) (mjit_ep_neq_bp_p ? (cfp)->bp : (cfp)->ep)

/* Return address of temporary variable location with index IND (it
   should be negative) in frame CFP.  */
static do_inline VALUE *
get_temp_addr(rb_control_frame_t *cfp, lindex_t ind)
{
    ptrdiff_t offset = ind;

    VM_ASSERT(offset < 0);
    return RTL_GET_BP(cfp) - offset;
}

/* Return address of temporary variable location with index IND (it
   should be negative) in frame CFP.  It is accurate and safe as we
   ignore mjit_ep_neq_bp_p whose value can be wrong in some cases.  */
static do_inline VALUE *
get_temp_addr_safe(rb_control_frame_t *cfp, lindex_t ind)
{
    ptrdiff_t offset = ind;

    VM_ASSERT(offset < 0);
    return cfp->bp - offset;
}

/* Return address of local variable location with index IND (it should
   be positive) in frame CFP.  */
static do_inline VALUE *
get_loc_addr(rb_control_frame_t *cfp, lindex_t ind)
{
    ptrdiff_t offset = ind;

    VM_ASSERT(offset > 0);
    return cfp->ep - offset;
}

/* Return address of local or temporary variable location with index
   IND in frame CFP.  */
static do_inline VALUE *
get_var_addr(rb_control_frame_t *cfp, lindex_t ind)
{
    ptrdiff_t offset = ind;

    return (offset < 0 ? RTL_GET_BP(cfp) : cfp->ep) - offset;
}


/* Return address of local variable location with index IND (it should
   be positive) in a frame with environment pointer EP.  */
static do_inline VALUE *
get_env_loc_addr(VALUE *ep, lindex_t ind)
{
    ptrdiff_t offset = ind;

    VM_ASSERT(offset > 0);
    return ep - offset;
}


/* Assign value V to local or temporary variable location with address
   RES and index RES_IND in frame CFP.  For local variable not on the
   stack we inform generational GC by using function vm_env_write.  */
static do_inline void
var_assign(rb_control_frame_t *cfp, VALUE *res, ptrdiff_t res_ind, VALUE v)
{
    VALUE *ep = cfp->ep;

    if (mjit_ep_neq_bp_p && ep != cfp->bp && res_ind >= 0) {
	vm_env_write(ep, (int) (res - ep), v);
    } else {
	*res = v;
    }
}


/* Execute the current iseq ISEQ of TH and return the result.  The
   iseq has BODY and TYPE.  Try to use JIT code first.  It is called
   only from a JIT code.  To generate a better code MJIT use the
   function when it knows the value of the ISEQ parameters.  */
static do_inline VALUE
mjit_vm_exec_0(rb_thread_t *th, rb_iseq_t *iseq,
	       struct rb_iseq_constant_body *body, int type) {
  VALUE result;
  
  VM_ASSERT(in_mjit_p);
  if ((result = mjit_execute_iseq_0(th, iseq, body, type)) == Qundef)
      result = vm_exec(th, TRUE);
  return result;
}

/* See the above function.  We use the function when the current iseq
   is unknown or change.  */
static do_inline VALUE
mjit_vm_exec(rb_thread_t *th) {
  VALUE result;
  
  VM_ASSERT(in_mjit_p);
  if ((result = mjit_execute_iseq(th)) == Qundef)
      result = vm_exec(th, TRUE);
  return result;
}

/* Set sp in CFP right after TEMP_VARS_NUM variable in frame CFP with
   bp value given by BP.  */
static do_inline void
set_default_sp_0(rb_control_frame_t *cfp, VALUE *bp,
		 unsigned int temp_vars_num) {
    cfp->sp = bp + 1 + temp_vars_num;
}

/* Set sp in CFP right after the last temporary variable of the frame.
   That is a default stack pointer value.  */
static do_inline void
set_default_sp(rb_control_frame_t *cfp, VALUE *bp) {
    set_default_sp_0(cfp, bp, cfp->iseq->body->temp_vars_num);
}

/* Call method given by CALLING, CI, and CC in the current thread TH
   and frame CFP.  Restore the default stack pointer value at the call
   finish.  */
static do_inline VALUE
call_method(rb_thread_t *th, rb_control_frame_t *cfp,
	    struct rb_calling_info *calling, CALL_INFO ci, CALL_CACHE cc)
{
    VALUE val = (*(cc)->call)(th, cfp, calling, ci, cc);

    if (val != Qundef)
	/* The call is finished with value VAL.  */
	set_default_sp(cfp, RTL_GET_BP(cfp));
    else if (in_mjit_p) {
	val = mjit_vm_exec(th);
	set_default_sp(cfp, RTL_GET_BP(cfp));
    }
    return val;
}

/* Call a RECV simple method (without block and irregular args) given
   by CI in the current thread TH and frame CFP.  It means preparing
   structure CALLING and checking and updating CC (vm_search_method
   call) and calling call_method.  */
static do_inline VALUE
call_simple_method(rb_thread_t *th, rb_control_frame_t *cfp,
		   CALL_INFO ci, CALL_CACHE cc, VALUE *recv)
{
    struct rb_calling_info calling;

    calling.block_handler = VM_BLOCK_HANDLER_NONE;
    calling.argc = ci->orig_argc;
    calling.recv = *recv;
    vm_search_method(ci, cc, *recv);
    return call_method(th, cfp, &calling, ci, cc);
}

/* Copy N values of frame CFP variables with starting location FROM to
   the same frame variables with starting location TO with index
   TO_IND.  */
static do_inline void
var2var_f(rb_control_frame_t *cfp, VALUE *to, rindex_t to_ind,
	  VALUE *from, rb_num_t n)
{
    VALUE *dest = to, *src = from;
    rb_num_t i;

    VM_ASSERT(dest > src && n > 0);
    var_assign(cfp, dest, to_ind, *src);
    for (i = 1; i < n; i++)
	dest[i] = src[i];
}

/* Swap values of CFP frame variables given by the locations OP1 and
   OP2 and their indexes OP1_IND and OP2_IND.  */
static do_inline void
var_swap_f(rb_control_frame_t *cfp,
	   VALUE *op1, rindex_t op1_ind, VALUE *op2, rindex_t op2_ind)
{
    VALUE tmp, *v1 = op1, *v2 = op2;

    tmp = *v1;
    var_assign(cfp, v1, op1_ind, *v2);
    var_assign(cfp, v2, op2_ind, tmp);
}

/* Assign value of CFP frame temporary variable OP to another temporary
   variable RES.  */
static do_inline void
temp2temp_f(rb_control_frame_t *cfp, VALUE *res, VALUE *op)
{
    *res = *op;
}

/* Swap values CFP frame temporary variables OP1 and OP2.  */
static do_inline void
temp_swap_f(rb_control_frame_t *cfp, VALUE *op1, VALUE *op2)
{
    VALUE t, *v1 = op1, *v2 = op2;

    t = *v1;
    *v1 = *v2;
    *v2 = t;
}

/* Assign value of CFP frame local variable OP to another local
   variable RES with index RES_IND.  */
static do_inline void
loc2loc_f(rb_control_frame_t *cfp, VALUE *res, lindex_t res_ind, VALUE *op)
{
    VM_ASSERT(res_ind > 0);
    var_assign(cfp, res, res_ind, *op);
}

/* Assign value of CFP frame local variable OP to temporary variable
   RES.  */
static do_inline void
loc2temp_f(rb_control_frame_t *cfp, VALUE *res, VALUE *op)
{
    *res = *op;
}

/* Assign value of CFP frame temporary variable OP to local variable
   RES with index RES_IND.  */
static do_inline void
temp2loc_f(rb_control_frame_t *cfp, VALUE *res, rindex_t res_ind, VALUE *op)
{
    var_assign(cfp, res, res_ind, *op);
}

/* Assign value of upper level variable with index OP from previous
   stack with LEVEL to temporary variable with location RES in frame
   CFP.  */
static do_inline void
uploc2temp_f(rb_control_frame_t *cfp, VALUE *res, sindex_t op, rb_num_t level)
{
    VALUE *ep = cfp->ep;
    int i, lev = (int) level;

    for (i = 0; i < lev; i++)
	ep = GET_PREV_EP(ep);
    *res = *get_env_loc_addr(ep, op);
}

/* Assign value of upper level variable with index OP from previous
   stack with LEVEL to local or temporary variable with location RES
   and index RES_IND in frame CFP.  */
static do_inline void
uploc2var_f(rb_control_frame_t *cfp, VALUE *res, rindex_t res_ind, sindex_t op, rb_num_t level)
{
    VALUE *ep = cfp->ep;
    int i, lev = (int) level;

    for (i = 0; i < lev; i++)
	ep = GET_PREV_EP(ep);
    var_assign(cfp, res, res_ind, *get_env_loc_addr(ep, op));
}

/* Assign value VAL to local variable RES with index RES_IND in frame
   CFP.  */
static do_inline void
val2loc_f(rb_control_frame_t *cfp, VALUE *res, rindex_t res_ind, VALUE val)
{
    var_assign(cfp, res, res_ind, val);
}

/* Assign value VAL to temporary variable RES in frame CFP.  */
static do_inline void
val2temp_f(rb_control_frame_t *cfp, VALUE *res, VALUE val)
{
    *res = val;
}

/* Check that sp of frame CFP has a default value.  */
static do_inline void
check_sp_default(rb_control_frame_t *cfp) {
    VM_ASSERT(cfp->sp == RTL_GET_BP(cfp) + 1 + cfp->iseq->body->temp_vars_num);
}

/* Assign string STR to local or temporary variable RES with index
   RES_IND in frame CFP.  */
static do_inline void
str2var_f(rb_control_frame_t *cfp, VALUE *res, rindex_t res_ind, VALUE str)
{
    check_sp_default(cfp);
    var_assign(cfp, res, res_ind, rb_str_resurrect(str));
}

/* Assign value of const ID of origin KLASS to local or temporary
   variable RES with index RES_IND in frame CFP.  */
static do_inline void
const_ld_val_f(rb_thread_t *th, rb_control_frame_t *cfp, ID id,
	       VALUE *res, rindex_t res_ind, VALUE klass)
{
    check_sp_default(cfp);
    var_assign(cfp, res, res_ind, vm_get_ev_const(th, klass, id, 0));
}

/* The same as above but KLASS_OP is location of the klass value.  */
static do_inline void
const2var_f(rb_thread_t *th, rb_control_frame_t *cfp, ID id,
	    VALUE *res, rindex_t res_ind, VALUE *klass_op)
{
    const_ld_val_f(th, cfp, id, res, res_ind, *klass_op);
}

/* It is analogous to const_ld_val but with using cache IC.  */
static do_inline void
const_cached_val_ld_f(rb_thread_t *th, rb_control_frame_t *cfp,
		      VALUE *res, rindex_t res_ind, VALUE klass, ID id, IC ic)
{
    rb_control_frame_t *reg_cfp = cfp; /* for GET_ISEQ */

    check_sp_default(cfp);
    if (ic->ic_serial == GET_GLOBAL_CONSTANT_STATE() &&
	(ic->ic_cref == NULL || ic->ic_cref == rb_vm_get_cref(GET_EP()))) {
	var_assign(cfp, res, res_ind, ic->ic_value.value);
    }
    else {
	VALUE v;

	v = vm_get_ev_const(th, klass, id, 0);
	var_assign(cfp, res, res_ind, v);
	VM_ASSERT(ic->ic_value.value != Qundef);
	ic->ic_value.value = v;
	ic->ic_serial = GET_GLOBAL_CONSTANT_STATE() - ruby_vm_const_missing_count;
	ic->ic_cref = vm_get_const_key_cref(GET_EP());
	ruby_vm_const_missing_count = 0;
    }
}

/* Speculatively assign value IC_VALUE of a const with IC_SERIAL (from
   IC cache) and IC_CREF to local or temporary variable with location
   RES and index RES_IND in frame CFP.  */
static do_inline int
mjit_const_cached_val_ld(rb_control_frame_t *cfp, rb_serial_t ic_serial, const rb_cref_t *ic_cref,
			 VALUE ic_value, VALUE *res, rindex_t res_ind)
{
    check_sp_default(cfp);
    if (ic_serial == GET_GLOBAL_CONSTANT_STATE() && ic_cref == NULL) {
	var_assign(cfp, res, res_ind, ic_value);
	return FALSE;
    }
    return TRUE;
}

/* If cache IC is valid, assign its value to local or temporary
   variable with location RES and index RES_IND in frame CFP, return
   TRUE.  Otherwise, assign Qnil and return FALSE.  */
static do_inline int
get_inline_cache_f(rb_control_frame_t *cfp, VALUE *res, rindex_t res_ind, IC ic)
{
    check_sp_default(cfp);
    if (ic->ic_serial == GET_GLOBAL_CONSTANT_STATE()
	&& (ic->ic_cref == NULL
	    || ic->ic_cref == rb_vm_get_cref(cfp->ep))) {
	var_assign(cfp, res, res_ind, ic->ic_value.value);
	return TRUE;
    }
    /* none */
    var_assign(cfp, res, res_ind, Qnil);
    return FALSE;
}

/* If IC_SERIAL and IC_REF are valid, assign IC_VALUE value to local or temporary
   variable with location RES and index RES_IND in frame CFP, return
   FALSE.  Otherwise, return TRUE if the speculation failed.  */
static do_inline int
mjit_get_inline_cache(rb_control_frame_t *cfp, rb_serial_t ic_serial, const rb_cref_t *ic_cref,
		      VALUE ic_value, VALUE *res, rindex_t res_ind)
{
    check_sp_default(cfp);
    if (ic_serial == GET_GLOBAL_CONSTANT_STATE() && ic_cref == NULL) {
	var_assign(cfp, res, res_ind, ic_value);
	return FALSE;
    }
    return TRUE;
}

/* Write value of location OP in frame CFP to cache IC.  */
static do_inline void
set_inline_cache_f(rb_control_frame_t *cfp, VALUE *op, IC ic)
{
    VALUE val = *op;

    check_sp_default(cfp);
    VM_ASSERT(ic->ic_value.value != Qundef);
    ic->ic_value.value = val;
    ic->ic_serial = GET_GLOBAL_CONSTANT_STATE() - ruby_vm_const_missing_count;
    ic->ic_cref = vm_get_const_key_cref(cfp->ep);
    ruby_vm_const_missing_count = 0;
}

/* Assign special object of VALUE_TYPE to local or temporary variable with
   location RES and index RES_IND in frame CFP.  */
static do_inline void
specialobj2var_f(rb_control_frame_t *cfp, VALUE *res, rindex_t res_ind, rb_num_t value_type)
{
    enum vm_special_object_type type = (enum vm_special_object_type)value_type;

    check_sp_default(cfp);
    switch (type) {
    case VM_SPECIAL_OBJECT_VMCORE:
	var_assign(cfp, res, res_ind, rb_mRubyVMFrozenCore);
	break;
    case VM_SPECIAL_OBJECT_CBASE:
	var_assign(cfp, res, res_ind, vm_get_cbase(cfp->ep));
	break;
    case VM_SPECIAL_OBJECT_CONST_BASE:
	var_assign(cfp, res, res_ind, vm_get_const_base(cfp->ep));
	break;
    default:
	rb_bug("putspecialobject insn: unknown value_type");
  }
}

/* Assign a special of TYPE with KEY (if TYPE is zero) to local or
   temporary variable with location RES and index RES_IND in frame
   CFP.  */
static do_inline void
special2var_f(rb_thread_t *th, rb_control_frame_t *cfp, VALUE *res,
	      rindex_t res_ind, rb_num_t key, rb_num_t type)
{
    rb_control_frame_t *reg_cfp = cfp; /* for GET_LEP */

    check_sp_default(cfp);
    var_assign(cfp, res, res_ind, vm_getspecial(th, GET_LEP(), key, type));
}

/* Assign self to local or temporary variable with location RES and index
   RES_IND in frame CFP.  */
static do_inline void
self2var_f(rb_control_frame_t *cfp, VALUE *res, rindex_t res_ind)
{
    rb_control_frame_t *reg_cfp = cfp; /* for GET_SELF */

    var_assign(cfp, res, res_ind, GET_SELF());
}

/* Assign global with ENTRY to local or temporary variable with
   location RES and index RES_IND in frame CFP.  */
static do_inline void
global2var_f(rb_control_frame_t *cfp, VALUE *res, rindex_t res_ind, GENTRY entry)
{
    check_sp_default(cfp);
    var_assign(cfp, res, res_ind, GET_GLOBAL((VALUE)entry));
}

/* Assign value of an instance variable with ID and occurrence cache
   IC to local or temporary variable with location RES and index
   RES_IND in frame CFP.  */
static do_inline void
ivar2var_f(rb_control_frame_t *cfp, VALUE *res, rindex_t res_ind, ID id, IC ic)
{
    rb_control_frame_t *reg_cfp = cfp; /* for GET_SELF */

    check_sp_default(cfp);
    var_assign(cfp, res, res_ind, vm_getinstancevariable(GET_SELF(), id, ic));
}

/* Return true if our speculation IVAR_SPEC about SELF ivars fails.
   We assume that SELF class has IC_SERIAL.  */
static do_inline int
mjit_check_self_p(VALUE self, rb_serial_t ic_serial, size_t ivar_spec) {
    VM_ASSERT(RB_TYPE_P(self, T_OBJECT) && ivar_spec != 0);
    if (ic_serial != RCLASS_SERIAL(RBASIC(self)->klass))
	return TRUE;
    return (ivar_spec == (size_t) -1 ? ROBJECT_NUMIV(self) != ROBJECT_EMBED_LEN_MAX : ROBJECT_NUMIV(self) <= ivar_spec);
}

/* Speculative ivar2var of SELF.  We know that SELF hash ivar with
   INDEX and has > ROBJECT_EMBED_LEN_MAX ivars if BIG_P.  Otherwise
   SELF has <= ROBJECT_EMBED_LEN_MAX ivars. */
static do_inline void
mjit_ivar2var_no_check(rb_control_frame_t *cfp, VALUE self, int big_p,
		       size_t index, VALUE *res, rindex_t res_ind)
{
    VALUE v;
    
    check_sp_default(cfp);
    v = (big_p ? vm_getivar_spec_big : vm_getivar_spec_small)(self, index);
    var_assign(cfp, res, res_ind, v);
}

/* Speculatively assign value of an instance variable of SELF with
   IC_SERIAL (from IC cache) and INDEX to local or temporary variable
   with location RES and index RES_IND in frame CFP.  We know
   RB_TYPE_P(self, T_OBJECT) is true that if TYPE_OBJ_P is true.  */
static do_inline int
mjit_ivar2var(rb_control_frame_t *cfp, VALUE self, int type_obj_p,
	      rb_serial_t ic_serial, size_t index, VALUE *res, rindex_t res_ind)
{
    VALUE v;
    
    check_sp_default(cfp);
    if ((v = vm_getivar_spec(self, type_obj_p, ic_serial, index)) == Qundef)
	return TRUE;
    var_assign(cfp, res, res_ind, v);
    return FALSE;
}

/* Assign value of a class variable with ID to local or temporary
   variable with location RES and index RES_IND in frame CFP.  */
static do_inline void
cvar2var_f(rb_control_frame_t *cfp, VALUE *res, rindex_t res_ind, ID id)
{
    rb_control_frame_t *reg_cfp = cfp; /* for GET_EP */
    VALUE *ep = GET_EP();

    check_sp_default(cfp);
    var_assign(cfp, res, res_ind, rb_cvar_get(vm_get_cvar_base(rb_vm_get_cref(ep), GET_CFP()), id));
}

/* Assign ISEQ to local or temporary variable with location RES and
   index RES_IND in frame CFP.  Set up iseq in_type_object_p.  */
static do_inline void
iseq2var_f(rb_control_frame_t *cfp, VALUE *res, rindex_t res_ind, ISEQ iseq)
{
    iseq->body->in_type_object_p = RB_TYPE_P(cfp->self, T_CLASS) && ! rb_special_class_p(cfp->self);
    var_assign(cfp, res, res_ind, (VALUE) iseq);
}

/* Assign value local or temporary variable with location FROM in
   frame CFP to an upper level local variable with index IDX in a
   frame with LEVEL (0 is CFP).  */
static do_inline void
var2uploc_f(rb_control_frame_t *cfp, rb_num_t idx, VALUE *from, rb_num_t level)
{
    VALUE *ep = cfp->ep;
    int i, lev = (int)level;

    for (i = 0; i < lev; i++)
	ep = GET_PREV_EP(ep);
    vm_env_write(ep, -(int) idx, *from);
}

/* Assign value VAL to an upper level local variable with index IDX in
   a frame with LEVEL (0 is CFP).  */
static do_inline void
val2uploc_f(rb_control_frame_t *cfp, rb_num_t idx, VALUE val, rb_num_t level)
{
    VALUE *ep = cfp->ep;
    int i, lev = (int)level;

    for (i = 0; i < lev; i++)
	ep = GET_PREV_EP(ep);
    vm_env_write(ep, -(int) idx, val);
}

/* Assign value with location FROM of frame CFP of thread TH to a
   local variable with index IDX in previous frame and return from the
   current frame.  */
static do_inline void
ret_to_loc_f(rb_thread_t *th, rb_control_frame_t *cfp, rb_num_t idx, VALUE *from)
{
    rb_control_frame_t *reg_cfp = cfp; /* for GET_EP, GET_CFP */
    VALUE *ep = cfp->ep;

    ep = GET_PREV_EP(ep);
    *get_env_loc_addr(ep, idx) = *from;
    if (vm_pop_frame(th, GET_CFP(), GET_EP())) {
	rb_bug("Error in rescue return");
    }
}

/* Analogous to ret_to_local_f but assign the value to a temporary
   variable.  */
static do_inline void
ret_to_temp_f(rb_thread_t *th, rb_control_frame_t *cfp, rb_num_t idx, VALUE *from)
{
    rb_control_frame_t *reg_cfp = cfp; /* for GET_EP, GET_CFP */
    rb_control_frame_t *prev_cfp = RUBY_VM_PREVIOUS_CONTROL_FRAME(cfp);

    *get_temp_addr(prev_cfp, idx) = *from;
    if (vm_pop_frame(th, GET_CFP(), GET_EP())) {
	rb_bug("Error in rescue return");
    }
}

/* Assign value of local or temporary variable VAL_OP in frame CFP to
   constant ID in class/module in location CBASE_OP.  */
static do_inline void
var2const_f(rb_control_frame_t *cfp, ID id, VALUE *val_op, VALUE *cbase_op)
{
    VALUE val = *val_op;
    VALUE cbase = *cbase_op;
    rb_control_frame_t *reg_cfp = cfp; /* for GET_SELF */

    check_sp_default(reg_cfp);
    vm_check_if_namespace(cbase);
    vm_ensure_not_refinement_module(GET_SELF());
    rb_const_set(cbase, id, val);
}

/* Assign value of local or temporary variable VAL_OP in frame CFP to
   global with ENTRY.  */
static do_inline void
var2global_f(rb_control_frame_t *cfp, GENTRY entry, VALUE *val_op)
{
    check_sp_default(cfp);
    SET_GLOBAL((VALUE) entry, *val_op);
}

/* Assign value VAL to an instance variable ID and occurrence cache
   IC.  */
static do_inline void
val2ivar_f(rb_control_frame_t *cfp, ID id, IC ic, VALUE val)
{
    rb_control_frame_t *reg_cfp = cfp; /* for GET_SELF */

    check_sp_default(cfp);
    vm_setinstancevariable(GET_SELF(), id, val, ic);
}

/* Speculative val2ivar of SELF.  We know that SELF has ivar with
   index and has > ROBJECT_EMBED_LEN_MAX ivars if BIG_P.  Otherwise
   SELF has <= ROBJECT_EMBED_LEN_MAX ivars. */
static do_inline void
mjit_val2ivar_no_check(rb_control_frame_t *cfp,
		       VALUE self, int big_p, size_t index, VALUE val) {
    check_sp_default(cfp);
    (big_p ? vm_setivar_spec_big : vm_setivar_spec_small)(self, index, val);
}

/* Speculatively assign value VAL to an instance variable of SELF with
   IC_SERIAL (from IC cache) and INDEX.  We know RB_TYPE_P(self,
   T_OBJECT) is true that if TYPE_OBJ_P is true.  */
static do_inline int
mjit_val2ivar(rb_control_frame_t *cfp, VALUE self, int type_obj_p,
	      rb_serial_t ic_serial, size_t index, VALUE val)
{
    check_sp_default(cfp);
    return vm_setivar_spec(self, type_obj_p, ic_serial, index, val) == Qundef;
}

/* As val2ivar_f but VAL_OP of CFP location of the value.  */
static do_inline void
var2ivar_f(rb_control_frame_t *cfp, ID id, IC ic, VALUE *val_op)
{
    val2ivar_f(cfp, id, ic, *val_op);
}

/* As mjit_val2ivar_no_check but with the value in location VAL_OP.  */
static do_inline void
mjit_var2ivar_no_check(rb_control_frame_t *cfp,
		       VALUE self, int big_p, size_t index, VALUE *val_op) {
    mjit_val2ivar_no_check(cfp, self, big_p, index, *val_op);
}

/* As mjit_val2ivar but with the value in location VAL_OP.  */
static do_inline int
mjit_var2ivar(rb_control_frame_t *cfp, VALUE self, int type_obj_p,
	      rb_serial_t ic_serial, size_t index, VALUE *val_op)
{
    return mjit_val2ivar(cfp, self, type_obj_p, ic_serial, index, *val_op);
}

/* Assign value of local or temporary variable with location VAL_OP in
   frame CFP to class variable ID.  */
static do_inline void
var2cvar_f(rb_control_frame_t *cfp, ID id, VALUE *val_op)
{
    rb_control_frame_t *reg_cfp = cfp; /* for GET_EP, GET_CFP, GET_SELF */

    check_sp_default(cfp);
    vm_ensure_not_refinement_module(GET_SELF());
    rb_cvar_set(vm_get_cvar_base(rb_vm_get_cref(GET_EP()), GET_CFP()), id, *val_op);
}

/* Assign value of local or temporary variable with location VAL_OP in
   frame CFP to special given by KEY.  */
static do_inline void
var2special_f(rb_thread_t *th, rb_control_frame_t *cfp, rb_num_t key,
	      VALUE *op)
{
    rb_control_frame_t *reg_cfp = cfp; /* for GET_LEP, GET_SELF */

    check_sp_default(cfp);
    vm_ensure_not_refinement_module(GET_SELF());
    lep_svar_set(th, GET_LEP(), key, *op);
}

/* Call without args method given by CD of object RECV in the current
   thread TH and frame CFP.  Return the call value.  */
static do_inline VALUE
op1_call(rb_thread_t *th, rb_control_frame_t *cfp, CALL_DATA cd, VALUE *recv)
{
    VALUE *sp;

    sp = get_temp_addr(cfp, cd->call_start);
    sp[0] = *recv;
    cfp->sp = sp + 1;
    return call_simple_method(th, cfp, &cd->call_info, &cd->call_cache, recv);
}

/* Finish an (arithmetic or compare) operation.  Put VAL into location
   given by RES and RES_IND.  Undefined VAL means calling an iseq to
   get the value.  Return non-zero if we need to cancel JITed code
   execution and don't use the code anymore (it happens usually after
   global changes, e.g. basic type operation redefinition).  */
static do_inline int
op_val_call_end(rb_thread_t *th, rb_control_frame_t *cfp, VALUE *res, rindex_t res_ind, VALUE val) {
    if (val == Qundef) {
	VM_ASSERT(! in_mjit_p);
	return 1;
    }
    var_assign(cfp, res, res_ind, val);
    if (! in_mjit_p)
	return 0;
    if ((cfp->ep[VM_ENV_DATA_INDEX_FLAGS] & VM_FRAME_FLAG_CANCEL) == 0)
	return 0;
    mjit_change_iseq(cfp->iseq);
    return 1;
}

/* Like above but without the assignment.  */
static do_inline int
op_call_end(rb_thread_t *th, rb_control_frame_t *cfp, VALUE val) {
    if (val == Qundef) {
	if (! in_mjit_p)
	    return 1;
	val = mjit_vm_exec(th);
    }
    if (! in_mjit_p)
	return 0;
    if ((cfp->ep[VM_ENV_DATA_INDEX_FLAGS] & VM_FRAME_FLAG_CANCEL) == 0)
	return 0;
    mjit_change_iseq(cfp->iseq);
    return 1;
}

/* Header of a function NAME with 1 operand of specific type (array,
   string, or hash).  */
#define ary_hash_f(name) static do_inline VALUE name(VALUE op)

/* Definitions of functions with 1 operand of specific type (array,
   string, or hash).  */
ary_hash_f(str_length) {return rb_str_length(op);}
ary_hash_f(ary_length) {return LONG2NUM(RARRAY_LEN(op));}
ary_hash_f(hash_length) {return INT2FIX(RHASH_SIZE(op));}

ary_hash_f(str_size) {return rb_str_length(op);}
ary_hash_f(ary_size) {return LONG2NUM(RARRAY_LEN(op));}
ary_hash_f(hash_size) {return INT2FIX(RHASH_SIZE(op));}

ary_hash_f(str_empty_p) {return RSTRING_LEN(op) == 0 ? Qtrue : Qfalse;}
ary_hash_f(ary_empty_p) {return RARRAY_LEN(op) == 0 ? Qtrue : Qfalse;}
ary_hash_f(hash_empty_p) {return RHASH_EMPTY_P(op) ? Qtrue : Qfalse;;}

/* Common function executing an insn with one operand OP and result
   RES with index RES_IND in frame CFP of thread TH.  The function has
   a fast execution path for operand of type string, array, or hash
   given correspondingly by functions STR_OP, ARY_OP, and HASH_OP.
   For the slow paths use call data CD.  Return non-zero if we started
   a new ISEQ execution (we need to update vm_exec_core regs in this
   case) or we need to cancel the current JITed code..  */
static do_inline int
ary_hash_op(rb_thread_t *th, rb_control_frame_t *cfp,
	    CALL_DATA cd, VALUE *res, rindex_t res_ind, VALUE *op,
	    enum ruby_basic_operators bop,
	    VALUE (*str_op)(VALUE),
	    VALUE (*ary_op)(VALUE),
	    VALUE (*hash_op)(VALUE))
{
    VALUE val;
    VALUE *src = op;

    if (! SPECIAL_CONST_P(*src)) {
	if (RBASIC_CLASS(*src) == rb_cString
	    && BASIC_OP_UNREDEFINED_P(bop, STRING_REDEFINED_OP_FLAG)) {
	    var_assign(cfp, res, res_ind, str_op(*src));
	    return 0;
	} else if (RBASIC_CLASS(*src) == rb_cArray
		   && BASIC_OP_UNREDEFINED_P(bop, ARRAY_REDEFINED_OP_FLAG)) {
	    var_assign(cfp, res, res_ind, ary_op(*src));
	    return 0;
	} else if (RBASIC_CLASS(*src) == rb_cHash
		   && BASIC_OP_UNREDEFINED_P(bop, HASH_REDEFINED_OP_FLAG)) {
	    var_assign(cfp, res, res_ind, hash_op(*src));
	    return 0;
	}
    }
    val = op1_call(th, cfp, cd, src);
    return op_val_call_end(th, cfp, res, res_ind, val);
}

/* A call of ary_has_op for operation BOP and suffix SUFF for fast
   path functions. */
#define ary_hash_call(suff, bop) ary_hash_op(th, cfp, cd,		\
					     res, res_ind, op, bop,	\
					     str_ ## suff, ary_ ## suff, \
					     hash_ ## suff)

/* Header of a function executing 1 operand insn NAME.  */
#define op1_fun(name) static do_inline int	  \
                      name ## _f(rb_thread_t *th, \
				 rb_control_frame_t *cfp,	\
				 CALL_DATA cd,		\
				 VALUE *res, rindex_t res_ind, VALUE *op)

/* Definitions of the functions executing 1 operand insns with fast
   execution path for string, array, or hash.  */
op1_fun(length) {return ary_hash_call(length, BOP_LENGTH);}
op1_fun(size) {return ary_hash_call(size, BOP_SIZE);}
op1_fun(empty_p) {return ary_hash_call(empty_p, BOP_EMPTY_P);}

/* It is analagous to the above functions but for insn succ with a
   fast path for fixnum and string.  */
op1_fun(succ) {
    VALUE val;
    VALUE *src = op;

    if (SPECIAL_CONST_P(*src)) {
	if (FIXNUM_P(*src)
	    && BASIC_OP_UNREDEFINED_P(BOP_SUCC, INTEGER_REDEFINED_OP_FLAG)) {
	    if (*src != LONG2FIX(FIXNUM_MAX)) {
		val = *src - 1 + INT2FIX(1);
	    }
	    else {
		val = LONG2NUM(FIXNUM_MAX + 1);
	    }
	    var_assign(cfp, res, res_ind, val);
	    return 0;
	}
    } else if (RBASIC_CLASS(*src) == rb_cString
	       && BASIC_OP_UNREDEFINED_P(BOP_SUCC, STRING_REDEFINED_OP_FLAG)) {
	var_assign(cfp, res, res_ind, rb_str_succ(*src));
	return 0;
    }
    val = op1_call(th, cfp, cd, src);
    return op_val_call_end(th, cfp, res, res_ind, val);
}

/* Function but for insn not with a fast path for pre-defined method
   not.  */
op1_fun(not) {
    VALUE val;
    VALUE *src = op;
    CALL_INFO ci = &cd->call_info;
    CALL_CACHE cc = &cd->call_cache;
    extern VALUE rb_obj_not(VALUE obj);

    vm_search_method(ci, cc, *src);

    if (check_cfunc(cc->me, rb_obj_not)) {
	val = RTEST(*src) ? Qfalse : Qtrue;
	var_assign(cfp, res, res_ind, val);
	return 0;
    }
    val = op1_call(th, cfp, cd, src);
    return op_val_call_end(th, cfp, res, res_ind, val);
}

/* Call method given by CD of object RECV with arg OP2 in the current
   thread TH and frame CFP.  As RECV and OP2 can any local or
   temporary variables put them on the stack in the order.  Return the
   call value.  */
static do_inline VALUE
op2_call(rb_thread_t *th, rb_control_frame_t *cfp,
	 CALL_DATA cd, VALUE *recv, VALUE op2) {
    VALUE *sp;

    sp = get_temp_addr(cfp, cd->call_start);
    sp[0] = *recv;
    sp[1] = op2;
    cfp->sp = sp + 2;
    /* Use sp as *recv can be overwritten.  */
    return call_simple_method(th, cfp, &cd->call_info, &cd->call_cache, sp);
}

/* Call method given by CD of object RECV with args OP2 and OP3 in the
   current thread TH and frame CFP.  As RECV, OP2, and OP3 can any local or
   temporary variables put them on the stack in the order.  Return the
   call value.  */
static do_inline VALUE
op3_call(rb_thread_t *th, rb_control_frame_t *cfp,
	 CALL_DATA cd, VALUE *recv, VALUE op2, VALUE op3) {
    VALUE *sp;

    sp = get_temp_addr(cfp, cd->call_start);
    sp[0] = *recv;
    sp[1] = op2;
    sp[2] = op3;
    cfp->sp = sp + 3;
    /* Use sp as *recv can be overwritten.  */
    return call_simple_method(th, cfp, &cd->call_info, &cd->call_cache, sp);
}

/* Header of a function NAME with args a and b implementing a fast
   execution path of comparison of a specific type.  */
#define cmpf(name) static do_inline int name(VALUE a, VALUE b)

/* Definitions of functions implementing a fast execution path of of
   comparison of fixnums.  */
cmpf(fix_num_eq) {return (SIGNED_VALUE) a == (SIGNED_VALUE) b;}
cmpf(fix_num_ne) {return (SIGNED_VALUE) a != (SIGNED_VALUE) b;}
cmpf(fix_num_lt) {return (SIGNED_VALUE) a < (SIGNED_VALUE) b;}
cmpf(fix_num_gt) {return (SIGNED_VALUE) a > (SIGNED_VALUE) b;}
cmpf(fix_num_le) {return (SIGNED_VALUE) a <= (SIGNED_VALUE) b;}
cmpf(fix_num_ge) {return (SIGNED_VALUE) a >= (SIGNED_VALUE) b;}

/* As above but for floats.  */
cmpf(float_num_eq) {return a == b;}
cmpf(float_num_ne) {return a != b;}
cmpf(float_num_lt) {return RFLOAT_VALUE(a) < RFLOAT_VALUE(b);}
cmpf(float_num_gt) {return RFLOAT_VALUE(a) > RFLOAT_VALUE(b);}
cmpf(float_num_le) {return RFLOAT_VALUE(a) <= RFLOAT_VALUE(b);}
cmpf(float_num_ge) {return RFLOAT_VALUE(a) >= RFLOAT_VALUE(b);}

/* As above but for doubles.  */
cmpf(double_num_eq) {return RFLOAT_VALUE(a) == RFLOAT_VALUE(b);}
cmpf(double_num_ne) {return RFLOAT_VALUE(a) != RFLOAT_VALUE(b);}
cmpf(double_num_lt) {return double_cmp_lt(RFLOAT_VALUE(a), RFLOAT_VALUE(b)) == Qtrue;}
cmpf(double_num_gt) {return double_cmp_gt(RFLOAT_VALUE(a), RFLOAT_VALUE(b)) == Qtrue;}
cmpf(double_num_le) {return double_cmp_le(RFLOAT_VALUE(a), RFLOAT_VALUE(b)) == Qtrue;}
cmpf(double_num_ge) {return double_cmp_ge(RFLOAT_VALUE(a), RFLOAT_VALUE(b)) == Qtrue;}

/* Common function executing a comparison of operands OP1 (value
   location) and OP2 (value) in frame CFP of thread TH.  The function
   has a fast execution path for operand of type fixnum, float, and
   doubles given correspondingly by functions FIX_NUM_CMP,
   FLOAT_NUM_CMP, and DOUBLE_NUM_CMP.  For the slow paths use call
   data CD.  If OP2_FIXNUM_P or OP2_FLONUM_P is true, it means that
   value OP2 is of the corresponding type.  If OFFSET is non-zero and
   we use a fast path, change the current insn at OFFSET to the
   corresponding speculative insn FIX_INSN_ID or FLO_INSN_ID.  Return
   the comparison result of Qundef if we started a new ISEQ execution
   (we need to update vm_exec_core regs in this case).  */
static do_inline VALUE
common_cmp(rb_thread_t *th,
	   rb_control_frame_t *cfp, CALL_DATA cd,
	   VALUE *op1, VALUE op2,
	   enum ruby_basic_operators bop,
	   int (*fix_num_cmp)(VALUE, VALUE),
	   int (*float_num_cmp)(VALUE, VALUE),
	   int (*double_num_cmp)(VALUE, VALUE),
	   int op2_fixnum_p, int op2_flonum_p,
	   int offset, int fix_insn_id, int flo_insn_id) {
    int cmp;
    if (((op2_fixnum_p && FIXNUM_P(*op1))
	 || (! op2_fixnum_p && ! op2_flonum_p && FIXNUM_2_P(*op1, op2)))
	&& BASIC_OP_UNREDEFINED_P(bop, INTEGER_REDEFINED_OP_FLAG)) {
	if (offset)
	    vm_change_insn(cfp->iseq, cfp->pc - offset, fix_insn_id);
	cmp = fix_num_cmp(*op1, op2);
	return cmp ? Qtrue : Qfalse;
    } else if ((op2_flonum_p && FLONUM_P(*op1)
	      || (! op2_fixnum_p && ! op2_flonum_p && FLONUM_2_P(*op1, op2)))
	       && BASIC_OP_UNREDEFINED_P(bop, FLOAT_REDEFINED_OP_FLAG)) {
	if (offset)
	    vm_change_insn(cfp->iseq, cfp->pc - offset, flo_insn_id);
	cmp = float_num_cmp(*op1, op2);
	return cmp ? Qtrue : Qfalse;
    }
    else if (! op2_fixnum_p && ! op2_flonum_p
	     && ! SPECIAL_CONST_P(*op1) && ! SPECIAL_CONST_P(op2)) {
	if (RBASIC_CLASS(*op1) == rb_cFloat && RBASIC_CLASS(op2) == rb_cFloat
	    && BASIC_OP_UNREDEFINED_P(bop, FLOAT_REDEFINED_OP_FLAG)) {
	    cmp = double_num_cmp(*op1, op2);
	    return cmp ? Qtrue : Qfalse;
	} else if (bop == BOP_EQ && RBASIC_CLASS(*op1) == rb_cString
		   && RBASIC_CLASS(op2) == rb_cString &&
		   BASIC_OP_UNREDEFINED_P(bop, STRING_REDEFINED_OP_FLAG)) {
	    return rb_str_equal(*op1, op2);
	}
#if 0
	else if (bop == BOP_NEQ && RBASIC_CLASS(*op1) == rb_cString
		 && RBASIC_CLASS(op2) == rb_cString &&
		 BASIC_OP_UNREDEFINED_P(bop, STRING_REDEFINED_OP_FLAG)) {
	    return RTEST(rb_str_equal(*op1, op2)) ? Qfalse : Qtrue;
	}
#endif
    } else if (bop == BOP_EQ) {
	CALL_INFO ci = &cd->call_info;
	CALL_CACHE cc = &cd->call_cache;

	vm_search_method(ci, cc, *op1);
	if (check_cfunc(cc->me, rb_obj_equal)) {
	    return *op1 == op2 ? Qtrue : Qfalse;
	}
    }
#if 0
    else if (bop == BOP_NEQ) {
	abort();
	vm_search_method(&cd->call_info, &cd->call_cache, *op1);
	if (check_cfunc(cc->me, rb_obj_not_equal)) {
	    val = opt_eq_func(*op1, op2, &cd->call_info, ci_eq, cc_eq);

	    if (val != Qundef) {
		return RTEST(val) ? Qfalse : Qtrue;
	    }
	}
    }
#endif
  return op2_call(th, cfp, cd, op1, op2);
}

/* As the function above but it also assigns the comparison value to
   temporary or local variable with location RES and index RES_IND if
   the operation is finished (it return zero in this case).  */
static do_inline int
do_cmp(rb_thread_t *th,
       rb_control_frame_t *cfp,
       CALL_DATA cd, VALUE *res, rindex_t res_ind, VALUE *op1, VALUE op2,
       enum ruby_basic_operators bop,
       int (*fix_num_cmp)(VALUE, VALUE),
       int (*float_num_cmp)(VALUE, VALUE),
       int (*double_num_cmp)(VALUE, VALUE),
       int op2_fixnum_p, int op2_flonum_p,
       int offset, int fix_insn_id, int flo_insn_id)
{
    VALUE val = common_cmp(th, cfp, cd, op1, op2, bop,
			   fix_num_cmp, float_num_cmp, double_num_cmp,
			   op2_fixnum_p, op2_flonum_p,
			   offset, fix_insn_id, flo_insn_id);

    return op_val_call_end(th, cfp, res, res_ind, val);
}

/* It is a call of do_cmp when we don't know OP2 type.  */
static do_inline int
cmp_op(rb_thread_t *th,
       rb_control_frame_t *cfp,
       CALL_DATA cd, VALUE *res, rindex_t res_ind, VALUE *op1, VALUE *op2,
       enum ruby_basic_operators bop,
       int (*fix_num_cmp)(VALUE, VALUE),
       int (*float_num_cmp)(VALUE, VALUE),
       int (*double_num_cmp)(VALUE, VALUE),
       int offset, int fix_insn_id, int flo_insn_id)
{
    VALUE *src1 = op1, src2 = *op2;

    return do_cmp(th, cfp, cd, res, res_ind, src1, src2,
		  bop, fix_num_cmp, float_num_cmp, double_num_cmp, FALSE, FALSE,
		  offset, fix_insn_id, flo_insn_id);
}

/* Header of a function executing 2 operand insn NAME with unknown type of OP2.  */
#define op2_fun(name) static do_inline int	  \
                      name ## _f(rb_thread_t *th, \
				 rb_control_frame_t *cfp, \
				 CALL_DATA cd,		\
				 VALUE *res, rindex_t res_ind, VALUE *op1, VALUE *op2)
/* A function call implementing a comparison insn (which can change
   the insn to speculative one) with operation BOP and suffix SUFF for
   fast execution path functions. */
#define cmp_call(suff, bop) cmp_op(th, cfp, cd,				\
				   res, res_ind, op1, op2, bop,		\
				   fix_num_ ## suff, float_num_ ## suff, \
				   double_num_ ## suff, \
				   6, BIN(i ## suff), BIN(f ## suff))

/* Definitions of the functions implementing comparison insns which
   can be transformed into a speculative variant.  */
op2_fun(eq) {return cmp_call(eq, BOP_EQ);}
op2_fun(ne) {return cmp_call(ne, BOP_NEQ);}
op2_fun(lt) {return cmp_call(lt, BOP_LT);}
op2_fun(gt) {return cmp_call(gt, BOP_GT);}
op2_fun(le) {return cmp_call(le, BOP_LE);}
op2_fun(ge) {return cmp_call(ge, BOP_GE);}

/* A function call implementing a comparison insn (which can not
   change the insn) with operation BOP and suffix SUFF for fast path
   functions. */
#define ucmp_call(suff, bop) cmp_op(th, cfp, cd, \
				    res, res_ind, op1, op2, bop, \
				    fix_num_ ## suff, float_num_ ## suff, \
				    double_num_ ## suff,		\
				    0, 0, 0)

/* Definitions of the functions implementing unchanging comparison
   insns.  */
op2_fun(ueq) {return ucmp_call(eq, BOP_EQ);}
op2_fun(une) {return ucmp_call(ne, BOP_NEQ);}
op2_fun(ult) {return ucmp_call(lt, BOP_LT);}
op2_fun(ugt) {return ucmp_call(gt, BOP_GT);}
op2_fun(ule) {return ucmp_call(le, BOP_LE);}
op2_fun(uge) {return ucmp_call(ge, BOP_GE);}

/* It is analogous to cmp_op with known value of the 2nd operand
   (IMM).  */
static do_inline int
cmp_imm_op(rb_thread_t *th,
	   rb_control_frame_t *cfp,
	   CALL_DATA cd, VALUE *res, rindex_t res_ind, VALUE *op1, VALUE imm,
	   enum ruby_basic_operators bop,
	   int (*fix_num_cmp)(VALUE, VALUE),
	   int (*float_num_cmp)(VALUE, VALUE),
	   int (*double_num_cmp)(VALUE, VALUE),
	   int fixnum_p, int flonum_p,
	   int offset, int fix_insn_id, int flo_insn_id)
{
    VALUE *src1 = op1;

    return do_cmp(th, cfp, cd, res, res_ind, src1, imm, bop,
		  fix_num_cmp, float_num_cmp, double_num_cmp, fixnum_p, flonum_p,
		  offset, fix_insn_id, flo_insn_id);
}

/* Analogous to op2_fun but with known second operand (IMM).  */
#define op2i_fun(name) static do_inline int	     \
                       name ## _f(rb_thread_t *th, \
				  rb_control_frame_t *cfp,	\
				  CALL_DATA cd,	\
				  VALUE *res, rindex_t res_ind, VALUE *op1, VALUE imm)
/* Analogous to cmp_call but with fixnum value IMM as the 2nd
   operand.  */
#define cmp_fix_call(suff, bop) cmp_imm_op(th, cfp, cd,			\
					   res, res_ind, op1, imm, bop,	\
					   fix_num_ ## suff, float_num_ ## suff, \
					   double_num_ ## suff, TRUE, FALSE, \
					   6, BIN(i ## suff ## i), 0)

/* Definitions of the functions implementing comparison insns with
   immediate of type fixnum which can be transformed to a speculative
   variant.  */
op2i_fun(gei) {return cmp_fix_call(ge, BOP_GE);}
op2i_fun(eqi) {return cmp_fix_call(eq, BOP_EQ);}
op2i_fun(nei) {return cmp_fix_call(ne, BOP_NEQ);}
op2i_fun(lti) {return cmp_fix_call(lt, BOP_LT);}
op2i_fun(gti) {return cmp_fix_call(gt, BOP_GT);}
op2i_fun(lei) {return cmp_fix_call(le, BOP_LE);}

/* A function call implementing a comparison insn (which can not
   change the insn) with operation BOP and suffix SUFF for fast path
   functions and immediate of type fixnum as the 2nd operand.  */
#define ucmp_fix_call(suff, bop) cmp_imm_op(th, cfp, cd, \
					    res, res_ind, op1, imm, bop, \
					    fix_num_ ## suff, float_num_ ## suff, \
					    double_num_ ## suff, TRUE, FALSE, \
					    0, 0, 0)

/* Definitions of the functions implementing non-changing comparison
   insns with immediate of type fixnum.  */
op2i_fun(ugei) {return ucmp_fix_call(ge, BOP_GE);}
op2i_fun(ueqi) {return ucmp_fix_call(eq, BOP_EQ);}
op2i_fun(unei) {return ucmp_fix_call(ne, BOP_NEQ);}
op2i_fun(ulti) {return ucmp_fix_call(lt, BOP_LT);}
op2i_fun(ugti) {return ucmp_fix_call(gt, BOP_GT);}
op2i_fun(ulei) {return ucmp_fix_call(le, BOP_LE);}

/* The same as cmp_fix_call but for immediate of flonum type.  */
#define cmp_flo_call(suff, bop) cmp_imm_op(th, cfp, cd,			\
					   res, res_ind, op1, imm, bop,	\
					   fix_num_ ## suff, float_num_ ## suff, \
					   double_num_ ## suff, FALSE, TRUE, \
					   6, 0, BIN(f ## suff ## f))

/* Definitions of changing comparison insns with flonum immediate.  */
op2i_fun(gef) {return cmp_flo_call(ge, BOP_GE);}
op2i_fun(eqf) {return cmp_flo_call(eq, BOP_EQ);}
op2i_fun(nef) {return cmp_flo_call(ne, BOP_NEQ);}
op2i_fun(ltf) {return cmp_flo_call(lt, BOP_LT);}
op2i_fun(gtf) {return cmp_flo_call(gt, BOP_GT);}
op2i_fun(lef) {return cmp_flo_call(le, BOP_LE);}

/* The same as ucmp_fix_call but for immediate of flonum type.  */
#define ucmp_flo_call(suff, bop) cmp_imm_op(th, cfp, cd, \
					    res, res_ind, op1, imm, bop, \
					    fix_num_ ## suff, float_num_ ## suff, \
					    double_num_ ## suff, FALSE, TRUE, \
					    0, 0, 0)

/* Definitions of unchanging comparison insns with flonum immediate.  */
op2i_fun(ugef) {return ucmp_flo_call(ge, BOP_GE);}
op2i_fun(ueqf) {return ucmp_flo_call(eq, BOP_EQ);}
op2i_fun(unef) {return ucmp_flo_call(ne, BOP_NEQ);}
op2i_fun(ultf) {return ucmp_flo_call(lt, BOP_LT);}
op2i_fun(ugtf) {return ucmp_flo_call(gt, BOP_GT);}
op2i_fun(ulef) {return ucmp_flo_call(le, BOP_LE);}

/* Common function used to implement a speculative comparison BOP of
   OP1 and OP2 guessing that the both operands are fix nums.
   FIX_NUM_CMP is a fast execution path function for fix nums.
   OP2_FIX_NUM_P is TRUE when we know for sure that OP2 is a fix num.
   Return the comparison result or Qundef if our guess was wrong.  */
static do_inline VALUE
common_spec_fix_cmp(VALUE op1, VALUE op2, enum ruby_basic_operators bop,
		    int (*fix_num_cmp)(VALUE, VALUE), int op2_fixnum_p)
{
    if (LIKELY((! mjit_bop_redefined_p || BASIC_OP_UNREDEFINED_P(bop, INTEGER_REDEFINED_OP_FLAG))
	       && ((op2_fixnum_p && FIXNUM_P(op1))
		   || (! op2_fixnum_p && FIXNUM_2_P(op1, op2)))))
	return fix_num_cmp(op1, op2) ? Qtrue : Qfalse;
    return Qundef;
}

/* As above but when we guess that the both operands are flo nums.  */
static do_inline VALUE
common_spec_flo_cmp(VALUE op1, VALUE op2, enum ruby_basic_operators bop,
		    int (*float_num_cmp)(VALUE, VALUE), int op2_flonum_p)
{
    if (LIKELY((! mjit_bop_redefined_p || BASIC_OP_UNREDEFINED_P(bop, FLOAT_REDEFINED_OP_FLAG))
	       && (op2_flonum_p && FLONUM_P(op1)
		   || (! op2_flonum_p && FLONUM_2_P(op1, op2)))))
	return float_num_cmp(op1, op2) ? Qtrue : Qfalse;
    return Qundef;
}

/* Do speculative comparison, assign the result to local or temporary
   variable in frame CFP with location RES and index RES_IND, and
   return FALSE.  If the speculation was wrong, change the insn to
   UINSN and return TRUE.  In later case, the modified insn should be
   re-executed.  */
static do_inline int
do_spec_fix_cmp(rb_control_frame_t *cfp, VALUE *res, rindex_t res_ind, VALUE *op1, VALUE op2,
		enum ruby_basic_operators bop,
		int (*fix_num_cmp)(VALUE, VALUE), int op2_fixnum_p, int uinsn)
{
    VALUE val = common_spec_fix_cmp(*op1, op2, bop, fix_num_cmp, op2_fixnum_p);

    if (val == Qundef) {
	vm_change_insn(cfp->iseq, cfp->pc - 6, uinsn);
	return TRUE;
    }
    var_assign(cfp, res, res_ind, val);
    return FALSE;
}

/* As above but when we guess that the both operands are flo nums.  */
static do_inline int
do_spec_flo_cmp(rb_control_frame_t *cfp, VALUE *res, rindex_t res_ind, VALUE *op1, VALUE op2,
		enum ruby_basic_operators bop,
		int (*float_num_cmp)(VALUE, VALUE), int op2_flonum_p, int uinsn)
{
    VALUE val = common_spec_flo_cmp(*op1, op2, bop, float_num_cmp, op2_flonum_p);

    if (val == Qundef) {
	vm_change_insn(cfp->iseq, cfp->pc - 6, uinsn);
	return TRUE;
    }
    var_assign(cfp, res, res_ind, val);
    return FALSE;
}

/* It is basically do_spec_fix_cmp when type OP2 is not known
   (non-immediate operand).  */
static do_inline int
spec_fix_cmp_op(rb_control_frame_t *cfp,
		VALUE *res, rindex_t res_ind, VALUE *op1, VALUE *op2,
		enum ruby_basic_operators bop,
		int (*fix_num_cmp)(VALUE, VALUE), int uinsn)
{
    return do_spec_fix_cmp(cfp, res, res_ind, op1, *op2, bop, fix_num_cmp, FALSE, uinsn);
}

/* As above but when we guess that the both operands are flo nums.  */
static do_inline int
spec_flo_cmp_op(rb_control_frame_t *cfp,
		VALUE *res, rindex_t res_ind, VALUE *op1, VALUE *op2,
		enum ruby_basic_operators bop,
		int (*float_num_cmp)(VALUE, VALUE), int uinsn)
{
  return do_spec_flo_cmp(cfp, res, res_ind, op1, *op2, bop, float_num_cmp, FALSE, uinsn);
}

/* Header of a function executing 2 operand insn NAME with unknown type of OP2.  */
#define spec_op2_fun(name) static do_inline int				\
                           name ## _f(rb_control_frame_t *cfp,				\
				      VALUE *res, rindex_t res_ind, VALUE *op1, VALUE *op2)

/* A function call implementing a speculative fix num comparison insn
   with operation BOP and suffix SUFF for fast path execution
   function. */
#define spec_fix_cmp_call(suff, bop) spec_fix_cmp_op(cfp,		\
						     res, res_ind, op1, op2, bop,\
						     fix_num_ ## suff, \
						     BIN(u ## suff))

/* Definitions of the functions implementing speculative fix num
   comparison insns.  */
spec_op2_fun(ieq) {return spec_fix_cmp_call(eq, BOP_EQ);}
spec_op2_fun(ine) {return spec_fix_cmp_call(ne, BOP_NEQ);}
spec_op2_fun(ilt) {return spec_fix_cmp_call(lt, BOP_LT);}
spec_op2_fun(igt) {return spec_fix_cmp_call(gt, BOP_GT);}
spec_op2_fun(ile) {return spec_fix_cmp_call(le, BOP_LE);}
spec_op2_fun(ige) {return spec_fix_cmp_call(ge, BOP_GE);}

/* As spec_fix_cmp_call but a flo num variant. */
#define spec_flo_cmp_call(suff, bop) spec_flo_cmp_op(cfp,		\
						     res, res_ind, op1, op2, bop,\
						     float_num_ ## suff, \
						     BIN(u ## suff))

/* Definitions of the functions implementing speculative flo num
   comparison insns.  */
spec_op2_fun(feq) {return spec_flo_cmp_call(eq, BOP_EQ);}
spec_op2_fun(fne) {return spec_flo_cmp_call(ne, BOP_NEQ);}
spec_op2_fun(flt) {return spec_flo_cmp_call(lt, BOP_LT);}
spec_op2_fun(fgt) {return spec_flo_cmp_call(gt, BOP_GT);}
spec_op2_fun(fle) {return spec_flo_cmp_call(le, BOP_LE);}
spec_op2_fun(fge) {return spec_flo_cmp_call(ge, BOP_GE);}

/* It is basically do_spec_fix_cmp when fix num IMM (immediate
   operand).  */
static do_inline int
spec_fix_cmp_imm_op(rb_control_frame_t *cfp,
		    VALUE *res, rindex_t res_ind, VALUE *op1, VALUE imm,
		    enum ruby_basic_operators bop,
		    int (*fix_num_cmp)(VALUE, VALUE), int fixnum_p, int uinsn)
{
  return do_spec_fix_cmp(cfp, res, res_ind, op1, imm, bop, fix_num_cmp, fixnum_p, uinsn);
}

/* As above but flo num variant.  */
static do_inline int
spec_flo_cmp_imm_op(rb_control_frame_t *cfp,
		    VALUE *res, rindex_t res_ind, VALUE *op1, VALUE imm,
		    enum ruby_basic_operators bop,
		    int (*flo_num_cmp)(VALUE, VALUE), int flonum_p, int uinsn)
{
  return do_spec_flo_cmp(cfp, res, res_ind, op1, imm, bop, flo_num_cmp, flonum_p, uinsn);
}

/* Header of a function executing 2 operand insn NAME with an
   immediate value as the 2nd operand.  */
#define spec_op2i_fun(name) static do_inline int	     \
                            name ## _f(rb_control_frame_t *cfp,	\
				       VALUE *res, rindex_t res_ind, VALUE *op1, VALUE imm)

/* A function call implementing a speculative fix num comparison insn
   with an immediate value as the 2nd operand.  */
#define spec_fix_cmp_imm_call(suff, bop) spec_fix_cmp_imm_op(cfp, \
							     res, res_ind, op1, imm, bop, \
							     fix_num_ ## suff, TRUE, \
							     BIN(u ## suff ## i))

/* Definitions of the functions implementing speculative fix num
   comparison insns with a fix num as an immediate 2nd operand.  */
spec_op2i_fun(ieqi) {return spec_fix_cmp_imm_call(eq, BOP_EQ);}
spec_op2i_fun(inei) {return spec_fix_cmp_imm_call(ne, BOP_NEQ);}
spec_op2i_fun(ilti) {return spec_fix_cmp_imm_call(lt, BOP_LT);}
spec_op2i_fun(igti) {return spec_fix_cmp_imm_call(gt, BOP_GT);}
spec_op2i_fun(ilei) {return spec_fix_cmp_imm_call(le, BOP_LE);}
spec_op2i_fun(igei) {return spec_fix_cmp_imm_call(ge, BOP_GE);}

/* As spec_fix_imm_cmp_call but a flo num variant. */
#define spec_flo_cmp_imm_call(suff, bop) spec_flo_cmp_imm_op(cfp, \
							     res, res_ind, op1, imm, bop, \
							     float_num_ ## suff, TRUE, \
							     BIN(u ## suff ## f))

/* Definitions of the functions implementing speculative flo num
   comparison insns with a flo num as an immediate 2nd operand.  */
spec_op2i_fun(feqf) {return spec_flo_cmp_imm_call(eq, BOP_EQ);}
spec_op2i_fun(fnef) {return spec_flo_cmp_imm_call(ne, BOP_NEQ);}
spec_op2i_fun(fltf) {return spec_flo_cmp_imm_call(lt, BOP_LT);}
spec_op2i_fun(fgtf) {return spec_flo_cmp_imm_call(gt, BOP_GT);}
spec_op2i_fun(flef) {return spec_flo_cmp_imm_call(le, BOP_LE);}
spec_op2i_fun(fgef) {return spec_flo_cmp_imm_call(ge, BOP_GE);}

/* It is a part of goto insn.  */
static do_inline void
goto_f(rb_thread_t *th, rb_control_frame_t *cfp) {
    check_sp_default(cfp);
    /* Attempt to thread switching.  */
    RUBY_VM_CHECK_INTS(th);
}

/* It is a part of bf (branch on false) insn.  Return a flag to make a
   jump depending on value of temporary or local variable with
   location OP in frame CFP.  */
static do_inline int
bf_f(rb_thread_t *th, rb_control_frame_t *cfp, VALUE *op) {
    VALUE val = *op;

    if (!RTEST(val)) {
	check_sp_default(cfp);
	RUBY_VM_CHECK_INTS(th);
	return TRUE;
    }
    return FALSE;
}

/* Analogous to above but implements a part of bt (branch on true)
   insn.  */
static do_inline int
bt_f(rb_thread_t *th, rb_control_frame_t *cfp, VALUE *op) {
    VALUE val = *op;

    if (RTEST(val)) {
	check_sp_default(cfp);
	RUBY_VM_CHECK_INTS(th);
	return TRUE;
    }
    return FALSE;
}

/* Analogous to above but implements a part of bnil (branch on nil)
   insn.  */
static do_inline int
bnil_f(rb_thread_t *th, rb_control_frame_t *cfp, VALUE *op) {
    VALUE val = *op;

    if (NIL_P(val)) {
	check_sp_default(cfp);
	RUBY_VM_CHECK_INTS(th);
	return TRUE;
    }
    return FALSE;
}

/* Make BOP comparison of local or temporary variable with location
   OP1 and value OP2.  Use fast execution path comparison functions
   FIX_NUM_CMP, FLOAT_NUM_CMP, and DOUBLE_NUM_CMP, otherwise use a
   method call given by call data CD.  Put the comparison result into
   local or temporary variable with location RES and index RES_IND in
   frame CFP of thread TH and pass it also through VAL.  Undefined VAL
   in JITed code says to cancel the current JITed code.  OP2_FIX_NUM_P
   and OP2_FLO_NUM_P are flags of that OP2 is correspondingly fix or
   flo num.  In case of non-zero OFFSET and using a fast execution
   path, change the insn to its speculative variant FIX_INSN_ID or
   FLO_NUM_ID.  Return a flag to make a jump depending on the
   comparison result and its treatment given by TRUE_P. */
static do_inline int
do_bcmp(rb_thread_t *th, rb_control_frame_t *cfp,
	CALL_DATA cd, VALUE *res, rindex_t res_ind, VALUE *op1, VALUE op2,
	VALUE *val,
	int true_p, enum ruby_basic_operators bop,
	int (*fix_num_cmp)(VALUE, VALUE),
	int (*float_num_cmp)(VALUE, VALUE),
	int (*double_num_cmp)(VALUE, VALUE),
	int op2_fixnum_p, int op2_flonum_p,
	int offset, int fix_insn_id, int flo_insn_id)
{
    VALUE v = common_cmp(th, cfp, cd, op1, op2, bop,
			 fix_num_cmp, float_num_cmp, double_num_cmp,
			 op2_fixnum_p, op2_flonum_p,
			 offset, fix_insn_id, flo_insn_id);
    if (! in_mjit_p) {
	*val = v;
	if (v == Qundef)
	    return FALSE;
	var_assign(cfp, res, res_ind, v);
	return true_p ? RTEST(v) : ! RTEST(v);
    }
    if (v == Qundef) {
	v = mjit_vm_exec(th);
    }
    if ((cfp->ep[VM_ENV_DATA_INDEX_FLAGS] & VM_FRAME_FLAG_CANCEL) == 0)
	*val = v;
    else
	*val = Qundef;
    var_assign(cfp, res, res_ind, v);
    return true_p ? RTEST(v) : ! RTEST(v);
}

/* It is a call of do_bcmp when we don't know OP2 type.  */
static do_inline int
bcmp_op(rb_thread_t *th, rb_control_frame_t *cfp,
	CALL_DATA cd, VALUE *res, rindex_t res_ind, VALUE *op1, VALUE *op2,
	VALUE *val,
	int true_p, enum ruby_basic_operators bop,
	int (*fix_num_cmp)(VALUE, VALUE),
	int (*float_num_cmp)(VALUE, VALUE),
	int (*double_num_cmp)(VALUE, VALUE),
	int offset, int fix_insn_id, int flo_insn_id)
{
    VALUE *src1 = op1, src2 = *op2;

    return do_bcmp(th, cfp, cd, res, res_ind, src1, src2, val, true_p, bop,
		   fix_num_cmp, float_num_cmp, double_num_cmp,
		   FALSE, FALSE, offset, fix_insn_id, flo_insn_id);
}

/* Header of a function executing branch and compare insn NAME with
   unknown type of OP2.  */
#define bcmp_fun(name) static do_inline int \
                       name ## _f(rb_thread_t *th, \
				  rb_control_frame_t *cfp,	\
				  CALL_DATA cd, \
				  VALUE *res, rindex_t res_ind, VALUE *op1, VALUE *op2, \
				  VALUE *val)

/* A function call implementing a comparison and branch insn (which
   can change the insn to a speculative variant) with operation BOP,
   suffix SUFF for fast execution path functions, and flag TRUE_P to
   treat the comparison result for the branch.  */
#define bcmp_call(suff, true_p, bop)				\
  bcmp_op(th, cfp, cd, res, res_ind, op1, op2, val,	\
	  true_p, bop, fix_num_ ## suff, float_num_ ## suff,	\
	  double_num_ ## suff, 7, \
	  (true_p) ? BIN(ibt ## suff) : BIN(ibf ## suff),		\
	  (true_p) ? BIN(fbt ## suff) : BIN(fbf ## suff))

/* Definitions of the functions implementing comparison and branch
   insns which can be transformed to a speculative variant.  */
bcmp_fun(bteq) {return bcmp_call(eq, TRUE, BOP_EQ);}
bcmp_fun(bfeq) {return bcmp_call(eq, FALSE, BOP_EQ);}
bcmp_fun(btne) {return bcmp_call(ne, TRUE, BOP_NEQ);}
bcmp_fun(bfne) {return bcmp_call(ne, FALSE, BOP_NEQ);}
bcmp_fun(btlt) {return bcmp_call(lt, TRUE, BOP_LT);}
bcmp_fun(bflt) {return bcmp_call(lt, FALSE, BOP_LT);}
bcmp_fun(btgt) {return bcmp_call(gt, TRUE, BOP_GT);}
bcmp_fun(bfgt) {return bcmp_call(gt, FALSE, BOP_GT);}
bcmp_fun(btle) {return bcmp_call(le, TRUE, BOP_LE);}
bcmp_fun(bfle) {return bcmp_call(le, FALSE, BOP_LE);}
bcmp_fun(btge) {return bcmp_call(ge, TRUE, BOP_GE);}
bcmp_fun(bfge) {return bcmp_call(ge, FALSE, BOP_GE);}

/* Analogous to bcmp_call but for insns which can not be changed. */
#define ubcmp_call(suff, true_p, bop) bcmp_op(th, cfp, cd,	\
					      res, res_ind, op1, op2, val, \
					      true_p, bop,		\
					      fix_num_ ## suff,		\
					      float_num_ ## suff,	\
					      double_num_ ## suff,	\
					      0, 0, 0)

/* Definitions of the functions implementing comparison and branch
   insns which can not be changed.  */
bcmp_fun(ubteq) {return ubcmp_call(eq, TRUE, BOP_EQ);}
bcmp_fun(ubfeq) {return ubcmp_call(eq, FALSE, BOP_EQ);}
bcmp_fun(ubtne) {return ubcmp_call(ne, TRUE, BOP_NEQ);}
bcmp_fun(ubfne) {return ubcmp_call(ne, FALSE, BOP_NEQ);}
bcmp_fun(ubtlt) {return ubcmp_call(lt, TRUE, BOP_LT);}
bcmp_fun(ubflt) {return ubcmp_call(lt, FALSE, BOP_LT);}
bcmp_fun(ubtgt) {return ubcmp_call(gt, TRUE, BOP_GT);}
bcmp_fun(ubfgt) {return ubcmp_call(gt, FALSE, BOP_GT);}
bcmp_fun(ubtle) {return ubcmp_call(le, TRUE, BOP_LE);}
bcmp_fun(ubfle) {return ubcmp_call(le, FALSE, BOP_LE);}
bcmp_fun(ubtge) {return ubcmp_call(ge, TRUE, BOP_GE);}
bcmp_fun(ubfge) {return ubcmp_call(ge, FALSE, BOP_GE);}

/* It is a call of do_bcmp when we know IMM type (fix or flo num).  */
static do_inline int
bcmp_imm_op(rb_thread_t *th, rb_control_frame_t *cfp,
	    CALL_DATA cd, VALUE *res, rindex_t res_ind, VALUE *op1, VALUE imm,
	    VALUE *val,
	    int true_p, enum ruby_basic_operators bop,
	    int (*fix_num_cmp)(VALUE, VALUE),
	    int (*float_num_cmp)(VALUE, VALUE),
	    int (*double_num_cmp)(VALUE, VALUE),
	    int fixnum_p, int flonum_p,
	    int offset, int fix_insn_id, int flo_insn_id)
{
    VALUE *src1 = op1;

    return do_bcmp(th, cfp, cd, res, res_ind, src1, imm, val, true_p, bop,
		   fix_num_cmp, float_num_cmp, double_num_cmp, fixnum_p,
		   flonum_p, offset, fix_insn_id, flo_insn_id);
}

/* Header of a function executing branch and compare insn NAME with
   known type of IMM.  */
#define bcmpi_fun(name) static do_inline int \
                        name ## _f(rb_thread_t *th, \
				   rb_control_frame_t *cfp, \
				   CALL_DATA cd,	\
				   VALUE *res, rindex_t res_ind, VALUE *op1, VALUE imm, \
				   VALUE *val)

/* Analogous to bcmp_call but with known the 2nd operand type (fix num
   immediate).  */
#define bcmp_fix_call(suff, true_p, bop)			 \
    bcmp_imm_op(th, cfp, cd, res, res_ind, op1, imm, val,	 \
		true_p, bop, fix_num_ ## suff, float_num_ ## suff,	\
		double_num_ ## suff, TRUE, FALSE, 7, \
		(true_p) ? BIN(ibt ## suff ## i) : BIN(ibf ## suff ## i),	\
		(true_p) ? BIN(fbt ## suff ## f) : BIN(fbf ## suff ## f))

/* Definitions of the functions implementing comparison and branch
   insns with fix num immediate which can be transformed to a
   speculative variant.  */
bcmpi_fun(bteqi) {return bcmp_fix_call(eq, TRUE, BOP_EQ);}
bcmpi_fun(bfeqi) {return bcmp_fix_call(eq, FALSE, BOP_EQ);}
bcmpi_fun(btnei) {return bcmp_fix_call(ne, TRUE, BOP_NEQ);}
bcmpi_fun(bfnei) {return bcmp_fix_call(ne, FALSE, BOP_NEQ);}
bcmpi_fun(btlti) {return bcmp_fix_call(lt, TRUE, BOP_LT);}
bcmpi_fun(bflti) {return bcmp_fix_call(lt, FALSE, BOP_LT);}
bcmpi_fun(btgti) {return bcmp_fix_call(gt, TRUE, BOP_GT);}
bcmpi_fun(bfgti) {return bcmp_fix_call(gt, FALSE, BOP_GT);}
bcmpi_fun(btlei) {return bcmp_fix_call(le, TRUE, BOP_LE);}
bcmpi_fun(bflei) {return bcmp_fix_call(le, FALSE, BOP_LE);}
bcmpi_fun(btgei) {return bcmp_fix_call(ge, TRUE, BOP_GE);}
bcmpi_fun(bfgei) {return bcmp_fix_call(ge, FALSE, BOP_GE);}

/* Analogous to bcmp_fix_call but for insns which can not be
   changed. */
#define ubcmp_fix_call(suff, true_p, bop) bcmp_imm_op(th, cfp, cd, \
						      res, res_ind, op1, imm, val, \
						      true_p, bop,	\
						      fix_num_ ## suff,	\
						      float_num_ ## suff, \
						      double_num_ ## suff, TRUE, FALSE, \
						      0, 0, 0)

/* Definitions of the functions implementing non-changing comparison
   and branch insns with immediate fix num as the 2nd operand.  */
bcmpi_fun(ubteqi) {return ubcmp_fix_call(eq, TRUE, BOP_EQ);}
bcmpi_fun(ubfeqi) {return ubcmp_fix_call(eq, FALSE, BOP_EQ);}
bcmpi_fun(ubtnei) {return ubcmp_fix_call(ne, TRUE, BOP_NEQ);}
bcmpi_fun(ubfnei) {return ubcmp_fix_call(ne, FALSE, BOP_NEQ);}
bcmpi_fun(ubtlti) {return ubcmp_fix_call(lt, TRUE, BOP_LT);}
bcmpi_fun(ubflti) {return ubcmp_fix_call(lt, FALSE, BOP_LT);}
bcmpi_fun(ubtgti) {return ubcmp_fix_call(gt, TRUE, BOP_GT);}
bcmpi_fun(ubfgti) {return ubcmp_fix_call(gt, FALSE, BOP_GT);}
bcmpi_fun(ubtlei) {return ubcmp_fix_call(le, TRUE, BOP_LE);}
bcmpi_fun(ubflei) {return ubcmp_fix_call(le, FALSE, BOP_LE);}
bcmpi_fun(ubtgei) {return ubcmp_fix_call(ge, TRUE, BOP_GE);}
bcmpi_fun(ubfgei) {return ubcmp_fix_call(ge, FALSE, BOP_GE);}

/* Analogous to bcmp_fix_call but for flo num immediate.  */
#define bcmp_flo_call(suff, true_p, bop)			\
  bcmp_imm_op(th, cfp, cd, res, res_ind, op1, imm, val,	 \
	      true_p, bop, fix_num_ ## suff, float_num_ ## suff, \
	      double_num_ ## suff, FALSE, TRUE,	 7, \
	      (true_p) ? BIN(ibt ## suff ## i) : BIN(ibf ## suff ## i),	\
	      (true_p) ? BIN(fbt ## suff ## f) : BIN(fbf ## suff ## f))

/* Definitions of the functions implementing comparison and branch
   insns with immediate flo num as the 2nd operand.  The insn can be
   changed to a speculative variant.  */
bcmpi_fun(bteqf) {return bcmp_flo_call(eq, TRUE, BOP_EQ);}
bcmpi_fun(bfeqf) {return bcmp_flo_call(eq, FALSE, BOP_EQ);}
bcmpi_fun(btnef) {return bcmp_flo_call(ne, TRUE, BOP_NEQ);}
bcmpi_fun(bfnef) {return bcmp_flo_call(ne, FALSE, BOP_NEQ);}
bcmpi_fun(btltf) {return bcmp_flo_call(lt, TRUE, BOP_LT);}
bcmpi_fun(bfltf) {return bcmp_flo_call(lt, FALSE, BOP_LT);}
bcmpi_fun(btgtf) {return bcmp_flo_call(gt, TRUE, BOP_GT);}
bcmpi_fun(bfgtf) {return bcmp_flo_call(gt, FALSE, BOP_GT);}
bcmpi_fun(btlef) {return bcmp_flo_call(le, TRUE, BOP_LE);}
bcmpi_fun(bflef) {return bcmp_flo_call(le, FALSE, BOP_LE);}
bcmpi_fun(btgef) {return bcmp_flo_call(ge, TRUE, BOP_GE);}
bcmpi_fun(bfgef) {return bcmp_flo_call(ge, FALSE, BOP_GE);}

/* Analogous to ubcmp_fix_call but for flo num immediate.  */
#define ubcmp_flo_call(suff, true_p, bop) bcmp_imm_op(th, cfp, cd, \
						      res, res_ind, op1, imm, val, \
						      true_p, bop,	\
						      fix_num_ ## suff,	\
						      float_num_ ## suff, \
						      double_num_ ## suff, FALSE, TRUE, \
						      0, 0, 0)

/* Definitions of the functions implementing non-changing comparison
   and branch insns with immediate flo num as the 2nd operand.  */
bcmpi_fun(ubteqf) {return ubcmp_flo_call(eq, TRUE, BOP_EQ);}
bcmpi_fun(ubfeqf) {return ubcmp_flo_call(eq, FALSE, BOP_EQ);}
bcmpi_fun(ubtnef) {return ubcmp_flo_call(ne, TRUE, BOP_NEQ);}
bcmpi_fun(ubfnef) {return ubcmp_flo_call(ne, FALSE, BOP_NEQ);}
bcmpi_fun(ubtltf) {return ubcmp_flo_call(lt, TRUE, BOP_LT);}
bcmpi_fun(ubfltf) {return ubcmp_flo_call(lt, FALSE, BOP_LT);}
bcmpi_fun(ubtgtf) {return ubcmp_flo_call(gt, TRUE, BOP_GT);}
bcmpi_fun(ubfgtf) {return ubcmp_flo_call(gt, FALSE, BOP_GT);}
bcmpi_fun(ubtlef) {return ubcmp_flo_call(le, TRUE, BOP_LE);}
bcmpi_fun(ubflef) {return ubcmp_flo_call(le, FALSE, BOP_LE);}
bcmpi_fun(ubtgef) {return ubcmp_flo_call(ge, TRUE, BOP_GE);}
bcmpi_fun(ubfgef) {return ubcmp_flo_call(ge, FALSE, BOP_GE);}

/* Header of a function executing most code of speculative comparison
   and branch insn NAME with unknown type of OP2.  */
#define spec_bcmp_fun(name) static do_inline int			\
                            name ## _f(rb_control_frame_t *cfp,			\
				       VALUE *res, rindex_t res_ind, VALUE *op1, VALUE *op2, \
				       VALUE *val)

/* If V is Qundef, change the insn to UINSN, remote back PC to
   re-execute the changed insn, and return FALSE.  Otherwise, assign V
   to local or temporary variable with location RES and index RES_IND
   in frame CFP and return flag to jump depending on V and TRUE_P.  */
static do_inline int
spec_bcmp_finish(rb_control_frame_t *cfp, VALUE *res, rindex_t res_ind, VALUE v, int true_p, int uinsn)
{
    if (v != Qundef) {
	var_assign(cfp, res, res_ind, v);
	return true_p ? RTEST(v) : ! RTEST(v);
    }
    vm_change_insn(cfp->iseq, cfp->pc - 7, uinsn);
    return FALSE;
}

/* Analogous to do_bcmp but it is a speculative variant when we guessing
   that the comparison operands are fix nums (we know it for sure for
   2nd operand if OP2_FIX_NUM_P).  */
static do_inline int
do_spec_fix_bcmp(rb_control_frame_t *cfp, VALUE *res, rindex_t res_ind, VALUE *op1, VALUE op2,
		 VALUE *val, int true_p, enum ruby_basic_operators bop,
		 int (*fix_num_cmp)(VALUE, VALUE), int op2_fixnum_p, int uinsn)
{
    VALUE v;

    *val = v = common_spec_fix_cmp(*op1, op2, bop, fix_num_cmp, op2_fixnum_p);
    return spec_bcmp_finish(cfp, res, res_ind, v, true_p, uinsn);
}

/* Analogous to do_spec_fix_bcmp but with guessing that the operands are flo nums.  */
static do_inline int
do_spec_flo_bcmp(rb_control_frame_t *cfp, VALUE *res, rindex_t res_ind, VALUE *op1, VALUE op2,
		 VALUE *val, int true_p, enum ruby_basic_operators bop,
		 int (*float_num_cmp)(VALUE, VALUE), int op2_flonum_p, int uinsn)
{
    VALUE v;

    *val = v = common_spec_flo_cmp(*op1, op2, bop, float_num_cmp, op2_flonum_p);
    return spec_bcmp_finish(cfp, res, res_ind, v, true_p, uinsn);
}

/* It is just a call of do_spec_fix_bcmp when we are not sure about
   the 2nd operand type.  */
static do_inline int
spec_fix_bcmp_op(rb_control_frame_t *cfp,
		 VALUE *res, rindex_t res_ind, VALUE *op1, VALUE *op2,
		 VALUE *val, int true_p, enum ruby_basic_operators bop,
		 int (*fix_num_cmp)(VALUE, VALUE), int uinsn)
{
    return do_spec_fix_bcmp(cfp, res, res_ind, op1, *op2, val, true_p, bop, fix_num_cmp, FALSE, uinsn);
}

/* It is just a call of do_spec_flo_bcmp when we are not sure about
   the 2nd operand type.  */
static do_inline int
spec_flo_bcmp_op(rb_control_frame_t *cfp,
		 VALUE *res, rindex_t res_ind, VALUE *op1, VALUE *op2,
		 VALUE *val, int true_p, enum ruby_basic_operators bop,
		 int (*float_num_cmp)(VALUE, VALUE), int uinsn)
{
    return do_spec_flo_bcmp(cfp, res, res_ind, op1, *op2, val, true_p, bop, float_num_cmp, FALSE, uinsn);
}

/* A function call implementing a speculative comparison and branch
   insn when we are guessing that the operands are fix nums.  */
#define spec_fix_bcmp_call(suff, true_p, bop) \
    spec_fix_bcmp_op(cfp, res, res_ind, op1, op2, val, true_p, bop, fix_num_ ## suff, \
		     (true_p) ? BIN(ubt ## suff) : BIN(ubf ## suff))

/* Definitions of the functions implementing speculative fix num
   comparison and branch insns.  */
spec_bcmp_fun(ibteq) {return spec_fix_bcmp_call(eq, TRUE, BOP_EQ);}
spec_bcmp_fun(ibfeq) {return spec_fix_bcmp_call(eq, FALSE, BOP_EQ);}
spec_bcmp_fun(ibtne) {return spec_fix_bcmp_call(ne, TRUE, BOP_NEQ);}
spec_bcmp_fun(ibfne) {return spec_fix_bcmp_call(ne, FALSE, BOP_NEQ);}
spec_bcmp_fun(ibtlt) {return spec_fix_bcmp_call(lt, TRUE, BOP_LT);}
spec_bcmp_fun(ibflt) {return spec_fix_bcmp_call(lt, FALSE, BOP_LT);}
spec_bcmp_fun(ibtgt) {return spec_fix_bcmp_call(gt, TRUE, BOP_GT);}
spec_bcmp_fun(ibfgt) {return spec_fix_bcmp_call(gt, FALSE, BOP_GT);}
spec_bcmp_fun(ibtle) {return spec_fix_bcmp_call(le, TRUE, BOP_LE);}
spec_bcmp_fun(ibfle) {return spec_fix_bcmp_call(le, FALSE, BOP_LE);}
spec_bcmp_fun(ibtge) {return spec_fix_bcmp_call(ge, TRUE, BOP_GE);}
spec_bcmp_fun(ibfge) {return spec_fix_bcmp_call(ge, FALSE, BOP_GE);}

/* Analogous to spec_fix_bcmp_call but for flo num speculation.  */
#define spec_flo_bcmp_call(suff, true_p, bop)				\
    spec_flo_bcmp_op(cfp, res, res_ind, op1, op2, val, true_p, bop, float_num_ ## suff, \
		     (true_p) ? BIN(ubt ## suff) : BIN(ubf ## suff))

/* Definitions of the functions implementing speculative flo num
   comparison and branch insns.  */
spec_bcmp_fun(fbteq) {return spec_flo_bcmp_call(eq, TRUE, BOP_EQ);}
spec_bcmp_fun(fbfeq) {return spec_flo_bcmp_call(eq, FALSE, BOP_EQ);}
spec_bcmp_fun(fbtne) {return spec_flo_bcmp_call(ne, TRUE, BOP_NEQ);}
spec_bcmp_fun(fbfne) {return spec_flo_bcmp_call(ne, FALSE, BOP_NEQ);}
spec_bcmp_fun(fbtlt) {return spec_flo_bcmp_call(lt, TRUE, BOP_LT);}
spec_bcmp_fun(fbflt) {return spec_flo_bcmp_call(lt, FALSE, BOP_LT);}
spec_bcmp_fun(fbtgt) {return spec_flo_bcmp_call(gt, TRUE, BOP_GT);}
spec_bcmp_fun(fbfgt) {return spec_flo_bcmp_call(gt, FALSE, BOP_GT);}
spec_bcmp_fun(fbtle) {return spec_flo_bcmp_call(le, TRUE, BOP_LE);}
spec_bcmp_fun(fbfle) {return spec_flo_bcmp_call(le, FALSE, BOP_LE);}
spec_bcmp_fun(fbtge) {return spec_flo_bcmp_call(ge, TRUE, BOP_GE);}
spec_bcmp_fun(fbfge) {return spec_flo_bcmp_call(ge, FALSE, BOP_GE);}

#define spec_bcmpi_fun(name) static do_inline int			\
                             name ## _f(rb_control_frame_t *cfp,					\
				       VALUE *res, rindex_t res_ind, VALUE *op1, VALUE imm, \
				       VALUE *val)

/* It is just a call of do_spec_fix_bcmp when we know that 2nd operand
   type is a fix num.  */
static do_inline int
spec_fix_bcmp_imm_op(rb_control_frame_t *cfp,
		     VALUE *res, rindex_t res_ind, VALUE *op1, VALUE imm,
		     VALUE *val, int true_p, enum ruby_basic_operators bop,
		     int (*fix_num_cmp)(VALUE, VALUE), int uinsn)
{
    return do_spec_fix_bcmp(cfp, res, res_ind, op1, imm, val, true_p, bop, fix_num_cmp, TRUE, uinsn);
}

/* It is just a call of do_spec_fix_bcmp when we know that 2nd operand
   type is a flo num.  */
static do_inline int
spec_flo_bcmp_imm_op(rb_control_frame_t *cfp,
		     VALUE *res, rindex_t res_ind, VALUE *op1, VALUE imm,
		     VALUE *val, int true_p, enum ruby_basic_operators bop,
		     int (*float_num_cmp)(VALUE, VALUE), int uinsn)
{
    return do_spec_flo_bcmp(cfp, res, res_ind, op1, imm, val, true_p, bop, float_num_cmp, TRUE, uinsn);
}

/* A call to implement a speculative fix num comparison and branch
   insn with fix num immediate.  */
#define spec_fix_imm_bcmp_call(suff, true_p, bop) \
    spec_fix_bcmp_imm_op(cfp, res, res_ind, op1, imm, val, true_p, bop, fix_num_ ## suff, \
			 (true_p) ? BIN(ubt ## suff ## i) : BIN(ubf ## suff ## i))

/* Definitions of the functions implementing speculative fix num
   comparison and branch insns with fix num immediat as the 2nd
   operand.  */
spec_bcmpi_fun(ibteqi) {return spec_fix_imm_bcmp_call(eq, TRUE, BOP_EQ);}
spec_bcmpi_fun(ibfeqi) {return spec_fix_imm_bcmp_call(eq, FALSE, BOP_EQ);}
spec_bcmpi_fun(ibtnei) {return spec_fix_imm_bcmp_call(ne, TRUE, BOP_NEQ);}
spec_bcmpi_fun(ibfnei) {return spec_fix_imm_bcmp_call(ne, FALSE, BOP_NEQ);}
spec_bcmpi_fun(ibtlti) {return spec_fix_imm_bcmp_call(lt, TRUE, BOP_LT);}
spec_bcmpi_fun(ibflti) {return spec_fix_imm_bcmp_call(lt, FALSE, BOP_LT);}
spec_bcmpi_fun(ibtgti) {return spec_fix_imm_bcmp_call(gt, TRUE, BOP_GT);}
spec_bcmpi_fun(ibfgti) {return spec_fix_imm_bcmp_call(gt, FALSE, BOP_GT);}
spec_bcmpi_fun(ibtlei) {return spec_fix_imm_bcmp_call(le, TRUE, BOP_LE);}
spec_bcmpi_fun(ibflei) {return spec_fix_imm_bcmp_call(le, FALSE, BOP_LE);}
spec_bcmpi_fun(ibtgei) {return spec_fix_imm_bcmp_call(ge, TRUE, BOP_GE);}
spec_bcmpi_fun(ibfgei) {return spec_fix_imm_bcmp_call(ge, FALSE, BOP_GE);}

/* A call to implement a speculative flo num comparison and branch
   insn with flo num immediate.  */
#define spec_flo_imm_bcmp_call(suff, true_p, bop)			\
    spec_flo_bcmp_imm_op(cfp, res, res_ind, op1, imm, val, true_p, bop, float_num_ ## suff, \
			 (true_p) ? BIN(ubt ## suff ## f) : BIN(ubf ## suff ## f))

/* Definitions of the functions implementing speculative flo num
   comparison and branch insns with flo num immediate as the 2nd
   operand.  */
spec_bcmpi_fun(fbteqf) {return spec_flo_imm_bcmp_call(eq, TRUE, BOP_EQ);}
spec_bcmpi_fun(fbfeqf) {return spec_flo_imm_bcmp_call(eq, FALSE, BOP_EQ);}
spec_bcmpi_fun(fbtnef) {return spec_flo_imm_bcmp_call(ne, TRUE, BOP_NEQ);}
spec_bcmpi_fun(fbfnef) {return spec_flo_imm_bcmp_call(ne, FALSE, BOP_NEQ);}
spec_bcmpi_fun(fbtltf) {return spec_flo_imm_bcmp_call(lt, TRUE, BOP_LT);}
spec_bcmpi_fun(fbfltf) {return spec_flo_imm_bcmp_call(lt, FALSE, BOP_LT);}
spec_bcmpi_fun(fbtgtf) {return spec_flo_imm_bcmp_call(gt, TRUE, BOP_GT);}
spec_bcmpi_fun(fbfgtf) {return spec_flo_imm_bcmp_call(gt, FALSE, BOP_GT);}
spec_bcmpi_fun(fbtlef) {return spec_flo_imm_bcmp_call(le, TRUE, BOP_LE);}
spec_bcmpi_fun(fbflef) {return spec_flo_imm_bcmp_call(le, FALSE, BOP_LE);}
spec_bcmpi_fun(fbtgef) {return spec_flo_imm_bcmp_call(ge, TRUE, BOP_GE);}
spec_bcmpi_fun(fbfgef) {return spec_flo_imm_bcmp_call(ge, FALSE, BOP_GE);}


/* Header of a function with NAME implementing a fast execution path
   of arithmetic two operands (OP1, OP2) operation.  */
#define arithmf(name) static do_inline VALUE name(VALUE op1, VALUE op2)

/* Definitions of the functions implementing a fast execution path of
   arithmetic two operands (OP1, OP2) operation.  */

arithmf(fix_num_plus) {
#ifndef LONG_LONG_VALUE
    VALUE msb = (VALUE)1 << ((sizeof(VALUE) * CHAR_BIT) - 1);
    VALUE val = op1 - 1 + op2;
    if ((~(op1 ^ op2) & (op1 ^ val)) & msb) {
	val = rb_int2big((SIGNED_VALUE)((val>>1) | (op1 & msb)));
    }
    return val;
#else
    return LONG2NUM(FIX2LONG(op1) + FIX2LONG(op2));
#endif
}

arithmf(float_num_plus) {
    return DBL2NUM(RFLOAT_VALUE(op1) + RFLOAT_VALUE(op2));
}

arithmf(double_num_plus) {return float_num_plus(op1, op2);}

arithmf(fix_num_minus) {
    long a, b, c;

    a = FIX2LONG(op1);
    b = FIX2LONG(op2);
    c = a - b;
    return LONG2NUM(c);
}

arithmf(float_num_minus) {
    return DBL2NUM(RFLOAT_VALUE(op1) - RFLOAT_VALUE(op2));
}

arithmf(double_num_minus) {return float_num_minus(op1, op2);}

arithmf(fix_num_mult) {
    return rb_fix_mul_fix(op1, op2);
}

arithmf(float_num_mult) {
    return DBL2NUM(RFLOAT_VALUE(op1) * RFLOAT_VALUE(op2));
}

arithmf(double_num_mult) {return float_num_mult(op1, op2);}


arithmf(fix_num_div) {
    if (FIX2LONG(op2) == 0)
	return Qundef;
    return rb_fix_div_fix(op1, op2);
}

arithmf(float_num_div) {
    return DBL2NUM(RFLOAT_VALUE(op1) / RFLOAT_VALUE(op2));
}

arithmf(double_num_div) {return float_num_div(op1, op2);}

arithmf(fix_num_mod)
{
    if (FIX2LONG(op2) == 0)
	return Qundef;
    return rb_fix_mod_fix(op1, op2);
}

arithmf(float_num_mod)
{
    return DBL2NUM(ruby_float_mod(RFLOAT_VALUE(op1), RFLOAT_VALUE(op2)));
}

arithmf(double_num_mod) {return float_num_mult(op1, op2);}


/* Common function executing an arithmetic operation of operands OP1
   (value location) and OP2 (value) in frame CFP of thread TH.  The
   function has a fast execution path for operand of type fixnum,
   float, and doubles given correspondingly by functions FIX_NUM_OP,
   FLOAT_NUM_OP, and DOUBLE_NUM_OP.  For the slow paths use call data
   CD.  If OP2_FIXNUM_P or OP2_FLONUM_P is true, it means that value
   OP2 is of the corresponding type.  If CHANGE_P is non-zero and we
   use a fast path, change the current insn to the corresponding
   speculative insn FIX_INSN_ID or FLO_INSN_ID.  Return non zero if we
   started a new ISEQ execution (we need to update vm_exec_core regs
   in this case) or we need to cancel the current JITed code..  */
static do_inline int
do_arithm(rb_thread_t *th,
	  rb_control_frame_t *cfp,
	  CALL_DATA cd, VALUE *res, rindex_t res_ind, VALUE *op1, VALUE op2,
	  enum ruby_basic_operators bop,
	  VALUE (*fix_num_op)(VALUE, VALUE),
	  VALUE (*float_num_op)(VALUE, VALUE),
	  VALUE (*double_num_op)(VALUE, VALUE),
	  int op2_fixnum_p, int op2_flonum_p,
	  int change_p, int fix_insn_id, int flo_insn_id)
{
    VALUE val;

    if (((op2_fixnum_p && FIXNUM_P(*op1))
	 || (! op2_fixnum_p && ! op2_flonum_p && FIXNUM_2_P(*op1, op2)))
	&& BASIC_OP_UNREDEFINED_P(bop, INTEGER_REDEFINED_OP_FLAG)) {
	val = fix_num_op(*op1, op2);
	if (val != Qundef || (bop != BOP_DIV && bop != BOP_MOD)) {
	    var_assign(cfp, res, res_ind, val);
	    if (change_p)
		vm_change_insn(cfp->iseq, cfp->pc - 6, fix_insn_id);
	    return 0;
	}
    } else if ((op2_flonum_p && FLONUM_P(*op1)
		|| (! op2_fixnum_p && ! op2_flonum_p && FLONUM_2_P(*op1, op2)))
	       && BASIC_OP_UNREDEFINED_P(bop, FLOAT_REDEFINED_OP_FLAG)) {
	var_assign(cfp, res, res_ind, float_num_op(*op1, op2));
	if (change_p)
	    vm_change_insn(cfp->iseq, cfp->pc - 6, flo_insn_id);
	return 0;
    }
    else if (! op2_fixnum_p && ! op2_flonum_p
	     && ! SPECIAL_CONST_P(*op1) && ! SPECIAL_CONST_P(op2)) {
	if (RBASIC_CLASS(*op1) == rb_cFloat && RBASIC_CLASS(op2) == rb_cFloat
	    && BASIC_OP_UNREDEFINED_P(bop, FLOAT_REDEFINED_OP_FLAG)) {
	    var_assign(cfp, res, res_ind, double_num_op(*op1, op2));
	    return 0;
	} else if (bop == BOP_PLUS && RBASIC_CLASS(*op1) == rb_cString
		   && RBASIC_CLASS(op2) == rb_cString
		   && BASIC_OP_UNREDEFINED_P(bop, STRING_REDEFINED_OP_FLAG)) {
	    var_assign(cfp, res, res_ind, rb_str_plus(*op1, op2));
	    return 0;
	} else if (bop == BOP_PLUS && RBASIC_CLASS(*op1) == rb_cArray
		   && BASIC_OP_UNREDEFINED_P(bop, ARRAY_REDEFINED_OP_FLAG)) {
	    var_assign(cfp, res, res_ind, rb_ary_plus(*op1, op2));
	    return 0;
	}
    }
    val = op2_call(th, cfp, cd, op1, op2);
    return op_val_call_end(th, cfp, res, res_ind, val);
}

/* It is just a call of do_arithm when we don't know the type of the
   2nd operand.  */
static do_inline int
arithm_op(rb_thread_t *th, rb_control_frame_t *cfp,
	  CALL_DATA cd, VALUE *res, rindex_t res_ind, VALUE *op1, VALUE *op2,
	  enum ruby_basic_operators bop,
	  VALUE (*fix_num_op)(VALUE, VALUE),
	  VALUE (*float_num_op)(VALUE, VALUE),
	  VALUE (*double_num_op)(VALUE, VALUE),
	  int change_p, int fix_insn_id, int flo_insn_id)
{
    VALUE *src1 = op1, src2 = *op2;

    return do_arithm(th, cfp, cd, res, res_ind, src1, src2,
		     bop, fix_num_op, float_num_op, double_num_op, FALSE, FALSE,
		     change_p, fix_insn_id, flo_insn_id);
}

/* A function call implementing an arithmetic operation (which can
   change the insn to speculative one) with operation BOP and suffix
   SUFF for fast execution path functions. */
#define arithm_call(suff, bop) arithm_op(th, cfp, cd, \
					 res, res_ind, op1, op2, bop,	\
					 fix_num_ ## suff, float_num_ ## suff, \
					 double_num_ ## suff,		\
					 TRUE, BIN(i ## suff), BIN(f ## suff))

/* Definitions of the functions implementing arithmetic insns which
   can be transformed into a speculative variant.  */
op2_fun(plus) {return arithm_call(plus, BOP_PLUS);}
op2_fun(minus) {return arithm_call(minus, BOP_MINUS);}
op2_fun(mult) {return arithm_call(mult, BOP_MULT);}
op2_fun(div) {return arithm_call(div, BOP_DIV);}
op2_fun(mod) {return arithm_call(mod, BOP_MOD);}

/* A function call implementing an arithmetic operation (which can not
   change the insn) with operation BOP and suffix SUFF for fast
   execution path functions. */
#define uarithm_call(suff, bop) arithm_op(th, cfp, cd, \
					  res, res_ind, op1, op2, bop, \
					  fix_num_ ## suff, float_num_ ## suff, \
					  double_num_ ## suff, FALSE, 0, 0)

/* Definitions of the functions implementing unchanging arithmetic
   insns.  */
op2_fun(uplus) {return uarithm_call(plus, BOP_PLUS);}
op2_fun(uminus) {return uarithm_call(minus, BOP_MINUS);}
op2_fun(umult) {return uarithm_call(mult, BOP_MULT);}
op2_fun(udiv) {return uarithm_call(div, BOP_DIV);}
op2_fun(umod) {return uarithm_call(mod, BOP_MOD);}

/* It is analogous to airthm_op with known value of the 2nd operand
   (IMM).  */
static do_inline int
arithm_imm_op(rb_thread_t *th, rb_control_frame_t *cfp,
	      CALL_DATA cd, VALUE *res, rindex_t res_ind, VALUE *op1, VALUE imm,
	      enum ruby_basic_operators bop,
	      VALUE (*fix_num_op)(VALUE, VALUE),
	      VALUE (*float_num_op)(VALUE, VALUE),
	      VALUE (*double_num_op)(VALUE, VALUE),
	      int fixnum_p, int flonum_p,
	      int change_p, int fix_insn_id, int flo_insn_id)
{
    VALUE *src1 = op1;

    return do_arithm(th, cfp, cd, res, res_ind, src1, imm,
		     bop, fix_num_op, float_num_op, double_num_op, fixnum_p, flonum_p,
		     change_p, fix_insn_id, flo_insn_id);
}

/* Analogous to arithm_call but when the 2nd operand is known to have
   fix num type.  */
#define arithm_fix_call(suff, bop) arithm_imm_op(th, cfp, cd, \
						 res, res_ind, op1, imm, bop, \
						 fix_num_ ## suff, float_num_ ## suff, \
						 double_num_ ## suff, TRUE, FALSE, \
						 TRUE, BIN(i ## suff ## i), 0)

/* Definitions of the functions implementing arithmetic insns with
   immediate of type fixnum which can be transformed to a speculative
   variant.  */
op2i_fun(plusi) {return arithm_fix_call(plus, BOP_PLUS);}
op2i_fun(minusi) {return arithm_fix_call(minus, BOP_MINUS);}
op2i_fun(multi) {return arithm_fix_call(mult, BOP_MULT);}
op2i_fun(divi) {return arithm_fix_call(div, BOP_DIV);}
op2i_fun(modi) {return arithm_fix_call(mod, BOP_MOD);}

/* A function call implementing an arithmetic insn (which can not
   change the insn) with operation BOP and suffix SUFF for fast path
   functions and immediate of type fixnum as the 2nd operand.  */
#define uarithm_fix_call(suff, bop) arithm_imm_op(th, cfp, cd, \
						  res, res_ind, op1, imm, bop, \
						  fix_num_ ## suff, float_num_ ## suff, \
						  double_num_ ## suff, TRUE, FALSE, \
						  FALSE, 0, 0)

/* Definitions of the functions implementing non-changing arithmetic
   insns with immediate of type fixnum.  */
op2i_fun(uplusi) {return uarithm_fix_call(plus, BOP_PLUS);}
op2i_fun(uminusi) {return uarithm_fix_call(minus, BOP_MINUS);}
op2i_fun(umulti) {return uarithm_fix_call(mult, BOP_MULT);}
op2i_fun(udivi) {return uarithm_fix_call(div, BOP_DIV);}
op2i_fun(umodi) {return uarithm_fix_call(mod, BOP_MOD);}

/* The same as arithm_fix_call but for immediate of flo num type.  */
#define arithm_flo_call(suff, bop) arithm_imm_op(th, cfp, cd,		\
						 res, res_ind, op1, imm, bop, \
						 fix_num_ ## suff, float_num_ ## suff, \
						 double_num_ ## suff, FALSE, TRUE, \
						 TRUE, 0, BIN(f ## suff ## f))

/* Definitions of the functions implementing arithmetic insns with
   immediate of type flo num.  The insn can be changed into a
   speculative variant.  */
op2i_fun(plusf) {return arithm_flo_call(plus, BOP_PLUS);}
op2i_fun(minusf) {return arithm_flo_call(minus, BOP_MINUS);}
op2i_fun(multf) {return arithm_flo_call(mult, BOP_MULT);}
op2i_fun(divf) {return arithm_flo_call(div, BOP_DIV);}
op2i_fun(modf) {return arithm_flo_call(mod, BOP_MOD);}

/* The same as uarithm_fix_call but for immediate of flonum type.  */
#define uarithm_flo_call(suff, bop) arithm_imm_op(th, cfp, cd, \
						  res, res_ind, op1, imm, bop, \
						  fix_num_ ## suff, float_num_ ## suff, \
						  double_num_ ## suff, FALSE, TRUE, \
						  FALSE, 0, 0)

/* Definitions of the functions implementing non-changing arithmetic
   insns with immediate of type flo num.  */
op2i_fun(uplusf) {return uarithm_flo_call(plus, BOP_PLUS);}
op2i_fun(uminusf) {return uarithm_flo_call(minus, BOP_MINUS);}
op2i_fun(umultf) {return uarithm_flo_call(mult, BOP_MULT);}
op2i_fun(udivf) {return uarithm_flo_call(div, BOP_DIV);}
op2i_fun(umodf) {return uarithm_flo_call(mod, BOP_MOD);}

/* Definitions of the functions implementing a speculative arithmetic
   operation on fix num or flo num operands.  If the speculation is
   wrong because of the operand values (not types), return Qundef.  */

arithmf(spec_fix_num_plus) {
#ifndef LONG_LONG_VALUE
    VALUE msb = (VALUE)1 << ((sizeof(VALUE) * CHAR_BIT) - 1);
    VALUE val = op1 - 1 + op2;

    if ((~(op1 ^ op2) & (op1 ^ val)) & msb)
	return Qundef;
    return val;
#else
    long v = FIX2LONG(op1) + FIX2LONG(op2);

    if (! RB_FIXABLE(v))
	return Qundef;
    return LONG2FIX(v);
#endif
}

arithmf(spec_fix_num_minus) {
    long a, b, c;

    a = FIX2LONG(op1);
    b = FIX2LONG(op2);
    c = a - b;
    if (! RB_FIXABLE(c))
	return Qundef;
    return LONG2FIX(c);
}

arithmf(spec_fix_num_mult) {
    long l1 = FIX2LONG(op1);
    long l2 = FIX2LONG(op2);

#ifdef DLONG
    DLONG v = (DLONG)l1 * (DLONG)l2;

    if (RB_FIXABLE(v))
	return LONG2FIX(v);
#else
    if (!MUL_OVERFLOW_FIXNUM_P(l1, l2))
	return LONG2FIX(l1 * l2);
#endif
    return Qundef;
}

arithmf(spec_fix_num_div) {
    long l1 = FIX2LONG(op1);
    long l2 = FIX2LONG(op2);
    long div, mod;

    if (l2 == 0)
	return Qundef;
    if (l1 == FIXNUM_MIN && l2 == -1)
	return LONG2NUM(-FIXNUM_MIN);
    div = l1 / l2;
    mod = l1 % l2;
    if (l2 > 0 ? mod < 0 : mod > 0)
	div -= 1;
    return LONG2FIX(div);
}

arithmf(spec_fix_num_mod) {
    long l1 = FIX2LONG(op1);
    long l2 = FIX2LONG(op2);
    long mod;

    if (l2 == 0)
	return Qundef;
    if (l1 == FIXNUM_MIN && l2 == -1)
	return LONG2FIX(0);
    mod = l1 % l2;
    if (l2 > 0 ? mod < 0 : mod > 0)
	mod += l2;
    return LONG2FIX(mod);
}

/* Do speculative fix num arithmetic operation, assign the result to
   local or temporary variable in frame CFP with location RES and
   index RES_IND, and return FALSE.  If the speculation was wrong,
   change the insn to UINSN and return TRUE.  In later case, the
   modified insn should be re-executed.  */
static do_inline int
do_spec_fix_arithm(rb_control_frame_t *cfp,
		   VALUE *res, rindex_t res_ind, VALUE *op1, VALUE op2,
		   enum ruby_basic_operators bop,
		   VALUE (*fix_num_op)(VALUE, VALUE),
		   int op2_fixnum_p, int uinsn)
{
    VALUE val;

    if (LIKELY((! mjit_bop_redefined_p || BASIC_OP_UNREDEFINED_P(bop, INTEGER_REDEFINED_OP_FLAG))
	       && ((op2_fixnum_p && FIXNUM_P(*op1))
		   || (! op2_fixnum_p && FIXNUM_2_P(*op1, op2))))) {
	val = fix_num_op(*op1, op2);
	if (val != Qundef) {
	    var_assign(cfp, res, res_ind, val);
	    return FALSE;
	}
    }
    vm_change_insn(cfp->iseq, cfp->pc - 6, uinsn);
    return TRUE;
}

/* Analogous to do_spec_fix_arithm but for flo num operand
   speculation.  */
static do_inline int
do_spec_flo_arithm(rb_control_frame_t *cfp,
		   VALUE *res, lindex_t res_ind, VALUE *op1, VALUE op2,
		   enum ruby_basic_operators bop,
		   VALUE (*float_num_op)(VALUE, VALUE),
		   int op2_flonum_p, int uinsn)
{
    if (LIKELY((! mjit_bop_redefined_p || BASIC_OP_UNREDEFINED_P(bop, FLOAT_REDEFINED_OP_FLAG))
	       && ((op2_flonum_p && FLONUM_P(*op1))
		   || (! op2_flonum_p && FLONUM_2_P(*op1, op2))))) {
	var_assign(cfp, res, res_ind, float_num_op(*op1, op2));
	return FALSE;
    }
    vm_change_insn(cfp->iseq, cfp->pc - 6, uinsn);
    return TRUE;
}

/* It is basically do_spec_fix_airthm when type OP2 is not known for
   sure (non-immediate operand).  */
static do_inline int
spec_fix_arithm_op(rb_control_frame_t *cfp,
		   VALUE *res, rindex_t res_ind, VALUE *op1, VALUE *op2,
		   enum ruby_basic_operators bop,
		   VALUE (*fix_num_op)(VALUE, VALUE), int uinsn)
{
    return do_spec_fix_arithm(cfp, res, res_ind, op1, *op2, bop, fix_num_op, FALSE, uinsn);
}

/* It is basically do_spec_flo_airthm when type OP2 is not known for
   sure (non-immediate operand).  */
static do_inline int
spec_flo_arithm_op(rb_control_frame_t *cfp,
		   VALUE *res, rindex_t res_ind, VALUE *op1, VALUE *op2,
		   enum ruby_basic_operators bop,
		   VALUE (*flo_num_op)(VALUE, VALUE), int uinsn)
{
    return do_spec_flo_arithm(cfp, res, res_ind, op1, *op2, bop, flo_num_op, FALSE, uinsn);
}

/* A function call implementing a speculative fix num arithmetic insn
   with operation BOP and suffix SUFF for fast path execution
   function. */
#define spec_fix_arithm_call(suff, bop) spec_fix_arithm_op(cfp, \
							   res, res_ind, op1, op2, bop, \
							   spec_fix_num_ ## suff, \
							   BIN(u ## suff))

/* Definitions of the functions implementing speculative fix num
   arithmetic insns.  */
spec_op2_fun(iplus) {return spec_fix_arithm_call(plus, BOP_PLUS);}
spec_op2_fun(iminus) {return spec_fix_arithm_call(minus, BOP_MINUS);}
spec_op2_fun(imult) {return spec_fix_arithm_call(mult, BOP_MULT);}
spec_op2_fun(idiv) {return spec_fix_arithm_call(div, BOP_DIV);}
spec_op2_fun(imod) {return spec_fix_arithm_call(mod, BOP_MOD);}

/* As spec_fix_arithm_call but a flo num variant. */
#define spec_flo_arithm_call(suff, bop) spec_flo_arithm_op(cfp, \
							   res, res_ind, op1, op2, bop, \
							   float_num_ ## suff, \
							   BIN(u ## suff))

/* Definitions of the functions implementing speculative flo num
   arithmetic insns.  */
spec_op2_fun(fplus) {return spec_flo_arithm_call(plus, BOP_PLUS);}
spec_op2_fun(fminus) {return spec_flo_arithm_call(minus, BOP_MINUS);}
spec_op2_fun(fmult) {return spec_flo_arithm_call(mult, BOP_MULT);}
spec_op2_fun(fdiv) {return spec_flo_arithm_call(div, BOP_DIV);}
spec_op2_fun(fmod) {return spec_flo_arithm_call(mod, BOP_MOD);}

/* It is basically do_spec_fix_arithm with fix num IMM (immediate
   operand).  */
static do_inline int
spec_fix_arithm_imm_op(rb_control_frame_t *cfp,
		       VALUE *res, rindex_t res_ind, VALUE *op1, VALUE imm,
		       enum ruby_basic_operators bop,
		       VALUE (*fix_num_op)(VALUE, VALUE), int uinsn)
{
    return do_spec_fix_arithm(cfp, res, res_ind, op1, imm, bop, fix_num_op, TRUE, uinsn);
}

/* It is basically do_spec_fix_arithm with flo num IMM (immediate
   operand).  */
static do_inline int
spec_flo_arithm_imm_op(rb_control_frame_t *cfp,
		       VALUE *res, rindex_t res_ind, VALUE *op1, VALUE imm,
		       enum ruby_basic_operators bop,
		       VALUE (*flo_num_op)(VALUE, VALUE), int uinsn)
{
    return do_spec_flo_arithm(cfp, res, res_ind, op1, imm, bop, flo_num_op, TRUE, uinsn);
}

/* A function call implementing a speculative fix num arithmetic insn
   with an immediate value as the 2nd operand.  */
#define spec_fix_arithm_imm_call(suff, bop) spec_fix_arithm_imm_op(cfp, \
								   res, res_ind, op1, imm, bop, \
								   spec_fix_num_ ## suff, \
								   BIN(u ## suff ## i))

/* Definitions of the functions implementing speculative fix num
   arithmetic insns with a fix num as an immediate 2nd operand.  */
spec_op2i_fun(iplusi) {return spec_fix_arithm_imm_call(plus, BOP_PLUS);}
spec_op2i_fun(iminusi) {return spec_fix_arithm_imm_call(minus, BOP_MINUS);}
spec_op2i_fun(imulti) {return spec_fix_arithm_imm_call(mult, BOP_MULT);}
spec_op2i_fun(idivi) {return spec_fix_arithm_imm_call(div, BOP_DIV);}
spec_op2i_fun(imodi) {return spec_fix_arithm_imm_call(mod, BOP_MOD);}

/* As spec_fix_arithm_imm_call but a flo num variant. */
#define spec_flo_arithm_imm_call(suff, bop) spec_flo_arithm_imm_op(cfp, \
								   res, res_ind, op1, imm, bop, \
								   float_num_ ## suff, \
								   BIN(u ## suff ## f))

/* Definitions of the functions implementing speculative flo num
   arithmetic insns with a flo num as an immediate 2nd operand.  */
spec_op2i_fun(fplusf) {return spec_flo_arithm_imm_call(plus, BOP_PLUS);}
spec_op2i_fun(fminusf) {return spec_flo_arithm_imm_call(minus, BOP_MINUS);}
spec_op2i_fun(fmultf) {return spec_flo_arithm_imm_call(mult, BOP_MULT);}
spec_op2i_fun(fdivf) {return spec_flo_arithm_imm_call(div, BOP_DIV);}
spec_op2i_fun(fmodf) {return spec_flo_arithm_imm_call(mod, BOP_MOD);}

/* Common function executing operation << for operands SRC1 (value
   location) and SRC2 (value) in frame CFP of thread TH.  The function
   has a fast execution path for operand of type string and array.
   For the slow paths use call data CD.  Assign the result to local or
   temporary variable with address RES and index RES_IND.  Return non
   zero if we started a new ISEQ execution (we need to update
   vm_exec_core regs in this case) or we need to cancel the current
   JITed code.  */
static do_inline int
do_ltlt(rb_thread_t *th, rb_control_frame_t *cfp,
	CALL_DATA cd, VALUE *res, rindex_t res_ind, VALUE *src1, VALUE src2)
{
    VALUE val;

    /* The concatenation might call other functions or raise
       exceptions: */
    check_sp_default(cfp);
    if (! SPECIAL_CONST_P(*src1)) {
	if (RBASIC_CLASS(*src1) == rb_cString
	    && BASIC_OP_UNREDEFINED_P(BOP_LTLT, STRING_REDEFINED_OP_FLAG)) {
	    var_assign(cfp, res, res_ind, rb_str_concat(*src1, src2));
	    return 0;
	} else if (RBASIC_CLASS(*src1) == rb_cArray
		   && BASIC_OP_UNREDEFINED_P(BOP_LTLT, ARRAY_REDEFINED_OP_FLAG)) {
	    var_assign(cfp, res, res_ind, rb_ary_push(*src1, src2));
	    return 0;
	}
    }
    val = op2_call(th, cfp, cd, src1, src2);
    return op_val_call_end(th, cfp, res, res_ind, val);
}

/* Definitions of functions implementing << and [] operations
   including variants with immediate operand (fix num or string for
   []).  */
op2_fun(ltlt)
{
    VALUE *src1 = op1, src2 = *op2;

    return do_ltlt(th, cfp, cd, res, res_ind, src1, src2);
}

op2i_fun(ltlti)
{
    VALUE *src1 = op1;

    return do_ltlt(th, cfp, cd, res, res_ind, src1, imm);
}

/* Common function executing an index operation of operands OP1
   (object) and OP2 (index) in frame CFP of thread TH.  Assign the
   result to local or temporary variable with address RES and index
   RES_IND.  The function has a fast execution path for indexing array
   and hash.  For the slow paths use call data CD.  If OP2_FIXNUM_P or
   OP2_STR_P is true, it means that value OP2 is of the corresponding
   type.  When we use a fast path, change the current insn to the
   corresponding speculative insn ARY_INSN_ID or HASH_INSN_ID unless
   they are nop.  Return non zero if we started a new ISEQ execution
   (we need to update vm_exec_core regs in this case) or we need to
   cancel the current JITed code.  */
static do_inline int
common_ind(rb_thread_t *th,
	   rb_control_frame_t *cfp,
	   CALL_DATA cd, VALUE *res, rindex_t res_ind, VALUE *op1, VALUE op2,
	   int op2_fixnum_p, int op2_str_p, int ary_insn_id, int hash_insn_id) {
    VALUE val;
    
    if (! SPECIAL_CONST_P(*op1)) {
	if (RBASIC_CLASS(*op1) == rb_cArray
	    && (! mjit_bop_redefined_p || BASIC_OP_UNREDEFINED_P(BOP_AREF, ARRAY_REDEFINED_OP_FLAG))
	    && (op2_fixnum_p || FIXNUM_P(op2))) {
	    var_assign(cfp, res, res_ind, rb_ary_entry(*op1, FIX2LONG(op2)));
	    if (ary_insn_id != BIN(nop))
		vm_change_insn(cfp->iseq, cfp->pc - 6, ary_insn_id);
	    return 0;
	} else if (RBASIC_CLASS(*op1) == rb_cHash
		   && (! mjit_bop_redefined_p || BASIC_OP_UNREDEFINED_P(BOP_AREF, HASH_REDEFINED_OP_FLAG))
		   && (! op2_str_p || rb_hash_compare_by_id_p(*op1) == Qfalse)) {
	    check_sp_default(cfp);
	    var_assign(cfp, res, res_ind, rb_hash_aref(*op1, op2));
	    if (hash_insn_id != BIN(nop))
		vm_change_insn(cfp->iseq, cfp->pc - 6, hash_insn_id);
	    return 0;
	}
    }
    val = op2_call(th, cfp, cd, op1, (op2_str_p ? rb_str_resurrect(op2) : op2));
    return op_val_call_end(th, cfp, res, res_ind, val);
}

op2_fun(ind)
{
    return common_ind(th, cfp, cd, res, res_ind, op1, *op2, FALSE, FALSE, BIN(aind), BIN(hind));
}

op2_fun(uind)
{
    return common_ind(th, cfp, cd, res, res_ind, op1, *op2, FALSE, FALSE, BIN(nop), BIN(nop));
}

op2i_fun(indi)
{
    return common_ind(th, cfp, cd, res, res_ind, op1, imm, TRUE, FALSE, BIN(aindi), BIN(hindi));
}

op2i_fun(uindi)
{
    return common_ind(th, cfp, cd, res, res_ind, op1, imm, TRUE, FALSE, BIN(nop), BIN(nop));
}

op2i_fun(inds)
{
    return common_ind(th, cfp, cd, res, res_ind, op1, imm, FALSE, TRUE, BIN(nop), BIN(hinds));
}

op2i_fun(uinds)
{
    return common_ind(th, cfp, cd, res, res_ind, op1, imm, FALSE, TRUE, BIN(nop), BIN(nop));
}

/* Speculative indexing insns: */
spec_op2_fun(aind) {
    VALUE ary = *op1;

    if (! SPECIAL_CONST_P(ary) && RBASIC_CLASS(ary) == rb_cArray
	&& (! mjit_bop_redefined_p || BASIC_OP_UNREDEFINED_P(BOP_AREF, ARRAY_REDEFINED_OP_FLAG))
	&& FIXNUM_P(*op2)) {
	unsigned long len = RARRAY_LEN(ary);
	const VALUE *ptr = RARRAY_CONST_PTR(ary);
	unsigned long offset = FIX2ULONG(*op2);
	if (offset < len) {
	    var_assign(cfp, res, res_ind, ptr[offset]);
	    return FALSE;
	}
    }
    vm_change_insn(cfp->iseq, cfp->pc - 6, BIN(uind));
    return TRUE;
}

spec_op2_fun(hind)
{
    if (! SPECIAL_CONST_P(*op1) && RBASIC_CLASS(*op1) == rb_cHash
	&& (! mjit_bop_redefined_p || BASIC_OP_UNREDEFINED_P(BOP_AREF, HASH_REDEFINED_OP_FLAG))) {
	check_sp_default(cfp);
	var_assign(cfp, res, res_ind, rb_hash_aref(*op1, *op2));
	return FALSE;
    }
    vm_change_insn(cfp->iseq, cfp->pc - 6, BIN(uind));
    return TRUE;
}

spec_op2i_fun(aindi)
{
    VALUE ary = *op1;

    if (! SPECIAL_CONST_P(ary) && RBASIC_CLASS(ary) == rb_cArray
	&& (! mjit_bop_redefined_p || BASIC_OP_UNREDEFINED_P(BOP_AREF, ARRAY_REDEFINED_OP_FLAG))) {
	long len = RARRAY_LEN(ary);
	const VALUE *ptr = RARRAY_CONST_PTR(ary);
	long offset = FIX2LONG(imm);

	if (offset < 0)
	    offset += len;
	if (offset >= 0 && offset < len) {
	    var_assign(cfp, res, res_ind, ptr[offset]);
	    return FALSE;
	}
    }
    vm_change_insn(cfp->iseq, cfp->pc - 6, BIN(uindi));
    return TRUE;
}

spec_op2i_fun(hindi)
{
    if (! SPECIAL_CONST_P(*op1) && RBASIC_CLASS(*op1) == rb_cHash
	&& (! mjit_bop_redefined_p || BASIC_OP_UNREDEFINED_P(BOP_AREF, HASH_REDEFINED_OP_FLAG))) {
	check_sp_default(cfp);
	var_assign(cfp, res, res_ind, rb_hash_aref(*op1, imm));
	return FALSE;
    }
    vm_change_insn(cfp->iseq, cfp->pc - 6, BIN(uindi));
    return TRUE;
}

spec_op2i_fun(hinds)
{
    if (! SPECIAL_CONST_P(*op1) && RBASIC_CLASS(*op1) == rb_cHash
	&& (! mjit_bop_redefined_p || BASIC_OP_UNREDEFINED_P(BOP_AREF, HASH_REDEFINED_OP_FLAG))
	&& rb_hash_compare_by_id_p(*op1) == Qfalse) {
	check_sp_default(cfp);
	var_assign(cfp, res, res_ind, rb_hash_aref(*op1, imm));
	return FALSE;
    }
    vm_change_insn(cfp->iseq, cfp->pc - 6, BIN(uinds));
    return TRUE;

}
/* Common function executing operation []= for operands OP1, IND, and
   OP3.  OP1 and OP3 are locations in frame CFP of thread TH.  The
   function has a fast execution path for operand of type array and
   hash.  For the slow paths use call data CD.  Return non zero if we
   started a new ISEQ execution (we need to update vm_exec_core regs
   in this case) or we need to cancel the current JITed code.  */
static do_inline int
common_indset(rb_thread_t *th, rb_control_frame_t *cfp,
	      CALL_DATA cd, VALUE *op1, VALUE ind, VALUE *op3,
	      int fixnum_p, int str_p, int ary_insn_id, int hash_insn_id)
{
    VALUE val, *recv = op1, el = *op3;

    if (!SPECIAL_CONST_P(*recv)) {
	if (RBASIC_CLASS(*recv) == rb_cArray
	    && (! mjit_bop_redefined_p || BASIC_OP_UNREDEFINED_P(BOP_ASET, ARRAY_REDEFINED_OP_FLAG))
	    && (fixnum_p || FIXNUM_P(ind))) {
	    rb_ary_store(*recv, FIX2LONG(ind), el);
	    if (ary_insn_id != BIN(nop))
		vm_change_insn(cfp->iseq, cfp->pc - 5, ary_insn_id);
	    return 0;
	} else if (RBASIC_CLASS(*recv) == rb_cHash
		   && BASIC_OP_UNREDEFINED_P(BOP_ASET, HASH_REDEFINED_OP_FLAG)
		   && (!str_p || rb_hash_compare_by_id_p(*recv) == Qfalse)) {
	    check_sp_default(cfp);
	    rb_hash_aset(*recv, ind, el);
	    if (ary_insn_id != BIN(nop))
		vm_change_insn(cfp->iseq, cfp->pc - 5, hash_insn_id);
	    return 0;
	}
    }
    val = op3_call(th, cfp, cd, recv, str_p ? rb_str_resurrect(ind) : ind, el);
    return op_call_end(th, cfp, val);
}
    

static do_inline int
indset_f(rb_thread_t *th, rb_control_frame_t *cfp,
	 CALL_DATA cd, VALUE *op1, VALUE *op2, VALUE *op3) {
    return common_indset(th, cfp, cd, op1, *op2, op3, FALSE, FALSE, BIN(aindset), BIN(hindset));
}

static do_inline int
uindset_f(rb_thread_t *th, rb_control_frame_t *cfp,
	  CALL_DATA cd, VALUE *op1, VALUE *op2, VALUE *op3) {
    return common_indset(th, cfp, cd, op1, *op2, op3, FALSE, FALSE, BIN(nop), BIN(nop));
}

static do_inline int
indseti_f(rb_thread_t *th, rb_control_frame_t *cfp,
	   CALL_DATA cd, VALUE *op1, VALUE imm, VALUE *op3) {
    return common_indset(th, cfp, cd, op1, imm, op3, TRUE, FALSE, BIN(aindseti), BIN(hindseti));
}

static do_inline int
uindseti_f(rb_thread_t *th, rb_control_frame_t *cfp,
	   CALL_DATA cd, VALUE *op1, VALUE imm, VALUE *op3) {
    return common_indset(th, cfp, cd, op1, imm, op3, TRUE, FALSE, BIN(nop), BIN(nop));
}

static do_inline int
indsets_f(rb_thread_t *th, rb_control_frame_t *cfp,
	  CALL_DATA cd, VALUE *op1, VALUE str, VALUE *op3) {
    return common_indset(th, cfp, cd, op1, str, op3, FALSE, TRUE, BIN(nop), BIN(hindsets));
}

static do_inline int
uindsets_f(rb_thread_t *th, rb_control_frame_t *cfp,
	   CALL_DATA cd, VALUE *op1, VALUE str, VALUE *op3) {
    return common_indset(th, cfp, cd, op1, str, op3, FALSE, TRUE, BIN(nop), BIN(nop));
}

/* Speculative []= insns:  */
static do_inline int
aindset_f(rb_control_frame_t *cfp, VALUE *op1, VALUE *op2, VALUE *op3) {
    VALUE ary = *op1;

    if (!SPECIAL_CONST_P(ary) && RBASIC_CLASS(ary) == rb_cArray
	&& (! mjit_bop_redefined_p || BASIC_OP_UNREDEFINED_P(BOP_ASET, ARRAY_REDEFINED_OP_FLAG))
	&& FIXNUM_P(*op2) && ! OBJ_FROZEN(ary) && FL_TEST(ary, ELTS_SHARED) == 0) {
	unsigned long len = RARRAY_LEN(ary);
	unsigned long offset = FIX2ULONG(*op2);

	if (offset < len) {
	    RARRAY_ASET(ary, offset, *op3);
	    return FALSE;
	}
    }
    vm_change_insn(cfp->iseq, cfp->pc - 5, BIN(uindset));
    return TRUE;
}

static do_inline int
hindset_f(rb_control_frame_t *cfp, VALUE *op1, VALUE *op2, VALUE *op3) {
    if (!SPECIAL_CONST_P(*op1) && RBASIC_CLASS(*op1) == rb_cHash
	&& (! mjit_bop_redefined_p || BASIC_OP_UNREDEFINED_P(BOP_ASET, HASH_REDEFINED_OP_FLAG))) {
	check_sp_default(cfp);
	rb_hash_aset(*op1, *op2, *op3);
	return FALSE;
    }
    vm_change_insn(cfp->iseq, cfp->pc - 5, BIN(uindset));
    return TRUE;
}

static do_inline int
aindseti_f(rb_control_frame_t *cfp, VALUE *op1, VALUE imm, VALUE *op3) {
    VALUE ary = *op1;

    if (!SPECIAL_CONST_P(ary) && RBASIC_CLASS(ary) == rb_cArray
	&& (! mjit_bop_redefined_p || BASIC_OP_UNREDEFINED_P(BOP_ASET, ARRAY_REDEFINED_OP_FLAG))
	&& ! OBJ_FROZEN(ary) && FL_TEST(ary, ELTS_SHARED) == 0) {
	long len = RARRAY_LEN(ary);
	long offset = FIX2LONG(imm);

	if (offset < 0)
	    offset += len;
	if (offset >= 0 && offset < len) {
	    RARRAY_ASET(ary, offset, *op3);
	    return FALSE;
	}
    }
    vm_change_insn(cfp->iseq, cfp->pc - 5, BIN(uindseti));
    return TRUE;
}

static do_inline int
hindseti_f(rb_control_frame_t *cfp, VALUE *op1, VALUE imm, VALUE *op3) {
    if (!SPECIAL_CONST_P(*op1) && RBASIC_CLASS(*op1) == rb_cHash
	&& (! mjit_bop_redefined_p || BASIC_OP_UNREDEFINED_P(BOP_ASET, HASH_REDEFINED_OP_FLAG))) {
	check_sp_default(cfp);
	rb_hash_aset(*op1, imm, *op3);
	return FALSE;
    }
    vm_change_insn(cfp->iseq, cfp->pc - 5, BIN(uindseti));
    return TRUE;
}

static do_inline int
hindsets_f(rb_control_frame_t *cfp, VALUE *op1, VALUE imm, VALUE *op3) {
    if (!SPECIAL_CONST_P(*op1) && RBASIC_CLASS(*op1) == rb_cHash
	&& (! mjit_bop_redefined_p || BASIC_OP_UNREDEFINED_P(BOP_ASET, HASH_REDEFINED_OP_FLAG))
	&& rb_hash_compare_by_id_p(*op1) == Qfalse) {
	check_sp_default(cfp);
	rb_hash_aset(*op1, imm, *op3);
	return FALSE;
    }
    vm_change_insn(cfp->iseq, cfp->pc - 5, BIN(uindsets));
    return TRUE;
}

/* Do string (location OP) freeze.  */
static do_inline void
freeze_string_f(rb_control_frame_t *cfp, VALUE *op, VALUE debug_info) {
    VALUE str = *op;

    check_sp_default(cfp);
    if (!NIL_P(debug_info)) {
	rb_ivar_set(str, id_debug_created_info, debug_info);
    }
    rb_str_freeze(str);
}

/* Convert a value in location OP to a string representation and put
   result to local or temporary variable with location RES and index
   RES_IND in frame CFP.  */
static do_inline void
to_string_f(rb_control_frame_t *cfp, VALUE *res, rindex_t res_ind, VALUE *op) {
    VALUE val = *op;

    /* rb_obj_as_string can use the stack -- so setup the right stack top.  */
    check_sp_default(cfp);
    var_assign(cfp, res, res_ind, rb_obj_as_string(val));
}

/* Concat CNT strings from temporary variables with start location
   START in frame CFP.  Put the result to location START.  */
static do_inline void
concat_strings_f(rb_control_frame_t *cfp, VALUE *start, rb_num_t cnt)
{
    check_sp_default(cfp);
    *start = rb_str_concat_literals(cnt, start);
}

/* Transform CNT strings from temporary variables with start location
   START in frame CFP into a regexp.  Put the result to location
   START.  */
static do_inline void
to_regexp_f(rb_control_frame_t *cfp, sindex_t start, rb_num_t opt, rb_num_t cnt)
{
    rb_num_t i;
    VALUE *res = get_temp_addr(cfp, start);
    VALUE rb_reg_new_ary(VALUE ary, int options);
    VALUE ary;

    check_sp_default(cfp);
    ary = rb_ary_tmp_new(cnt);
    for (i = 0; i < cnt; i++) {
	rb_ary_store(ary, i, res[i]);
    }
    *res = rb_reg_new_ary(ary, (int)opt);
    rb_ary_clear(ary);
}

/* Check an existence of a OP_TYPE definition given by OBJ (usually a
   symbol) and assign the result to temporary or local variable with
   location RES and index RES_IND in frame CFP.  Value in location OP
   is an additional arg used for some checks.  */
static do_inline void
defined_p_f(rb_thread_t *th, rb_control_frame_t *cfp, VALUE *res, rindex_t res_ind, VALUE *op,
	    rb_num_t op_type, VALUE obj, VALUE needstr)
{
    VALUE val = *op;

    check_sp_default(cfp);
    var_assign(cfp, res, res_ind, vm_defined(th, cfp, op_type, obj, needstr, val));
    set_default_sp(cfp, RTL_GET_BP(cfp));
}

/* As defined_p but the additional arg is given by value V.  */
static do_inline void
val_defined_p_f(rb_thread_t *th, rb_control_frame_t *cfp, VALUE *res, rindex_t res_ind, VALUE v,
		rb_num_t op_type, VALUE obj, VALUE needstr)
{
    check_sp_default(cfp);
    var_assign(cfp, res, res_ind, vm_defined(th, cfp, op_type, obj, needstr, v));
}


/* Assign the result of call freeze method of string STR to local or
   temporary variable with location RES and index RES_IND in frame
   CFP.  */
static do_inline void
str_freeze_call_f(rb_control_frame_t *cfp, VALUE *res, rindex_t res_ind, VALUE str)
{
    check_sp_default(cfp);
    var_assign(cfp, res, res_ind,
		 (BASIC_OP_UNREDEFINED_P(BOP_FREEZE, STRING_REDEFINED_OP_FLAG)
		  ? str : rb_funcall(rb_str_resurrect(str), idFreeze, 0)));
}

/* Initiate a call of method with caller ORIG_ARGC and FLAG.  The
   receiver and arguments are in temporary variables starting with
   location with index CALL_START.  If SIMPLE_P is true, a block for
   the call is not given.  Otherwise, the block is given by BLOCKISEQ.
   If RECV_SET_P is TRUE, the receiver is not on the stack and it is
   given by RECV.  Put it on the stack (a reserved location) in this
   case.  Return block (if any) of the call through BLOCK_HANDLER.  */
static do_inline void
call_setup_0(rb_thread_t *th, rb_control_frame_t *cfp,
	     VALUE *block_handler, int orig_argc, unsigned int flag, sindex_t call_start,
	     ISEQ blockiseq, VALUE recv, int recv_set_p, int simple_p)
{
    VALUE *top = get_temp_addr(cfp, call_start);

    if (recv_set_p)
	*top = recv;
    cfp->sp = top + orig_argc + 1;
    if (simple_p)
	*block_handler = VM_BLOCK_HANDLER_NONE;
    else {
	if (flag & VM_CALL_ARGS_BLOCKARG)
	    cfp->sp++;
	vm_caller_setup_arg_block_0(th, cfp, block_handler, flag, blockiseq, FALSE);
    }
}

/* Mostly the above but initiate a call of method given by CI using CALLING.  */
static do_inline void
call_setup(rb_thread_t *th, rb_control_frame_t *cfp,
	   struct rb_calling_info *calling, CALL_INFO ci, sindex_t call_start,
	   ISEQ blockiseq, VALUE recv,
	   int recv_set_p, int simple_p)
{
    call_setup_0(th, cfp, &calling->block_handler, ci->orig_argc, ci->flag, call_start,
		 blockiseq, recv, recv_set_p, simple_p);
    calling->argc = ci->orig_argc;
    calling->recv = *get_temp_addr(cfp, call_start);
}

/* As above but also update CC (if it is obsolete) by calling
   vm_search_method.  */
static do_inline void
call_common(rb_thread_t *th, rb_control_frame_t *cfp,
	    struct rb_calling_info *calling, CALL_INFO ci, CALL_CACHE cc, sindex_t call_start,
	    ISEQ blockiseq, VALUE recv,
	    int recv_set_p, int simple_p)
{
    call_setup(th, cfp, calling, ci, call_start, blockiseq, recv, recv_set_p, simple_p);
    vm_search_method(ci, cc, calling->recv);
}

/* Call a method without a block.  */
static do_inline void
simple_call_f(rb_thread_t *th, rb_control_frame_t *cfp,
	      struct rb_calling_info *calling, CALL_DATA cd, sindex_t call_start)
{
    call_common(th, cfp, calling, &cd->call_info, &cd->call_cache, call_start, NULL, Qundef, FALSE, TRUE);
}

/* Call a method without a block with putting SELF on the stack.  */
static do_inline void
simple_call_self_f(rb_thread_t *th, rb_control_frame_t *cfp,
		   struct rb_calling_info *calling, CALL_DATA cd, sindex_t call_start)
{
    rb_control_frame_t *reg_cfp = cfp; /* for GET_SELF */

    call_common(th, cfp, calling, &cd->call_info, &cd->call_cache, call_start, NULL, GET_SELF(), TRUE, TRUE);
}

/* Call a method without a block with putting value of local or
   temporary variable with location RECV_OP on the stack.  */
static do_inline void
simple_call_recv_f(rb_thread_t *th, rb_control_frame_t *cfp,
		   struct rb_calling_info *calling, CALL_DATA cd, sindex_t call_start,
		   VALUE *recv_op)
{
    VALUE recv = *recv_op;

    call_common(th, cfp, calling, &cd->call_info, &cd->call_cache, call_start, NULL, recv, TRUE, TRUE);
}

static do_inline void
vmcore_call_f(rb_thread_t *th, rb_control_frame_t *cfp,
	      struct rb_calling_info *calling, CALL_DATA cd, sindex_t call_start,
	      ISEQ blockiseq)
{
    VALUE *top = get_temp_addr(cfp, call_start);
    CALL_INFO ci = &cd->call_info;
    CALL_CACHE cc = &cd->call_cache;

    VM_ASSERT(ci->orig_argc == 0);
    specialobj2var_f(cfp, get_temp_addr(cfp, call_start), call_start, VM_SPECIAL_OBJECT_VMCORE);
    vm_caller_setup_arg_block(th, cfp, calling, ci, blockiseq, FALSE);
    calling->argc = 0;
    calling->recv = *top;
    vm_search_method(ci, cc, calling->recv);
}

/* Call a method with block BLOCKISEQ.  */
static do_inline void
call_f(rb_thread_t *th, rb_control_frame_t *cfp,
       struct rb_calling_info *calling, CALL_DATA cd, sindex_t call_start,
       ISEQ blockiseq)
{
    call_common(th, cfp, calling, &cd->call_info, &cd->call_cache, call_start, blockiseq, Qundef, FALSE, FALSE);
}

/* Call a method with block BLOCKISEQ and putting SELF on the stack.  */
static do_inline void
call_self_f(rb_thread_t *th, rb_control_frame_t *cfp,
	    struct rb_calling_info *calling, CALL_DATA cd, sindex_t call_start,
	    ISEQ blockiseq)
{
    rb_control_frame_t *reg_cfp = cfp; /* for GET_SELF */

    call_common(th, cfp, calling, &cd->call_info, &cd->call_cache, call_start, blockiseq, GET_SELF(), TRUE, FALSE);
}

/* Call a method with block BLOCKISEQ and putting value of local or
   temporary variable with location RECV_OP on the stack.  */
static do_inline void
call_recv_f(rb_thread_t *th, rb_control_frame_t *cfp,
	    struct rb_calling_info *calling, CALL_DATA cd, sindex_t call_start,
	    ISEQ blockiseq, VALUE *recv_op)
{
    VALUE recv = *recv_op;

    call_common(th, cfp, calling, &cd->call_info, &cd->call_cache, call_start, blockiseq, recv, TRUE, FALSE);
}

/* Called only from JIT code to finish a call insn.  Pass VAL through
   RES.  Return non-zero if we need to cancel JITed code execution and
   don't use the code anymore. */
static do_inline int
mjit_call_finish(rb_thread_t *th, rb_control_frame_t *cfp,
		 unsigned int temp_vars_num, VALUE val, VALUE *res) {
    *res = val;
    if (! mjit_ep_neq_bp_p && cfp->bp != cfp->ep) {
        set_default_sp_0(cfp, cfp->bp, temp_vars_num);
        return TRUE;
    }
    set_default_sp_0(cfp, RTL_GET_BP(cfp), temp_vars_num);
    return (cfp->ep[VM_ENV_DATA_INDEX_FLAGS] & VM_FRAME_FLAG_CANCEL) != 0;
}
 
/* Called only from JIT code to finish a call insn of ISEQ with BODY,
   TYPE and TEMP_VARS_NUM.  Pass the result through RES.  Return
   non-zero if we need to cancel JITed code execution and don't use
   the code anymore. */
static do_inline int
mjit_call_iseq_finish(rb_thread_t *th, rb_control_frame_t *cfp,
		      rb_iseq_t *iseq, struct rb_iseq_constant_body *body,
		      int type, unsigned int temp_vars_num, VALUE *res) {
    VALUE v = mjit_vm_exec_0(th, iseq, body, type);
    return mjit_call_finish(th, cfp, temp_vars_num, v, res);
}
 
/* The function is used to implement highly speculative call of method
   ME with ISEQ with BODY, TYPE, PARAM_SIZE, LOCAL_SIZE, and
   STACK_MAX.  The call has ARGC, FLAG, BLOCKISEQ, and reciever RECV.
   The call params starts with CALL_START location.  The call is
   simple if SIMPLE_P.  The call should put the reciever on the stack
   if RECV_SET_P.  The caller has CALLER_TEMP_VARS_NUM temps.  Return
   the result through RES.  To generate a better code MJIT use the
   function when it knows the value of the ISEQ parameters.  */
static do_inline int
mjit_iseq_call(rb_thread_t *th, rb_control_frame_t *cfp, const rb_callable_method_entry_t *me,
	       rb_iseq_t *iseq, struct rb_iseq_constant_body *body, VALUE *pc,
	       int type, int param_size, int local_size,
	       unsigned int caller_temp_vars_num, unsigned int stack_max,
	       int argc, unsigned int flag, sindex_t call_start, ISEQ blockiseq,
	       VALUE recv, int recv_set_p, int simple_p, VALUE *res) {
    VALUE block_handler;
    
    call_setup_0(th, cfp, &block_handler, argc, flag, call_start,
		 blockiseq, recv, recv_set_p, simple_p);
    vm_call_iseq_setup_normal_0(th, cfp, me, iseq, recv, argc, block_handler,
				pc, param_size, local_size, stack_max);
    return mjit_call_iseq_finish(th, cfp, iseq, body, type, caller_temp_vars_num, res);
}

/* A block call given by call data CD with args in temporary variables
   starting with one with index CALL_START.  */
static do_inline VALUE
call_block_f(rb_thread_t *th, rb_control_frame_t *cfp,
	     CALL_DATA cd, sindex_t call_start)
{
    rb_control_frame_t *reg_cfp = cfp; /* for GET_CFP */
    VALUE *top = get_temp_addr(cfp, call_start);
    CALL_INFO ci = &cd->call_info;
    struct rb_calling_info calling;

    calling.argc = ci->orig_argc;
    calling.block_handler = VM_BLOCK_HANDLER_NONE;
    calling.recv = GET_SELF();
    cfp->sp = top + calling.argc;
    return vm_invoke_block(th, GET_CFP(), &calling, ci);
}

/* The corresponding method call of super class with args in temporary
   variables starting with one with index CALL_START and block
   BLOCKISEQ.  The call is defined by call data CD and REC_VAL
   (a receiver).  */
static do_inline void
call_super_f(rb_thread_t *th, rb_control_frame_t *cfp,
	     struct rb_calling_info *calling, CALL_DATA cd, sindex_t call_start,
	     ISEQ blockiseq, VALUE rec_val)
{
    rb_control_frame_t *reg_cfp = cfp; /* for GET_CFP */
    VALUE *top = get_temp_addr(cfp, call_start);
    CALL_INFO ci = &cd->call_info;
    CALL_CACHE cc = &cd->call_cache;

    *top = rec_val;
    calling->argc = ci->orig_argc;
    cfp->sp = top + calling->argc + 1 + ((ci->flag & VM_CALL_ARGS_BLOCKARG) ? 1 : 0);
    vm_caller_setup_arg_block(th, cfp, calling, ci, blockiseq, TRUE);
    calling->recv = GET_SELF();
    vm_search_super_method(th, GET_CFP(), calling, ci, cc);
}

/* Create a range OP1..OP2 (OP1 and OP2 value locations) and assign
   result to local or temporary variable with location RES and RES_IND
   in frame CFP.  FLAG is flag of OP2 exclusion.  */
static do_inline void
make_range_f(rb_control_frame_t *cfp, VALUE *res, rindex_t res_ind, VALUE *op1, VALUE *op2, rb_num_t flag)
{
    VALUE low = *op1, high = *op2;

    check_sp_default(cfp);
    var_assign(cfp, res, res_ind, rb_range_new(low, high, (int)flag));
}

/* Create an array and assign the result to local or temporary variable
   with location RES and RES_IND in frame CFP.  NUM elements are given
   in temporary variables starting with location START.  */
static do_inline void
make_array_f(rb_control_frame_t *cfp, VALUE *res, rindex_t res_ind, sindex_t start, rb_num_t num)
{
    check_sp_default(cfp);
    var_assign(cfp, res, res_ind, rb_ary_new4((long) num, get_var_addr(cfp, start)));
}

/* Create a hash and assign the result to local or temporary variable
   with location RES and RES_IND in frame CFP.  Number of NUM of keys
   and elements are given in temporary variables starting with
   location START.  */
static do_inline void
make_hash_f(rb_control_frame_t *cfp, VALUE *res, rindex_t res_ind, sindex_t start, rb_num_t num)
{
    VALUE val;
    rb_num_t i;

    check_sp_default(cfp);
#if 0
    /* TODO: Implement dtrace support (symbol visibility and weakness).  */
    RUBY_DTRACE_CREATE_HOOK(HASH, num);
#endif
    
    val = rb_hash_new();

    for (i = 0; i < num; i += 2) {
	VALUE *els = get_var_addr(cfp, start);
	const VALUE k = els[i];
	const VALUE v = els[i + 1];

	rb_hash_aset(val, k, v);
    }

    var_assign(cfp, res, res_ind, val);
}

/* Return TRUE if V1 is less than V2 using data CMP_OPT about
   the comparison optimizability.  */
static do_inline int
optimized_min(VALUE v1, VALUE v2, struct cmp_opt_data *cmp_opt) {
#define id_cmp  idCmp
    return OPTIMIZED_CMP(v1, v2, *cmp_opt) < 0;
#undef id_cmp
}

/* As above but return TRUE if V1 is greater than V2.  */
static do_inline int
optimized_max(VALUE v1, VALUE v2, struct cmp_opt_data *cmp_opt) {
#define id_cmp  idCmp
    return OPTIMIZED_CMP(v1, v2, *cmp_opt) > 0;
#undef id_cmp
}

/* Find min/max (depending on MID and FUNC) of NUM elements in
   temporary variables with location starting with START.  Assign it
   to local or temporary variable with location RES and RES_IND in
   frame CFP.  */
static do_inline void
common_new_array_min_max(rb_control_frame_t *cfp, VALUE *res, rindex_t res_ind,
			 sindex_t start, rb_num_t num, ID mid,
			 int (*func)(VALUE, VALUE, struct cmp_opt_data *))
{
    VALUE val;

    check_sp_default(cfp);
    VM_ASSERT((long) start < 0);
    if (BASIC_OP_UNREDEFINED_P(BOP_MIN, ARRAY_REDEFINED_OP_FLAG)) {
	if (num == 0) {
	    val = Qnil;
	}
	else {
	    struct cmp_opt_data cmp_opt = { 0, 0 };
	    VALUE result = Qundef;
	    rb_num_t i;

	    result = *get_temp_addr(cfp, start);
	    for (i = 1; i < num; i++) {
		const VALUE v = *get_temp_addr(cfp, start - i);
		if (result == Qundef || func(v, result, &cmp_opt)) {
		    result = v;
		}
	    }
	    val = result == Qundef ? Qnil : result;
	}
    }
    else {
	VALUE ary = rb_ary_new4((long) num, get_temp_addr(cfp, start));
	/* Should we be more accurate with the stack here ???  */
	val = rb_funcall(ary, mid, 0);
    }
    var_assign(cfp, res, res_ind, val);
}

/* It is just a call of common_new_array_min_max for MIN.  */
static do_inline void
new_array_min_f(rb_control_frame_t *cfp, VALUE *res, rindex_t res_ind, sindex_t start, rb_num_t num)
{
    common_new_array_min_max(cfp, res, res_ind, start, num, idMin, optimized_min);
}

/* It is just a call of common_new_array_min_max for MAX.  */
static do_inline void
new_array_max_f(rb_control_frame_t *cfp, VALUE *res, rindex_t res_ind, sindex_t start, rb_num_t num)
{
    common_new_array_min_max(cfp, res, res_ind, start, num, idMax, optimized_max);
}

/* Create a copy of array ARR and assign it to local or temporary
   variable with location RES and RES_IND in frame CFP.  */
static do_inline void
clone_array_f(rb_control_frame_t *cfp, VALUE *res, rindex_t res_ind, VALUE arr)
{
    check_sp_default(cfp);
    var_assign(cfp, res, res_ind, rb_ary_resurrect(arr));
}

/* Put NUM elements of array in temporary OP1 on the stack starting
   with OP1 according to FLAG (see details in vm_expandarray).  */
static do_inline void
spread_array_f(rb_control_frame_t *cfp, VALUE *op1, rb_num_t num, rb_num_t flag)
{
    VALUE *ary_ptr = op1;

    cfp->sp = ary_ptr;
    vm_expandarray(cfp, *ary_ptr, num, (int)flag);
    set_default_sp(cfp, RTL_GET_BP(cfp));
}

/* Assign the result array to local or temporary variable with location RES
   and index RES_IND in frame CFP.  ARR is first converted by method
   to_a and depending on the result and FLAG can be a new array or
   copy of existing one.  */
static do_inline void
splat_array_f(rb_control_frame_t *cfp, VALUE *res, rindex_t res_ind, VALUE *arr, VALUE flag)
{
    VALUE ary = *arr;
    VALUE tmp;

    check_sp_default(cfp);
    tmp = rb_check_convert_type(ary, T_ARRAY, "Array", "to_a");

    if (NIL_P(tmp))
	tmp = rb_ary_new3(1, ary);
    else if (RTEST(flag))
	tmp = rb_ary_dup(tmp);
    var_assign(cfp, res, res_ind, tmp);
}

/* Concat two arrays in local or temporary variables in locations OP1
   and OP2 to to local or temporary variable with location RES and
   index RES_IND in frame CFP.  OP1 and OP2 are first converted by
   method to_a unless they are not arrays.  */
static do_inline void
concat_array_f(rb_control_frame_t *cfp, VALUE *res, rindex_t res_ind, VALUE *op1, VALUE *op2)
{
    VALUE tmp1, tmp2, ary1 = *op1, ary2 = *op2;

    check_sp_default(cfp);
    tmp1 = rb_check_convert_type(ary1, T_ARRAY, "Array", "to_a");
    tmp2 = rb_check_convert_type(ary2, T_ARRAY, "Array", "to_a");

    if (NIL_P(tmp1))
	tmp1 = rb_ary_new3(1, ary1);

    if (NIL_P(tmp2))
	tmp2 = rb_ary_new3(1, ary2);

    if (tmp1 == ary1)
	tmp1 = rb_ary_dup(ary1);
    var_assign(cfp, res, res_ind, rb_ary_concat(tmp1, tmp2));
}

/* Return TRUE if bit KEYWORD_INDEX in a value of local variable with
   location KW_BITS_INDEX in frame CFP is present or key KEYWORD_INDEX
   is present in a hash KEYWORD_INDEX.  It means we should jump to the
   destination of insn BKW.  */
static do_inline int
bkw_f(rb_thread_t *th, rb_control_frame_t *cfp, rb_num_t kw_bits_index, rb_num_t keyword_index) {
    rb_control_frame_t *reg_cfp = cfp; /* for GET_EP */
    const VALUE *ep = GET_EP();
    const VALUE kw_bits = *(ep - kw_bits_index);
    VALUE val;

    if (FIXNUM_P(kw_bits)) {
	int bits = FIX2INT(kw_bits);
	val = (bits &(0x01 << keyword_index)) ? Qfalse : Qtrue;
    }
    else {
	VM_ASSERT(RB_TYPE_P(kw_bits, T_HASH));
	val = rb_hash_has_key(kw_bits, INT2FIX(keyword_index));
	val = val ? Qfalse : Qtrue;
    }
    if (RTEST(val)) {
	check_sp_default(cfp);
	RUBY_VM_CHECK_INTS(th);
	return TRUE;
    }
    return FALSE;
}

/* Match a value (target) in local or temporary variable with location
   OP1 with a pattern in local or temporary variable with location
   OP2.  Assign the result of matching (Qtrue of Qfalse) to local or
   temporary variable with location RES and index RES_IND in frame
   CFP.

   `FLAG & VM_CHECKMATCH_TYPE_MASK' describe how to check the pattern.
   VM_CHECKMATCH_TYPE_WHEN means ignoring the target and check the pattern is truthy.
   VM_CHECKMATCH_TYPE_CASE means checking `pattern === target'.
   VM_CHECKMATCH_TYPE_RESCUE means checking `pattern.kind_op?(Module) && pattern == target'.
   If `flag & VM_CHECKMATCH_ARRAY' is not 0, then OP2 is actually an array of patterns.

 */
static do_inline void
check_match_f(rb_control_frame_t *cfp, VALUE *res, rindex_t res_ind,
	      VALUE *op1, VALUE *op2, rb_num_t flag)
{
    VALUE val, target = *op1, pattern = *op2;
    enum vm_check_match_type checkmatch_type =
	(enum vm_check_match_type) (flag & VM_CHECKMATCH_TYPE_MASK);

    check_sp_default(cfp);
    val = Qfalse;
    VM_ASSERT((int) result < 0);
    if (flag & VM_CHECKMATCH_ARRAY) {
	long i;
	for (i = 0; i < RARRAY_LEN(pattern); i++) {
	    if (RTEST(check_match(RARRAY_AREF(pattern, i), target, checkmatch_type))) {
		val = Qtrue;
		break;
	    }
	}
    } else {
	if (RTEST(check_match(pattern, target, checkmatch_type))) {
	    val = Qtrue;
	}
    }
    var_assign(cfp, res, res_ind, val);
}

/* Match and branch.  Operands meaning is the same as for
   check_match_f.  Return TRUE if the result is Qtrue.  It means we
   should jump to the branch destination.  */
static do_inline int
bt_match_f(rb_thread_t *th, rb_control_frame_t *cfp,
	   VALUE *res, rindex_t res_ind, VALUE *op1, VALUE *op2, rb_num_t flag)
{
    VALUE val, target = *op1, pattern = *op2;
    enum vm_check_match_type checkmatch_type =
	(enum vm_check_match_type) (flag & VM_CHECKMATCH_TYPE_MASK);

    check_sp_default(cfp);
    val = Qfalse;
    VM_ASSERT((int) result < 0);
    if (flag & VM_CHECKMATCH_ARRAY) {
	int i;
	for (i = 0; i < RARRAY_LEN(pattern); i++) {
	    if (RTEST(check_match(RARRAY_AREF(pattern, i), target, checkmatch_type))) {
		val = Qtrue;
		break;
	    }
	}
    } else {
	if (RTEST(check_match(pattern, target, checkmatch_type))) {
	    val = Qtrue;
	}
    }
    var_assign(cfp, res, res_ind, val);
    if (val == Qtrue) {
	RUBY_VM_CHECK_INTS(th);
	return TRUE;
    }
    return FALSE;
}

/* Match REGEX with a string in local or temporary variable of
   location STR_OP.  Assign the result to local or temporary variable
   location with address RES and index RES_IND in frame CFP.  */
static do_inline void
regexp_match1_f(rb_control_frame_t *cfp, VALUE *res, rindex_t res_ind,
		VALUE regex, VALUE *str_op)
{
    VALUE str = *str_op;

    check_sp_default(cfp);
    if (BASIC_OP_UNREDEFINED_P(BOP_MATCH, REGEXP_REDEFINED_OP_FLAG)) {
	var_assign(cfp, res, res_ind, rb_reg_match(regex, str));
    } else {
	var_assign(cfp, res, res_ind, rb_funcall(regex, idEqTilde, 1, str));
    }
}

/* Analogous to regexp_match1 but we don't know that value in STR_OP
   is a string.  Return non zero if we started a new ISEQ execution
   (we need to update vm_exec_core regs in this case) or we need to
   cancel the current JITed code.  */
static do_inline int
regexp_match2_f(rb_thread_t *th, rb_control_frame_t *cfp, CALL_DATA cd,
		VALUE *res, rindex_t res_ind, VALUE *str_op, VALUE *regex_op) {
    VALUE v, *str = str_op, regex = *regex_op;

    check_sp_default(cfp);
    if (CLASS_OF(*str) == rb_cString &&
	BASIC_OP_UNREDEFINED_P(BOP_MATCH, STRING_REDEFINED_OP_FLAG)) {
	var_assign(cfp, res, res_ind, rb_reg_match(regex, *str));
	return 0;
    }
    v = op2_call(th, cfp, cd, str, regex);
    return op_val_call_end(th, cfp, res, res_ind, v);
}

/* An optimized version for Ruby case with case operand in local or
   temporary variable with location OP in frame CFP.  HASH contains
   "when" values and the corresponding destination offsets, ELSE_OFFSET
   is the destination offset for absent OP.  Return the destination
   offset or zero if we should not jump.  */
static do_inline OFFSET
case_dispatch_f(rb_control_frame_t *cfp, VALUE *op,
		CDHASH hash, OFFSET else_offset)
{
    VALUE key = *op;

    check_sp_default(cfp);
    switch(TYPE(key)) {
    case -1:
    case T_FIXNUM:
    case T_FLOAT:
    case T_SYMBOL:
    case T_BIGNUM:
    case T_STRING:
	if (BASIC_OP_UNREDEFINED_P(BOP_EQQ,
				   SYMBOL_REDEFINED_OP_FLAG |
				   INTEGER_REDEFINED_OP_FLAG |
				   FLOAT_REDEFINED_OP_FLAG |
				   NIL_REDEFINED_OP_FLAG    |
				   TRUE_REDEFINED_OP_FLAG   |
				   FALSE_REDEFINED_OP_FLAG  |
				   STRING_REDEFINED_OP_FLAG)) {
	    st_data_t val;

	    if (RB_FLOAT_TYPE_P(key)) {
		double kval = RFLOAT_VALUE(key);
		if (!isinf(kval) && modf(kval, &kval) == 0.0) {
		    key = FIXABLE(kval) ? LONG2FIX((long)kval) : rb_dbl2big(kval);
		}
	    }
	    if (st_lookup(RHASH_TBL_RAW(hash), key, &val)) {
		return FIX2INT((VALUE) val);
	    }
	    else {
		return else_offset;
	    }
	}
    }
    return 0;
}

/* Throw an exception THROWOBJ in frame CFP of thread TH with
   additional info THROW_STATE.  */
static do_inline VALUE
raise_except_val_f(rb_thread_t *th, rb_control_frame_t *cfp, VALUE throwobj,
		   rb_num_t throw_state)
{
    rb_control_frame_t *reg_cfp = cfp; /* for GET_CFP */

    check_sp_default(cfp);
    RUBY_VM_CHECK_INTS(th);
    return vm_throw(th, GET_CFP(), throw_state, throwobj);
}

/* As raise_except_val_f but THROWOBJ is in local or temporary
   variable with location OP.  */
static do_inline VALUE
raise_except_f(rb_thread_t *th, rb_control_frame_t *cfp, VALUE *op,
	     rb_num_t throw_state)
{
    return raise_except_val_f(th, cfp, *op, throw_state);
}

/* Start definition of a class/module with ID and CLASS_ISEQ and
   FLAGS.  Class path and super are in local or temporary with
   locations OP1 and OP2.  STACK_TOP is where the stack top should
   set.  */
static do_inline void
define_class(rb_thread_t *th, rb_control_frame_t *cfp, ID id, ISEQ class_iseq, rb_num_t flags,
	     VALUE *op1, VALUE *op2, sindex_t stack_top)
{
    VALUE cbase = *op1, super = *op2;
    VALUE klass;
    rb_vm_defineclass_type_t type = VM_DEFINECLASS_TYPE(flags);
    rb_control_frame_t *reg_cfp = cfp; /* for GET_BLOCK_PTR */

    /* Op2 is always on stack.  */
    cfp->sp = get_temp_addr(cfp, stack_top);
    switch (type) {
      case VM_DEFINECLASS_TYPE_CLASS:
	/* val is dummy.  classdef returns class scope value */

	if (VM_DEFINECLASS_HAS_SUPERCLASS_P(flags)
	    && ! RB_TYPE_P(super, T_CLASS)) {
	    rb_raise(rb_eTypeError, "superclass must be a Class (%"PRIsVALUE" given)",
		     rb_obj_class(super));
	}

	vm_check_if_namespace(cbase);

	/* find klass */
	rb_autoload_load(cbase, id);
	if ((klass = vm_search_const_defined_class(cbase, id)) != 0) {
	    /* already exist */
	    klass = (VM_DEFINECLASS_SCOPED_P(flags) ?
		     rb_public_const_get_at(klass, id) : rb_const_get_at(klass, id));
	    if (!RB_TYPE_P(klass, T_CLASS)) {
		rb_raise(rb_eTypeError, "%"PRIsVALUE" is not a class", rb_id2str(id));
	    }

	    if (VM_DEFINECLASS_HAS_SUPERCLASS_P(flags)) {
		VALUE tmp;
		tmp = rb_class_real(RCLASS_SUPER(klass));

		if (tmp != super) {
		    rb_raise(rb_eTypeError, "superclass mismatch for class %"PRIsVALUE"",
		    rb_id2str(id));
		}
	    }
	}
	else {
	    /* new class declaration */
	    if (!VM_DEFINECLASS_HAS_SUPERCLASS_P(flags)) {
		super = rb_cObject;
	    }
	    klass = rb_define_class_id(id, super);
	    rb_set_class_path_string(klass, cbase, rb_id2str(id));
	    rb_const_set(cbase, id, klass);
	    rb_class_inherited(super, klass);
	}
	break;
      case VM_DEFINECLASS_TYPE_SINGLETON_CLASS:
	/* val is dummy.  classdef returns class scope value */
	/* super is dummy */
	klass = rb_singleton_class(cbase);
	break;
      case VM_DEFINECLASS_TYPE_MODULE:
	/* val is dummy.  classdef returns class scope value */
	/* super is dummy */

	vm_check_if_namespace(cbase);

	/* find klass */
	if ((klass = vm_search_const_defined_class(cbase, id)) != 0) {
	    klass = (VM_DEFINECLASS_SCOPED_P(flags) ?
		     rb_public_const_get_at(klass, id) : rb_const_get_at(klass, id));
	    /* already exist */
	    if (!RB_TYPE_P(klass, T_MODULE)) {
		rb_raise(rb_eTypeError, "%"PRIsVALUE" is not a module", rb_id2str(id));
	    }
	}
	else {
	    /* new module declaration */
	    klass = rb_define_module_id(id);
	    rb_set_class_path_string(klass, cbase, rb_id2str(id));
	    rb_const_set(cbase, id, klass);
	}
	break;
      default:
	rb_bug("unknown defineclass type: %d", (int)type);
    }
    rb_iseq_check(class_iseq);

    /* enter scope */
    vm_push_frame(th, class_iseq, VM_FRAME_MAGIC_CLASS | VM_ENV_FLAG_LOCAL, klass,
	          GET_BLOCK_HANDLER(),
	          (VALUE)vm_cref_push(th, klass, NULL, FALSE),
	          class_iseq->body->iseq_encoded, GET_SP(),
		  class_iseq->body->local_table_size,
		  class_iseq->body->stack_max);

}

/* This function is called only from JIT code to define a class and assign
   it to temporary variable with location STACK_TOP.  */
static __attribute__ ((unused)) void
define_class_f(rb_thread_t *th, rb_control_frame_t *cfp, ID id, ISEQ class_iseq, rb_num_t flags,
	       VALUE *op1, VALUE *op2, sindex_t stack_top) {
    define_class(th, cfp, id, class_iseq, flags, op1, op2, stack_top);
    *get_temp_addr(cfp, stack_top) = mjit_vm_exec(th);
    set_default_sp(cfp, RTL_GET_BP(cfp));
}

/* Run ISEQ once storing the result in IC.  Assign the result to local
   or temporary variable location with address RES and index RES_IND
   in frame CFP of thread TH.  */
static do_inline void
run_once_f(rb_thread_t *th, rb_control_frame_t *cfp, VALUE *res, rindex_t res_ind, ISEQ iseq, IC ic)
{
    rb_control_frame_t *reg_cfp = cfp; /* for GET_ISEQ */
    VALUE val;
    union iseq_inline_storage_entry *is = (union iseq_inline_storage_entry *)ic;
    rb_thread_t *thread_done = ((rb_thread_t *)(0x1));

    check_sp_default(cfp);

 retry_finish:
    if (is->once.running_thread == thread_done) {
        val = is->once.value;
	var_assign(cfp, res, res_ind, val);
    } else if (is->once.running_thread == NULL) {
        is->once.running_thread = th;
	val = is->once.value = rb_ensure(vm_once_exec,(VALUE)iseq, vm_once_clear,(VALUE)is);
	var_assign(cfp, res, res_ind, val);
	/* is->once.running_thread is cleared by vm_once_clear() */
	is->once.running_thread = thread_done; /* success */
	rb_iseq_add_mark_object(GET_ISEQ(), val);
    } else if (is->once.running_thread == th) {
        /* recursive once */
        val = vm_once_exec((VALUE)iseq);
	var_assign(cfp, res, res_ind, val);
    } else {
	/* waiting for finish */
	RUBY_VM_CHECK_INTS(th);
	rb_thread_schedule();
	goto retry_finish;
    }
}

static do_inline void
call_dtrace_hook(rb_thread_t *th) {
#if 0
    /* TODO: Implement dtrace support (symbol visibility and weakness).  */
    if (RUBY_DTRACE_METHOD_ENTRY_ENABLED() ||
	RUBY_DTRACE_METHOD_RETURN_ENABLED() ||
	RUBY_DTRACE_CMETHOD_ENTRY_ENABLED() ||
	RUBY_DTRACE_CMETHOD_RETURN_ENABLED()) {
	
	switch(flag) {
	case RUBY_EVENT_CALL:
	    RUBY_DTRACE_METHOD_ENTRY_HOOK(th, 0, 0);
	    break;
	case RUBY_EVENT_C_CALL:
	    RUBY_DTRACE_CMETHOD_ENTRY_HOOK(th, 0, 0);
	    break;
	case RUBY_EVENT_RETURN:
	    RUBY_DTRACE_METHOD_RETURN_HOOK(th, 0, 0);
	    break;
	case RUBY_EVENT_C_RETURN:
	    RUBY_DTRACE_CMETHOD_RETURN_HOOK(th, 0, 0);
	    break;
	}
    }
#endif
}

/* Trace an insn returning RET_VAL from frame CFP of thread TH using
   even FLAG.  */
static do_inline void
ret_trace(rb_thread_t *th, rb_control_frame_t *cfp, rb_event_flag_t flag, VALUE ret_val)
{
    rb_control_frame_t *reg_cfp = cfp; /* for GET_SELF */

    if (! mjit_trace_p)
	return;
    call_dtrace_hook(th);
    EXEC_EVENT_HOOK(th, flag, GET_SELF(), 0, 0, 0 /* id and klass are resolved at callee */,
		    (flag & (RUBY_EVENT_RETURN | RUBY_EVENT_B_RETURN)) ? ret_val : Qundef);
}

/* Return value V from frame CFP of thread TH.  Pass the value through
   VAL.  Trace event NF.  Return a flag to finish vm_exec_core.  */
static do_inline int
val_ret_f(rb_thread_t *th, rb_control_frame_t *cfp, VALUE v, rb_num_t nf, VALUE *val) {
    cfp->sp = RTL_GET_BP(cfp) + 1;
    ret_trace(th, cfp, (rb_event_flag_t) nf, v);

    RUBY_VM_CHECK_INTS(th);

    if (! vm_pop_frame(th, cfp, cfp->ep)) {
	*val = v;
	return 0;
    } else {
#if OPT_CALL_THREADED_CODE
	th->retval = v;
	*val = 0;
#else
	*val = v;
#endif
	return 1;
    }
}

/* As val_ret_f but return value is in a temporary variable with
   location RET_OP.  */
static do_inline int
temp_ret_f(rb_thread_t *th, rb_control_frame_t *cfp, VALUE *ret_op, rb_num_t nf, VALUE *val) {
    VALUE v = *ret_op;

    return val_ret_f(th, cfp, v, nf, val);
}

/* As temp_ret_f but the return value is a local variable.  */
static do_inline int
loc_ret_f(rb_thread_t *th, rb_control_frame_t *cfp, VALUE *ret_op, rb_num_t nf, VALUE *val) {
    VALUE v = *ret_op;

    return val_ret_f(th, cfp, v, nf, val);
}

/* Finish a return by putting VAL on the stack top and restoring the
   default stack top.  */
static do_inline void
finish_ret(rb_control_frame_t *cfp, VALUE val) {
    *cfp->sp = val;
    cfp->sp = RTL_GET_BP(cfp) + 1 + cfp->iseq->body->temp_vars_num;
}

/* Non-return trace execution.  */
static do_inline void
trace_f(rb_thread_t *th, rb_control_frame_t *cfp, rb_num_t nf) {
    rb_event_flag_t flag = (rb_event_flag_t)nf;
    rb_control_frame_t *reg_cfp = cfp; /* for GET_SELF */

    if (! mjit_trace_p)
	/* We are speculating in JITed code that there is no
	   tracing.  */
	return;
    call_dtrace_hook(th);
    EXEC_EVENT_HOOK(th, flag, GET_SELF(), 0, 0, 0 /* id and klass are resolved at callee */,
		    Qundef);
}

/* As mjit_call_finish but when VAL is undefined call mjit_vm_exec. */
static do_inline int
mjit_general_call_finish(rb_thread_t *th, rb_control_frame_t *cfp, VALUE val, VALUE *res) {
    if (val == Qundef) {
	val = mjit_vm_exec(th);
    }
    return mjit_call_finish(th, cfp, cfp->iseq->body->temp_vars_num, val, res);
}
 
/* Called only from JIT code to implement a call insn and pass the
   call result through RES.  Return non-zero if we need to cancel
   JITed code execution and don't use the code anymore.  */
static do_inline int
mjit_call_method(rb_thread_t *th, rb_control_frame_t *cfp, struct rb_calling_info *calling,
	         CALL_DATA cd, VALUE *res) {
    CALL_INFO ci = &cd->call_info;
    CALL_CACHE cc = &cd->call_cache;
    VALUE val = (*(cc)->call)(th, cfp, calling, ci, cc);

    return mjit_general_call_finish(th, cfp, val, res);
}

/* Called only from JIT code to implement a call insn and pass the
   call result through RES.  Return non-zero if we need to cancel
   JITed code execution and not to use the code anymore.  */
static do_inline int
mjit_call_iseq_normal(rb_thread_t *th, rb_control_frame_t *cfp, struct rb_calling_info *calling,
		      CALL_DATA cd, int param, int local, VALUE *res) {
    CALL_INFO ci = &cd->call_info;
    CALL_CACHE cc = &cd->call_cache;
    VALUE val = vm_call_iseq_setup_normal(th, cfp, calling, ci, cc, 0, param, local);

    return mjit_general_call_finish(th, cfp, val, res);
}

/* Called only from JIT code to finish a call block insn and pass the
   result through RES.  Return non-zero if we need to cancel JITed
   code execution and don't use the code anymore.  */
static do_inline int
mjit_call_block_end(rb_thread_t *th, rb_control_frame_t *cfp, VALUE val, VALUE *res) {
    return mjit_general_call_finish(th, cfp, val, res);
}

/* Called only from JIT code to check call cache attributes
   METHOD_STATE and CLASS_SERIAL for object OBJ.  Return non-zero if
   they are obsolete.  */
static do_inline int
mjit_check_cc_attr_p(VALUE obj, rb_serial_t method_state, rb_serial_t class_serial) {
    return GET_GLOBAL_METHOD_STATE() != method_state || RCLASS_SERIAL(CLASS_OF(obj)) != class_serial;
}

/* Called only from JIT code to get ivar value with INDEX from the
   corresponding call cache.  Return non-zero if the speculation
   failed.  Otherwise, return zero and the value through VAL.  */
static do_inline int
mjit_call_ivar(VALUE obj, unsigned int index, VALUE *val) {
    if (LIKELY(RB_TYPE_P(obj, T_OBJECT) && index > 0)) {
	if (LIKELY(index <= ROBJECT_NUMIV(obj)))
	    *val = ROBJECT_IVPTR(obj)[index - 1];
	else
	    *val = Qnil;
	return FALSE;
    }
    return TRUE;
}

/* Called only from JIT code to set ivar value VAL with INDEX from the
   corresponding call cache.  Return non-zero if the speculation
   failed.  */
static do_inline int
mjit_call_setivar(VALUE obj, unsigned int index, VALUE val) {
    rb_check_frozen(obj);
    if (LIKELY(RB_TYPE_P(obj, T_OBJECT) && index > 0 && index <= ROBJECT_NUMIV(obj))) {
	RB_OBJ_WRITE(obj, &ROBJECT_IVPTR(obj)[index - 1], val);
	return FALSE;
    }
    return TRUE;
}

/* Highly speculative call of a cfunc with method identifier MID and
   method ME.  The call has FLAG, ARGC arguments staring with
   CALL_START, BLOCKISEQ, reciever RECV, address KW_ARG where pointer
   to keyword arg info is (if any).  The call is simple if SIMPLE_P.
   The call should put the reciever on the stack if RECV_SET_P.  The
   caller has CALLER_TEMP_VARS_NUM temps.  Return the call result
   through VAL.  */
static do_inline int
mjit_call_cfunc(rb_thread_t *th, rb_control_frame_t *cfp,
		ID mid, const rb_callable_method_entry_t *me,
		unsigned int caller_temp_vars_num,
		int argc, unsigned int flag, struct rb_call_info_kw_arg **kw_arg,
		sindex_t call_start, ISEQ blockiseq,
		VALUE recv, int recv_set_p, int simple_p, VALUE *val) {
    VALUE v, block_handler;
    
    call_setup_0(th, cfp, &block_handler, argc, flag, call_start,
		 blockiseq, recv, recv_set_p, simple_p);
    v = vm_call_cfunc_0(th, cfp, recv, block_handler, argc, flag, kw_arg, mid, me);
    return mjit_call_finish(th, cfp, caller_temp_vars_num, v, val);
}

/* NOP insn.  */
static do_inline void
nop_f(rb_control_frame_t *cfp) {
}
