/**********************************************************************

  mjit_rtl.c - MRI RTL method JIT compiler

  Copyright (C) 2017, 2018 Vladimir Makarov <vmakarov@redhat.com>.

**********************************************************************/

#include "internal.h"
#include "vm_core.h"
#include "vm_exec.h"
#include "iseq.h"
#include "mjit.h"
#include "insns.inc"
#include "insns_info.inc"
#include "vm_insnhelper.h"

/* Info about insn resulted into a mutation.  */
struct mjit_mutation_insns {
    enum ruby_vminsn_type insn;
    size_t pc; /* the relative insn pc.  */
};

/* State of global speculation.
   TODO: Fine grain flags for different bop redefinitions.  */
struct global_spec_state {
    /* The following flags reflect presence of tracing and basic
       operation redefinitions.  */
    unsigned int trace_p:1;
    unsigned int bop_redefined_p:1;
};

/* Set up S to the currently possible speculation state.  */
static void
setup_global_spec_state(struct global_spec_state *s) {
    int i;
    
    s->trace_p = ruby_vm_event_flags != 0;
    s->bop_redefined_p = FALSE;
    for (i = 0; i < BOP_LAST_; i++)
	if (GET_VM()->redefined_flag[i] != 0) {
	    s->bop_redefined_p = TRUE;
	    break;
	}
}

/* The unit structure that holds metadata of ISeq for its
   compilation.  */
struct rb_mjit_compile_info {
    char init_p;
    /* True if we use C vars for temporary Ruby variables during last
       iseq translation.  */
    char use_temp_vars_p;
    char used_code_p;
    /* The following flag reflects speculation about equality of ep
       and bp which we used during last iseq translation.  */
    char ep_neq_bp_p;
    /* -1 means we speculate that self has few instance variables.
       Positive means we speculate that self has > IVAR_SPEC instance
       variables and IVAR_SPEC > ROBJECT_EMBED_LEN_MAX.  Zero means we
       know nothing about the number.  */
    size_t ivar_spec;
    /* Serial number of self for speculative translations for nonzero
       ivar_spec. */
    rb_serial_t ivar_serial;
    /* The following member is used to generate a code with global
       speculation.  */
    struct global_spec_state spec_state;
    /* See the corresponding fields in iseq_constant_body.  The values
       are saved for GCed iseqs.  */
    unsigned long resume_calls, stop_calls, jit_calls, failed_jit_calls;
    /* Number of JIT code mutations (and cancellations).  */
    int jit_mutations_num;
    /* Array of structures describing insns which initiated mutations.
       The array has JIT_MUTATIONS_NUM defined elements.  */
    struct mjit_mutation_insns *mutation_insns;
    /* map: var (local or temp) num -> flag of that it might be a double */
    char doubles_p[1];
};

/* All or most code for execution of any byte code insn is contained
   in the corresponding C function (see rtl_exec.c).  The following
   structure describes how to use the function (mostly its parameters
   passing) to implement the insn.  */
struct insn_fun_features {
    /* Pass argument ec.  */
    char ec_p : 1;
    /* True if the first insn operand is a continuation insn (see
       comments of insns.def).  We don't pass such operands.  */
    char skip_first_p : 1;
    /* Just go to spec section if the function returns non-zero.  */
    char op_end_p : 1;
    /* Pass structure calling and call function mjit_call_method
       afterwards.  */
    char call_p : 1;
    /* Defined only for call insns.  True if the call recv should be
       present on the stack.  */
    char recv_p : 1;
    /* Jump the dest (1st of 2nd insn operand) if the function returns
       non-zero.  */
    char jmp_p : 1;
    /* It is a bcmp insn, call function mjit_bcmp_end if it is
       necessary.  */
    char bcmp_p : 1;
    /* A value passed to function jmp_bcmp_end.  */
    char jmp_true_p : 1;
    /* Use a separate code to generate C code for the insn.  */
    char special_p : 1;
    /* Flag of an insn which can become a speculative one.  */
    char changing_p : 1;
    /* Flag of a speculative insn.  */
    char speculative_p : 1;
    /* Flag of a simple insn whose all operands are given by one
       operand number.  */
    char simple_p : 1;
    /* True if it is a speculative compare double insn or other double
       insn.  */
    char cmp_double_p : 1, double_p : 1;
    /* True if any speculative double insn contains imm. double
       operand.  */
    char imm_double_p : 1;
    /* True if it is some special assignment insn, e.g. swap.  */
    char special_assign_p : 1;
    /* True if it is frame related insn.  */
    char frame_p : 1;
    /* Number of insn operand which is the result.  Zero if there is
       no result.  */
    signed char result;
};

/* Return features of C function corresponding to the byte code INSN
   through F.  */
static void
get_insn_fun_features(VALUE insn, struct insn_fun_features *f) {
    f->ec_p = f->skip_first_p = f->op_end_p = f->bcmp_p = FALSE;
    f->call_p = f->recv_p = f->jmp_p = f->jmp_true_p = FALSE;
    f->special_p = f->changing_p = f->speculative_p = f->simple_p = FALSE;
    f->imm_double_p = f->double_p = f->cmp_double_p = f->special_assign_p = f->frame_p = FALSE;
    f->result = 2;
    switch (insn) {
    case BIN(const_cached_val_ld):
	f->frame_p = TRUE;
	/* Fall through.  */
    case BIN(special2var):
    case BIN(defined_p):
    case BIN(val_defined_p):
    case BIN(check_match):
	f->result = 1;
	f->ec_p = TRUE;
	break;
    case BIN(define_class):
	f->frame_p = TRUE;
	/* Fall through.  */
    case BIN(var2special):
	f->result = 0;
	f->ec_p = TRUE;
	break;
    case BIN(const2var):
    case BIN(const_ld_val):
	f->ec_p = TRUE;
	break;
    case BIN(simple_call):
    case BIN(call):
    case BIN(vmcore_call):
    case BIN(call_super):
	f->recv_p = TRUE;
	/* Fall through.  */
    case BIN(simple_call_self):
    case BIN(simple_call_recv):
    case BIN(call_self):
    case BIN(call_recv):
    case BIN(call_super_val):
	f->frame_p = f->ec_p = f->call_p = TRUE;
	break;
    case BIN(length):
    case BIN(size):
    case BIN(empty_p):
    case BIN(succ):
    case BIN(not):
    case BIN(plus):
    case BIN(minus):
    case BIN(mult):
    case BIN(div):
    case BIN(mod):
    case BIN(ltlt):
    case BIN(ind):
    case BIN(eq):
    case BIN(ne):
    case BIN(lt):
    case BIN(gt):
    case BIN(le):
    case BIN(ge):
    case BIN(plusi):
    case BIN(minusi):
    case BIN(multi):
    case BIN(divi):
    case BIN(modi):
    case BIN(plusf):
    case BIN(minusf):
    case BIN(multf):
    case BIN(divf):
    case BIN(modf):
    case BIN(ltlti):
    case BIN(indi):
    case BIN(inds):
    case BIN(eqi):
    case BIN(eqf):
    case BIN(nei):
    case BIN(nef):
    case BIN(lti):
    case BIN(ltf):
    case BIN(gti):
    case BIN(gtf):
    case BIN(lei):
    case BIN(lef):
    case BIN(gei):
    case BIN(gef):
	f->changing_p = TRUE;
	/* falls through */
    case BIN(unot):
    case BIN(uplus):
    case BIN(uminus):
    case BIN(umult):
    case BIN(udiv):
    case BIN(umod):
    case BIN(ueq):
    case BIN(une):
    case BIN(ult):
    case BIN(ugt):
    case BIN(ule):
    case BIN(uge):
    case BIN(uplusi):
    case BIN(uminusi):
    case BIN(umulti):
    case BIN(udivi):
    case BIN(umodi):
    case BIN(uplusf):
    case BIN(uminusf):
    case BIN(umultf):
    case BIN(udivf):
    case BIN(umodf):
    case BIN(ueqi):
    case BIN(ueqf):
    case BIN(unei):
    case BIN(unef):
    case BIN(ulti):
    case BIN(ultf):
    case BIN(ugti):
    case BIN(ugtf):
    case BIN(ulei):
    case BIN(ulef):
    case BIN(ugei):
    case BIN(ugef):
    case BIN(regexp_match2):
    case BIN(uind):
    case BIN(uindi):
    case BIN(uinds):
	f->ec_p = f->op_end_p = TRUE;
	break;
    case BIN(splus):
    case BIN(sminus):
    case BIN(smult):
    case BIN(sdiv):
    case BIN(smod):
    case BIN(seq):
    case BIN(sne):
    case BIN(slt):
    case BIN(sgt):
    case BIN(sle):
    case BIN(sge):
	f->changing_p = TRUE;
	/* falls through */
    case BIN(suplus):
    case BIN(suminus):
    case BIN(sumult):
    case BIN(sudiv):
    case BIN(sumod):
    case BIN(sueq):
    case BIN(sune):
    case BIN(sult):
    case BIN(sugt):
    case BIN(sule):
    case BIN(suge):
	f->simple_p = f->ec_p = f->op_end_p = TRUE;
	break;
    case BIN(feqf):
    case BIN(fnef):
    case BIN(fltf):
    case BIN(fgtf):
    case BIN(flef):
    case BIN(fgef):
	f->imm_double_p = TRUE;
	/* falls through */
    case BIN(feq):
    case BIN(fne):
    case BIN(flt):
    case BIN(fgt):
    case BIN(fle):
    case BIN(fge):
	f->speculative_p = f->cmp_double_p = TRUE;
	break;
    case BIN(fplusf):
    case BIN(fminusf):
    case BIN(fmultf):
    case BIN(fdivf):
    case BIN(fmodf):
	f->imm_double_p = TRUE;
	/* falls through */
    case BIN(fplus):
    case BIN(fminus):
    case BIN(fmult):
    case BIN(fdiv):
    case BIN(fmod):
	f->double_p = TRUE;
	/* falls through */
    case BIN(spec_not):
    case BIN(iplus):
    case BIN(iminus):
    case BIN(imult):
    case BIN(idiv):
    case BIN(imod):
    case BIN(aind):
    case BIN(hind):
    case BIN(ieq):
    case BIN(ine):
    case BIN(ilt):
    case BIN(igt):
    case BIN(ile):
    case BIN(ige):
    case BIN(iplusi):
    case BIN(iminusi):
    case BIN(imulti):
    case BIN(idivi):
    case BIN(imodi):
    case BIN(aindi):
    case BIN(hindi):
    case BIN(hinds):
    case BIN(ieqi):
    case BIN(inei):
    case BIN(ilti):
    case BIN(igti):
    case BIN(ilei):
    case BIN(igei):
	f->speculative_p = TRUE;
	break;
    case BIN(sfplus):
    case BIN(sfminus):
    case BIN(sfmult):
    case BIN(sfdiv):
    case BIN(sfmod):
	f->double_p = TRUE;
	/* falls through */
    case BIN(siplus):
    case BIN(siminus):
    case BIN(simult):
    case BIN(sidiv):
    case BIN(simod):
    case BIN(sieq):
    case BIN(sine):
    case BIN(silt):
    case BIN(sigt):
    case BIN(sile):
    case BIN(sige):
	f->simple_p = f->speculative_p = TRUE;
	break;
    case BIN(sfeq):
    case BIN(sfne):
    case BIN(sflt):
    case BIN(sfgt):
    case BIN(sfle):
    case BIN(sfge):
	f->cmp_double_p = f->simple_p = f->speculative_p = TRUE;
	break;
    case BIN(indset):
    case BIN(indseti):
    case BIN(indsets):
	f->changing_p = TRUE;
	/* falls through */
    case BIN(uindset):
    case BIN(uindseti):
    case BIN(uindsets):
	f->result = 0;
	f->ec_p = f->op_end_p = TRUE;
	break;
    case BIN(aindset):
    case BIN(hindset):
    case BIN(aindseti):
    case BIN(hindseti):
    case BIN(hindsets):
	f->result = 0;
	f->speculative_p = TRUE;
	break;
    case BIN(goto):
    case BIN(case_dispatch):
	f->result = 0;
	/* falls through */
    case BIN(get_inline_cache):
	f->special_p = TRUE;
	break;
    case BIN(bt):
    case BIN(bf):
    case BIN(bnil):
    case BIN(bkw):
    case BIN(btype):
	f->result = 0;
	f->ec_p = f->jmp_p = TRUE;
	break;
    case BIN(bteq):
    case BIN(btne):
    case BIN(btlt):
    case BIN(btgt):
    case BIN(btle):
    case BIN(btge):
    case BIN(bteqi):
    case BIN(bteqf):
    case BIN(btnei):
    case BIN(btnef):
    case BIN(btlti):
    case BIN(btltf):
    case BIN(btgti):
    case BIN(btgtf):
    case BIN(btlei):
    case BIN(btlef):
    case BIN(btgei):
    case BIN(btgef):
	f->changing_p = TRUE;
	/* falls through */
    case BIN(ubteq):
    case BIN(ubtne):
    case BIN(ubtlt):
    case BIN(ubtgt):
    case BIN(ubtle):
    case BIN(ubtge):
    case BIN(ubteqi):
    case BIN(ubteqf):
    case BIN(ubtnei):
    case BIN(ubtnef):
    case BIN(ubtlti):
    case BIN(ubtltf):
    case BIN(ubtgti):
    case BIN(ubtgtf):
    case BIN(ubtlei):
    case BIN(ubtlef):
    case BIN(ubtgei):
    case BIN(ubtgef):
	f->result = 4;
	f->ec_p = f->jmp_p = f->jmp_true_p = f->bcmp_p = f->skip_first_p = TRUE;
	break;
    case BIN(bfeq):
    case BIN(bfne):
    case BIN(bflt):
    case BIN(bfgt):
    case BIN(bfle):
    case BIN(bfge):
    case BIN(bfeqi):
    case BIN(bfeqf):
    case BIN(bfnei):
    case BIN(bfnef):
    case BIN(bflti):
    case BIN(bfltf):
    case BIN(bfgti):
    case BIN(bfgtf):
    case BIN(bflei):
    case BIN(bflef):
    case BIN(bfgei):
    case BIN(bfgef):
	f->changing_p = TRUE;
	/* falls through */
    case BIN(ubfeq):
    case BIN(ubfne):
    case BIN(ubflt):
    case BIN(ubfgt):
    case BIN(ubfle):
    case BIN(ubfge):
    case BIN(ubfeqi):
    case BIN(ubfeqf):
    case BIN(ubfnei):
    case BIN(ubfnef):
    case BIN(ubflti):
    case BIN(ubfltf):
    case BIN(ubfgti):
    case BIN(ubfgtf):
    case BIN(ubflei):
    case BIN(ubflef):
    case BIN(ubfgei):
    case BIN(ubfgef):
	f->result = 4;
	f->ec_p = f->jmp_p = f->bcmp_p = f->skip_first_p = TRUE;
	break;
    case BIN(fbteqf):
    case BIN(fbtnef):
    case BIN(fbtltf):
    case BIN(fbtgtf):
    case BIN(fbtlef):
    case BIN(fbtgef):
	f->imm_double_p = TRUE;
	/* falls through */
    case BIN(fbteq):
    case BIN(fbtne):
    case BIN(fbtlt):
    case BIN(fbtgt):
    case BIN(fbtle):
    case BIN(fbtge):
	f->result = 4;
	f->jmp_true_p = f->double_p = f->jmp_p = f->bcmp_p = f->skip_first_p = f->speculative_p = TRUE;
	break;
    case BIN(fbfeqf):
    case BIN(fbfnef):
    case BIN(fbfltf):
    case BIN(fbfgtf):
    case BIN(fbflef):
    case BIN(fbfgef):
	f->imm_double_p = TRUE;
	/* falls through */
    case BIN(fbfeq):
    case BIN(fbfne):
    case BIN(fbflt):
    case BIN(fbfgt):
    case BIN(fbfle):
    case BIN(fbfge):
	f->result = 4;
	f->double_p = f->jmp_p = f->bcmp_p = f->skip_first_p = f->speculative_p = TRUE;
	break;
    case BIN(ibteq):
    case BIN(ibtne):
    case BIN(ibtlt):
    case BIN(ibtgt):
    case BIN(ibtle):
    case BIN(ibtge):
    case BIN(ibteqi):
    case BIN(ibtnei):
    case BIN(ibtlti):
    case BIN(ibtgti):
    case BIN(ibtlei):
    case BIN(ibtgei):
	f->jmp_true_p = TRUE;
	/* falls through */
    case BIN(ibfeq):
    case BIN(ibfne):
    case BIN(ibflt):
    case BIN(ibfgt):
    case BIN(ibfle):
    case BIN(ibfge):
    case BIN(ibfeqi):
    case BIN(ibfnei):
    case BIN(ibflti):
    case BIN(ibfgti):
    case BIN(ibflei):
    case BIN(ibfgei):
	f->result = 4;
	f->jmp_p = f->bcmp_p = f->skip_first_p = f->speculative_p = TRUE;
	break;
    case BIN(temp_ret):
    case BIN(loc_ret):
    case BIN(val_ret):
    case BIN(raise_except):
    case BIN(raise_except_val):
    case BIN(call_block):
	f->call_p = f->frame_p = TRUE;
	/* falls through */
    case BIN(nop):
	f->special_p = TRUE;
	f->result = 0;
	break;
    case BIN(var2var):
    case BIN(var_swap):
    case BIN(temp_swap):
    case BIN(temp_reverse):
	f->special_assign_p = TRUE;
	break;
    case BIN(temp2temp):
    case BIN(loc2loc):
    case BIN(loc2temp):
    case BIN(temp2loc):
    case BIN(uploc2temp):
    case BIN(uploc2var):
    case BIN(val2temp):
    case BIN(val2loc):
    case BIN(str2var):
    case BIN(specialobj2var):
    case BIN(self2var):
    case BIN(global2var):
    case BIN(ivar2var):
    case BIN(cvar2var):
    case BIN(iseq2var):
    case BIN(to_string):
    case BIN(concat_strings):
    case BIN(to_regexp):
    case BIN(str_freeze_call):
    case BIN(str_uminus):
    case BIN(make_range):
    case BIN(make_array):
    case BIN(make_hash):
    case BIN(new_array_min):
    case BIN(new_array_max):
    case BIN(clone_array):
    case BIN(spread_array):
    case BIN(splat_array):
    case BIN(concat_array):
    case BIN(check_keyword):
    case BIN(regexp_match1):
    case BIN(str2sym):
	f->result = 1;
	break;
    case BIN(set_inline_cache):
    case BIN(var2uploc):
    case BIN(val2uploc):
    case BIN(var2const):
    case BIN(var2global):
    case BIN(temp2ivar):
    case BIN(loc2ivar):
    case BIN(val2ivar):
    case BIN(var2cvar):
    case BIN(freeze_string):
	f->result = 0;
	break;
    case BIN(run_once):
    case BIN(get_block_param):
      f->ec_p = TRUE;
    case BIN(get_block_param_proxy):
      f->result = 1;
    case BIN(set_block_param):
      break;
    case BIN(call_c_func):
	/* C code for the following insns should be never
	   generated.  */
    case BIN(cont_btcmp):
    case BIN(cont_bfcmp):
    default:
	fprintf(stderr, "Not implemented %s\n", insn_name(insn));
	abort();
	break;
    }
}

/* Return a safe version of INSN.  */
static VALUE
get_safe_insn(VALUE insn) {
    switch (insn) {
    case BIN(not): case BIN(spec_not): return BIN(unot);
    case BIN(plus): case BIN(iplus): case BIN(fplus): return BIN(uplus);
    case BIN(minus): case BIN(iminus): case BIN(fminus): return BIN(uminus);
    case BIN(mult): case BIN(imult): case BIN(fmult): return BIN(umult);
    case BIN(div): case BIN(idiv): case BIN(fdiv): return BIN(udiv);
    case BIN(mod): case BIN(imod): case BIN(fmod): return BIN(umod);
    case BIN(eq): case BIN(ieq): case BIN(feq): return BIN(ueq);
    case BIN(ne): case BIN(ine): case BIN(fne): return BIN(une);
    case BIN(lt): case BIN(ilt): case BIN(flt): return BIN(ult);
    case BIN(gt): case BIN(igt): case BIN(fgt): return BIN(ugt);
    case BIN(le): case BIN(ile): case BIN(fle): return BIN(ule);
    case BIN(ge): case BIN(ige): case BIN(fge): return BIN(uge);
    case BIN(splus): case BIN(siplus): case BIN(sfplus): return BIN(suplus);
    case BIN(sminus): case BIN(siminus): case BIN(sfminus): return BIN(suminus);
    case BIN(smult): case BIN(simult): case BIN(sfmult): return BIN(sumult);
    case BIN(sdiv): case BIN(sidiv): case BIN(sfdiv): return BIN(sudiv);
    case BIN(smod): case BIN(simod): case BIN(sfmod): return BIN(sumod);
    case BIN(seq): case BIN(sieq): case BIN(sfeq): return BIN(sueq);
    case BIN(sne): case BIN(sine): case BIN(sfne): return BIN(sune);
    case BIN(slt): case BIN(silt): case BIN(sflt): return BIN(sult);
    case BIN(sgt): case BIN(sigt): case BIN(sfgt): return BIN(sugt);
    case BIN(sle): case BIN(sile): case BIN(sfle): return BIN(sule);
    case BIN(sge): case BIN(sige): case BIN(sfge): return BIN(suge);
    case BIN(plusi): case BIN(iplusi): return BIN(uplusi);
    case BIN(minusi): case BIN(iminusi): return BIN(uminusi);
    case BIN(multi): case BIN(imulti): return BIN(umulti);
    case BIN(divi): case BIN(idivi): return BIN(udivi);
    case BIN(modi): case BIN(imodi): return BIN(umodi);
    case BIN(eqi): case BIN(ieqi): return BIN(ueqi);
    case BIN(nei): case BIN(inei): return BIN(unei);
    case BIN(lti): case BIN(ilti): return BIN(ulti);
    case BIN(gti): case BIN(igti): return BIN(ugti);
    case BIN(lei): case BIN(ilei): return BIN(ulei);
    case BIN(gei): case BIN(igei): return BIN(ugei);
    case BIN(plusf): case BIN(fplusf): return BIN(uplusf);
    case BIN(minusf): case BIN(fminusf): return BIN(uminusf);
    case BIN(multf): case BIN(fmultf): return BIN(umultf);
    case BIN(divf): case BIN(fdivf): return BIN(udivf);
    case BIN(modf): case BIN(fmodf): return BIN(umodf);
    case BIN(eqf): case BIN(feqf): return BIN(ueqf);
    case BIN(nef): case BIN(fnef): return BIN(unef);
    case BIN(ltf): case BIN(fltf): return BIN(ultf);
    case BIN(gtf): case BIN(fgtf): return BIN(ugtf);
    case BIN(lef): case BIN(flef): return BIN(ulef);
    case BIN(gef): case BIN(fgef): return BIN(ugef);
    case BIN(bteq): case BIN(ibteq): case BIN(fbteq): return BIN(ubteq);
    case BIN(btne): case BIN(ibtne): case BIN(fbtne): return BIN(ubtne);
    case BIN(btlt): case BIN(ibtlt): case BIN(fbtlt): return BIN(ubtlt);
    case BIN(btgt): case BIN(ibtgt): case BIN(fbtgt): return BIN(ubtgt);
    case BIN(btle): case BIN(ibtle): case BIN(fbtle): return BIN(ubtle);
    case BIN(btge): case BIN(ibtge): case BIN(fbtge): return BIN(ubtge);
    case BIN(bteqi): case BIN(ibteqi): return BIN(ubteqi);
    case BIN(btnei): case BIN(ibtnei): return BIN(ubtnei);
    case BIN(btlti): case BIN(ibtlti): return BIN(ubtlti);
    case BIN(btgti): case BIN(ibtgti): return BIN(ubtgti);
    case BIN(btlei): case BIN(ibtlei): return BIN(ubtlei);
    case BIN(btgei): case BIN(ibtgei): return BIN(ubtgei);
    case BIN(bteqf): case BIN(fbteqf): return BIN(ubteqf);
    case BIN(btnef): case BIN(fbtnef): return BIN(ubtnef);
    case BIN(btltf): case BIN(fbtltf): return BIN(ubtltf);
    case BIN(btgtf): case BIN(fbtgtf): return BIN(ubtgtf);
    case BIN(btlef): case BIN(fbtlef): return BIN(ubtlef);
    case BIN(btgef): case BIN(fbtgef): return BIN(ubtgef);
    case BIN(bfeq): case BIN(ibfeq): case BIN(fbfeq): return BIN(ubfeq);
    case BIN(bfne): case BIN(ibfne): case BIN(fbfne): return BIN(ubfne);
    case BIN(bflt): case BIN(ibflt): case BIN(fbflt): return BIN(ubflt);
    case BIN(bfgt): case BIN(ibfgt): case BIN(fbfgt): return BIN(ubfgt);
    case BIN(bfle): case BIN(ibfle): case BIN(fbfle): return BIN(ubfle);
    case BIN(bfge): case BIN(ibfge): case BIN(fbfge): return BIN(ubfge);
    case BIN(bfeqi): case BIN(ibfeqi): return BIN(ubfeqi);
    case BIN(bfnei): case BIN(ibfnei): return BIN(ubfnei);
    case BIN(bflti): case BIN(ibflti): return BIN(ubflti);
    case BIN(bfgti): case BIN(ibfgti): return BIN(ubfgti);
    case BIN(bflei): case BIN(ibflei): return BIN(ubflei);
    case BIN(bfgei): case BIN(ibfgei): return BIN(ubfgei);
    case BIN(bfeqf): case BIN(fbfeqf): return BIN(ubfeqf);
    case BIN(bfnef): case BIN(fbfnef): return BIN(ubfnef);
    case BIN(bfltf): case BIN(fbfltf): return BIN(ubfltf);
    case BIN(bfgtf): case BIN(fbfgtf): return BIN(ubfgtf);
    case BIN(bflef): case BIN(fbflef): return BIN(ubflef);
    case BIN(bfgef): case BIN(fbfgef): return BIN(ubfgef);
    default:
	return insn;
    }
}



/* Minimal number of chars to keep NBITS bits.  */
static size_t bit2char(size_t nbits) { return (nbits + CHAR_BIT - 1) / CHAR_BIT; }

/* Set up NBIT bit in V.  */
static void
set_bit(char *v, size_t nbit) {
    v[nbit / CHAR_BIT] |= 1 << (nbit % CHAR_BIT);
}

/* Reset up NBIT bit in V.  */
static void
reset_bit(char *v, size_t nbit) {
    v[nbit / CHAR_BIT] &= ~(1 << (nbit % CHAR_BIT));
}

/* Get NBIT bit in V.  */
static int
get_bit(char *v, size_t nbit) {
    return (v[nbit / CHAR_BIT] >> (nbit % CHAR_BIT)) & 1;
}

#ifndef CFG_DEBUG
#define CFG_DEBUG 0
#endif

/* Basic block */
struct bb;

/* CFG edge (an edge connecting two BBs) */
struct edge {
    /* Source and destination of the edge  */
    struct bb *src, *dst;
    /* All edges with the same source or destination are linked
       through the following fields.  */
    struct list_node src_link;
    struct list_node dst_link;
};

struct bb {
    size_t num; /* basic block order number. */
    /* Position of the first BB insn and position right after the last
       BB insn.  */
    size_t start, end;
    /* Used for some algorithms.  */
    size_t temp;
    /* True if BB can be achieved through exception table using a
       continuation label.  */
    int except_p;
    /* Bit sets (vectors) containg kill, gen, in, and out sets for
       calculation variables which are of floating point type.  */
    char *kill, *gen, *in, *out;
    /* All bbs are linked through the following field.  */
    struct list_node bb_link;
    /* Lists of edges where given BB is a source or destination.  */
    struct list_head in_edges, out_edges;
};

/* Control flow graph of a method being JITted.  */
struct cfg {
    size_t bbs_num; /* number of BBs in CFG  */
    /* A map: insn pos to BB containg the insn:  */
    struct bb **pos2bb;
    /* Artificial BBs which are enter and exit of the CFG: */
    struct bb *entry_bb, *exit_bb;
    /* List of all CFG BBs:  */
    struct list_head bbs;
};

/* Create and return an edges from SRC to DST.  Include the edge into
   BB edge lists.  Return NULL in case of allocation error.  */
static struct edge *create_edge(struct bb *src, struct bb *dst) {
    struct edge *edge = xmalloc(sizeof (struct edge));

    if (edge == NULL)
	return NULL;
    edge->src = src;
    edge->dst = dst;
    list_add_tail(&dst->in_edges, &edge->dst_link);
    list_add_tail(&src->out_edges, &edge->src_link);
    return edge;
}

/* Free BB sets. */
static void
free_bb_sets(struct bb *bb) {
    if (bb->kill != NULL)
	free(bb->kill);
    if (bb->gen != NULL)
	free(bb->gen);
    if (bb->in != NULL)
	free(bb->in);
    if (bb->out != NULL)
	free(bb->out);
}

/* Create and return BB whose the first insn is at position START.
   Allocate BB sets for VARS_NUM locals and temps.  Return NULL in
   case of allocation error.  */
static struct bb *
create_bb(size_t start, size_t vars_num) {
    struct bb *bb = xmalloc(sizeof (struct bb));
    size_t size = bit2char(vars_num);
    
    if (bb == NULL)
	return NULL;
    bb->kill = bb->gen = bb->in = bb->out = NULL;
    if ((bb->kill = xmalloc(sizeof(char) * size)) == NULL
	|| (bb->gen = xmalloc(sizeof(char) * size)) == NULL
	|| (bb->in = xmalloc(sizeof(char) * size)) == NULL
	|| (bb->out = xmalloc(sizeof(char) * size)) == NULL) {
	free_bb_sets(bb);
	return NULL;
    }
    bb->except_p = FALSE;
    bb->start = start;
    bb->end = 0;
    list_head_init(&bb->in_edges);
    list_head_init(&bb->out_edges);
    return bb;
}

/* Free BB and its sets and all edges coming into BB.  Remove BB from
   the BB list if UNLIST_P.  */
static void
free_bb(struct bb *bb, int unlist_p) {
    struct edge *edge, *next_edge;

    if (unlist_p)
	list_del(&bb->bb_link);
    list_for_each_safe(&bb->in_edges, edge, next_edge, dst_link) {
	list_del(&edge->dst_link);
	free(edge);
    }
    free_bb_sets(bb);
    free(bb);
}

/* Free all CFG (including BBs and edges).  */
static void
free_cfg(struct cfg *cfg) {
    struct bb *bb, *next_bb;
    
    if (cfg == NULL)
	return;
    if (cfg->pos2bb != NULL)
	free(cfg->pos2bb);
    if (cfg->entry_bb != NULL)
	free_bb(cfg->entry_bb, FALSE);
    if (cfg->exit_bb != NULL)
	free_bb(cfg->exit_bb, FALSE);
    list_for_each_safe(&cfg->bbs, bb, next_bb, bb_link) {
	free_bb(bb, TRUE);
    }
    free(cfg);
}

/* Return BB in CFG starting at position POS.  Return NULL in case of
   error.  Create BB if it is not created yet.  In case of creation,
   include it into BBS list and update CFG->POS2BB.  */
static struct bb *
create_bb_from_pos(size_t pos, struct cfg *cfg,
				     struct list_head *bbs, size_t vars_num) {
    struct bb *curr_bb;
    
    if ((curr_bb = cfg->pos2bb[pos]) == NULL) {
	if ((curr_bb = create_bb(pos, vars_num)) == NULL)
	    return NULL;
	cfg->pos2bb[pos] = curr_bb;
	list_add_tail(bbs, &curr_bb->bb_link);
    }
    return curr_bb;
}

/* Build and return CFG for ISEQ.  Return NULL in case of
   allocation error.  In case of error, free all already allocated
   memory for CFG.  BBs in CFG are ordered according their original
   order in the iseq.  */
static struct cfg *
build_cfg(const rb_iseq_t *iseq) {
    struct cfg *cfg = xmalloc(sizeof (struct cfg));
    struct rb_iseq_constant_body *body = iseq->body;
    const VALUE *code = body->rtl_encoded;
    size_t i, pos, len, dst, size = body->rtl_size;
    size_t vars_num = body->local_table_size + body->temp_vars_num;
    int create_bb_p, prev_jmp_p;
    VALUE insn;
    struct insn_fun_features features;
    struct bb *bb, *curr_bb, *next_bb;
    const struct iseq_catch_table *rtl_catch_table = body->rtl_catch_table;
    LIST_HEAD(non_cfg_bbs);

    if (cfg == NULL)
	return NULL;
    cfg->bbs_num = 0;
    cfg->entry_bb = cfg->exit_bb = NULL;
    list_head_init(&cfg->bbs);
    if ((cfg->entry_bb = create_bb(0, vars_num)) == NULL)
	goto err;
    cfg->entry_bb->num = cfg->bbs_num++;
    cfg->pos2bb = xmalloc(sizeof (struct bb *) * size);
    if (cfg->pos2bb == NULL) {
	free(cfg);
	return NULL;
    }
    memset (cfg->pos2bb, 0, sizeof (struct bb *) * size);
    if (rtl_catch_table != NULL)
	for (i = 0; i < rtl_catch_table->size; i++) {
	    if ((bb = create_bb_from_pos(rtl_catch_table->entries[i].cont, cfg,
					 &non_cfg_bbs, vars_num)) == NULL)
		goto err;
	    bb->except_p = TRUE;
	}
    for (pos = 0; pos < size;) { /* Create bbs for labels. */
	insn = code[pos];
#if OPT_DIRECT_THREADED_CODE || OPT_CALL_THREADED_CODE
	insn = rb_vm_insn_addr2insn((void *) insn);
#endif
	len = insn_len(insn);
	get_insn_fun_features(insn, &features);
	if (features.jmp_p || insn == BIN(goto)) {
	    dst = pos + len + code[pos + (! features.bcmp_p || insn == BIN(goto) ? 1 : 2)];
	    if (create_bb_from_pos(dst, cfg, &non_cfg_bbs, vars_num) == NULL)
		goto err;
	}
	pos += len;
    }
    curr_bb = NULL;
    create_bb_p = TRUE;
    prev_jmp_p = FALSE;
    for (pos = 0; pos < size;) {
	if ((bb = cfg->pos2bb[pos]) != NULL || create_bb_p) {
	    if (bb != NULL)
		list_del(&bb->bb_link);
	    else if ((bb = create_bb(pos, vars_num)) == NULL)
		goto err;
	    bb->num = cfg->bbs_num++;
	    list_add_tail(&cfg->bbs, &bb->bb_link);
	    if (curr_bb != NULL) {
		if (! prev_jmp_p && create_edge(curr_bb, bb) == NULL)
		    goto err;
		curr_bb->end = pos;
	    }
	    curr_bb = bb;
	}
	cfg->pos2bb[pos] = curr_bb;
	create_bb_p = FALSE;
	insn = code[pos];
#if OPT_DIRECT_THREADED_CODE || OPT_CALL_THREADED_CODE
	insn = rb_vm_insn_addr2insn((void *) insn);
#endif
	len = insn_len(insn);
	get_insn_fun_features(insn, &features);
	if (features.jmp_p || insn == BIN(goto)) {
	    dst = pos + len + code[pos + (! features.bcmp_p || insn == BIN(goto) ? 1 : 2)];
	    curr_bb->end = pos + len;
	    bb = cfg->pos2bb[dst];
	    assert(bb != NULL);
	    if (create_edge(curr_bb, bb) == NULL)
		goto err;
	    create_bb_p = TRUE;
	}
	prev_jmp_p = insn == BIN(goto);
	pos += len;
    }
    if (curr_bb != NULL)
	curr_bb->end = pos;
    assert(list_top(&non_cfg_bbs, struct bb, bb_link) == NULL);
    if ((cfg->exit_bb = create_bb(size, vars_num)) == NULL)
	goto err;
    cfg->exit_bb->num = cfg->bbs_num++;
    list_for_each_safe(&cfg->bbs, bb, next_bb, bb_link) {
	if (list_top(&bb->in_edges, struct edge, dst_link) == NULL
	    && create_edge(cfg->entry_bb, bb) == NULL)
	    goto err;
	if (list_top(&bb->out_edges, struct edge, src_link) == NULL
	    && create_edge(bb, cfg->exit_bb) == NULL)
	    goto err;
    }
    return cfg;
 err:
    if (cfg->entry_bb != NULL)
	free_bb(cfg->entry_bb, FALSE);
    if (cfg->exit_bb != NULL)
	free_bb(cfg->exit_bb, FALSE);
    list_for_each_safe(&non_cfg_bbs, bb, next_bb, bb_link) {
	free_bb(bb, TRUE);
    }
    free_cfg(cfg);
    return NULL;
}

#if CFG_DEBUG

/* Print all incoming (if IN_P) or outcoming BB edges.  */
static void
print_edges(int in_p, struct bb *bb) {
    struct edge *e;
    
    fprintf (stderr, " %s edges:", in_p ? "in" : "out");
    if (in_p) {
	list_for_each(&bb->in_edges, e, dst_link) {
	    fprintf(stderr, " %lu", (unsigned long) e->src->num);
	}
    } else {
	list_for_each(&bb->out_edges, e, src_link) {
	    fprintf(stderr, " %lu", (unsigned long) e->dst->num);
	}
    }
    fprintf (stderr, "\n");
}

/* Print set V of size NBITS.  Use title NAME.  */
static void
print_set(const char *name, char *v, size_t nbits) {
    size_t i;
    
    fprintf (stderr, "  %s:", name);
    for (i = 0; i < nbits; i++)
	if (get_bit(v, i))
	    fprintf (stderr, "  %3lu", (unsigned long) i);
    fprintf (stderr, "\n");
}

/* Print BB, its order number, its insns, sets, and incoming and
   outcoming edges.  */
static void
print_bb(const VALUE *code, struct bb *bb, size_t vars_num) {
    size_t pos;
    VALUE insn;
    
    fprintf (stderr, "BB %lu:\n", (unsigned long) bb->num);
    print_edges(TRUE, bb);
    print_edges(FALSE, bb);
    print_set("gen", bb->gen, vars_num);
    print_set("kill", bb->kill, vars_num);
    print_set("in", bb->in, vars_num);
    print_set("out", bb->out, vars_num);
    for (pos = bb->start; pos < bb->end; ) {
	insn = code[pos];
#if OPT_DIRECT_THREADED_CODE || OPT_CALL_THREADED_CODE
	insn = rb_vm_insn_addr2insn((void *) insn);
#endif
	fprintf(stderr, "%04lu %s\n", pos, insn_name(insn));
	pos += insn_len(insn);
    }
}

/* Print all CFG BBs of ISEQ.  */
static void print_cfg(const rb_iseq_t *iseq, struct cfg *cfg) {
    struct bb *bb;
    size_t vars_num = iseq->body->local_table_size + iseq->body->temp_vars_num;
    
    print_bb(iseq->body->rtl_encoded, cfg->entry_bb, vars_num);
    list_for_each(&cfg->bbs, bb, bb_link) {
	print_bb(iseq->body->rtl_encoded, bb, vars_num);
    }
    print_bb(iseq->body->rtl_encoded, cfg->exit_bb, vars_num);
}
#endif



/* Describes parameters affecting ISEQ compilation.  The parameters
   are calculated for every ISEQ compilation.  */
struct translation_control {
    /* True if we should not speculative insns.  */
    char safe_p;
    /* True to use C local variables for ISEQ RTL local and temporary
       variables.  */
    char use_local_vars_p;
    char use_temp_vars_p;
};

/* Return C code string representing address of local/temporary
   variable with index IND.  TCP defines where values of the ISEQ
   local/temporary variables will be kept inside C function
   representing the ISEQ.  */
static const char *
get_op_str(char *buf, ptrdiff_t ind, struct translation_control *tcp) {
    if (ind < 0) {
	if (tcp->use_temp_vars_p)
	    sprintf(buf, "&t%ld", (long) -ind - 1);
	else
	    sprintf(buf, "get_temp_addr(cfp, %ld)", (long) ind);
    } else if (! tcp->use_local_vars_p) {
	sprintf(buf, "get_loc_addr(cfp, %ld)", (long) ind);
    } else {
	sprintf(buf, "&v%ld", (long) ind - VM_ENV_DATA_SIZE);
    }
    return buf;
}

/* Move args of a call insn in code with position POS to the MRI
   stack.  Reserve a stack slot for the call receiver if RECV_P.  */
static void
generate_param_setup(FILE *f, const VALUE *code, size_t pos, int recv_p) {
    /* Always the 1st call insn operand.  */
    CALL_DATA cd = (CALL_DATA) code[pos + 1];
    /* Always the 2nd call insn operand.  */
    int call_start = -(int) (ptrdiff_t) code[pos + 2];
    int i, args_num = cd->call_info.orig_argc;

    if (cd->call_info.flag & VM_CALL_ARGS_BLOCKARG)
	args_num++;
    for (i = !recv_p; i <= args_num; i++)
	fprintf(f, "  *get_temp_addr(cfp, %d) = t%d;\n", -call_start - i, call_start + i - 1);
}

/* Return a string which is C assignment of failed_insn_pc of insn
   with POS.  Use BUF as a string container.  */
static const char *
set_failed_insn_str(char *buf, size_t pos) {
    sprintf(buf, "failed_insn_pc = %lu; ", pos);
    return buf;
}

/* Return string representing C code setting cfp pc to PC.  Use BUF as
   the string container.  */
static const char *
generate_set_pc(char *buf, const VALUE *pc) {
    sprintf(buf, "  cfp->pc = (void *) 0x%"PRIxVALUE ";\n", (VALUE) pc);
    return buf;
}

/* Return number of mutations which insn with POS from iseq with
   compile info COMPILE_INFO caused.  */
static int
get_insn_mutation_num(struct rb_mjit_compile_info *compile_info, size_t pos) {
    int i, num = 0;
    
    for (i = 0; i < compile_info->jit_mutations_num; i++)
	if (compile_info->mutation_insns[i].insn != BIN(nop)
	    && pos == compile_info->mutation_insns[i].pc)
	    num++;
    return num;
}

/* An aditional argument to generate cases for values in case_dispatch
   insn hash.  */
struct case_arg {
    long last_dst; /* last printed dst  */
    /* iseq basic offset for destinations in case_dispatch  */
    size_t offset;
    FILE *f; /* a file where to print */
};

/* Print a case for destination VAL in case_dispatch insn.  An
   additional info to do this is given by ARG. */
static int
print_case(st_data_t key, st_data_t val, st_data_t arg) {
    struct case_arg *case_arg = (struct case_arg *) arg;
    long dst = FIX2LONG(val);

    if (dst != case_arg->last_dst) {
	fprintf(case_arg->f, "    case %ld: goto l%ld;\n",
		dst, case_arg->offset + FIX2LONG(val));
	case_arg->last_dst = dst;
    }
    return ST_CONTINUE;
}

/* Return number for operand TO.  The iseq has LOCAL_TABLE_SIZE
   locals.  Local numbers (non-negative TO) starts with zero.
   Temporary (negative TO) numbers start after local ones.  */
static long
get_var_double_num(long to, int local_table_size) {
    to = to >= 0 ? to - VM_ENV_DATA_SIZE : -to + local_table_size - 1;
    assert(to >= 0);
    return to;
}

/* Update GEN and KILL for INSN BB in CFG of iseq with CODE and
   LOCAL_TABLE_SIZE locals.  Insn position, length, and features are
   correspondingly POS, LEN, and FEATURES.  Next insn is NEXT_INSN.
   Don't setup GEN and KILL for insn result if IGNORE_RESULT_P.  If F
   is not NULL, output code setting C double var for val2loc and
   val2temp with floating point value.  Return TRUE if we should
   ignore result of the next insn.  */
static int
update_bb_kill_gen_by_insn(FILE *f, const VALUE *code, int local_table_size, struct cfg *cfg,
			   VALUE insn, size_t pos, size_t len,
			   struct insn_fun_features *features,
			   int ignore_result_p, VALUE next_insn) {
    struct bb *bb;
    int ignore_next_p;
    long to;
    rb_num_t n;
    
    if (cfg == NULL)
	return FALSE;
    bb = cfg->pos2bb[pos];
    ignore_next_p = FALSE;
    if (features->special_assign_p) {
	switch (insn) {
	case BIN(var2var):
	    to = get_var_double_num((long) code[pos + 1], local_table_size);
	    n = (rb_num_t) code[pos + 3];
	    while (n-- > 0) {
		set_bit(bb->kill, to);
		reset_bit(bb->gen, to++);
	    }
	    break;
	case BIN(var_swap):
	case BIN(temp_swap):
	    assert(insn == BIN(var_swap) && (long) code[pos + 1] >= 0 && (long) code[pos + 2] >= 0
		   || insn == BIN(temp_swap) && (long) code[pos + 1] < 0 && (long) code[pos + 2] < 0);
	    to = get_var_double_num((long) code[pos + 1], local_table_size);
	    set_bit(bb->kill, to);
	    reset_bit(bb->gen, to);
	    to = get_var_double_num((long) code[pos + 2], local_table_size);
	    set_bit(bb->kill, to);
	    reset_bit(bb->gen, to);
	    break;
	case BIN(temp_reverse):
	    assert((long) code[pos + 2] < 0);
	    to = get_var_double_num((long) code[pos + 2], local_table_size);
	    n = (rb_num_t) code[pos + 1];
	    while (n-- > 0)
		set_bit(bb->kill, to);
	    reset_bit(bb->gen, to++);
	    break;
	default:
	    assert(FALSE);
	}
    } else if (features->result != 0) {
	to = get_var_double_num((long) code[pos + features->result], local_table_size);
	if (! features->double_p || features->cmp_double_p || features->bcmp_p) {
	  if ((insn != BIN(val2loc) && insn != BIN(val2temp)) || !FLONUM_P(code[pos + 2])) {
	    if (! ignore_result_p) {
	      set_bit(bb->kill, to);
	      reset_bit(bb->gen, to);
	    }
	  } else {
	    set_bit(bb->gen, to);
	    if (f != NULL)
	      fprintf(f, "  d%ld = rb_float_flonum_value(0x%"PRIxVALUE ");\n", to, code[pos + 2]);
	  }
	} else if (next_insn != BIN(temp2loc)
		   || code[pos + len + 2] != code[pos + features->result]
		   || bb != cfg->pos2bb[pos +len]) { /* not in the same BB */
	    set_bit(bb->gen, to);
	} else {
	    set_bit(bb->kill, to);
	    to = get_var_double_num((long) code[pos + len + 1], local_table_size);
	    assert(to >= 0);
	    set_bit(bb->gen, to);
	    ignore_next_p = TRUE;
	}
    }
    return ignore_next_p;
}

/* Return number of CFG double C var for result (if RES_P) or input
   operand OP of insn with length I_LEN at position POS in CODE whose
   next insn is NEXT_INSN.  LOCAL_TABLE_SIZE is number of locals for
   CFG iseq.  */
static long
get_op_double_num(struct cfg *cfg, int local_table_size, const VALUE *code,
		  size_t pos, int i_len, VALUE op, int res_p, VALUE next_insn) {
    long to;
    struct bb *bb;

    if (cfg == NULL)
	return -1;
    bb = cfg->pos2bb[pos];
    to = (long) op;
    if (res_p && next_insn == BIN(temp2loc) && code[pos + i_len + 2] == op
	&& bb == cfg->pos2bb[pos + i_len]) { /* The same BB */
	to = (long) code[pos + i_len + 1];
    }
    to = get_var_double_num(to, local_table_size);
    return ! res_p && ! get_bit(bb->gen, to) ? -1 : to;
}

/* Function used to sort CFG BBs according their order number.  */
static int
bb_compare(const void *a1, const void *a2) {
    const struct bb *bb1 = (const struct bb *) a1;
    const struct bb *bb2 = (const struct bb *) a2;

    return bb1->num < bb2->num ? -1 : bb1->num > bb2->num ? 1 : 0;
}

/* Return TRUE if BB is reachable in CFG.  */
static int
bb_reachable_p(struct bb *bb, struct cfg *cfg) {
    struct edge *e;
    
    if (bb->except_p)
	return TRUE; /* reachable by exception */
    list_for_each(&bb->in_edges, e, dst_link) {
	if (e->src != cfg->entry_bb)
	    return TRUE;
    }
    return FALSE;
}

/* Build CFG bb sets for ISEQ.

   Our data flow problem:
    bb.in = for all predecessors p: and (p.out);
    bb.out = bb.in and not bb.kill or bb.gen;

  Floating point arithmetic speculative insns set up GEN.  All other
  insns with results set up KILL.
*/
static void
build_bb_sets(const rb_iseq_t *iseq, struct cfg *cfg) {
    VALUE insn, next_insn;
    char b;
    struct insn_fun_features features;
    struct rb_iseq_constant_body *body = iseq->body;
    const VALUE *code = body->rtl_encoded;
    size_t pos, len, size = body->rtl_size;
    size_t i, n, iter, to, from;
    int change_p, first_p, ignore_result_p;
    size_t set_size = bit2char(body->temp_vars_num + body->local_table_size);
    struct bb *dst, *curr_bb = NULL;
    struct bb **bbs, **next_bbs, **temp;
    struct edge *e;

    /* Calculate gen and kill sets: */
    for (pos = 0, ignore_result_p = FALSE; pos < size; ) {
	if (cfg != NULL && curr_bb != cfg->pos2bb[pos]) { /* BB start */
	    curr_bb = cfg->pos2bb[pos];
	    memset(curr_bb->gen, 0, set_size);
	    memset(curr_bb->kill, 0, set_size);
	}
	insn = code[pos];
#if OPT_DIRECT_THREADED_CODE || OPT_CALL_THREADED_CODE
	insn = rb_vm_insn_addr2insn((void *) insn);
#endif
	len = insn_len(insn);
	if (pos + len >= size)
	    next_insn = BIN(nop);
	else {
	    next_insn = code[pos + len];
#if OPT_DIRECT_THREADED_CODE || OPT_CALL_THREADED_CODE
	    next_insn = rb_vm_insn_addr2insn((void *) next_insn);
#endif
	}
	get_insn_fun_features(insn, &features);
	ignore_result_p = update_bb_kill_gen_by_insn(NULL, code, body->local_table_size, cfg,
						   insn, pos, len, &features, ignore_result_p, next_insn);
	pos += len;
    }
    /* Calculate in and out.  It is forward data flow problem:  */
    if ((bbs = xmalloc(cfg->bbs_num * sizeof (struct bb *))) == NULL)
	return;
    if ((next_bbs = xmalloc(cfg->bbs_num * sizeof (struct bb *))) == NULL) {
	free(bbs);
	return;
    }
    n = 0;
    list_for_each(&cfg->bbs, curr_bb, bb_link) {
	curr_bb->temp = 0;
	bbs[n++] = curr_bb;
    }
    for (iter = 1;; iter++) {
	to = 0;
	for (from = 0; from < n; from++) {
	    curr_bb = bbs[from];
	    first_p = TRUE;
	    list_for_each(&curr_bb->in_edges, e, dst_link) {
		for (i = 0; i < set_size; i++)
		    if (e->src == cfg->entry_bb)
			curr_bb->in[i] = 0;
		    else if (first_p)
			curr_bb->in[i] = e->src->out[i];
		    else if (bb_reachable_p(e->src, cfg))
			curr_bb->in[i] &= e->src->out[i];
		first_p = FALSE;
	    }
	    change_p = FALSE;
	    for (i = 0; i < set_size; i++) {
		b = (curr_bb->in[i] & ~curr_bb->kill[i]) | curr_bb->gen[i];
		if (b != curr_bb->out[i])
		    change_p = TRUE;
		curr_bb->out[i] = b;
	    }
	    if (change_p) {
		list_for_each(&curr_bb->out_edges, e, src_link) {
		    dst = e->dst;
		    if (dst->temp != iter) {
			dst->temp = iter;
			next_bbs[to++] = dst;
		    }
		}
	    }
	}
	if (to == 0)
	    break;
	n = to;
	/* We could decrease the iteration number if we ordered BBs
	   according reverse their postorder in CFG.  But sorting
	   according BB order number is simple and pretty good enough
	   for a typical iseq GFG.  */
	qsort(next_bbs, n, sizeof(struct bb *), bb_compare);
	temp = bbs;
	bbs = next_bbs;
	next_bbs = temp;
    }
    free(bbs);
    free(next_bbs);
}

/* Output C code implementing an iseq UI insn starting with position
   POS to file F.  Generate the code according to TCP.  */
static int
translate_iseq_insn(FILE *f, size_t pos, const rb_iseq_t *iseq,
		    struct rb_mjit_compile_info *compile_info,
		    struct cfg *cfg, int *ignore_next_p, struct translation_control *tcp) {
    struct rb_iseq_constant_body *body = iseq->body;
    size_t size = body->rtl_size;
    const VALUE *code = body->rtl_encoded;
    VALUE insn, next_insn, op;
    int len, i, ivar_p, const_p, insn_mutation_num, ignore_result_p = *ignore_next_p;
    long to;
    const char *types;
    const char *iname;
    struct insn_fun_features features;
    struct rb_call_cache local_cc;
    struct iseq_inline_cache_entry local_ic;
    CALL_CACHE cc = NULL;
    CALL_INFO ci = NULL;
    char buf[150];

    insn_mutation_num = get_insn_mutation_num(compile_info, pos);
    insn = code[pos];
#if OPT_DIRECT_THREADED_CODE || OPT_CALL_THREADED_CODE
    insn = rb_vm_insn_addr2insn((void *) insn);
#endif
    if (tcp->safe_p || insn_mutation_num != 0)
	insn = get_safe_insn(insn);
    len = insn_len(insn);
    if (pos + len >= size)
	next_insn = BIN(nop);
    else {
	next_insn = code[pos + len];
#if OPT_DIRECT_THREADED_CODE || OPT_CALL_THREADED_CODE
	next_insn = rb_vm_insn_addr2insn((void *) next_insn);
#endif
    }
    types = insn_op_types(insn);
    iname = insn_name(insn);
    fprintf(f, "l%ld:\n", pos);
    if (mjit_opts.debug)
	fprintf(f, "  /* %s:%u */\n", RSTRING_PTR(body->location.label), rb_iseq_line_no(iseq, pos, TRUE));
#if MJIT_INSN_STATISTICS
    fprintf(f, "  jit_insns_num++;\n");
#endif
    get_insn_fun_features(insn, &features);
    ivar_p = const_p = FALSE;
    if (! features.speculative_p)
	fprintf(f, "%s", generate_set_pc(buf, &code[pos] + (features.frame_p ? len : 0)));
    if (features.call_p && ! features.special_p) {
	/* CD is always the 1st operand.  */
	cc = &((CALL_DATA) code[pos + 1])->call_cache;
	ci = &((CALL_DATA) code[pos + 1])->call_info;
	/* Remember cc can change in the interpreter thread in
	   parallel.  TODO: Make the following atomic.  */
	local_cc = *cc;
    } else if (insn == BIN(ivar2var) || insn == BIN(temp2ivar)
	       || insn == BIN(loc2ivar) || insn == BIN(val2ivar)) {
	ivar_p = TRUE;
	local_ic = *(IC) (insn == BIN(ivar2var) ? code[pos + 3] : code[pos + 2]);
    } else if (insn == BIN(const_cached_val_ld) || insn == BIN(get_inline_cache)) {
	const_p = TRUE;
	local_ic = *(IC) (insn == BIN(get_inline_cache) ? code[pos + 3] : code[pos + 4]);
    }
    if (features.special_p) {
	switch (insn) {
	case BIN(nop):
	    break;
	case BIN(goto):
	    fprintf(f, "  rb_vm_check_ints(ec);\n");
	    fprintf(f, "  goto l%ld;\n", pos + len + code[pos + 1]);
	    break;
	case BIN(get_inline_cache): {
	    unsigned long dest = pos + len + code[pos + 1];

	    assert(const_p);
	    if (tcp->safe_p || insn_mutation_num != 0
		|| local_ic.ic_serial != ruby_vm_global_constant_state) {
		fprintf(f, "  if (%s_f(cfp, %s, (void *) 0x%"PRIxVALUE "))\n  ",
			iname, get_op_str(buf, code[pos + 2], tcp), code[pos + 3]);
	    } else {
		fprintf(f, "  if (mjit_get_inline_cache(cfp, %llu, %llu, 0x%"PRIxVALUE ", %s",
			(unsigned long long) local_ic.ic_serial,
			(unsigned long long) local_ic.ic_cref, local_ic.ic_value.value,
			get_op_str(buf, code[pos + 2], tcp));
		fprintf(f, ")) {\n  %s", generate_set_pc(buf, &code[pos]));
		fprintf(f, "    %sgoto stop_spec;\n  }\n", set_failed_insn_str(buf, pos));
	    }
	    fprintf(f, "  goto l%ld;\n", dest);
	    break;
	}
	case BIN(case_dispatch): {
	    CDHASH hash = code[pos + 2];
	    struct case_arg arg;
	    
	    fprintf(f, "  switch (case_dispatch_f(cfp, %s, 0x%"PRIxVALUE ", %ld)) {\n",
		    get_op_str(buf, code[pos + 1], tcp), (VALUE) hash, code[pos + 3]);
	    fprintf(f, "    case 0: break;\n");
	    arg.last_dst = 0;
	    arg.offset = pos + len;
	    arg.f = f;
	    st_foreach(RHASH_TBL_RAW(hash), print_case, (st_data_t) &arg);  
	    fprintf(f, "  }\n");
	    break;
	}
	case BIN(temp_ret):
	    fprintf(f, "  %s_f(ec, cfp, %s, &val);\n  return val;\n",
		    iname, get_op_str(buf, code[pos + 1], tcp));
	    break;
	case BIN(loc_ret):
	    fprintf(f, "  %s_f(ec, cfp, %s, &val);\n  return val;\n",
		    iname, get_op_str(buf, code[pos + 1], tcp));
	    break;
	case BIN(val_ret):
	    fprintf(f, "  %s_f(ec, cfp, %"PRIuVALUE ", &val);\n  return val;\n",
		    iname, code[pos + 1]);
	    break;
	case BIN(raise_except):
	    fprintf(f, "  val = %s_f(ec, cfp, %s, %"PRIuVALUE ");\n",
		    iname, get_op_str(buf, code[pos + 1], tcp), code[pos + 2]);
	    fprintf(f, "  ec->errinfo = val; rb_ec_tag_jump(ec, ec->tag->state);\n");
	    break;
	case BIN(raise_except_val):
	    fprintf(f, "  val = %s_f(ec, cfp, 0x%"PRIxVALUE ", %"PRIuVALUE ");\n",
		    iname, code[pos + 1], code[pos + 2]);
	    fprintf(f, "  ec->errinfo = val; rb_ec_tag_jump(ec, ec->tag->state);\n");
	    break;
	case BIN(call_block):
	    /* Generate copying temps to the stack.  */
	    if (tcp->use_temp_vars_p)
		generate_param_setup(f, code, pos, TRUE);
	    fprintf(f, "  val = %s_f(ec, cfp, (void *) 0x%"PRIxVALUE ", (sindex_t) %"PRIdVALUE ");\n",
	            iname, code[pos + 1], code[pos + 2]);
	    fprintf(f, "  if (mjit_call_block_end(ec, cfp, val, %s)) {\n",
	            get_op_str(buf, code[pos + 2], tcp));
	    fprintf(f, "    %sgoto stop_spec;\n  }\n", set_failed_insn_str(buf, pos));
	    break;
	default:
	    break;
	}
    } else if (!tcp->safe_p && features.call_p && insn_mutation_num == 0
	       && ruby_vm_global_method_state == local_cc.method_state
	       && (local_cc.call == (vm_call_handler) vm_call_cfunc
		   || vm_call_iseq_setup_normal_p(local_cc.call))) {
	int simple_p = (insn == BIN(simple_call)
			|| insn == BIN(simple_call_self) || insn == BIN(simple_call_recv));
	int self_p = insn == BIN(call_self) || insn == BIN(simple_call_self);
	ptrdiff_t call_start = code[pos + 2];
	ptrdiff_t recv_op = (insn == BIN(call_recv)
			     ? (ptrdiff_t) code[pos + 4] : insn == BIN(simple_call_recv)
			     ? (ptrdiff_t) code[pos + 3] : call_start);
	VALUE block_iseq = simple_p ? 0 : code[pos + 3];
	const char *rec;
	char buf2[150];
	
	if (tcp->use_temp_vars_p)
	    /* Generate copying temps to the stack.  */
	    generate_param_setup(f, code, pos, features.recv_p);
	if (insn != BIN(call_super_val))
	    rec = (self_p ? "&cfp->self" : get_op_str(buf2, recv_op, tcp));
	else {
	    sprintf (buf2, "0x%"PRIxVALUE, code[pos + 4]);
	    rec = buf2;
	}
	fprintf(f, "  if (mjit_check_cc_attr_p(*%s, %llu, %llu)) {\n",
		rec, (unsigned long long) local_cc.method_state,
		(unsigned long long) local_cc.class_serial);
	fprintf(f, "  %s", generate_set_pc(buf, &code[pos]));
	fprintf(f, "    %sgoto stop_spec;\n  }\n", set_failed_insn_str(buf, pos));
	if (local_cc.call == (vm_call_handler) vm_call_cfunc) {
	    fprintf(f, "  if (mjit_call_cfunc(ec, cfp, %llu, (void *) 0x%"PRIxVALUE
		    ", %u, %d, 0x%x, (void *) 0x%"PRIxVALUE
		    ", %ld, (void *) 0x%"PRIxVALUE ", *%s, %d, %d",
		    (unsigned long long) ci->mid, (VALUE) local_cc.me,
		    iseq->body->temp_vars_num, ci->orig_argc, ci->flag, (VALUE) &((struct rb_call_data_with_kwarg *)ci)->kw_arg,
		    call_start, block_iseq, rec, !features.recv_p, simple_p);
	} else {
	    const rb_iseq_t *callee_iseq = rb_iseq_check(local_cc.me->def->body.iseq.iseqptr);
	    struct rb_iseq_constant_body *callee_body = callee_iseq->body;

	    fprintf(f, "  if (mjit_iseq_call(ec, cfp, (void *) 0x%"PRIxVALUE ", (void *) 0x%"PRIxVALUE
		    ", (void *) 0x%"PRIxVALUE ", %d, (void *) 0x%"PRIxVALUE
		    ", %d, %d, %d, %d, %u, %d, 0x%x, %ld, (void *) 0x%"PRIxVALUE
		    ", *%s, %d, %d",
		    (VALUE) local_cc.me, (VALUE) callee_iseq,
		    (VALUE) callee_body, callee_body->catch_except_p, (VALUE) callee_body->rtl_encoded,
		    callee_body->type, callee_body->param.size, callee_body->local_table_size,
		    iseq->body->temp_vars_num, callee_body->stack_max,
		    ci->orig_argc, ci->flag, call_start, block_iseq,
		    rec, !features.recv_p, simple_p);
	}
	fprintf(f, ", %s)) {\n", get_op_str(buf, call_start, tcp));
	fprintf(f, "    %sgoto stop_spec;\n  }\n", set_failed_insn_str(buf, pos));
    } else if (!tcp->safe_p && features.call_p && insn_mutation_num == 0
	       && ruby_vm_global_method_state == local_cc.method_state
	       && (local_cc.call == (vm_call_handler) vm_call_ivar
		   || local_cc.call == (vm_call_handler) vm_call_attrset)
	       && local_cc.aux.index > 0) {
	ptrdiff_t call_start = code[pos + 2];
	long call_ivar_obj_op = features.recv_p ? code[pos + 2] : code[pos + 3];
	const char *rec;
	
	assert(insn == BIN(simple_call_recv) || insn == BIN(simple_call)
	       || insn == BIN(simple_call_self) || insn == BIN(call_super) || insn == BIN(call_super_val));
	if (insn != BIN(call_super_val))
	    rec = (insn == BIN(simple_call_self)
		   ? "&cfp->self" : get_op_str(buf, call_ivar_obj_op, tcp));
	else {
	    sprintf (buf, "0x%"PRIxVALUE, code[pos + 4]);
	    rec = buf;
	}
	fprintf(f, "  if (mjit_check_cc_attr_p(*%s, %llu, %llu) || ",
		rec, (unsigned long long) local_cc.method_state,
		(unsigned long long) local_cc.class_serial);
	if (local_cc.call == (vm_call_handler) vm_call_ivar) {
	    fprintf(f, "mjit_call_ivar(*%s, %u, ", rec, (unsigned) local_cc.aux.index);
	    fprintf(f, "%s)) {\n", get_op_str(buf, call_start, tcp));
	} else {
	    fprintf(f, "mjit_call_setivar(*%s, %u, ", rec, (unsigned) local_cc.aux.index);
	    fprintf(f, "*%s)) {\n", get_op_str(buf, call_start - 1, tcp));
	}
	fprintf(f, "  %s", generate_set_pc(buf, &code[pos]));
	fprintf(f, "    %sgoto stop_spec;\n  }\n", set_failed_insn_str(buf, pos));
    } else if (!tcp->safe_p && ivar_p && insn_mutation_num == 0) {
	assert(insn == BIN(ivar2var) || insn == BIN(temp2ivar) || insn == BIN(loc2ivar) || insn == BIN(val2ivar));
	if (compile_info->ivar_spec != 0) {
	    if (insn == BIN(ivar2var))
		fprintf(f, "  mjit_ivar2var_no_check(cfp, self, %d, %llu, %s);\n",
			compile_info->ivar_spec != (size_t) -1, (unsigned long long) local_ic.ic_value.index,
			get_op_str(buf, code[pos + 1], tcp));
	    else {
		fprintf(f, "  mjit_%s_no_check(cfp, self, %d, %llu, ",
			iname, compile_info->ivar_spec != (size_t) -1,
			(unsigned long long) local_ic.ic_value.index);
		if (insn == BIN(temp2ivar) || insn == BIN(loc2ivar))
		    fprintf (f, "%s", get_op_str(buf, code[pos + 3], tcp));
		else
		    fprintf (f, "0x%"PRIxVALUE, code[pos + 3]);
		fprintf (f, ");\n");
	    }
	} else {
	    if (insn == BIN(ivar2var))
		fprintf(f, "  if (mjit_ivar2var(cfp, self, %d, %llu, %llu, %s",
			iseq->body->in_type_object_p, (unsigned long long) local_ic.ic_serial,
			(unsigned long long) local_ic.ic_value.index,
			get_op_str(buf, code[pos + 1], tcp));
	    else {
		fprintf(f, "  if (mjit_%s(cfp, self, %d, %llu, %llu, ",
			iname, iseq->body->in_type_object_p, (unsigned long long) local_ic.ic_serial,
			(unsigned long long) local_ic.ic_value.index);
		if (insn == BIN(temp2ivar) || insn == BIN(loc2ivar))
		    fprintf (f, "%s", get_op_str(buf, code[pos + 3], tcp));
		else
		    fprintf (f, "0x%"PRIxVALUE, code[pos + 3]);
	    }
	    fprintf(f, ")) {\n  %s", generate_set_pc(buf, &code[pos]));
	    fprintf(f, "    %sgoto stop_spec;\n  }\n", set_failed_insn_str(buf, pos));
	}
    } else if (!tcp->safe_p && const_p && insn_mutation_num == 0 && insn == BIN(const_cached_val_ld)
	       && local_ic.ic_serial == ruby_vm_global_constant_state) {
	assert(insn == BIN(const_cached_val_ld));
	fprintf(f, "  if (mjit_const_cached_val_ld(cfp, %llu, %llu, 0x%"PRIxVALUE ", %s",
		(unsigned long long) local_ic.ic_serial,
		(unsigned long long) local_ic.ic_cref, local_ic.ic_value.value,
		get_op_str(buf, code[pos + 1], tcp));
	fprintf(f, ")) {\n  %s", generate_set_pc(buf, &code[pos]));
	fprintf(f, "    %sgoto stop_spec;\n  }\n", set_failed_insn_str(buf, pos));
    } else {
	if (tcp->use_temp_vars_p && features.call_p)
	    /* Generate copying temps to the stack.  */
	    generate_param_setup(f, code, pos, features.recv_p);
	if (features.jmp_p && (features.bcmp_p || features.speculative_p))
	    fprintf(f, "  flag = ");
	else
	    fprintf(f, (features.jmp_p || features.op_end_p
			|| features.speculative_p
			? "  if (" : "  "));
	fprintf(f, "%s_f(", iname);
	if (features.ec_p)
	    fprintf(f, "ec, ");
	fprintf(f, "cfp");
	if (features.call_p)
	    fprintf(f, ", &calling");
	for (i = (features.jmp_p ? 2 : 1) + (features.skip_first_p ? 1 : 0); i < len; i++) {
	    op = code[pos + i];
	    if (types[i - 1] != TS_VARIABLE && types[i - 1] != TS_INSN
		&& (! features.speculative_p || insn == BIN(spec_not) || types[i - 1] != TS_CALLDATA))
		fprintf(f, ", ");
	    switch (types[i - 1]) {
	    case TS_OFFSET:		/* LONG */
		fprintf(f, "%"PRIdVALUE, (VALUE)(pos + len + op));
		break;
	    case TS_NUM:		/* ULONG */
	    case TS_ID:
		fprintf(f, "%"PRIuVALUE, op);
		break;
	    case TS_LINDEX:
	    case TS_VINDEX:
	    case TS_TINDEX:
		fprintf(f, "%s", get_op_str(buf, op, tcp));
		if (features.simple_p) {
		    fprintf(f, ", %s", get_op_str(buf, op, tcp));
		    fprintf(f, ", %s", get_op_str(buf, (ptrdiff_t) op - 1, tcp));
		}
		break;
	    case TS_RINDEX:
		fprintf(f, "%s, (lindex_t) %"PRIdVALUE,
			get_op_str(buf, op, tcp), op);
		break;
	    case TS_SINDEX:
		fprintf(f, "(lindex_t) %"PRIdVALUE, op);
		break;
	    case TS_IC:
	    case TS_ISE:
		fprintf(f, "(void *) 0x%"PRIxVALUE, op);
		break;
	    case TS_CALLDATA:
		if (features.speculative_p && insn != BIN(spec_not))
		    break;
	    case TS_CDHASH:
	    case TS_VALUE:
	    case TS_ISEQ:
	    case TS_CALLINFO:
	    case TS_CALLCACHE:
	    case TS_GENTRY:
		fprintf(f, "(void *) 0x%"PRIxVALUE, op);
		break;
	    case TS_VARIABLE:
		break;
	    case TS_INSN:
		/* An insn operand should be never processed.  */
		break;
	    default:
		fprintf(stderr, "Unknown %d operand %c of %s\n", i, types[i - 1], iname);
		break;
	    }
	}
	assert(! (features.double_p || features.cmp_double_p) || features.speculative_p);
	if (features.op_end_p)
	    fprintf(f, ")) {\n    %sgoto stop_spec;\n  }\n", set_failed_insn_str(buf, pos));
	else if (features.speculative_p) {
	    if (features.jmp_p) {
		fprintf(f, ", &val, &new_insn");
		if (features.double_p) {
		    assert(features.bcmp_p && ! features.simple_p);
		    to = get_op_double_num(cfg, body->local_table_size, code, pos, len,
					   code[pos + features.result + 1],
					   FALSE, next_insn);
		    if (to < 0)
			fprintf(f, ", 0");
		    else {
			assert(compile_info->doubles_p == NULL || compile_info->doubles_p[to]);
			fprintf(f, ", &d%ld", to);
		    }
		    if (! features.imm_double_p) {
			to = get_op_double_num(cfg, body->local_table_size, code, pos, len,
					       code[pos + features.result + 2],
					       FALSE, next_insn);
			if (to < 0)
			    fprintf(f, ", 0");
			else {
			    assert(compile_info->doubles_p == NULL || compile_info->doubles_p[to]);
			    fprintf(f, ", &d%ld", to);
			}
		    }
		}
		fprintf(f, ");\n  if (val == RUBY_Qundef) {\n");
	    } else {
		fprintf(f, ", &new_insn");
		if (features.double_p || features.cmp_double_p) {
		    if (features.double_p) {
			to = get_op_double_num(cfg, body->local_table_size, code, pos, len,
					       code[pos + features.result], TRUE, next_insn);
			if (to < 0)
			    fprintf(f, ", 0");
			else {
			    assert(compile_info->doubles_p == NULL || compile_info->doubles_p[to]);
			    fprintf(f, ", &d%ld", to);
			}
		    }
		    to = get_op_double_num(cfg, body->local_table_size, code, pos, len,
					   features.simple_p
					   ? code[pos + features.result]
					   : code[pos + features.result + 1],
					   FALSE, next_insn);
		    if (to < 0)
			fprintf(f, ", 0");
		    else {
			assert(compile_info->doubles_p == NULL || compile_info->doubles_p[to]);
			fprintf(f, ", &d%ld", to);
		    }
		    if (! features.imm_double_p) {
			to = get_op_double_num(cfg, body->local_table_size, code, pos, len,
					       features.simple_p
					       ? (VALUE) ((long) code[pos + features.result] - 1)
					       : code[pos + features.result + 2],
					       FALSE, next_insn);
			if (to < 0)
			    fprintf(f, ", 0");
			else {
			    assert(compile_info->doubles_p == NULL || compile_info->doubles_p[to]);
			    fprintf(f, ", &d%ld", to);
			}
		    }
		}
		fprintf(f, ")) {\n");
	    }
	    fprintf(f, "  %s", generate_set_pc(buf, &code[pos]));
	    fprintf(f, "    vm_change_insn(cfp->iseq, (void *) 0x%"PRIxVALUE ", new_insn);\n",
		    (VALUE) &code[pos]);
	    fprintf(f, "    %sgoto stop_spec;\n  }\n", set_failed_insn_str(buf, pos));
	    if (features.jmp_p) {
		unsigned long dest = pos + len + code[pos + 2];
		fprintf(f, "  if (flag) goto l%ld;\n", dest);
	    }
	} else if (! features.jmp_p) {
	    fprintf(f, ");\n");
	    if (insn == BIN(run_once)) {
	      fprintf(f, "  if (! mjit_ep_neq_bp_p && cfp->bp != cfp->ep) {\n");
	      fprintf(f, "    mjit_ep_eq_bp_fail(cfp->iseq); return RUBY_Qundef;\n");
	      fprintf(f, "  }\n");
	    }
	} else if (! features.bcmp_p) {
	    fprintf(f, "))\n    goto l%ld;\n", pos + len + code[pos + 1]);
	} else {
            unsigned long dest = pos + len + code[pos + 2];
	    
	    fprintf(f, ", &val);\n  if (val == RUBY_Qundef) {\n");
	    fprintf(f, "    if (flag)%s", generate_set_pc(buf, &code[dest]));
	    fprintf(f, "    %sgoto stop_spec;\n  }\n", set_failed_insn_str(buf, pos));
	    fprintf(f, "  if (flag) goto l%lu;\n", dest);
	}
	if (features.call_p) {
	    ptrdiff_t call_start = code[pos + 2];
	    
	    if (!tcp->safe_p && ruby_vm_global_method_state == local_cc.method_state
		&& vm_call_iseq_setup_normal_p(local_cc.call) && insn_mutation_num == 0) {
		const rb_iseq_t *callee_iseq = rb_iseq_check(local_cc.me->def->body.iseq.iseqptr);

		fprintf(f, "  if (mjit_check_cc_attr_p(calling.recv, %llu, %llu))",
			(long long unsigned) local_cc.method_state,
			(long long unsigned) local_cc.class_serial);
		fprintf(f, " {\n  %s", generate_set_pc(buf, &code[pos]));
		fprintf(f, "    %sgoto stop_spec;\n  }\n", set_failed_insn_str(buf, pos));
		fprintf(f, "  if (mjit_call_iseq_normal(ec, cfp, &calling, (void *) 0x%"PRIxVALUE ", %d, %d, %s)) {\n",
			code[pos + 1], callee_iseq->body->param.size, callee_iseq->body->local_table_size,
			get_op_str(buf, call_start, tcp));
		fprintf(f, "    %sgoto stop_spec;\n  }\n", set_failed_insn_str(buf, pos));
	    } else {
		fprintf(f, "  if (mjit_call_method(ec, cfp, &calling, (void *) 0x%"PRIxVALUE ", %s)) {\n",
			code[pos + 1], get_op_str(buf, call_start, tcp));
		fprintf(f, "    goto cancel;\n  }\n");
	    }
        }
    }
    *ignore_next_p = update_bb_kill_gen_by_insn(f, code, body->local_table_size, cfg,
						insn, pos, len, &features, ignore_result_p, next_insn);
    return len;
}

/* Update initial values of ISEQ COMPILE_INFO ivar_spec, ivar_serial, and
   use_temp_vars_p.  */
static void
update_compile_info_from_insns(const rb_iseq_t *iseq, struct rb_mjit_compile_info *compile_info) {
    struct rb_iseq_constant_body *body = iseq->body;
    size_t pos, len, ic_disp, size = body->rtl_size;
    rb_serial_t ivar_serial;
    int ivar_spec_update_p;
    size_t ivar_access_num, index, max_ivar_spec_index;
    const VALUE *code = body->rtl_encoded;
    VALUE insn, next_insn;
    IC ic;
    long res;
    
    ivar_spec_update_p = TRUE;
    ivar_serial = 0;
    ivar_access_num = 0;
    max_ivar_spec_index = 0;
    for (pos = 0; pos < size;) {
	insn = code[pos];
#if OPT_DIRECT_THREADED_CODE || OPT_CALL_THREADED_CODE
	insn = rb_vm_insn_addr2insn((void *) insn);
#endif
	ic_disp = 2;
	len = insn_len(insn);
	if (pos + len >= size)
	    next_insn = BIN(nop);
	else {
	    next_insn = code[pos + len];
#if OPT_DIRECT_THREADED_CODE || OPT_CALL_THREADED_CODE
	    next_insn = rb_vm_insn_addr2insn((void *) next_insn);
#endif
	}
	switch (insn) {
	case BIN(var2var):
	case BIN(temp_reverse):
	case BIN(concat_strings):
	case BIN(to_regexp):
	case BIN(make_array):
	case BIN(make_hash):
	case BIN(new_array_min):
	case BIN(new_array_max):
	case BIN(spread_array):
	case BIN(define_class):
	    compile_info->use_temp_vars_p = FALSE;
	    break;
	case BIN(ivar2var):
	    ic_disp = 3;
	    /* falls through */
	case BIN(val2ivar):
	case BIN(temp2ivar):
	case BIN(loc2ivar):
	    ic = (IC) code[pos + ic_disp];
	    if (ivar_access_num == 0)
		ivar_serial = ic->ic_serial;
	    else if (ivar_serial != ic->ic_serial)
		ivar_spec_update_p = FALSE;
	    index = ic->ic_value.index;
	    if (ivar_access_num == 0 || max_ivar_spec_index < index)
		max_ivar_spec_index = index;
	    ivar_access_num++;
	    break;
	    /* We consider non-speculative insns too as they can
	       become speculative before actual translation.  TODO:
	       copy iseq to prevent its change and remove
	       non-speculative insns here.  */
	case BIN(plusf):
	case BIN(minusf):
	case BIN(multf):
	case BIN(divf):
	case BIN(modf):
	case BIN(plus):
	case BIN(minus):
	case BIN(mult):
	case BIN(div):
	case BIN(mod):
	case BIN(splus):
	case BIN(sminus):
	case BIN(smult):
	case BIN(sdiv):
	case BIN(smod):
	case BIN(fplusf):
	case BIN(fminusf):
	case BIN(fmultf):
	case BIN(fdivf):
	case BIN(fmodf):
	case BIN(fplus):
	case BIN(fminus):
	case BIN(fmult):
	case BIN(fdiv):
	case BIN(fmod):
	case BIN(sfplus):
	case BIN(sfminus):
	case BIN(sfmult):
	case BIN(sfdiv):
	case BIN(sfmod): {
	    res = get_var_double_num((long) code[pos + 2], body->local_table_size);
	    assert(res >= 0);
	    if (pos + len >= size || next_insn != BIN(temp2loc)
		|| code[pos + len + 2] != code[pos + 2]) {
		compile_info->doubles_p[res] = TRUE;
	    } else {
		/* There is temp2loc insn moving result of speculative
		   floating point insn.  TODO: check that they are in
		   the same BB and remove setting double for res.  */
		compile_info->doubles_p[res] = TRUE;
		res = get_var_double_num((long) code[pos + len + 1], body->local_table_size);
		assert(res >= 0);
		compile_info->doubles_p[res] = TRUE;
	    }
	    break;
	}
	case BIN(val2loc):
	case BIN(val2temp):
	    if (FLONUM_P(code[pos + 2])) {
		res = get_var_double_num((long) code[pos + 1], body->local_table_size);
		assert(res >= 0);
		compile_info->doubles_p[res] = TRUE;
	    }
	    break;
	}
	pos += len;
    }
    if (ivar_spec_update_p && ivar_access_num > 2 && body->in_type_object_p) {
	/* We have enough ivar accesses to make whole function
	   speculation about them.  */
	compile_info->ivar_spec = (max_ivar_spec_index >= ROBJECT_EMBED_LEN_MAX
				   ? max_ivar_spec_index : (size_t) -1);
	compile_info->ivar_serial = ivar_serial;
    }
    if (body->catch_except_p)
	compile_info->use_temp_vars_p = FALSE;
}

static void
initiate_compile_info(const rb_iseq_t *iseq, struct rb_mjit_compile_info *compile_info) {
    int i;
    
    compile_info->init_p = TRUE;
    compile_info->ep_neq_bp_p = FALSE;
    compile_info->ivar_spec = 0;
    compile_info->ivar_serial = 0;
    compile_info->use_temp_vars_p = TRUE;
    compile_info->used_code_p = FALSE;
    compile_info->jit_mutations_num = 0;
    memset(compile_info->doubles_p, 0, sizeof (char) * (iseq->body->temp_vars_num + iseq->body->local_table_size));
    for (i = 0; i <= mjit_opts.max_mutations; i++)
	compile_info->mutation_insns[i].insn = BIN(nop);
    update_compile_info_from_insns(iseq, compile_info);
}

/* Compile RTL ISeq to C code in F using compile info COMPILE_INFO and FUNCNAME.
   It returns 1 if it succeeds to compile. */
int
mjit_rtl_compile(FILE *f, const rb_iseq_t *iseq,
		 struct rb_mjit_compile_info *compile_info, const char *funcname)
{
    int ep_neq_bp_p, ignore_result_p;
    struct cfg *cfg;
    struct rb_iseq_constant_body *body = iseq->body;
    size_t i, size = body->rtl_size;
    struct translation_control tc;
    struct bb *curr_bb;

    if (! compile_info->init_p)
	initiate_compile_info(iseq, compile_info);
    if (!mjit_opts.debug) {
        fprintf(f, "#undef OPT_CHECKED_RUN\n");
        fprintf(f, "#define OPT_CHECKED_RUN 0\n\n");
    }
#ifdef _WIN32
    fprintf(f, "__declspec(dllexport)\n");
#endif
#if MJIT_INSN_STATISTICS
    fprintf(f, "extern unsigned long jit_insns_num;\n");
#endif
    setup_global_spec_state(&compile_info->spec_state);
    ep_neq_bp_p = FALSE;
    if (compile_info->ep_neq_bp_p) {
	ep_neq_bp_p = TRUE;
    }
    fprintf(f, "static const char mjit_profile_p = %d;\n", mjit_opts.profile);
    fprintf(f, "static const char mjit_trace_p = %u;\n", compile_info->spec_state.trace_p);
    fprintf(f, "static const char mjit_bop_redefined_p = %u;\n", compile_info->spec_state.bop_redefined_p);
    fprintf(f, "static const char mjit_ep_neq_bp_p = %d;\n", ep_neq_bp_p);
    fprintf(f, "VALUE\n%s(rb_execution_context_t *ec, rb_control_frame_t *cfp)\n{\n", funcname);
    tc.safe_p = compile_info->jit_mutations_num >= mjit_opts.max_mutations;
    tc.use_temp_vars_p = compile_info->use_temp_vars_p;
    /* If the current iseq contains a block, we should not use C vars
       for local Ruby vars because a binding can be created in a block
       and used inside for access to a variable of the current iseq.
       The current frame local vars will be saved as bp and ep
       equality is changed into their inequality after the binding
       call.  */
    tc.use_local_vars_p = ! body->parent_iseq_p && ! ep_neq_bp_p && tc.use_temp_vars_p;
    fprintf(f, "  struct rb_calling_info calling;\n  VALUE val; int flag;\n");
    fprintf(f, "  enum ruby_vminsn_type new_insn;\n");
    fprintf(f, "  static const char mutation_num = %d;\n", compile_info->jit_mutations_num);
    fprintf(f, "  size_t failed_insn_pc;\n");
    fprintf(f, "  VALUE self = cfp->self;\n");
    if (tc.use_local_vars_p)
	for (i = 0; i < body->local_table_size; i++)
	    fprintf(f, "  VALUE v%lu;\n", i);
    if (tc.use_temp_vars_p)
	for (i = 0; i <= body->temp_vars_num; i++)
	    fprintf(f, "  VALUE t%lu;\n", i);
    cfg = NULL;
    if (0 && ! tc.safe_p && tc.use_local_vars_p && compile_info->doubles_p != NULL)
	for (i = 0; i < body->local_table_size + body->temp_vars_num; i++)
	    if (compile_info->doubles_p[i]) {
		if (cfg == NULL && (cfg = build_cfg(iseq)) == NULL)
		    break;
		fprintf(f, "  double d%lu;\n", i);
	    }
    if (! ep_neq_bp_p) {
	fprintf(f, "  if (cfp->bp != cfp->ep) {\n");
	fprintf(f, "    mjit_ep_eq_bp_fail(cfp->iseq); return RUBY_Qundef;\n  }\n");
    }
    if (compile_info->ivar_spec != 0) {
	fprintf(f, "  if (mjit_check_self_p(self, %llu, %lu)) {\n",
		(unsigned long long) compile_info->ivar_serial, compile_info->ivar_spec);
	fprintf(f, "    mjit_ivar_spec_fail(cfp->iseq); return RUBY_Qundef;\n  }\n");
    }
    fprintf(f, "  set_default_sp_0(cfp, cfp->bp, %u);\n",
	    body->temp_vars_num);
    if (tc.use_local_vars_p) {
	for (i = 0; i < body->local_table_size; i++)
	    fprintf(f, "  v%ld = *get_loc_addr(cfp, %ld);\n", i, i + VM_ENV_DATA_SIZE);
    }
    if (body->param.flags.has_opt) {
	int n;
	
	fprintf(f, "  switch (cfp->pc - cfp->iseq->body->rtl_encoded) {\n");
	for (n = 1; n <= body->param.opt_num; n++) {
	    fprintf(f, "  case %d: goto l%d;\n", (int) body->param.opt_table[n],
		    (int) body->param.opt_table[n]);
	}
	fprintf(f, "  }\n");
    }
    if (cfg != NULL) {
	build_bb_sets(iseq, cfg);
#if CFG_DEBUG
	print_cfg(iseq, cfg);
#endif
    }
    ignore_result_p = FALSE;
    curr_bb = NULL;
    for (i = 0; i < size;) {
	if (cfg != NULL && curr_bb != cfg->pos2bb[i]) { /* BB start */
	    curr_bb = cfg->pos2bb[i];
	    memcpy(curr_bb->gen, curr_bb->in, bit2char(body->temp_vars_num + body->local_table_size));
	}
	i += translate_iseq_insn(f, i, iseq, compile_info, cfg, &ignore_result_p, &tc);
    }
    if (cfg != NULL)
	free_cfg(cfg);
    fprintf(f, "stop_spec:\n");
    fprintf(f, "  mjit_store_failed_spec_insn(cfp->iseq, failed_insn_pc, mutation_num);\n");
    fprintf(f, "  mjit_change_iseq(cfp->iseq, 1);\n");
    fprintf(f, "cancel:\n");
    if (tc.use_local_vars_p) {
	for (i = 0; i < body->local_table_size; i++)
	    fprintf(f, "  *get_loc_addr(cfp, %ld) = v%ld;\n", i + VM_ENV_DATA_SIZE, (long) i);
    }
    if (tc.use_temp_vars_p) {
	for (i = 0; i <= body->temp_vars_num; i++)
	    fprintf(f, "  *get_temp_addr_safe(cfp, %ld) = t%ld;\n", -1 - (long) i, (long) i);
    }
    fprintf(f, "  return RUBY_Qundef;\n} /* end of %s */\n", funcname);
    return ferror(f) == 0;
}

struct rb_mjit_compile_info *
mjit_create_rtl_compile_info(const rb_iseq_t *iseq) {
    struct rb_mjit_compile_info *compile_info;

    compile_info = ruby_xcalloc(sizeof(struct rb_mjit_compile_info)
				+ sizeof (char) * (iseq->body->temp_vars_num + iseq->body->local_table_size), 1);
    if (compile_info != NULL) {
	compile_info->mutation_insns = ruby_xmalloc(sizeof (struct mjit_mutation_insns) * (mjit_opts.max_mutations + 1));
	if (compile_info->mutation_insns == NULL) {
	    xfree(compile_info);
	    compile_info = NULL;
	}
    }
    return compile_info;
}

void
mjit_free_rtl_compile_info(struct rb_mjit_compile_info *compile_info) {
    xfree(compile_info);
}

/* Called when an ISEQ insn with a relative PC caused MUTATION_NUM-th
   mutation.  We just collect this info in mutation_insns array. */
MJIT_FUNC_EXPORTED void
mjit_store_failed_spec_insn(rb_iseq_t *iseq, size_t pc, int mutation_num)  {
    struct rb_mjit_compile_info *compile_info = mjit_iseq_compile_info(iseq);
    VALUE insn;

    insn = iseq->body->rtl_encoded[pc];
#if OPT_DIRECT_THREADED_CODE || OPT_CALL_THREADED_CODE
    insn = rb_vm_insn_addr2insn((void *) insn);
#endif
    compile_info->mutation_insns[mutation_num].pc = pc;
    compile_info->mutation_insns[mutation_num].insn = (enum ruby_vminsn_type)insn;
}

static int
spec_recompile_guard(struct rb_mjit_compile_info *compile_info) {
    if (compile_info->jit_mutations_num >= mjit_opts.max_mutations)
	return FALSE;
    compile_info->jit_mutations_num++;
    return TRUE;
}

MJIT_FUNC_EXPORTED void
mjit_redo_iseq(rb_iseq_t *iseq, int spec_fail_p) {
    mjit_recompile_iseq(iseq, spec_recompile_guard);
}

static int
recompile_ep_eq_bp_guard(struct rb_mjit_compile_info *compile_info) {
    compile_info->ep_neq_bp_p = TRUE;
    return TRUE;
}

MJIT_FUNC_EXPORTED void
mjit_ep_eq_bp_fail(rb_iseq_t *iseq) {
    mjit_recompile_iseq(iseq, recompile_ep_eq_bp_guard);
}

static int
recompile_ivar_spec_guard(struct rb_mjit_compile_info *compile_info) {
    compile_info->ivar_spec = 0;
    return spec_recompile_guard(compile_info);
}

MJIT_FUNC_EXPORTED void
mjit_ivar_spec_fail(rb_iseq_t *iseq) {
    mjit_recompile_iseq(iseq, recompile_ivar_spec_guard);
}
