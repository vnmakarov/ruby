#include "sir.h"
#include "internal/compile.h"
#include "internal/class.h"
#include "internal/numeric.h"
#include "vm_callinfo.h"
#define YJIT_STATS 0 /* used in vm_insnhelper.h */
#include "vm_insnhelper.h"

#include "builtin.h"
#include "insns.inc"
#include "insns_info.inc"

#include "mirjit.h"

#include "mir.h"
#include "mir-gen.h"

#ifndef HAVE_LIBMIR

int
rb_mirjit_enabled_p(void) { return FALSE; }

#else

#include "c2mir.h"

// Return time in milliseconds as a double.
#ifndef _WIN32
#include <sys/time.h>
static double real_usec_time(void) {
  struct timeval tv;

  gettimeofday(&tv, NULL);
  return tv.tv_usec + tv.tv_sec * 1000000.0;
}
#else
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
static double real_usec_time(void) {
  LARGE_INTEGER freq, count;

  if (QueryPerformanceFrequency(&freq) && QueryPerformanceCounter (&count))
    return (double)count.QuadPart / ((double)freq.QuadPart * 1.0E-6);
  return 0.0;
}
#endif

static const char *header = "typedef void *CALL_DATA;\n"
    "typedef void *IC;\n"
    "typedef void *IVC;\n"
    "typedef void *ISEQ;\n"
    "typedef void *rb_insn_func_t;\n"
    "typedef void *RB_BUILTIN;\n"
    "typedef unsigned uint32_t;\n"
#if SIZEOF_LONG == SIZEOF_VOIDP
    "typedef unsigned long VALUE;\n"
    "typedef long SIGNED_VALUE;\n"
    "typedef unsigned long ID;\n"
#elif SIZEOF_LONG_LONG == SIZEOF_VOIDP
    "typedef unsigned LONG_LONG VALUE;\n"
    "typedef LONG_LONG SIGNED_VALUE;\n"
    "typedef unsigned LONG_LONG ID;\n"
#else
# error ---->> ruby requires sizeof(void*) == sizeof(long) or sizeof(LONG_LONG) to be compiled. <<----
#endif
    "typedef long OFFSET;\n"
    "typedef double DBL;\n"
    "typedef unsigned long lindex_t;\n"
    "typedef unsigned long rb_num_t;\n"
#if defined(_WIN32)
    "typdef long rb_atomic_t;\n"
#else
    "typedef unsigned rb_atomic_t;\n"
#endif
    "typedef struct rb_control_frame_struct {\n"
    "  VALUE *pc __mirc_attribute__ ((antialias (val)));\n"
    "  VALUE *sp __mirc_attribute__ ((antialias (val)));\n"
    "  void *iseq;\n"
    "  VALUE self;\n"
    "  VALUE *ep __mirc_attribute__ ((antialias (val)));\n"
    "  void *block_code;\n"
    "  VALUE *__bp__;\n"
#if VM_DEBUG_BP_CHECK
    "  VALUE *bp_check;\n"
#endif
    "  void *jit_return;\n"
    "} rb_control_frame_t;\n"
#if defined(__x86_64__)
    "register void *ec asm(\"r13\");\n"
    "register VALUE *pc asm(\"r14\") __mirc_attribute__ ((antialias (val)));\n"
    "register rb_control_frame_t *cfp asm(\"r15\");\n"
#endif
    "extern DBL mirjit_rfloat_value(VALUE);\n"
    "extern VALUE mirjit_dbl2num(DBL);\n"
    "extern VALUE mirjit_fplus(VALUE, VALUE);\n"
    "extern VALUE mirjit_fplusf(VALUE, DBL);\n"
    "extern VALUE mirjit_fminus(VALUE, VALUE);\n"
    "extern VALUE mirjit_fminusf(VALUE, DBL);\n"
    "extern VALUE mirjit_frminusf(DBL, VALUE);\n"
    "extern VALUE mirjit_fmult(VALUE, VALUE);\n"
    "extern VALUE mirjit_fmultf(VALUE, DBL);\n"
    "extern VALUE mirjit_fdiv(VALUE, VALUE);\n"
    "extern VALUE mirjit_fdivf(VALUE, DBL);\n"
    "extern VALUE mirjit_frdivf(DBL, VALUE);\n"
    "extern VALUE mirjit_fmod(VALUE, VALUE);\n"
    "extern VALUE mirjit_fmodf(VALUE, DBL);\n"
    "extern VALUE mirjit_frmodf(DBL, VALUE);\n"
#ifdef MIRJIT_RESTART_STAT
    "extern long mirjit_run_nums, mirjit_restart_nums;\n"
#endif
    "\n";

DEF_VARR(char);

static void
output_str(VARR(char) *buff, const char *str) {
    size_t len, i;

    if ((len = VARR_LENGTH(char, buff)) > 0 && VARR_GET(char, buff, len - 1) == '\0')
        VARR_TRUNC(char, buff, len - 1);
    for (i = 0;; i++) {
        VARR_PUSH(char, buff, str[i]);
        if (str[i] == '\0') break;
    }
}

#define TEMP_STR_SIZE 2048
static void
#ifdef __GNUC__
__attribute__ ((format (__printf__, 2, 3)))
#endif
fmt_output(VARR(char) *buff, const char *fmt, ...) {
    char temp_str[TEMP_STR_SIZE];
    va_list ap;

    va_start(ap, fmt);
    if (vsnprintf(temp_str, TEMP_STR_SIZE, fmt, ap) >= TEMP_STR_SIZE) {
        fprintf(stderr, "internal MIR JIT error\n");
        exit(1);
    }
    va_end(ap);
    output_str(buff, temp_str);
}

#define MIRJIT_CONST_PARAM_STR(type, id, init_expr, fmt) #type
#define MIRJIT_CONST_FMT(type, id, init_expr, fmt) "%" fmt
#define MIRJIT_CONST_VAL(type, id, init_expr, fmt) id

#define MIRJIT_CONST_PARAM_STR_PTR(type, id, init_expru) "void *"
#define MIRJIT_CONST_FMT_PTR(type, id, init_expr) "0x%" PRIxPTR
#define MIRJIT_CONST_VAL_PTR(type, id, init_expr) (uintptr_t)id

#define vm_method_cfunc_entry mirjit_method_cfunc_entry
#define O(...) fmt_output(buff, __VA_ARGS__)

static void gen_obj_frozen(VARR(char) *buff, const char *obj) {
    /* if (RB_OBJ_FROZEN_RAW(self)) goto restart; */
    O("  if (__builtin_expect(*((VALUE *)((char *)%s + %d)) & 0x%"PRIxVALUE ", 0)) goto restart;\n",
      obj, (int)offsetof(struct RBasic, flags), (VALUE)RUBY_FL_FREEZE);
}

static void gen_rb_ractor_main_p(VARR(char) *buff, const char *t) {
    O("((%s = *(VALUE **)((char *)ec + %d)) == 0", /* (t = ec->thread_ptr) == NULL */
      t, (int)offsetof(rb_execution_context_t, thread_ptr));
    O(" || *(VALUE **)((char *)%s + %d)", /* t->ractor */
      t, (int)offsetof(rb_thread_t, ractor));
    O(" == *(VALUE **)((char *)*(VALUE **)((char *)%s + %d)) + %d)", /* t->vm->ractor.main_ractor */
      t, (int)offsetof(rb_thread_t, vm), (int)offsetof(rb_vm_t, ractor.main_ractor));
}

static void gen_class_of(VARR(char) *buff, const char *klass, const char *recv) {
    O("    if ((%s & 0x%x) || (%s & ~(VALUE)0x%x) == 0) goto restart;\n", /* RB_SPECIAL_CONST_P */
      recv, (int)RUBY_IMMEDIATE_MASK, recv, (int)Qnil);
    O("    %s = *(VALUE *)((char *)%s + %d);\n", klass, recv, (int)offsetof(struct RBasic, klass)); /* RBASIC_CLASS(recv) */
}

static void gen_write_barrier(VARR(char) *buff, const char *a, const char *b) {
    /* if (!RB_SPECIAL_CONST_P(b)) { rb_gc_writebarrier(a,b); goto exit; } */
    O("  if (!((%s & 0x%x) || (%s & ~(VALUE)0x%x) == 0)) {\n", b, (int)RUBY_IMMEDIATE_MASK, b, (int)Qnil);
    O("    rb_gc_writebarrier(%s, %s);\n", a, b);
#if 0
    O("    goto exit;\n"); /* to keep better optimization of the fast path */
#endif
    O("  }\n");
}

static void gen_array_aref_len(VARR(char) *buff, const char *aref, const char *len, const char *ary) {
    O("  if (*((VALUE *)((char *)%s + %d)) & 0x%"PRIxVALUE ") {\n", ary,
      (int)offsetof(struct RBasic, flags), (VALUE)RARRAY_EMBED_FLAG);
    O("    %s = %d;\n", len, (int)RARRAY_EMBED_LEN_MAX);
    O("    %s = (VALUE *)((char *)%s + %d);\n", aref, ary, (int)offsetof(struct RArray, as.ary));
    O("  } else {\n");
    O("    %s = *(long *)((char *)%s + %d);\n", len, ary, (int)offsetof(struct RArray, as.heap.len));
    O("    %s = *(VALUE **)((char *)%s + %d);\n", aref, ary, (int)offsetof(struct RArray, as.heap.ptr));
    O("  }\n");
}

static void gen_check_overflow0(VARR(char) *buff, const char *cfp, const char *sp, int slots_num) {
    /* CHECK_VM_STACK_OVERFLOW0(cfp, sp, slots_num): */
    O("    if (__builtin_expect((char *)%s <= (char *)%s + %ld, 0)) goto restart;\n", cfp, sp,
      (long)(slots_num * sizeof(VALUE) + sizeof(struct rb_control_frame_struct)));
}

/* Used var names: ec, cfp, pc, argv, recv, specval */
static void gen_push_frame(VARR(char) *buff, int local_size, int stack_max, int send_insn_len,
                           VALUE cme, VALUE type, VALUE iseq, VALUE start_pc) {
    O("    void *new_sp = argv - 1;\n"); /* poping args including recv (sp before leave) */
    /* new_cfp = RUBY_VM_NEXT_CONTROL_FRAME(ec->cfp): */
    O("    void *new_cfp = (char *)cfp - %d;\n", (int)sizeof(rb_control_frame_t));
    /* CHECK_VM_STACK_OVERFLOW0(new_cfp, new_sp, local_size + stack_max): */
    gen_check_overflow0(buff, "new_cfp", "new_sp", local_size + stack_max);
    O("    pc += %d;\n", send_insn_len);
    O("    cfp->pc = pc;\n");
    O("    cfp->sp = new_sp;\n"); /* cfp->sp */
    O("    cfp = new_cfp; pc = 0x%"PRIxVALUE ";\n", start_pc);
    if (local_size <= 6) {
        for (int i = 0; i < local_size; i++)
            O("    *sp++ = 0x%x;\n", (int)Qnil);
    } else {
        O("    for (int i = 0; i < %d; i++) *sp = 0x%x;\n", local_size, (int)Qnil);
    }
    O("    *sp++= 0x%"PRIxVALUE ";\n", cme); /* *sp++ = cref_or_me */
    O("    *sp++= specval;\n"); /* *sp++ = specval */
    O("    *sp++= 0x%"PRIxVALUE ";\n", type); /* *sp++ = type */
    /* cfp->pc = pc; cfp->sp = sp; cfp->iseq = iseq; cfp->self = self: */
    O("    cfp->pc = pc;\n");
    O("    cfp->sp = sp;\n");
    O("    cfp->iseq = (void *)0x%"PRIxVALUE ";\n", iseq);
    O("    cfp->self = recv;\n");
    /* cfp->ep = sp - 1; cfp->block_code = NULL; cfp->__bp__ = sp; cfp->jit_return = NULL: */
    O("    cfp->ep = sp - 1;\n");
    O("    cfp->block_code = 0;\n");
    O("    cfp->__bp__ = sp;\n");
    O("    cfp->jit_return = 0;\n");
    O("    *(VALUE *)((char *)ec + %d) = cfp;\n", (int)offsetof(rb_execution_context_t, cfp)); /* ec->cfp = cfp */
}

/* Used vars: ec, cfp, sp, pc, val */
static void gen_pop_frame(VARR(char) *buff, int res_on_stack_p) {
    if (res_on_stack_p) O("  val = *--sp;\n");
    /* cfp = RUBY_VM_PREVIOUS_CONTROL_FRAME(cfp): */
    O("  cfp++;\n");
    O("  *(VALUE *)((char *)ec + %d) = cfp;\n", (int)offsetof(rb_execution_context_t, cfp)); /* ec->cfp = cfp */
    O("  pc = cfp->pc;\n");
    O("  sp = cfp->sp;\n");
    O("  *sp++ = val;\n");
    /* Put interrupt check here to have non-zero pc after cfunc return: */
    O("  if (*(rb_atomic_t *)((char*)ec+%d) & ~(VALUE)*(rb_atomic_t *)((char*)ec+%d)) goto restart;\n",
      (int)offsetof(rb_execution_context_t, interrupt_flag), (int)offsetof(rb_execution_context_t, interrupt_mask));
}

static int
gen_insn_code(VARR(char) *buff, const VALUE *insn_pc, enum ruby_vminsn_type insn_id) {
    const VALUE *operands = &insn_pc[1];
    const char *op;
    int rv_p, v1_p, v2_p, imm1_p, imm2_p, nil_p, start, stack_ops_num;

    switch (insn_id) {
    case BIN(pop):
        O("  *sp--;\n");
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    case BIN(dup):
        O("  val = sp[-1];\n");
        O("  *sp++ = val;\n");
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    case BIN(dupn): {
        rb_num_t n = (rb_num_t)operands[0];
        O("  pv = &sp[-%ld];\n", (long)n);
        O("  pv2 = sp; sp += %ld;\n", (long)n);
        O("  memcpy(pv2, pv, %ld);\n", (long)(sizeof (VALUE) * n));
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    }
    case BIN(swap):
        O("  val = sp[-1];\n");
        O("  sp[-1] = sp[-2];\n");
        O("  sp[-2] = val;\n");
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    case BIN(topn): {
        rb_num_t n = (rb_num_t)operands[0];
        O("  val = sp[-%ld];\n", (long)n + 1);
        O("  *sp++ = val;\n");
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    }
    case BIN(setn): {
        rb_num_t n = (rb_num_t)operands[0];
        O("  val = sp[- 1];\n");
        O("  sp[-%ld] = val;\n", (long)n + 1);
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    }
    case BIN(adjuststack): {
        rb_num_t n = (rb_num_t)operands[0];
        O("  sp -= %ld;\n", (long)n);
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    }
    case BIN(putnil):
        O("  *sp++ = (VALUE)0x%x;\n", (int)Qnil);
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    case BIN(putself):
        O("  *sp++ = cfp->self;\n");
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    case BIN(putobject):
        O("  *sp++ = 0x%"PRIxVALUE ";\n", operands[0]);
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    case BIN(putobject_INT2FIX_0_):
        O("  *sp++ = 1;\n");
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    case BIN(putobject_INT2FIX_1_):
        O("  *sp++ = 3;\n");
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    case BIN(getlocal): {
        int level = (int)(rb_num_t)operands[1];
        if (level != 0) {
            O("  {\n");
            O("    VALUE *vp = ep;");
            if (level <= 3) {
                for (int i = 0; i < level; i++)
                    O("    vp = (VALUE *)(vp[-1] & ~0x03);\n"); /* vp = GET_PREV_EP(vp); */
            } else {
                O("    for (int i = 0; i < %d; i++) vp = (VALUE *)(vp[-1] & ~0x03);\n", level);
            }
            O("    val = vp[-%d];\n", (int)(lindex_t)operands[0]);
            O("    *sp++ = val;\n");
            O("    pc += %d;\n", insn_len(insn_id));
            O("  }\n");
            break;
        }
    }
        /* fall through */
    case BIN(getlocal_WC_0):
        O("  val = ep[-%d];\n", (int)(lindex_t)operands[0]);
        O("  *sp++ = val;\n");
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    case BIN(setlocal): {
        int level = (int)(rb_num_t)operands[1];
        if (level != 0) {
            O("  {\n");
            O("    VALUE *vp = ep;");
            if (level <= 3) {
                for (int i = 0; i < level; i++)
                    O("    vp = (VALUE *)(vp[-1] & ~0x03);\n"); /* vp = GET_PREV_EP(vp); */
            } else {
                O("    for (int i = 0; i < %d; i++) vp = (VALUE *)(vp[-1] & ~0x03);\n", level);
            }
            O("    flags = *vp;\n");
            O("    if (__builtin_expect((flags & 0x%lx) != 0, 0)) goto restart;\n", (long)VM_ENV_FLAG_WB_REQUIRED);
            O("    val = *--sp;\n");
            O("    vp[-%d] = val;\n", (int)(lindex_t)operands[0]);
            O("    pc += %d;\n", insn_len(insn_id));
            O("  }\n");
            break;
        }
    }
        /* fall through */
    case BIN(setlocal_WC_0):
        O("  flags = *ep;\n");
        O("  if (__builtin_expect((flags & 0x%lx) != 0, 0)) goto restart;\n", (long)VM_ENV_FLAG_WB_REQUIRED);
        O("  val = *--sp;\n");
        O("  ep[-%d] = val;\n", (int)(lindex_t)operands[0]);
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    case BIN(sir_mov):
        O("  flags = *ep;\n");
        O("  if (__builtin_expect((flags & 0x%lx) != 0, 0)) goto restart;\n", (long)VM_ENV_FLAG_WB_REQUIRED);
        O("  val = ep[-%d];\n", (int)(lindex_t)operands[1]);
        O("  ep[-%d] = val;\n", (int)(lindex_t)operands[0]);
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    case BIN(sir_movi):
        O("  flags = *ep;\n");
        O("  if (__builtin_expect((flags & 0x%lx) != 0, 0)) goto restart;\n", (long)VM_ENV_FLAG_WB_REQUIRED);
        O("  ep[-%d] = 0x%"PRIxVALUE ";\n", (int)(lindex_t)operands[0], operands[1]);
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    case BIN(sir_setlocal): /* setlocal w/o write barrier check */
        O("  val = *--sp;\n");
        O("  ep[-%d] = val;\n", (int)(lindex_t)operands[0]);
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    case BIN(check_fixtype):
        O("  val = ep[-%d];\n", (int)(lindex_t)operands[0]);
        O("  if (!(val & 0x%x)) goto restart;\n", (int)RUBY_FIXNUM_FLAG);
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    case BIN(check_stack_fixtype):
        O("  if (!(sp[-%d] & 0x%x)) goto restart;\n", (int)(lindex_t)operands[0] + 1, (int)RUBY_FIXNUM_FLAG);
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    case BIN(sir_iminussvv):
    case BIN(sir_iplussvv):
    case BIN(sir_imultsvv):
    case BIN(sir_idivsvv):
    case BIN(sir_imodsvv):
    case BIN(sir_iorsvv):
    case BIN(sir_iandsvv):
    case BIN(sir_ieqsvv):
    case BIN(sir_ineqsvv):
    case BIN(sir_iltsvv):
    case BIN(sir_igtsvv):
    case BIN(sir_ilesvv):
    case BIN(sir_igesvv):
    case BIN(sir_iarefsvv):
        rv_p = imm1_p = imm2_p = FALSE;
        v1_p = v2_p = TRUE;
        goto op2;
    case BIN(sir_iminussvi):
    case BIN(sir_iplussvi):
    case BIN(sir_imultsvi):
    case BIN(sir_idivsvi):
    case BIN(sir_imodsvi):
    case BIN(sir_iorsvi):
    case BIN(sir_iandsvi):
    case BIN(sir_ieqsvi):
    case BIN(sir_ineqsvi):
    case BIN(sir_iltsvi):
    case BIN(sir_igtsvi):
    case BIN(sir_ilesvi):
    case BIN(sir_igesvi):
    case BIN(sir_iarefsvi):
        rv_p = v2_p = imm1_p = FALSE;
        v1_p = imm2_p = TRUE;
        goto op2;
    case BIN(sir_iminussiv):
    case BIN(sir_iplussiv):
    case BIN(sir_imultsiv):
    case BIN(sir_idivsiv):
    case BIN(sir_imodsiv):
    case BIN(sir_iorsiv):
    case BIN(sir_iandsiv):
    case BIN(sir_ieqsiv):
    case BIN(sir_ineqsiv):
    case BIN(sir_iltsiv):
    case BIN(sir_igtsiv):
    case BIN(sir_ilesiv):
    case BIN(sir_igesiv):
    case BIN(sir_iarefsiv):
        rv_p = v1_p = imm2_p = FALSE;
        v2_p = imm1_p = TRUE;
        goto op2;
    case BIN(sir_iminussvs):
    case BIN(sir_iplussvs):
    case BIN(sir_imultsvs):
    case BIN(sir_idivsvs):
    case BIN(sir_imodsvs):
    case BIN(sir_iorsvs):
    case BIN(sir_iandsvs):
    case BIN(sir_ieqsvs):
    case BIN(sir_ineqsvs):
    case BIN(sir_iltsvs):
    case BIN(sir_igtsvs):
    case BIN(sir_ilesvs):
    case BIN(sir_igesvs):
    case BIN(sir_iarefsvs):
        rv_p = v2_p = imm1_p = imm2_p = FALSE;
        v1_p = TRUE;
        goto op2;
    case BIN(sir_iminusssv):
    case BIN(sir_iplusssv):
    case BIN(sir_imultssv):
    case BIN(sir_idivssv):
    case BIN(sir_imodssv):
    case BIN(sir_iorssv):
    case BIN(sir_iandssv):
    case BIN(sir_ieqssv):
    case BIN(sir_ineqssv):
    case BIN(sir_iltssv):
    case BIN(sir_igtssv):
    case BIN(sir_ilessv):
    case BIN(sir_igessv):
    case BIN(sir_iarefssv):
        rv_p = v1_p = imm1_p = imm2_p = FALSE;
        v2_p = TRUE;
        goto op2;
    case BIN(sir_iminusssi):
    case BIN(sir_iplusssi):
    case BIN(sir_imultssi):
    case BIN(sir_idivssi):
    case BIN(sir_imodssi):
    case BIN(sir_iorssi):
    case BIN(sir_iandssi):
    case BIN(sir_ieqssi):
    case BIN(sir_ineqssi):
    case BIN(sir_iltssi):
    case BIN(sir_igtssi):
    case BIN(sir_ilessi):
    case BIN(sir_igessi):
    case BIN(sir_iarefssi):
        rv_p = v1_p = v2_p = imm1_p = FALSE;
        imm2_p = TRUE;
        goto op2;
    case BIN(sir_iminussis):
    case BIN(sir_iplussis):
    case BIN(sir_imultsis):
    case BIN(sir_idivsis):
    case BIN(sir_imodsis):
    case BIN(sir_iorsis):
    case BIN(sir_iandsis):
    case BIN(sir_ieqsis):
    case BIN(sir_ineqsis):
    case BIN(sir_iltsis):
    case BIN(sir_igtsis):
    case BIN(sir_ilesis):
    case BIN(sir_igesis):
    case BIN(sir_iarefsis):
        rv_p = v1_p = v2_p = imm2_p = FALSE;
        imm1_p = TRUE;
        goto op2;
    case BIN(sir_iminussss):
    case BIN(sir_iplussss):
    case BIN(sir_imultsss):
    case BIN(sir_idivsss):
    case BIN(sir_imodsss):
    case BIN(sir_iorsss):
    case BIN(sir_iandsss):
    case BIN(sir_ieqsss):
    case BIN(sir_ineqsss):
    case BIN(sir_iltsss):
    case BIN(sir_igtsss):
    case BIN(sir_ilesss):
    case BIN(sir_igesss):
    case BIN(sir_iarefsss):
        rv_p = v1_p = v2_p = imm1_p = imm2_p = FALSE;
        goto op2;
    case BIN(sir_iminusvvv):
    case BIN(sir_iplusvvv):
    case BIN(sir_imultvvv):
    case BIN(sir_idivvvv):
    case BIN(sir_imodvvv):
    case BIN(sir_iorvvv):
    case BIN(sir_iandvvv):
    case BIN(sir_ieqvvv):
    case BIN(sir_ineqvvv):
    case BIN(sir_iltvvv):
    case BIN(sir_igtvvv):
    case BIN(sir_ilevvv):
    case BIN(sir_igevvv):
    case BIN(sir_iarefvvv):
        rv_p = v1_p = v2_p = TRUE;
        imm1_p = imm2_p = FALSE;
        goto op2;
    case BIN(sir_iminusvvi):
    case BIN(sir_iplusvvi):
    case BIN(sir_imultvvi):
    case BIN(sir_idivvvi):
    case BIN(sir_imodvvi):
    case BIN(sir_iorvvi):
    case BIN(sir_iandvvi):
    case BIN(sir_ieqvvi):
    case BIN(sir_ineqvvi):
    case BIN(sir_iltvvi):
    case BIN(sir_igtvvi):
    case BIN(sir_ilevvi):
    case BIN(sir_igevvi):
    case BIN(sir_iarefvvi):
        rv_p = v1_p = imm2_p = TRUE;
        v2_p = imm1_p = FALSE;
        goto op2;
    case BIN(sir_iminusviv):
    case BIN(sir_iplusviv):
    case BIN(sir_imultviv):
    case BIN(sir_idivviv):
    case BIN(sir_imodviv):
    case BIN(sir_iorviv):
    case BIN(sir_iandviv):
    case BIN(sir_ieqviv):
    case BIN(sir_ineqviv):
    case BIN(sir_iltviv):
    case BIN(sir_igtviv):
    case BIN(sir_ileviv):
    case BIN(sir_igeviv):
    case BIN(sir_iarefviv):
        rv_p = v2_p = imm1_p = TRUE;
        v1_p = imm2_p = FALSE;
        goto op2;
    case BIN(sir_iminusvvs):
    case BIN(sir_iplusvvs):
    case BIN(sir_imultvvs):
    case BIN(sir_idivvvs):
    case BIN(sir_imodvvs):
    case BIN(sir_iorvvs):
    case BIN(sir_iandvvs):
    case BIN(sir_ieqvvs):
    case BIN(sir_ineqvvs):
    case BIN(sir_iltvvs):
    case BIN(sir_igtvvs):
    case BIN(sir_ilevvs):
    case BIN(sir_igevvs):
    case BIN(sir_iarefvvs):
        rv_p = v1_p = TRUE;
        v2_p = imm1_p = imm2_p = FALSE;
        goto op2;
    case BIN(sir_iminusvsv):
    case BIN(sir_iplusvsv):
    case BIN(sir_imultvsv):
    case BIN(sir_idivvsv):
    case BIN(sir_imodvsv):
    case BIN(sir_iorvsv):
    case BIN(sir_iandvsv):
    case BIN(sir_ieqvsv):
    case BIN(sir_ineqvsv):
    case BIN(sir_iltvsv):
    case BIN(sir_igtvsv):
    case BIN(sir_ilevsv):
    case BIN(sir_igevsv):
    case BIN(sir_iarefvsv):
        rv_p = v2_p = TRUE;
        v1_p = imm1_p = imm2_p = FALSE;
        goto op2;
    case BIN(sir_iminusvsi):
    case BIN(sir_iplusvsi):
    case BIN(sir_imultvsi):
    case BIN(sir_idivvsi):
    case BIN(sir_imodvsi):
    case BIN(sir_iorvsi):
    case BIN(sir_iandvsi):
    case BIN(sir_ieqvsi):
    case BIN(sir_ineqvsi):
    case BIN(sir_iltvsi):
    case BIN(sir_igtvsi):
    case BIN(sir_ilevsi):
    case BIN(sir_igevsi):
    case BIN(sir_iarefvsi):
        rv_p = imm2_p = TRUE;
        v1_p = v2_p = imm1_p = FALSE;
        goto op2;
    case BIN(sir_iminusvis):
    case BIN(sir_iplusvis):
    case BIN(sir_imultvis):
    case BIN(sir_idivvis):
    case BIN(sir_imodvis):
    case BIN(sir_iorvis):
    case BIN(sir_iandvis):
    case BIN(sir_ieqvis):
    case BIN(sir_ineqvis):
    case BIN(sir_iltvis):
    case BIN(sir_igtvis):
    case BIN(sir_ilevis):
    case BIN(sir_igevis):
    case BIN(sir_iarefvis):
        rv_p = imm1_p = TRUE;
        v1_p = v2_p = imm2_p = FALSE;
        goto op2;
    case BIN(sir_iminusvss):
    case BIN(sir_iplusvss):
    case BIN(sir_imultvss):
    case BIN(sir_idivvss):
    case BIN(sir_imodvss):
    case BIN(sir_iorvss):
    case BIN(sir_iandvss):
    case BIN(sir_ieqvss):
    case BIN(sir_ineqvss):
    case BIN(sir_iltvss):
    case BIN(sir_igtvss):
    case BIN(sir_ilevss):
    case BIN(sir_igevss):
    case BIN(sir_iarefvss):
        rv_p = TRUE;
        v1_p = v2_p = imm1_p = imm2_p = FALSE;
        goto op2;
    op2:
        start = rv_p ? 1 : 0;
        stack_ops_num = 0;
        if (v2_p) {
            O("  val2 = ep[-%d];\n", (int)(lindex_t)operands[v1_p || imm1_p ? start + 1 : start]);
        } else if (imm2_p) {
            O("  val2 = 0x%"PRIxVALUE ";\n", operands[v1_p || imm1_p ? start + 1 : start]);
        } else {
            O("  val2 = sp[-1];\n");
            stack_ops_num++;
        }
        if (v1_p) {
            O("  val = ep[-%d];\n", (int)(lindex_t)operands[start]);
        } else {
            O("  val = sp[-%d];\n", v2_p || imm2_p ? 1 : 2);
            stack_ops_num++;
        }
        switch (insn_id) {
        case BIN(sir_iplussvv):
        case BIN(sir_iplussvi):
        case BIN(sir_iplussiv):
        case BIN(sir_iplussvs):
        case BIN(sir_iplusssv):
        case BIN(sir_iplusssi):
        case BIN(sir_iplussis):
        case BIN(sir_iplusvvv):
        case BIN(sir_iplusvvi):
        case BIN(sir_iplusviv):
        case BIN(sir_iplusvvs):
        case BIN(sir_iplusvsv):
        case BIN(sir_iplusvsi):
        case BIN(sir_iplusvis):
        case BIN(sir_iplusvss):
            O("  if (__builtin_expect (__builtin_add_overflow((long)val, (long)val2-1, &r), 0)) goto restart;\n");
            break;
        case BIN(sir_iminussvv):
        case BIN(sir_iminussvi):
        case BIN(sir_iminussiv):
        case BIN(sir_iminussvs):
        case BIN(sir_iminusssv):
        case BIN(sir_iminusssi):
        case BIN(sir_iminussis):
        case BIN(sir_iminusvvv):
        case BIN(sir_iminusvvi):
        case BIN(sir_iminusviv):
        case BIN(sir_iminusvvs):
        case BIN(sir_iminusvsv):
        case BIN(sir_iminusvsi):
        case BIN(sir_iminusvis):
        case BIN(sir_iminusvss):
            O("  if (__builtin_expect (__builtin_sub_overflow((long)val, (long)val2-1, &r), 0)) goto restart;\n");
            break;
        case BIN(sir_imultsvv):
        case BIN(sir_imultsvi):
        case BIN(sir_imultsiv):
        case BIN(sir_imultsvs):
        case BIN(sir_imultssv):
        case BIN(sir_imultssi):
        case BIN(sir_imultsis):
        case BIN(sir_imultvvv):
        case BIN(sir_imultvvi):
        case BIN(sir_imultviv):
        case BIN(sir_imultvvs):
        case BIN(sir_imultvsv):
        case BIN(sir_imultvsi):
        case BIN(sir_imultvis):
        case BIN(sir_imultvss):
            O("  {\n");
            O("     SIGNED_VALUE mul, x = (SIGNED_VALUE)val >> 1, y = (SIGNED_VALUE)val2 >> 1;\n");
            O("     if (__builtin_mul_overflow(x, y, &mul) || (r = (VALUE)mul << 1) >> 1 != (VALUE)mul) goto restart;\n");
            O("     r++;\n");
            O("  }\n");
            break;
        case BIN(sir_idivsvv):
        case BIN(sir_idivsvi):
        case BIN(sir_idivsiv):
        case BIN(sir_idivsvs):
        case BIN(sir_idivssv):
        case BIN(sir_idivssi):
        case BIN(sir_idivsis):
        case BIN(sir_idivvvv):
        case BIN(sir_idivvvi):
        case BIN(sir_idivviv):
        case BIN(sir_idivvvs):
        case BIN(sir_idivvsv):
        case BIN(sir_idivvsi):
        case BIN(sir_idivvis):
        case BIN(sir_idivvss):
            O("  {\n");
            O("     SIGNED_VALUE div, mod, x = (SIGNED_VALUE)val >> 1, y = (SIGNED_VALUE)val2 >> 1;\n");
            O("     if (x == %lld && y == -1) goto restart;\n", (long long)FIXNUM_MIN);
            O("     div = x / y; mod = x %% y;\n");
            O("     if (y > 0 ? mod < 0 : mod > 0) div -= 1;\n");
            O("     r = (VALUE)div << 1 + 1;\n");
            O("  }\n");
            break;
        case BIN(sir_imodsvv):
        case BIN(sir_imodsvi):
        case BIN(sir_imodsiv):
        case BIN(sir_imodsvs):
        case BIN(sir_imodssv):
        case BIN(sir_imodssi):
        case BIN(sir_imodsis):
        case BIN(sir_imodvvv):
        case BIN(sir_imodvvi):
        case BIN(sir_imodviv):
        case BIN(sir_imodvvs):
        case BIN(sir_imodvsv):
        case BIN(sir_imodvsi):
        case BIN(sir_imodvis):
        case BIN(sir_imodvss):
            O("  {\n");
            O("     SIGNED_VALUE mod, x = (SIGNED_VALUE)val >> 1, y = (SIGNED_VALUE)val2 >> 1;\n");
            O("     if (x == %lld && y == -1) {\n", (long long)FIXNUM_MIN);
            O("       r = 1;\n");
            O("     } else {\n");
            O("       mod = x %% y;\n");
            O("       if (y > 0 ? mod < 0 : mod > 0) mod += y;\n");
            O("       r = (VALUE)mod << 1 + 1;\n");
            O("     }\n");
            O("  }\n");
            break;
        case BIN(sir_iorsvv):
        case BIN(sir_iorsvi):
        case BIN(sir_iorsiv):
        case BIN(sir_iorsvs):
        case BIN(sir_iorssv):
        case BIN(sir_iorssi):
        case BIN(sir_iorsis):
        case BIN(sir_iorvvv):
        case BIN(sir_iorvvi):
        case BIN(sir_iorviv):
        case BIN(sir_iorvvs):
        case BIN(sir_iorvsv):
        case BIN(sir_iorvsi):
        case BIN(sir_iorvis):
        case BIN(sir_iorvss):
            O("  r = val | val2;\n");
            break;
        case BIN(sir_iandsvv):
        case BIN(sir_iandsvi):
        case BIN(sir_iandsiv):
        case BIN(sir_iandsvs):
        case BIN(sir_iandssv):
        case BIN(sir_iandssi):
        case BIN(sir_iandsis):
        case BIN(sir_iandvvv):
        case BIN(sir_iandvvi):
        case BIN(sir_iandviv):
        case BIN(sir_iandvvs):
        case BIN(sir_iandvsv):
        case BIN(sir_iandvsi):
        case BIN(sir_iandvis):
        case BIN(sir_iandvss):
            O("  r = (val & val2) | 1;\n");
            break;
        case BIN(sir_ieqsvv):
        case BIN(sir_ieqsvi):
        case BIN(sir_ieqsiv):
        case BIN(sir_ieqsvs):
        case BIN(sir_ieqssv):
        case BIN(sir_ieqssi):
        case BIN(sir_ieqsis):
        case BIN(sir_ieqvvv):
        case BIN(sir_ieqvvi):
        case BIN(sir_ieqviv):
        case BIN(sir_ieqvvs):
        case BIN(sir_ieqvsv):
        case BIN(sir_ieqvsi):
        case BIN(sir_ieqvis):
        case BIN(sir_ieqvss):
            O("  r = val == val2 ? 0x%x : 0x%x;\n", (int)Qtrue, (int)Qfalse);
            break;
        case BIN(sir_ineqsvv):
        case BIN(sir_ineqsvi):
        case BIN(sir_ineqsiv):
        case BIN(sir_ineqsvs):
        case BIN(sir_ineqssv):
        case BIN(sir_ineqssi):
        case BIN(sir_ineqsis):
        case BIN(sir_ineqvvv):
        case BIN(sir_ineqvvi):
        case BIN(sir_ineqviv):
        case BIN(sir_ineqvvs):
        case BIN(sir_ineqvsv):
        case BIN(sir_ineqvsi):
        case BIN(sir_ineqvis):
        case BIN(sir_ineqvss):
            O("  r = val != val2 ? 0x%x : 0x%x;\n", (int)Qtrue, (int)Qfalse);
            break;
        case BIN(sir_iltsvv):
        case BIN(sir_iltsvi):
        case BIN(sir_iltsiv):
        case BIN(sir_iltsvs):
        case BIN(sir_iltssv):
        case BIN(sir_iltssi):
        case BIN(sir_iltsis):
        case BIN(sir_iltvvv):
        case BIN(sir_iltvvi):
        case BIN(sir_iltviv):
        case BIN(sir_iltvvs):
        case BIN(sir_iltvsv):
        case BIN(sir_iltvsi):
        case BIN(sir_iltvis):
        case BIN(sir_iltvss):
            O("  r = (SIGNED_VALUE)val < (SIGNED_VALUE)val2 ? 0x%x : 0x%x;\n", (int)Qtrue, (int)Qfalse);
            break;
        case BIN(sir_igtsvv):
        case BIN(sir_igtsvi):
        case BIN(sir_igtsiv):
        case BIN(sir_igtsvs):
        case BIN(sir_igtssv):
        case BIN(sir_igtssi):
        case BIN(sir_igtsis):
        case BIN(sir_igtvvv):
        case BIN(sir_igtvvi):
        case BIN(sir_igtviv):
        case BIN(sir_igtvvs):
        case BIN(sir_igtvsv):
        case BIN(sir_igtvsi):
        case BIN(sir_igtvis):
        case BIN(sir_igtvss):
            O("  r = (SIGNED_VALUE)val > (SIGNED_VALUE)val2 ? 0x%x : 0x%x;\n", (int)Qtrue, (int)Qfalse);
            break;
        case BIN(sir_ilesvv):
        case BIN(sir_ilesvi):
        case BIN(sir_ilesiv):
        case BIN(sir_ilesvs):
        case BIN(sir_ilessv):
        case BIN(sir_ilessi):
        case BIN(sir_ilesis):
        case BIN(sir_ilevvv):
        case BIN(sir_ilevvi):
        case BIN(sir_ileviv):
        case BIN(sir_ilevvs):
        case BIN(sir_ilevsv):
        case BIN(sir_ilevsi):
        case BIN(sir_ilevis):
        case BIN(sir_ilevss):
            O("  r = (SIGNED_VALUE)val <= (SIGNED_VALUE)val2 ? 0x%x : 0x%x;\n", (int)Qtrue, (int)Qfalse);
            break;
        case BIN(sir_igesvv):
        case BIN(sir_igesvi):
        case BIN(sir_igesiv):
        case BIN(sir_igesvs):
        case BIN(sir_igessv):
        case BIN(sir_igessi):
        case BIN(sir_igesis):
        case BIN(sir_igevvv):
        case BIN(sir_igevvi):
        case BIN(sir_igeviv):
        case BIN(sir_igevvs):
        case BIN(sir_igevsv):
        case BIN(sir_igevsi):
        case BIN(sir_igevis):
        case BIN(sir_igevss):
            O("  r = (SIGNED_VALUE)val >= (SIGNED_VALUE)val2 ? 0x%x : 0x%x;\n", (int)Qtrue, (int)Qfalse);
            break;
        case BIN(sir_iarefsvv):
        case BIN(sir_iarefsvi):
        case BIN(sir_iarefsiv):
        case BIN(sir_iarefsvs):
        case BIN(sir_iarefssv):
        case BIN(sir_iarefssi):
        case BIN(sir_iarefsis):
        case BIN(sir_iarefvvv):
        case BIN(sir_iarefvvi):
        case BIN(sir_iarefviv):
        case BIN(sir_iarefvvs):
        case BIN(sir_iarefvsv):
        case BIN(sir_iarefvsi):
        case BIN(sir_iarefvis):
        case BIN(sir_iarefvss):
            gen_class_of(buff, "r", "val");
            O("  if (r != 0x%"PRIxVALUE ") goto restart;\n", rb_cArray);
            gen_array_aref_len(buff, "pv", "r", "val"); /* r is len */
            /* r = rb_ary_entry_internal(val, FIX2LONG(val2)); */
            O("  val2 = (SIGNED_VALUE)val2 >> 1;\n"); /* offset = FIX2LONG(val2) */
            O("  if (r == 0) {\n");
            O("    r = 0x%x;\n", (int)Qnil);
            O("  } else if ((SIGNED_VALUE)val2 < 0 ) {\n");
            O("    val2 = (SIGNED_VALUE)val2 + (SIGNED_VALUE)r;\n");
            O("    if ((SIGNED_VALUE)val2 < 0) {\n");
            O("      r = 0x%x;\n", (int)Qnil);
            O("    } else {\n");
            O("      r = pv[val2];\n");
            O("    }\n");
            O("  } else if (r <= val2) {\n");
            O("    r = 0x%x;\n", (int)Qnil);
            O("  } else {\n");
            O("    r = pv[val2];\n");
            O("  }\n");
            break;
        default:
            assert(FALSE);
        }
        if (stack_ops_num != 0) O("  sp -= %d;\n", stack_ops_num);
        if (rv_p)
            O("  ep[-%d] = r;\n", (int)(lindex_t)operands[0]);
        else
            O("  *sp++ = r;\n");
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    case BIN(sir_iasetsis):
        stack_ops_num = 2;
        O("  r = sp[-2]; val2 = sp[-1];\n");
        O("  val = 0x%"PRIxVALUE ";\n", operands[0]);
        goto iaset;
    case BIN(sir_iasetsiv):
        stack_ops_num = 1;
        O("  r = sp[-1];\n");
        O("  val = 0x%"PRIxVALUE ";\n", operands[0]);
        O("  val2 = ep[-%d];\n", (int)(lindex_t)operands[1]);
        goto iaset;
    case BIN(sir_iasetviv):
        stack_ops_num = 0;
        O("  r = ep[-%d];\n", (int)(lindex_t)operands[0]);
        O("  val = 0x%"PRIxVALUE ";\n", operands[1]);
        O("  val2 = ep[-%d];\n", (int)(lindex_t)operands[2]);
        goto iaset;
    case BIN(sir_iasetvis):
        stack_ops_num = 1;
        O("  r = ep[-%d];\n", (int)(lindex_t)operands[0]);
        O("  val = 0x%"PRIxVALUE ";\n", operands[1]);
        O("  val2 = sp[-1];\n");
        goto iaset;
    case BIN(sir_iasetsss):
        stack_ops_num = 3;
        O("  r = sp[-3]; val = sp[-2]; val2 = sp[-1];\n");
        goto iaset;
    case BIN(sir_iasetsvv):
        stack_ops_num = 1;
        O("  r = sp[-1];\n");
        O("  val = ep[-%d];\n", (int)(lindex_t)operands[0]);
        O("  val2 = ep[-%d];\n", (int)(lindex_t)operands[1]);
        goto iaset;
    case BIN(sir_iasetsvs):
        stack_ops_num = 2;
        O("  r = sp[-2]; val2 = sp[-1];\n");
        O("  val = ep[-%d];\n", (int)(lindex_t)operands[0]);
        goto iaset;
    case BIN(sir_iasetssv):
        stack_ops_num = 2;
        O("  r = sp[-2]; val = sp[-1];\n");
        O("  val2 = ep[-%d];\n", (int)(lindex_t)operands[0]);
        goto iaset;
    case BIN(sir_iasetvss):
        stack_ops_num = 2;
        O("  r = ep[-%d];\n", (int)(lindex_t)operands[0]);
        O("  val = sp[-2]; val2 = sp[-1];\n");
        goto iaset;
    case BIN(sir_iasetvvv):
        stack_ops_num = 0;
        O("  r = ep[-%d];\n", (int)(lindex_t)operands[0]);
        O("  val = ep[-%d];\n", (int)(lindex_t)operands[1]);
        O("  val2 = ep[-%d];\n", (int)(lindex_t)operands[2]);
        goto iaset;
    case BIN(sir_iasetvvs):
        stack_ops_num = 1;
        O("  r = ep[-%d];\n", (int)(lindex_t)operands[0]);
        O("  val = ep[-%d];\n", (int)(lindex_t)operands[1]);
        O("  val2 = sp[-1];\n");
        goto iaset;
    case BIN(sir_iasetvsv):
        stack_ops_num = 1;
        O("  r = ep[-%d];\n", (int)(lindex_t)operands[0]);
        O("  val = sp[-1];\n");
        O("  val2 = ep[-%d];\n", (int)(lindex_t)operands[1]);
    iaset: /* r[val] = val2; */
        gen_class_of(buff, "t", "r");
        O("  if (t != 0x%"PRIxVALUE ") goto restart;\n", rb_cArray);
        gen_array_aref_len(buff, "pv", "t", "r"); /* t is len */
        /* rb_ary_store(r, FIX2LONG(val), val2); */
        O("  val = (SIGNED_VALUE)val >> 1;\n"); /* offset = FIX2LONG(val) */
        O("  if ((SIGNED_VALUE)val < 0 ) {\n");
        O("    val = (SIGNED_VALUE)val + (SIGNED_VALUE)t;\n");
        O("    if ((SIGNED_VALUE)val < 0) goto restart;\n");
        O("  }\n");
        O("  if (val >= t) goto restart;\n"); /* needs expansion */
        gen_obj_frozen(buff, "r");
        O("  pv[val] = val2;\n");
        gen_write_barrier(buff, "r", "val2"); /* RB_OBJ_WRITE (r, slot, val2) */
        if (stack_ops_num != 0) O("  sp -= %d;\n", stack_ops_num);
        O("  *sp++ = val2;\n");
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    case BIN(sir_ibeqss):
        op = "==";
        goto ibss;
    case BIN(sir_ibneqss):
        op = "!=";
        goto ibss;
    case BIN(sir_ibltss):
        op = "<";
        goto ibss;
    case BIN(sir_ibgtss):
        op = ">";
        goto ibss;
    case BIN(sir_ibless):
        op = "<=";
        goto ibss;
    case BIN(sir_ibgess):
        op = ">=";
    ibss:
        v1_p = v2_p = imm2_p = FALSE;
        goto ib;
    case BIN(sir_ibeqvv):
        op = "==";
        goto ibvv;
    case BIN(sir_ibneqvv):
        op = "!=";
        goto ibvv;
    case BIN(sir_ibltvv):
        op = "<";
        goto ibvv;
    case BIN(sir_ibgtvv):
        op = ">";
        goto ibvv;
    case BIN(sir_iblevv):
        op = "<=";
        goto ibvv;
    case BIN(sir_ibgevv):
        op = ">=";
    ibvv:
        v1_p = v2_p = TRUE;
        imm2_p = FALSE;
        goto ib;
    case BIN(sir_ibeqvi):
        op = "==";
        goto ibvi;
    case BIN(sir_ibneqvi):
        op = "!=";
        goto ibvi;
    case BIN(sir_ibltvi):
        op = "<";
        goto ibvi;
    case BIN(sir_ibgtvi):
        op = ">";
        goto ibvi;
    case BIN(sir_iblevi):
        op = "<=";
        goto ibvi;
    case BIN(sir_ibgevi):
        op = ">=";
    ibvi:
        v1_p = imm2_p = TRUE;
        v2_p = FALSE;
        goto ib;
    case BIN(sir_ibeqvs):
        op = "==";
        goto ibvs;
    case BIN(sir_ibneqvs):
        op = "!=";
        goto ibvs;
    case BIN(sir_ibltvs):
        op = "<";
        goto ibvs;
    case BIN(sir_ibgtvs):
        op = ">";
        goto ibvs;
    case BIN(sir_iblevs):
        op = "<=";
        goto ibvs;
    case BIN(sir_ibgevs):
        op = ">=";
    ibvs:
        v1_p = TRUE;
        v2_p = imm2_p = FALSE;
        goto ib;
    case BIN(sir_ibeqsv):
        op = "==";
        goto ibsv;
    case BIN(sir_ibneqsv):
        op = "!=";
        goto ibsv;
    case BIN(sir_ibltsv):
        op = "<";
        goto ibsv;
    case BIN(sir_ibgtsv):
        op = ">";
        goto ibsv;
    case BIN(sir_iblesv):
        op = "<=";
        goto ibsv;
    case BIN(sir_ibgesv):
        op = ">=";
    ibsv:
        v1_p = imm2_p = FALSE;
        v2_p = TRUE;
        goto ib;
    case BIN(sir_ibeqsi):
        op = "==";
        goto ibsi;
    case BIN(sir_ibneqsi):
        op = "!=";
        goto ibsi;
    case BIN(sir_ibltsi):
        op = "<";
        goto ibsi;
    case BIN(sir_ibgtsi):
        op = ">";
        goto ibsi;
    case BIN(sir_iblesi):
        op = "<=";
        goto ibsi;
    case BIN(sir_ibgesi):
        op = ">=";
    ibsi:
        v1_p = v2_p = FALSE;
        imm2_p = TRUE;
    ib:
        O("  if (*(rb_atomic_t *)((char*)ec+%d) & ~(VALUE)*(rb_atomic_t *)((char*)ec+%d)) goto restart;\n",
          (int)offsetof(rb_execution_context_t, interrupt_flag), (int)offsetof(rb_execution_context_t, interrupt_mask));
        if (v2_p)
            O("  val2 = ep[-%d];\n", (int)(lindex_t)operands[v1_p ? 3 : 2]);
        else if (imm2_p)
            O("  val2 = 0x%"PRIxVALUE ";\n", operands[v1_p ? 3 : 2]);
        else
            O("  val2 = *--sp;\n");
        if (v1_p)
            O("  val = ep[-%d];\n", (int)(lindex_t)operands[2]);
        else
            O("  val = *--sp;\n");
        O("  if ((long)val %s (long)val2)\n", op);
        O("    pc += %d;\n", insn_len(insn_id) + (int)(OFFSET)operands[0]); /* true label */
        O("  else\n");
        O("    pc += %d;\n", insn_len(insn_id) + (int)(OFFSET)operands[1]); /* false label */
        break;
    case BIN(sir_fplus):
        O("  val = sp[-2]; val2 = *--sp;\n");
        O("  sp[-1] = mirjit_fplus(val, val2);\n");
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    case BIN(sir_fplusf):
        O("  val = sp[-1]; d = operands[0];\n");
        O("  sp[-1] = mirjit_fplusf(val, d);\n");
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    case BIN(sir_fminus):
        O("  val = sp[-2]; val2 = *--sp;\n");
        O("  sp[-1] = mirjit_fminus(val, val2);\n");
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    case BIN(sir_fminusf):
        O("  val = sp[-1]; d = operands[0];\n");
        O("  sp[-1] = mirjit_fminusf(val, d);\n");
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    case BIN(sir_frminusf):
        O("  val = sp[-1]; d = operands[0];\n");
        O("  sp[-1] = mirjit_frminusf(d, val);\n");
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    case BIN(sir_fmult):
        O("  val = sp[-2]; val2 = *--sp;\n");
        O("  sp[-1] = mirjit_fmult(val, val2);\n");
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    case BIN(sir_fmultf):
        O("  val = sp[-1]; d = operands[0];\n");
        O("  sp[-1] = mirjit_fmultf(val, d);\n");
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    case BIN(sir_fdiv):
        O("  val = sp[-2]; val2 = *--sp;\n");
        O("  sp[-1] = mirjit_fdiv(val, val2);\n");
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    case BIN(sir_fdivf):
        O("  val = sp[-1]; d = operands[0];\n");
        O("  sp[-1] = mirjit_fdivf(val, d);\n");
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    case BIN(sir_frdivf):
        O("  val = sp[-1]; d = operands[0];\n");
        O("  sp[-1] = mirjit_frdivf(d, val);\n");
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    case BIN(sir_fmod):
        O("  val = sp[-2]; val2 = *--sp;\n");
        O("  sp[-1] = mirjit_fmod(val, val2);\n");
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    case BIN(sir_fmodf):
        O("  val = sp[-1]; d = operands[0];\n");
        O("  sp[-1] = mirjit_fmodf(val, d);\n");
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    case BIN(sir_frmodf):
        O("  val = sp[-1]; d = operands[0];\n");
        O("  sp[-1] = mirjit_frmodf(d, val);\n");
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    case BIN(sir_jump): // ??? test only for backward jumps/branches
        O("  if (*(rb_atomic_t *)((char*)ec+%d) & ~(VALUE)*(rb_atomic_t *)((char*)ec+%d)) goto restart;\n",
          (int)offsetof(rb_execution_context_t, interrupt_flag), (int)offsetof(rb_execution_context_t, interrupt_mask));
        O("  pc += %d;\n", insn_len(insn_id) + (int)(OFFSET)operands[0]); /* label */
        break;
    case BIN(sir_branchnil):
        nil_p = TRUE;
        goto branch;
    case BIN(sir_branch):
        nil_p = FALSE;
    branch:
        O("  if (*(rb_atomic_t *)((char*)ec+%d) & ~(VALUE)*(rb_atomic_t *)((char*)ec+%d)) goto restart;\n",
          (int)offsetof(rb_execution_context_t, interrupt_flag), (int)offsetof(rb_execution_context_t, interrupt_mask));
        O("  val = *--sp;\n");
        if (nil_p)
            O("  if (val == 0x%x)", (int)Qnil); /* NIL_P */
        else
            O("  if (val & ~0x%x)\n", (int)Qnil); /* RTEST */
        O("    pc += %d;\n", insn_len(insn_id) + (int)(OFFSET)operands[0]); /* true label */
        O("  else\n");
        O("    pc += %d;\n", insn_len(insn_id) + (int)(OFFSET)operands[1]); /* false label */
        break;
    case BIN(sir_getinlinecache):
#if SIZEOF_SERIAL_T > SIZEOF_VALUE
        return FALSE;
#else
#define IMEMO_CONST_CACHE_SHAREABLE IMEMO_FL_USER0
        /* struct iseq_inline_constant_cache_entry *ice = ic->entry;
           if (ice && GET_IC_SERIAL(ice) == GET_GLOBAL_CONSTANT_STATE()
               && ((ice->flags & IMEMO_CONST_CACHE_SHAREABLE)
                   || (th = ec->thread_ptr) == NULL || th->ractor == th->vm->ractor.main_ractor)) {
             if (ice->ic_cref != NULL) goto restart;
             *sp++ = ice->value;
             JUMP(dst);
          } else {
             *sp++ = Qnil;
             JUMP(dst2);
          } */
        O("  pv = *(VALUE **)((char *)0x%"PRIxVALUE " + %d);\n", /* pv is ice */
          operands[2], (int)offsetof(struct iseq_inline_constant_cache, entry));
        O("  if (pv && *(VALUE **)((char *)pv + %d) == *(VALUE *)0x%"PRIxVALUE
          " && ((*(VALUE *)((char *)pv + %d) & 0x%"PRIxVALUE ") || ",
          (int)offsetof(struct iseq_inline_constant_cache_entry, ic_serial.raw),
          (VALUE)&ruby_vm_global_constant_state,
          (int)offsetof(struct iseq_inline_constant_cache_entry, flags),
          (VALUE)IMEMO_CONST_CACHE_SHAREABLE);
        gen_rb_ractor_main_p(buff, "pv2");
        O(")) {\n");
        O("    if (*(VALUE **)((char *)pv + %d)) goto restart;\n", /* complex case involving vm_get_cref */
          (int)offsetof(struct iseq_inline_constant_cache_entry, ic_cref));
        O("    *sp++ = *(VALUE **)((char *)pv + %d);\n",
          (int)offsetof(struct iseq_inline_constant_cache_entry, value));
        O("    pc += %d;\n", insn_len(insn_id) + (int)(OFFSET)operands[0]); /* dst */
        O("  } else {\n");
        O("    *sp++ = 0x%x;\n", (int)Qnil);
        O("    pc += %d;\n", insn_len(insn_id) + (int)(OFFSET)operands[1]); /* dst2 */
        O("  }\n");
        break;
#endif
    case BIN(leave):
        O("  flags = *ep;\n");
        O("  if (flags & 0x%"PRIxVALUE ") goto restart;\n", (VALUE)VM_FRAME_FLAG_FINISH);
        if (OPT_CHECKED_RUN)
            O("  if (__builtin_expect(sp - 1 != cfp->__bp__, 0)) goto restart;\n");
        gen_pop_frame(buff, TRUE);
        break;
    case BIN(sir_iseq_send_without_block): // ??? visibility
        CALL_DATA cd = (CALL_DATA)operands[0];
        struct rb_iseq_bbv *bbv_ref = (struct rb_iseq_bbv *)operands[1];
        rb_iseq_t *iseq = bbv_ref->iseq;
        struct rb_iseq_constant_body *body = iseq->body;
        const struct rb_callcache *cc = cd->cc;
        int argc = vm_ci_argc(cd->ci);
        int param_size = body->param.size;
        const struct rb_callable_method_entry_struct *cme = vm_cc_cme(cc);
        VALUE *start_pc = body->bbv_code + bbv_ref->bbv_start;
        int local_size = body->local_table_size - param_size;
        int stack_max = body->stack_max;

        assert(callable_method_entry_p(cme) && argc == param_size);
        O("  {\n");
        O("    void *cc = 0x%"PRIxVALUE ";\n", (VALUE)cc);
        O("    VALUE klass, *argv, recv = sp[-%d];\n", argc + 1);
        gen_class_of(buff, "klass", "recv");
        O("    if (__builtin_expect(*(VALUE *)((char *)cc + %d) != klass, 0)) goto restart;\n",
          (int)offsetof(struct rb_callcache, klass));
        O("    argv = (VALUE *)sp - %d;\n", argc);
        O("    VALUE specval = 0x%"PRIxVALUE ";\n", (VALUE)VM_BLOCK_HANDLER_NONE);
        gen_push_frame(buff, local_size, stack_max, (int)insn_len(insn_id), (VALUE)cme,
                       (VALUE)(VM_FRAME_MAGIC_METHOD | VM_ENV_FLAG_LOCAL),
                       (VALUE)iseq, (VALUE)start_pc);
        O("  }\n");
        break;
        // ??? fp insns
        // ??? general rtl insns
    case BIN(sir_getivar):
    case BIN(sir_igetivar):
    case BIN(sir_setivar): {
#if SIZEOF_SERIAL_T != SIZEOF_VALUE
        return FALSE;
#else
        IVC ic = (IVC)operands[1];
        struct rb_iv_index_tbl_entry *entry = ic->entry;
        rb_serial_t class_serial = entry == NULL ? 0 : entry->class_serial;
        uint32_t index = entry->index;

        O("  val = cfp->self;\n");
        O("  val2 = *(VALUE *)((char *)val + %d);\n", (int)offsetof(struct RBasic, klass)); /* self class */
        O("  if (*(VALUE *)((char *)val2 + %d) != 0x%"PRIxVALUE ") goto restart;\n",
          (int)offsetof(struct RClass, class_serial), (VALUE)class_serial);
        if (insn_id == BIN(sir_setivar)) gen_obj_frozen(buff, "val");
        O("  if (*((VALUE *)((char *)val + %d)) & 0x%"PRIxVALUE ") {\n", /* if (RB_FL_ANY_RAW(obj, ROBJECT_EMBED)) */
          (int)offsetof(struct RBasic, flags), (VALUE)ROBJECT_EMBED);
        /* if (index >= ROBJECT_NUMIV(val)) goto restart; pv = &ROBJECT_IVPTR(val)[index] */
        O("    if (__builtin_expect(%u >= %u, 0)) goto restart;\n",
          (unsigned)index, (unsigned)ROBJECT_EMBED_LEN_MAX);
        O("    pv = &((VALUE *)((char *)val + %d))[%u];\n",
          (int)offsetof(struct RObject, as.ary), (unsigned)index);
        O("  } else {\n");
        O("    if (__builtin_expect(%u >= *(uint32_t *)((char *)val + %d), 0)) goto restart;\n",
          (unsigned)index, (int)offsetof(struct RObject, as.heap.numiv));
        O("    pv = &(*(VALUE **)((char *)val + %d))[%u];\n",
          (int)offsetof(struct RObject, as.heap.ivptr), (unsigned)index);
        O("  }\n");
        if (insn_id == BIN(sir_setivar)) {// ???
            O("  r = *--sp;\n");
            O("  *pv = r;\n"); /* RB_OBJ_WRITE(self, &pv[index], r); */
            O("  pc += %d;\n", insn_len(insn_id));
            gen_write_barrier(buff, "val", "r");
        } else {
            O("  r = *pv;\n");
            if (insn_id == BIN(sir_igetivar)) {
                O("  if (!(r & 0x%x)) goto restart;\n", (int)RUBY_FIXNUM_FLAG); /* if (!FIXNUM_P(r)) goto restart */
            }
            O("  *sp++= r;\n");
            O("  pc += %d;\n", insn_len(insn_id));
        }
        break;
#endif
    }
    case BIN(sir_send_ivget):
    case BIN(sir_send_ivset): {
#if SIZEOF_SERIAL_T != SIZEOF_VALUE
        return FALSE;
#else
        VALUE klass = operands[0];
        unsigned index = (unsigned)(lindex_t)operands[1];
        O("  val = sp[%d];\n", insn_id == BIN(sir_send_ivset) ? -2 : -1); /* recv */
        gen_class_of(buff, "val2", "val");
        O("  if (__builtin_expect(val2 != 0x%"PRIxVALUE ", 0)) goto restart;\n", klass);
        if (insn_id == BIN(sir_send_ivset)) {
            O("  r = sp[-1];\n"); /* val */
            gen_obj_frozen(buff, "val");
        }
        O("  if (*((VALUE *)((char *)val + %d)) & 0x%"PRIxVALUE ") {\n", /* if (RB_FL_ANY_RAW(obj, ROBJECT_EMBED)) */
          (int)offsetof(struct RBasic, flags), (VALUE)ROBJECT_EMBED);
        /* if (index >= ROBJECT_NUMIV(val)) goto restart; pv = &ROBJECT_IVPTR(val)[index] */
        O("    if (__builtin_expect(%u >= %u, 0)) goto restart;\n",
          (unsigned)index, (unsigned)ROBJECT_EMBED_LEN_MAX);
        O("    pv = &((VALUE *)((char *)val + %d))[%u];\n",
          (int)offsetof(struct RObject, as.ary), (unsigned)index);
        O("  } else {\n");
        O("    if (__builtin_expect(%u >= *(uint32_t *)((char *)val + %d), 0)) goto restart;\n",
          (unsigned)index, (int)offsetof(struct RObject, as.heap.numiv));
        O("    pv = &(*(VALUE **)((char *)val + %d))[%u];\n",
          (int)offsetof(struct RObject, as.heap.ivptr), (unsigned)index);
        O("  }\n");
        if (insn_id == BIN(sir_send_ivget)) {
            O("  sp[-1] = *pv;\n");
            O("  pc += %d;\n", insn_len(insn_id));
        } else {
            O("  sp--;\n");
            O("  *pv = r;\n");
            O("  sp[-1] = r;\n");
            O("  pc += %d;\n", insn_len(insn_id));
            gen_write_barrier(buff, "val", "r");
        }
        break;
#endif
    }
#ifdef MIRJIT_ITER_INSNS
    case BIN(sir_iter_start): {
        int (*f)(VALUE, VALUE *) = (void *)operands[0];
        O("  val = sp[-1];\n");
        O("  if (!((int (*)(VALUE, VALUE *))0x%"PRIxVALUE ")(val, &val2)) goto restart;\n", (VALUE)f); /* f(arg, index)*/
        O("  *sp++ = val2;\n");
        O("  *sp++ = 0x%"PRIxVALUE ";\n", Qnil);
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    }
    case BIN(sir_iter_body): {
        struct rb_iseq_bbv *bbv_ref = (struct rb_iseq_bbv *)operands[1];
        rb_iseq_t *iseq = bbv_ref->iseq;
        struct rb_iseq_constant_body * body = iseq->body;
        int param_size = body->param.size;
        int lead_num = body->param.lead_num;
        VALUE *start_pc = body->bbv_code + bbv_ref->bbv_start;
        int local_size = body->local_table_size - param_size;
        int stack_max = body->stack_max;
        const struct rb_callable_method_entry_struct *cme = bbv_ref->cme;
        int (*f)(VALUE, VALUE, VALUE *) = (void *)operands[2];
        long argc = 1;

        O("  {\n");
        O("    VALUE self, recv, index, arg, ary;\n");
        O("    recv = sp[-3];\n");
        O("    index = sp[-2];\n");
        O("    if (!((int (*)(VALUE, VALUE, VALUE *))0x%"PRIxVALUE ")(recv, index, &arg)) {\n", (VALUE)f);
        O("      sp -= 2;\n");
        O("      sp[-1] = arg;\n"); /* result */
        O("      pc += %d;\n", insn_len(insn_id) + (int)(OFFSET)operands[0]);
        O("      goto exit;\n");
        O("    }\n");
        O("    sp--;\n"); /* remove res from iter_start or previous sir_iter_body */
        O("    self = cfp->self;\n"); /* self */
        O("    *sp = self;\n");
        if (param_size > 1 && argc == 1) {
            O("    ary = rb_check_array_type(arg);\n");
            O("    if (ary != 0x%x) {\n", (int)Qnil);
            O("      long i, len;\n");
            O("      VALUE *aref;\n");
            gen_array_aref_len(buff, "aref", "len", "ary");
            /* CHECK_VM_STACK_OVERFLOW(GET_CFP(), lead_num): */
            gen_check_overflow0(buff, "cfp", "sp", lead_num);
            O("      if (len > lead_num) len = %d;\n", lead_num);
            O("      for (i = 0; i < len; i++)\n");
            O("          sp[i + 1] = aref[i];\n");
            O("      for (; i < lead_num; i++) sp[i + 1] = 0x%x;\n", (int)Qnil);
            O("    } else {\n");
        } else {
            O("    {\n");
            O("      sp[1] = arg;\n");
        }
        O("    }\n");
        O("    VALUE *argv = sp + 1;\n");
        O("    VALUE specval = cfp->ep | 1;\n");
        O("    recv = self;\n"); /* self */
        gen_push_frame(buff, local_size, stack_max, (int)insn_len(insn_id), (VALUE)cme,
                       (VALUE)VM_FRAME_MAGIC_METHOD, (VALUE)iseq, (VALUE)start_pc);
        O("  }\n");
        break;
    }
#endif
    case BIN(sir_cfunc_send): {
        CALL_DATA cd = (CALL_DATA)operands[0];
        rb_iseq_t *block_iseq = (rb_iseq_t *)operands[1];
        const struct rb_callcache *cc = cd->cc;
        int argc = vm_ci_argc(cd->ci);
        const struct rb_callable_method_entry_struct *cme = vm_cc_cme(cc);
        const rb_method_cfunc_t *cfunc = vm_method_cfunc_entry(cme);
        rb_cfunc_invoker_t invoker = cfunc->invoker;
        rb_cfunc_func_t func = cfunc->func;

    /*  if (klass != CLASS_OF(recv)) goto sir_cfunc_send_exit;
        ???RUBY_DTRACE_CMETHOD_ENTRY_HOOK(ec, cme->owner, cme->def->original_id);
        ???EXEC_EVENT_HOOK(ec, RUBY_EVENT_C_CALL, recv, cme->def->original_id, vm_ci_mid(cd->ci), cme->owner, Qundef);
        if (blockiseq == NULL) {
          bh = VM_BLOCK_HANDLER_NONE;
        } else {
          captured = VM_CFP_TO_CAPTURED_BLOCK(GET_CFP());
          captured->code.iseq = blockiseq;
          bh = VM_BH_FROM_ISEQ_BLOCK(captured);
        }
        vm_push_frame(ec, NULL, VM_FRAME_MAGIC_CFUNC | VM_FRAME_FLAG_CFRAME | VM_ENV_FLAG_LOCAL, recv,
                      bh, (VALUE)cme, 0, cfp->sp, 0, 0);
        reg_cfp->sp = argv - 1;
        val = (*invoker)(recv, argc, reg_cfp->sp + 1, func); // can be improved by calling cfunc->func(arg ... )
        if (__builtin_expect(reg_cfp != ec->cfp + 1, 0)) rb_check_cfp_consistency(ec, reg_cfp, "sir_cfunc_send");
        rb_vm_pop_frame(ec); */

        O("  {\n");
        O("    void *cc = 0x%"PRIxVALUE ";\n", (VALUE)cc);
        O("    VALUE klass, *argv = sp - %d, recv = sp[-%d];\n", argc, argc + 1);
        gen_class_of(buff, "klass", "recv");
        O("    if (__builtin_expect(*(VALUE *)((char *)cc + %d) != klass, 0)) goto restart;\n",
          (int)offsetof(struct rb_callcache, klass));
        O("    argv = (VALUE *)sp - %d;\n", argc);
        if (block_iseq == NULL) {
            O("    VALUE specval = 0x%"PRIxVALUE ";\n", (VALUE)VM_BLOCK_HANDLER_NONE);
        } else {
            O("    VALUE *captured = (VALUE *)((char*)cfp + %d);\n", (int)offsetof(struct rb_captured_block, self));
            O("    *(VALUE *)((char*)captured + %d) = 0x%"PRIxVALUE ";\n",
              (int)offsetof(struct rb_captured_block, code.iseq), (VALUE)block_iseq);
            O("    VALUE specval = (VALUE)captured | 1;\n");
        }
        O("    VALUE *old_cfp = cfp;\n");
        gen_push_frame(buff, 0, 0, (int)insn_len(insn_id), (VALUE)cme,
                       (VALUE)(VM_FRAME_MAGIC_CFUNC | VM_FRAME_FLAG_CFRAME | VM_ENV_FLAG_LOCAL),
                       (VALUE)0, (VALUE)0);
        O("    sp = argv - 1;\n"); /* poping args including recv (sp before leave) */
        O("    VALUE (*invoker)(VALUE, int, VALUE, VALUE) = (void *)0x%"PRIxVALUE ";\n", (VALUE)invoker);
        O("    val = invoker(recv, %d, sp + 1, 0x%"PRIxVALUE ");\n", argc, (VALUE)func);
        O("    if (__builtin_expect((VALUE)old_cfp != *(VALUE *)((char *)ec + %d) + %d, 0))\n",
          (int)offsetof(rb_execution_context_t, cfp), (int)sizeof(rb_control_frame_t));
        O("       rb_check_cfp_consistency(ec, cfp, \"sir_cfunc_send\");\n");
        gen_pop_frame(buff, FALSE);
        O("  }\n");
        break;
    }
    case BIN(sir_send_without_block):
    default: return FALSE;
    }
    return TRUE;
}

static MIR_context_t ctx;

static VARR(char) *jit_c_code;

struct input {
  const char *input_name;
  size_t curr_char, code_len;
  const uint8_t *code;
  struct c2mir_options options;
};

static struct input curr_input;

static int t_getc(void *data) {
  struct input *input = data;
  return input->curr_char >= input->code_len ? EOF : input->code[input->curr_char++];
}

static VALUE mirjit_dbl2num(DBL d) {return DBL2NUM(d);}
static DBL mirjit_rfloat_value(VALUE v) {return RFLOAT_VALUE(v);}

static VALUE mirjit_fplus(VALUE v1, VALUE v2){ return DBL2NUM(RFLOAT_VALUE(v1) + RFLOAT_VALUE(v2)); }
static VALUE mirjit_fplusf(VALUE v, DBL d){ return DBL2NUM(RFLOAT_VALUE(v) + d); }
static VALUE mirjit_fminus(VALUE v1, VALUE v2){ return DBL2NUM(RFLOAT_VALUE(v1) - RFLOAT_VALUE(v2)); }
static VALUE mirjit_fminusf(VALUE v, DBL d){ return DBL2NUM(RFLOAT_VALUE(v) - d); }
static VALUE mirjit_frminusf(DBL d, VALUE v){ return DBL2NUM(d - RFLOAT_VALUE(v)); }
static VALUE mirjit_fmult(VALUE v1, VALUE v2){ return DBL2NUM(RFLOAT_VALUE(v1) * RFLOAT_VALUE(v2)); }
static VALUE mirjit_fmultf(VALUE v, DBL d){ return DBL2NUM(RFLOAT_VALUE(v) * d); }
static VALUE mirjit_fdiv(VALUE v1, VALUE v2){ return DBL2NUM(ruby_float_div(RFLOAT_VALUE(v1), RFLOAT_VALUE(v2))); }
static VALUE mirjit_fdivf(VALUE v, DBL d){ return DBL2NUM(ruby_float_div(RFLOAT_VALUE(v), d)); }
static VALUE mirjit_frdivf(DBL d, VALUE v){ return DBL2NUM(ruby_float_div(d, RFLOAT_VALUE(v))); }
static VALUE mirjit_fmod(VALUE v1, VALUE v2){ return DBL2NUM(ruby_float_mod(RFLOAT_VALUE(v1), RFLOAT_VALUE(v2))); }
static VALUE mirjit_fmodf(VALUE v, DBL d){ return DBL2NUM(ruby_float_mod(RFLOAT_VALUE(v), d)); }
static VALUE mirjit_frmodf(DBL d, VALUE v){ return DBL2NUM(ruby_float_mod(d, RFLOAT_VALUE(v))); }

#ifdef MIRJIT_RESTART_STAT
long mirjit_run_nums, mirjit_restart_nums;
#endif

typedef struct resolver_el {const char *name; void *addr;} resolver_el_t;
static resolver_el_t resolver_els[] = {
    {"rb_gc_writebarrier", rb_gc_writebarrier},
    {"rb_check_array_type", rb_check_array_type},
    {"rb_check_cfp_consistency", rb_check_cfp_consistency},
    {"mirjit_rfloat_value", mirjit_rfloat_value},
    {"mirjit_dbl2num", mirjit_dbl2num},
    {"mirjit_fplus", mirjit_fplus},
    {"mirjit_fplusf", mirjit_fplusf},
    {"mirjit_fminus", mirjit_fminus},
    {"mirjit_fminusf", mirjit_fminusf},
    {"mirjit_frminusf", mirjit_frminusf},
    {"mirjit_fmult", mirjit_fmult},
    {"mirjit_fmultf", mirjit_fmultf},
    {"mirjit_fdiv", mirjit_fdiv},
    {"mirjit_fdivf", mirjit_fdivf},
    {"mirjit_frdivf", mirjit_frdivf},
    {"mirjit_fplus", mirjit_fplus},
    {"mirjit_fplusf", mirjit_fplusf},
    {"mirjit_fmod", mirjit_fmod},
    {"mirjit_fmodf", mirjit_fmodf},
    {"mirjit_frmodf", mirjit_frmodf},
#ifdef MIRJIT_RESTART_STAT
    {"mirjit_run_nums", &mirjit_run_nums},
    {"mirjit_restart_nums", &mirjit_restart_nums},
#endif
};

DEF_HTAB(resolver_el_t);
static HTAB(resolver_el_t) *resolver_tab;

static htab_hash_t resolver_hash(resolver_el_t el, void *arg) {
    return st_hash(el.name, strlen(el.name), 42);
}

static int resolver_eq_p(resolver_el_t el1, resolver_el_t el2, void *arg) {
    return strcmp(el1.name, el2.name) == 0;
}

static void resolver_init(void) {
    HTAB_CREATE(resolver_el_t, resolver_tab,
                2 * sizeof(resolver_els) / sizeof(resolver_el_t),
                resolver_hash, resolver_eq_p, NULL);
    for (size_t i = 0; i < sizeof(resolver_els) / sizeof(resolver_el_t); i++) {
        int MAYBE_UNUSED(exist_p);
        resolver_el_t el, MAYBE_UNUSED(tab_el);

        el = resolver_els[i];
        exist_p = HTAB_DO(resolver_el_t, resolver_tab, el, HTAB_INSERT, tab_el);
        assert(!exist_p);
    }
}

static void *import_resolver(const char *name) {
    resolver_el_t el, tab_el;

    el.name = name;
    if (HTAB_DO(resolver_el_t, resolver_tab, el, HTAB_FIND, tab_el))
        return tab_el.addr;
    assert(FALSE);
    return NULL;
}

static void resolver_finish(void) {
    HTAB_DESTROY(resolver_el_t, resolver_tab);
}

#define CONCAT(a, b) a ## b
#define BRANCH_CASES(name)      \
    case CONCAT(name, ss):	\
    case CONCAT(name, sv):	\
    case CONCAT(name, si):	\
    case CONCAT(name, vs):	\
    case CONCAT(name, vv):	\
    case CONCAT(name, vi)

static void
gen_bbv_code(VARR(char) *buff, struct rb_iseq_bbv *bbv) {
    int len, pc, next_pc, mirjit_debug_p = rb_mirjit_debug_p(), insns_num = 0, bbv_pc = -1;
    int finish_jit_code_p;
    rb_iseq_t *iseq = bbv->iseq;
    struct rb_iseq_constant_body *body = iseq->body;
    enum ruby_vminsn_type insn_id, next_insn_id;

    finish_jit_code_p = FALSE;
    assert (bbv->bbv_start < bbv->bbv_bound);
    for (pc = bbv->bbv_start, insn_id = rb_vm_insn_addr2insn((void *)body->bbv_code[pc]);
         pc < bbv->bbv_bound;
         pc = next_pc, insn_id = next_insn_id) {
        len = insn_len(insn_id);
        next_pc = pc + len;
        next_insn_id = next_pc < bbv->bbv_bound ? rb_vm_insn_addr2insn((void *)body->bbv_code[next_pc]) : 0;
        switch (insn_id) {
        case BIN(sir_bbv_cont):
            if (bbv_pc >= 0) { /* generate code for previous bbv part and start generation for this part */
                next_insn_id = insn_id;
                next_pc = pc;
                finish_jit_code_p = TRUE;
                break;
            }
            /* Fall through: */
        case BIN(sir_bbv_start):
            bbv_pc = pc;
            VARR_TRUNC(char, buff, 0);
            insns_num = 0;
            O("%s", header);
#if !defined(__x86_64__)
            O("VALUE *jit_bbv%d(void *ec, void *cfp, VALUE *pc) {\n", pc);
#else
            O("void jit_bbv%d(void) {\n", pc);
#endif
            O("  VALUE val, val2, r, t, flags, *pv, *pv2;\n");
            O("  VALUE *ep __mirc_attribute__ ((antialias (val))) = cfp->ep;\n");
            O("  VALUE *sp __mirc_attribute__ ((antialias (val))) = cfp->sp;\n");
#if defined(__x86_64__) && defined(JRET)
            /* skip sir_bbv_{start, cont} */
            O("  pc += %d;\n", insn_len(BIN(sir_bbv_start)));
#endif
#ifdef MIRJIT_RESTART_STAT
            O("  mirjit_run_nums++;\n");
#endif
            break;
        default:
            if (!gen_insn_code(buff, body->bbv_code + pc, insn_id)) {
                finish_jit_code_p = TRUE;
            } else {
                insns_num++;
                O("l%d:;\n", (int)(pc - bbv->bbv_start));
#if 0
                switch (insn_id) {
                BRANCH_CASES(BIN(sir_ibeq)):
                BRANCH_CASES(BIN(sir_ibneq)):
                BRANCH_CASES(BIN(sir_iblt)):
                BRANCH_CASES(BIN(sir_ibgt)):
                BRANCH_CASES(BIN(sir_ible)):
                BRANCH_CASES(BIN(sir_ibge)):
                case BIN(branch2):
                case BIN(branchnil2): {
                    OFFSET dst;
                    if (((dst = (OFFSET)body->bbv_code[pc + 1]) < 0 && pc + len + dst >= bbv_pc)
                        || ((dst = (OFFSET)body->bbv_code[pc + 2]) < 0 && pc + len + dst >= bbv_pc)) {
                        O("  if ((VALUE)pc == 0x%llx) goto l%d;\n",
                                   (long long unsigned)(body->bbv_code + pc + len + dst),
                                   (int)(pc + len + dst - bbv->bbv_start));
                    }
                    break;
                }
                default:
                    break;
                }
#endif
            }
            break;
        }
        if (!finish_jit_code_p) continue;
        if (bbv_pc >= 0 && insns_num > 0) {
#ifdef MIRJIT_RESTART_STAT
            O("  goto exit;\n");
#endif
            O(" restart:\n");
#ifdef MIRJIT_RESTART_STAT
            O("  mirjit_restart_nums++;\n");
#endif
            O(" exit:\n");
            O("  cfp->sp = sp;\n");
            O("  cfp->pc = pc;\n");
#if defined(__x86_64__)
#ifdef JRET
            O("  __builtin_jret (*(void **)pc);\n}\n");
#else
            O("  return;\n}\n");
#endif
#else
            O("  return pc;\n}\n");
#endif
            if (mirjit_debug_p) {
                fprintf(stderr, "C code for BBV 0x%llx @ %d:\n%s",
                        (unsigned long long)bbv, bbv_pc - bbv->bbv_start, VARR_ADDR(char, buff));
            }
            curr_input.code = (uint8_t *)VARR_ADDR(char, buff);
            curr_input.code_len = strlen((char *) curr_input.code);
            curr_input.curr_char = 0;
            double start_time = real_usec_time();
            if (!c2mir_compile (ctx, &curr_input.options, t_getc, &curr_input, "jit_c_code.c", NULL)) {
                if (mirjit_debug_p) {
                    fprintf(stderr, "Failure in compilation of C code for BBV 0x%llx @ %d\n",
                            (unsigned long long)bbv, bbv_pc - bbv->bbv_start);
                }
            } else {
                if (mirjit_debug_p) {
                    fprintf(stderr, "Finish C to MIR compilation -- compilation time %.0f usec\n",
                            real_usec_time() - start_time);
                    MIR_gen_set_debug_file(ctx, 0, stderr);
                    MIR_gen_set_debug_level(ctx, 0, 3);
                }
                MIR_module_t module = DLIST_TAIL(MIR_module_t, *MIR_get_module_list(ctx));
                MIR_load_module(ctx, module);
                MIR_link(ctx, MIR_set_lazy_gen_interface, import_resolver);
                MIR_item_t func = DLIST_TAIL(MIR_item_t, module->items);
                while(func != NULL && func->item_type != MIR_func_item)
                    func = DLIST_PREV(MIR_item_t, func);
                assert(func != NULL && func->item_type == MIR_func_item);
                void *thunk = MIR_gen(ctx, 0, func);
#ifdef JRET
                body->bbv_code[bbv_pc] = (VALUE) _MIR_get_thunk_addr(ctx, thunk);
#else
                body->bbv_code[bbv_pc] = (VALUE)sir_dispatch_table[BIN(sir_bbv_jit_call)];
                body->bbv_code[bbv_pc + 1] = (VALUE) _MIR_get_thunk_addr(ctx, thunk);
#endif
            }
        }
        bbv_pc = -1;
        finish_jit_code_p = FALSE;
    }
}

void
rb_jit_bbv(struct rb_iseq_bbv *bbv) {
    gen_bbv_code(jit_c_code, bbv);
}

static struct rb_mirjit_options mirjit_options;

int
rb_mirjit_enabled_p(void) { return mirjit_options.mirjit_enabled_p; }

int
rb_mirjit_debug_p(void) { return mirjit_options.debug_p; }

void
rb_mirjit_init(struct rb_mirjit_options *options) {
    mirjit_options = *options;
    ctx = MIR_init();
    VARR_CREATE(char, jit_c_code, 0);
    c2mir_init(ctx);
    MIR_gen_init(ctx, 1);
    curr_input.options.message_file = stderr;
    curr_input.options.output_file_name = NULL;
    curr_input.options.debug_p = curr_input.options.verbose_p = FALSE;
    curr_input.options.ignore_warnings_p = TRUE;
    curr_input.options.asm_p = curr_input.options.object_p = FALSE;
    curr_input.options.no_prepro_p = curr_input.options.prepro_only_p = FALSE;
    curr_input.options.syntax_only_p = curr_input.options.pedantic_p = FALSE;
    resolver_init();
}

void
rb_mirjit_finish(void) {
    if (ctx == NULL) return; /* not initialized */
#ifdef MIRJIT_RESTART_STAT
    fprintf(stderr, "MIRJIT CODE RUNS=%ld, RESTARTS=%ld\n", mirjit_run_nums, mirjit_restart_nums);
#endif
    resolver_finish();
    VARR_DESTROY(char, jit_c_code);
    MIR_gen_finish(ctx);
    c2mir_finish(ctx);
    MIR_finish(ctx);
}

#endif /* #ifdef HAVE_LIBMIR */
