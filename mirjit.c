/**********************************************************************

  mirjit.c - Code for generating C code for MIR from SIR

  Copyright (C) 2022 Vladimir Makarov vmakarov@redhat.com

**********************************************************************/

#include <float.h>
#include "sir.h"
#include "internal/compile.h"
#include "internal/class.h"
#include "internal/numeric.h"
#include "vm_callinfo.h"
#include "shape.h"
#define YJIT_STATS 0 /* used in vm_insnhelper.h */
#include "vm_insnhelper.h"

#include "builtin.h"
#include "insns.inc"
#include "insns_info.inc"

#include "mirjit.h"

#if !defined(HAVE_LIBMIR) || !defined(HAVE_PTHREAD_H)

int
rb_mirjit_enabled_p(void) { return FALSE; }

enum ruby_vminsn_type rb_mirjit_insn_decode(VALUE *slot) {
    return rb_vm_insn_addr2insn((void *)*slot);
}

void *rb_mirjit_get_pc_dispatch_addr(VALUE *pc) {
    return NULL;
}

void
rb_mirjit_mark_bbv_change(struct rb_iseq_bbv *bbv) {
}

void
rb_rejit_bbv(struct rb_iseq_bbv *bbv) {
}

#else

#include "mir.h"
#include "mir-gen.h"

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

static st_table *pc_dispatch_addr_map; /* pc to original dispatch insn addr before it was changed to jitted code */

void *rb_mirjit_get_pc_dispatch_addr(VALUE *pc) {
    st_data_t value;

    if (rb_st_lookup(pc_dispatch_addr_map, (st_data_t)pc, &value)) return (void *)value;
    return NULL;
}

void rb_mirjit_set_pc_dispatch_addr(VALUE *pc, void *addr) {
    st_insert(pc_dispatch_addr_map, (st_data_t)pc, (st_data_t)addr);
}

enum ruby_vminsn_type rb_mirjit_insn_decode(VALUE *slot) {
    void *addr;

    if ((addr = rb_mirjit_get_pc_dispatch_addr(slot)) == NULL) addr = (void *)*slot;
    return rb_vm_insn_addr2insn(addr);
}

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
#if SIZEOF_SHAPE_T == 4
#if SIZEOF_INT != 4
# error ---->> mirjit requires int of size 4
#endif
    "typedef unsigned int shape_id_t;\n"
#else
#if SIZEOF_SHORT != 2
# error ---->> mirjit requires short of size 2
#endif
    "typedef unsigned short shape_id_t;\n"
#endif
#if defined(_WIN32)
    "typdef long rb_atomic_t;\n"
#else
    "typedef unsigned rb_atomic_t;\n"
#endif
    "typedef struct rb_control_frame_struct {\n"
    "  VALUE *pc /*__mirc_attribute__ ((antialias (val)))*/;\n"
    "  VALUE *sp /*__mirc_attribute__ ((antialias (val)))*/;\n"
    "  void *iseq;\n"
    "  VALUE self;\n"
    "  VALUE *ep /*__mirc_attribute__ ((antialias (val)))*/;\n"
    "  void *block_code;\n"
    "  VALUE *__bp__;\n"
#if VM_DEBUG_BP_CHECK
    "  VALUE *bp_check;\n"
#endif
    "  void *jit_return;\n"
    "} rb_control_frame_t;\n"
#if defined(__x86_64__)
    "register void *ec asm(\"r13\");\n"
    "register VALUE *pc asm(\"r14\") /*__mirc_attribute__ ((antialias (val)))*/;\n"
    "register rb_control_frame_t *cfp asm(\"r15\");\n"
#endif
    "extern VALUE *rb_sir_get_jump_safe_pc(const rb_control_frame_t *cfp, const VALUE *pc);\n"
#ifdef FLONUM_INSNS
    "extern VALUE mirjit_fplus(VALUE, VALUE);\n"
    "extern VALUE mirjit_fplusf(VALUE, DBL);\n"
    "extern VALUE mirjit_frplusf(DBL, VALUE);\n"
    "extern VALUE mirjit_fminus(VALUE, VALUE);\n"
    "extern VALUE mirjit_fminusf(VALUE, DBL);\n"
    "extern VALUE mirjit_frminusf(DBL, VALUE);\n"
    "extern VALUE mirjit_fmult(VALUE, VALUE);\n"
    "extern VALUE mirjit_fmultf(VALUE, DBL);\n"
    "extern VALUE mirjit_frmultf(DBL, VALUE);\n"
    "extern VALUE mirjit_fdiv(VALUE, VALUE);\n"
    "extern VALUE mirjit_fdivf(VALUE, DBL);\n"
    "extern VALUE mirjit_frdivf(DBL, VALUE);\n"
    "extern VALUE mirjit_fmod(VALUE, VALUE);\n"
    "extern VALUE mirjit_fmodf(VALUE, DBL);\n"
    "extern VALUE mirjit_frmodf(DBL, VALUE);\n"
    "extern VALUE mirjit_feq(VALUE, VALUE);\n"
    "extern VALUE mirjit_feqf(VALUE, DBL);\n"
    "extern VALUE mirjit_freqf(DBL, VALUE);\n"
    "extern VALUE mirjit_fneq(VALUE, VALUE);\n"
    "extern VALUE mirjit_fneqf(VALUE, DBL);\n"
    "extern VALUE mirjit_frneqf(DBL, VALUE);\n"
    "extern VALUE mirjit_flt(VALUE, VALUE);\n"
    "extern VALUE mirjit_fltf(VALUE, DBL);\n"
    "extern VALUE mirjit_frltf(DBL, VALUE);\n"
    "extern VALUE mirjit_fgt(VALUE, VALUE);\n"
    "extern VALUE mirjit_fgtf(VALUE, DBL);\n"
    "extern VALUE mirjit_frgtf(DBL, VALUE);\n"
    "extern VALUE mirjit_fle(VALUE, VALUE);\n"
    "extern VALUE mirjit_flef(VALUE, DBL);\n"
    "extern VALUE mirjit_frlef(DBL, VALUE);\n"
    "extern VALUE mirjit_fge(VALUE, VALUE);\n"
    "extern VALUE mirjit_fgef(VALUE, DBL);\n"
    "extern VALUE mirjit_frgef(DBL, VALUE);\n"
#endif
    "extern void printf(const char *, ...);\n"
    "extern void *memcpy(void *, const void *, VALUE);\n"
#ifdef MIRJIT_STAT
    "extern long mirjit_run_nums, mirjit_restart_nums;\n"
    "extern long mirjit_profile[], mirjit_cancel[];\n"
    "extern int rb_vm_insn_addr2insn(const void *);\n"
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

#define TEMP_STR_SIZE 4096
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

static void gen_insn_stat(VARR(char) *buff, enum ruby_vminsn_type insn_id) {
#ifdef MIRJIT_STAT
    O("  mirjit_profile[%d]++;\n", (int)insn_id);
#endif
}

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

static void gen_special_const_p(VARR(char) *buff, const char *v) { /* RB_SPECIAL_CONST_P */
    O("((%s & 0x%x) || (%s & ~(VALUE)0x%x) == 0)", v, (int)RUBY_IMMEDIATE_MASK, v, (int)Qnil);
}

static void gen_float_guard(VARR(char) *buff, const char *v) {
    /* if (!FLONUM_P(v) && (RB_SPECIAL_CONST_P(v) || RBASIC_CLASS(v) != rb_rcFloat)) goro restart; */
    O("  if ((%s & 0x%x) != 0x%x && (", v, (int)RUBY_FLONUM_MASK, (int)RUBY_FLONUM_FLAG);
    gen_special_const_p(buff, v);
    O(" || *(VALUE *)((char *)%s + %d) != 0x%"PRIxVALUE ")) goto restart;\n",
      v, (int)offsetof(struct RBasic, klass), rb_cFloat);
}
static void gen_class_of(VARR(char) *buff, const char *klass, const char *recv) {
    O("    if (");
    gen_special_const_p(buff, recv);
    /* if (SPECIAL_CONST_P(recv)) goto restart; klass = RBASIC_CLASS(recv) */
    O(") goto restart;\n    %s = *(VALUE *)((char *)%s + %d);\n", klass, recv, (int)offsetof(struct RBasic, klass));
}

static void gen_write_barrier(VARR(char) *buff, const char *a, const char *b) {
    /* if (!RB_SPECIAL_CONST_P(b)) { rb_gc_writebarrier(a,b); goto exit; } */
    O("  if (!");
    gen_special_const_p(buff, b);
    O(") {\n    rb_gc_writebarrier(%s, %s);\n", a, b);
#if 0
    O("    goto exit;\n"); /* to keep better optimization of the fast path */
#endif
    O("  }\n");
}

static void gen_array_aref_len(VARR(char) *buff, const char *aref, const char *len, const char *ary) {
    O("  flags = *((VALUE *)((char *)%s + %d));\n", ary, (int)offsetof(struct RBasic, flags));
    O("  if (flags & 0x%"PRIxVALUE ") {\n", (VALUE)RARRAY_EMBED_FLAG);
    O("    %s = (flags & 0x%"PRIxVALUE ") >> 0x%"PRIxVALUE ";\n", len,
      (VALUE)RARRAY_EMBED_LEN_MASK, (VALUE)RARRAY_EMBED_LEN_SHIFT);
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
                           VALUE cme, VALUE type, VALUE iseq, const char *start_pc) {
    O("    void *new_sp = argv - 1;\n"); /* poping args including recv (sp before leave) */
    /* new_cfp = RUBY_VM_NEXT_CONTROL_FRAME(ec->cfp): */
    O("    void *new_cfp = (char *)cfp - %d;\n", (int)sizeof(rb_control_frame_t));
    /* CHECK_VM_STACK_OVERFLOW0(new_cfp, new_sp, local_size + stack_max): */
    gen_check_overflow0(buff, "new_cfp", "new_sp", local_size + stack_max);
    if (send_insn_len != 0) O("    pc += %d;\n", send_insn_len);
    O("    cfp->pc = pc;\n");
    O("    cfp->sp = new_sp;\n"); /* cfp->sp */
    O("    cfp = new_cfp; pc = %s;\n", start_pc);
    if (local_size <= 6) {
        for (int i = 0; i < local_size; i++)
            O("    *sp++ = 0x%x;\n", (int)Qnil);
    } else {
        O("    for (int i = 0; i < %d; i++) *sp++ = 0x%x;\n", local_size, (int)Qnil);
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

static void gen_obj_shape(VARR(char) *buff, const char *shape, const char *obj){
    O("  %s = (shape_id_t)((*((VALUE *)((char *)%s + %d)) >> 0x%x) & 0x%lx);\n",
      shape, obj, (int)offsetof(struct RBasic, flags), (int)SHAPE_FLAG_SHIFT, (unsigned long)SHAPE_MASK);
}

static int
gen_insn_code(VARR(char) *buff, const VALUE *insn_pc, enum ruby_vminsn_type insn_id) {
    const VALUE *operands = &insn_pc[1];
    const char *op, *fn;
    int level, rv_p, v1_p, v2_p, imm1_p, imm2_p, fop_p, check_p, siaset_p, nil_p, start, stack_ops_num;

    fop_p = check_p = siaset_p = FALSE;
    switch (insn_id) {
    case BIN(nop):
    case BIN(nop2):
        gen_insn_stat(buff, insn_id);
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    case BIN(pop):
        gen_insn_stat(buff, insn_id);
        O("  *sp--;\n");
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    case BIN(dup):
        gen_insn_stat(buff, insn_id);
        O("  val = sp[-1];\n");
        O("  *sp++ = val;\n");
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    case BIN(dupn): {
        gen_insn_stat(buff, insn_id);
        rb_num_t n = (rb_num_t)operands[0];
        O("  pv = &sp[-%ld];\n", (long)n);
        O("  pv2 = sp; sp += %ld;\n", (long)n);
        O("  memcpy(pv2, pv, %ld);\n", (long)(sizeof (VALUE) * n));
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    }
    case BIN(swap):
        gen_insn_stat(buff, insn_id);
        O("  val = sp[-1];\n");
        O("  sp[-1] = sp[-2];\n");
        O("  sp[-2] = val;\n");
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    case BIN(topn): {
        gen_insn_stat(buff, insn_id);
        rb_num_t n = (rb_num_t)operands[0];
        O("  val = sp[-%ld];\n", (long)n + 1);
        O("  *sp++ = val;\n");
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    }
    case BIN(setn): {
        gen_insn_stat(buff, insn_id);
        rb_num_t n = (rb_num_t)operands[0];
        O("  val = sp[- 1];\n");
        O("  sp[-%ld] = val;\n", (long)n + 1);
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    }
    case BIN(adjuststack): {
        gen_insn_stat(buff, insn_id);
        rb_num_t n = (rb_num_t)operands[0];
        O("  sp -= %ld;\n", (long)n);
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    }
    case BIN(putnil):
        gen_insn_stat(buff, insn_id);
        O("  *sp++ = (VALUE)0x%x;\n", (int)Qnil);
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    case BIN(putself):
        gen_insn_stat(buff, insn_id);
        O("  *sp++ = cfp->self;\n");
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    case BIN(putobject):
        gen_insn_stat(buff, insn_id);
        O("  *sp++ = 0x%"PRIxVALUE ";\n", operands[0]);
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    case BIN(putobject_INT2FIX_0_):
        gen_insn_stat(buff, insn_id);
        O("  *sp++ = 1;\n");
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    case BIN(putobject_INT2FIX_1_):
        gen_insn_stat(buff, insn_id);
        O("  *sp++ = 3;\n");
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    case BIN(getlocal_WC_1):
        level = 1;
        goto getlocal;
    case BIN(getlocal):
        level = (int)(rb_num_t)operands[1];
    getlocal:
        if (level != 0) {
            gen_insn_stat(buff, insn_id);
            O("  {\n");
            O("    VALUE *vp = ep;\n");
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
        /* fall through */
    case BIN(getlocal_WC_0):
        gen_insn_stat(buff, insn_id);
        O("  val = ep[-%d];\n", (int)(lindex_t)operands[0]);
        O("  *sp++ = val;\n");
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    case BIN(setlocal_WC_1):
        level = 1;
        goto setlocal;
    case BIN(setlocal):
        level = (int)(rb_num_t)operands[1];
    setlocal:
        if (level != 0) {
            gen_insn_stat(buff, insn_id);
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
        /* fall through */
    case BIN(setlocal_WC_0):
        gen_insn_stat(buff, insn_id);
        O("  flags = *ep;\n");
        O("  if (__builtin_expect((flags & 0x%lx) != 0, 0)) goto restart;\n", (long)VM_ENV_FLAG_WB_REQUIRED);
        O("  val = *--sp;\n");
        O("  ep[-%d] = val;\n", (int)(lindex_t)operands[0]);
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    case BIN(sir_mov):
        gen_insn_stat(buff, insn_id);
        O("  flags = *ep;\n");
        O("  if (__builtin_expect((flags & 0x%lx) != 0, 0)) goto restart;\n", (long)VM_ENV_FLAG_WB_REQUIRED);
        O("  val = ep[%d];\n", (int)(EPDISP)operands[1]);
        O("  ep[%d] = val;\n", (int)(EPDISP)operands[0]);
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    case BIN(sir_movi):
        gen_insn_stat(buff, insn_id);
        O("  flags = *ep;\n");
        O("  if (__builtin_expect((flags & 0x%lx) != 0, 0)) goto restart;\n", (long)VM_ENV_FLAG_WB_REQUIRED);
        O("  ep[%d] = 0x%"PRIxVALUE ";\n", (int)(EPDISP)operands[0], operands[1]);
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    case BIN(sir_setlocal): /* setlocal w/o write barrier check */
        gen_insn_stat(buff, insn_id);
        O("  val = *--sp;\n");
        O("  ep[%d] = val;\n", (int)(EPDISP)operands[0]);
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    case BIN(sir_sfminussvv):
    case BIN(sir_sfplussvv):
    case BIN(sir_sfmultsvv):
    case BIN(sir_sfdivsvv):
    case BIN(sir_sfmodsvv):
    case BIN(sir_sfeqsvv):
    case BIN(sir_sfneqsvv):
    case BIN(sir_sfltsvv):
    case BIN(sir_sfgtsvv):
    case BIN(sir_sflesvv):
    case BIN(sir_sfgesvv):
        check_p = TRUE;
        /* fall through */
    case BIN(sir_fminussvv):
    case BIN(sir_fplussvv):
    case BIN(sir_fmultsvv):
    case BIN(sir_fdivsvv):
    case BIN(sir_fmodsvv):
    case BIN(sir_feqsvv):
    case BIN(sir_fneqsvv):
    case BIN(sir_fltsvv):
    case BIN(sir_fgtsvv):
    case BIN(sir_flesvv):
    case BIN(sir_fgesvv):
        fop_p = TRUE;
        goto svv;
    case BIN(sir_siminussvv):
    case BIN(sir_siplussvv):
    case BIN(sir_simultsvv):
    case BIN(sir_sidivsvv):
    case BIN(sir_simodsvv):
    case BIN(sir_siorsvv):
    case BIN(sir_siandsvv):
    case BIN(sir_sieqsvv):
    case BIN(sir_sineqsvv):
    case BIN(sir_siltsvv):
    case BIN(sir_sigtsvv):
    case BIN(sir_silesvv):
    case BIN(sir_sigesvv):
        check_p = TRUE;
        /* fall through */
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
    case BIN(sir_siarefsvv):
    svv:
        rv_p = imm1_p = imm2_p = FALSE;
        v1_p = v2_p = TRUE;
        goto op2;
    case BIN(sir_sfminussvi):
    case BIN(sir_sfplussvi):
    case BIN(sir_sfmultsvi):
    case BIN(sir_sfdivsvi):
    case BIN(sir_sfmodsvi):
    case BIN(sir_sfeqsvi):
    case BIN(sir_sfneqsvi):
    case BIN(sir_sfltsvi):
    case BIN(sir_sfgtsvi):
    case BIN(sir_sflesvi):
    case BIN(sir_sfgesvi):
        check_p = TRUE;
        /* fall through */
    case BIN(sir_fminussvi):
    case BIN(sir_fplussvi):
    case BIN(sir_fmultsvi):
    case BIN(sir_fdivsvi):
    case BIN(sir_fmodsvi):
    case BIN(sir_feqsvi):
    case BIN(sir_fneqsvi):
    case BIN(sir_fltsvi):
    case BIN(sir_fgtsvi):
    case BIN(sir_flesvi):
    case BIN(sir_fgesvi):
        fop_p = TRUE;
        goto svi;
    case BIN(sir_siminussvi):
    case BIN(sir_siplussvi):
    case BIN(sir_simultsvi):
    case BIN(sir_sidivsvi):
    case BIN(sir_simodsvi):
    case BIN(sir_siorsvi):
    case BIN(sir_siandsvi):
    case BIN(sir_sieqsvi):
    case BIN(sir_sineqsvi):
    case BIN(sir_siltsvi):
    case BIN(sir_sigtsvi):
    case BIN(sir_silesvi):
    case BIN(sir_sigesvi):
        check_p = TRUE;
        /* fall through */
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
    case BIN(sir_siarefsvi):
    svi:
        rv_p = v2_p = imm1_p = FALSE;
        v1_p = imm2_p = TRUE;
        goto op2;
    case BIN(sir_sfminussiv):
    case BIN(sir_sfplussiv):
    case BIN(sir_sfmultsiv):
    case BIN(sir_sfdivsiv):
    case BIN(sir_sfmodsiv):
    case BIN(sir_sfeqsiv):
    case BIN(sir_sfneqsiv):
    case BIN(sir_sfltsiv):
    case BIN(sir_sfgtsiv):
    case BIN(sir_sflesiv):
    case BIN(sir_sfgesiv):
        check_p = TRUE;
        /* fall through */
    case BIN(sir_fminussiv):
    case BIN(sir_fplussiv):
    case BIN(sir_fmultsiv):
    case BIN(sir_fdivsiv):
    case BIN(sir_fmodsiv):
    case BIN(sir_feqsiv):
    case BIN(sir_fneqsiv):
    case BIN(sir_fltsiv):
    case BIN(sir_fgtsiv):
    case BIN(sir_flesiv):
    case BIN(sir_fgesiv):
        fop_p = TRUE;
        goto siv;
    case BIN(sir_siminussiv):
    case BIN(sir_siplussiv):
    case BIN(sir_simultsiv):
    case BIN(sir_sidivsiv):
    case BIN(sir_simodsiv):
    case BIN(sir_siorsiv):
    case BIN(sir_siandsiv):
    case BIN(sir_sieqsiv):
    case BIN(sir_sineqsiv):
    case BIN(sir_siltsiv):
    case BIN(sir_sigtsiv):
    case BIN(sir_silesiv):
    case BIN(sir_sigesiv):
        check_p = TRUE;
        /* fall through */
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
    case BIN(sir_siarefsiv):
    siv:
        rv_p = v1_p = imm2_p = FALSE;
        v2_p = imm1_p = TRUE;
        goto op2;
    case BIN(sir_sfminussvs):
    case BIN(sir_sfplussvs):
    case BIN(sir_sfmultsvs):
    case BIN(sir_sfdivsvs):
    case BIN(sir_sfmodsvs):
    case BIN(sir_sfeqsvs):
    case BIN(sir_sfneqsvs):
    case BIN(sir_sfltsvs):
    case BIN(sir_sfgtsvs):
    case BIN(sir_sflesvs):
    case BIN(sir_sfgesvs):
        check_p = TRUE;
        /* fall through */
    case BIN(sir_fminussvs):
    case BIN(sir_fplussvs):
    case BIN(sir_fmultsvs):
    case BIN(sir_fdivsvs):
    case BIN(sir_fmodsvs):
    case BIN(sir_feqsvs):
    case BIN(sir_fneqsvs):
    case BIN(sir_fltsvs):
    case BIN(sir_fgtsvs):
    case BIN(sir_flesvs):
    case BIN(sir_fgesvs):
        fop_p = TRUE;
        goto svs;
    case BIN(sir_siminussvs):
    case BIN(sir_siplussvs):
    case BIN(sir_simultsvs):
    case BIN(sir_sidivsvs):
    case BIN(sir_simodsvs):
    case BIN(sir_siorsvs):
    case BIN(sir_siandsvs):
    case BIN(sir_sieqsvs):
    case BIN(sir_sineqsvs):
    case BIN(sir_siltsvs):
    case BIN(sir_sigtsvs):
    case BIN(sir_silesvs):
    case BIN(sir_sigesvs):
        check_p = TRUE;
        /* fall through */
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
    case BIN(sir_siarefsvs):
    svs:
        rv_p = v2_p = imm1_p = imm2_p = FALSE;
        v1_p = TRUE;
        goto op2;
    case BIN(sir_sfminusssv):
    case BIN(sir_sfplusssv):
    case BIN(sir_sfmultssv):
    case BIN(sir_sfdivssv):
    case BIN(sir_sfmodssv):
    case BIN(sir_sfeqssv):
    case BIN(sir_sfneqssv):
    case BIN(sir_sfltssv):
    case BIN(sir_sfgtssv):
    case BIN(sir_sflessv):
    case BIN(sir_sfgessv):
        check_p = TRUE;
        /* fall through */
    case BIN(sir_fminusssv):
    case BIN(sir_fplusssv):
    case BIN(sir_fmultssv):
    case BIN(sir_fdivssv):
    case BIN(sir_fmodssv):
    case BIN(sir_feqssv):
    case BIN(sir_fneqssv):
    case BIN(sir_fltssv):
    case BIN(sir_fgtssv):
    case BIN(sir_flessv):
    case BIN(sir_fgessv):
        fop_p = TRUE;
        goto ssv;
    case BIN(sir_siminusssv):
    case BIN(sir_siplusssv):
    case BIN(sir_simultssv):
    case BIN(sir_sidivssv):
    case BIN(sir_simodssv):
    case BIN(sir_siorssv):
    case BIN(sir_siandssv):
    case BIN(sir_sieqssv):
    case BIN(sir_sineqssv):
    case BIN(sir_siltssv):
    case BIN(sir_sigtssv):
    case BIN(sir_silessv):
    case BIN(sir_sigessv):
        check_p = TRUE;
        /* fall through */
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
    case BIN(sir_siarefssv):
    ssv:
        rv_p = v1_p = imm1_p = imm2_p = FALSE;
        v2_p = TRUE;
        goto op2;
    case BIN(sir_sfminusssi):
    case BIN(sir_sfplusssi):
    case BIN(sir_sfmultssi):
    case BIN(sir_sfdivssi):
    case BIN(sir_sfmodssi):
    case BIN(sir_sfeqssi):
    case BIN(sir_sfneqssi):
    case BIN(sir_sfltssi):
    case BIN(sir_sfgtssi):
    case BIN(sir_sflessi):
    case BIN(sir_sfgessi):
        check_p = TRUE;
        /* fall through */
    case BIN(sir_fminusssi):
    case BIN(sir_fplusssi):
    case BIN(sir_fmultssi):
    case BIN(sir_fdivssi):
    case BIN(sir_fmodssi):
    case BIN(sir_feqssi):
    case BIN(sir_fneqssi):
    case BIN(sir_fltssi):
    case BIN(sir_fgtssi):
    case BIN(sir_flessi):
    case BIN(sir_fgessi):
        fop_p = TRUE;
        goto ssi;
    case BIN(sir_siminusssi):
    case BIN(sir_siplusssi):
    case BIN(sir_simultssi):
    case BIN(sir_sidivssi):
    case BIN(sir_simodssi):
    case BIN(sir_siorssi):
    case BIN(sir_siandssi):
    case BIN(sir_sieqssi):
    case BIN(sir_sineqssi):
    case BIN(sir_siltssi):
    case BIN(sir_sigtssi):
    case BIN(sir_silessi):
    case BIN(sir_sigessi):
        check_p = TRUE;
        /* fall through */
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
    case BIN(sir_siarefssi):
    ssi:
        rv_p = v1_p = v2_p = imm1_p = FALSE;
        imm2_p = TRUE;
        goto op2;
    case BIN(sir_sfminussis):
    case BIN(sir_sfplussis):
    case BIN(sir_sfmultsis):
    case BIN(sir_sfdivsis):
    case BIN(sir_sfmodsis):
    case BIN(sir_sfeqsis):
    case BIN(sir_sfneqsis):
    case BIN(sir_sfltsis):
    case BIN(sir_sfgtsis):
    case BIN(sir_sflesis):
    case BIN(sir_sfgesis):
        check_p = TRUE;
        /* fall through */
    case BIN(sir_fminussis):
    case BIN(sir_fplussis):
    case BIN(sir_fmultsis):
    case BIN(sir_fdivsis):
    case BIN(sir_fmodsis):
    case BIN(sir_feqsis):
    case BIN(sir_fneqsis):
    case BIN(sir_fltsis):
    case BIN(sir_fgtsis):
    case BIN(sir_flesis):
    case BIN(sir_fgesis):
        fop_p = TRUE;
        goto sis;
    case BIN(sir_siminussis):
    case BIN(sir_siplussis):
    case BIN(sir_simultsis):
    case BIN(sir_sidivsis):
    case BIN(sir_simodsis):
    case BIN(sir_siorsis):
    case BIN(sir_siandsis):
    case BIN(sir_sieqsis):
    case BIN(sir_sineqsis):
    case BIN(sir_siltsis):
    case BIN(sir_sigtsis):
    case BIN(sir_silesis):
    case BIN(sir_sigesis):
        check_p = TRUE;
        /* fall through */
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
    case BIN(sir_siarefsis):
    sis:
        rv_p = v1_p = v2_p = imm2_p = FALSE;
        imm1_p = TRUE;
        goto op2;
    case BIN(sir_sfminussss):
    case BIN(sir_sfplussss):
    case BIN(sir_sfmultsss):
    case BIN(sir_sfdivsss):
    case BIN(sir_sfmodsss):
    case BIN(sir_sfeqsss):
    case BIN(sir_sfneqsss):
    case BIN(sir_sfltsss):
    case BIN(sir_sfgtsss):
    case BIN(sir_sflesss):
    case BIN(sir_sfgesss):
        check_p = TRUE;
        /* fall through */
    case BIN(sir_fminussss):
    case BIN(sir_fplussss):
    case BIN(sir_fmultsss):
    case BIN(sir_fdivsss):
    case BIN(sir_fmodsss):
    case BIN(sir_feqsss):
    case BIN(sir_fneqsss):
    case BIN(sir_fltsss):
    case BIN(sir_fgtsss):
    case BIN(sir_flesss):
    case BIN(sir_fgesss):
        fop_p = TRUE;
        goto sss;
    case BIN(sir_siminussss):
    case BIN(sir_siplussss):
    case BIN(sir_simultsss):
    case BIN(sir_sidivsss):
    case BIN(sir_simodsss):
    case BIN(sir_siorsss):
    case BIN(sir_siandsss):
    case BIN(sir_sieqsss):
    case BIN(sir_sineqsss):
    case BIN(sir_siltsss):
    case BIN(sir_sigtsss):
    case BIN(sir_silesss):
    case BIN(sir_sigesss):
        check_p = TRUE;
        /* fall through */
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
    case BIN(sir_siarefsss):
    sss:
        rv_p = v1_p = v2_p = imm1_p = imm2_p = FALSE;
        goto op2;
    case BIN(sir_feqvvv):
    case BIN(sir_fneqvvv):
    case BIN(sir_fltvvv):
    case BIN(sir_fgtvvv):
    case BIN(sir_flevvv):
    case BIN(sir_fgevvv):
        fop_p = TRUE;
        /* fall through */
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
        rv_p = v1_p = v2_p = TRUE;
        imm1_p = imm2_p = FALSE;
        goto op2;
    case BIN(sir_feqvvi):
    case BIN(sir_fneqvvi):
    case BIN(sir_fltvvi):
    case BIN(sir_fgtvvi):
    case BIN(sir_flevvi):
    case BIN(sir_fgevvi):
        fop_p = TRUE;
        /* fall through */
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
        rv_p = v1_p = imm2_p = TRUE;
        v2_p = imm1_p = FALSE;
        goto op2;
    case BIN(sir_feqviv):
    case BIN(sir_fneqviv):
    case BIN(sir_fltviv):
    case BIN(sir_fgtviv):
    case BIN(sir_fleviv):
    case BIN(sir_fgeviv):
        fop_p = TRUE;
        /* fall through */
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
        rv_p = v2_p = imm1_p = TRUE;
        v1_p = imm2_p = FALSE;
        goto op2;
    case BIN(sir_feqvvs):
    case BIN(sir_fneqvvs):
    case BIN(sir_fltvvs):
    case BIN(sir_fgtvvs):
    case BIN(sir_flevvs):
    case BIN(sir_fgevvs):
        fop_p = TRUE;
        /* fall through */
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
        rv_p = v1_p = TRUE;
        v2_p = imm1_p = imm2_p = FALSE;
        goto op2;
    case BIN(sir_feqvsv):
    case BIN(sir_fneqvsv):
    case BIN(sir_fltvsv):
    case BIN(sir_fgtvsv):
    case BIN(sir_flevsv):
    case BIN(sir_fgevsv):
        fop_p = TRUE;
        /* fall through */
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
        rv_p = v2_p = TRUE;
        v1_p = imm1_p = imm2_p = FALSE;
        goto op2;
    case BIN(sir_feqvsi):
    case BIN(sir_fneqvsi):
    case BIN(sir_fltvsi):
    case BIN(sir_fgtvsi):
    case BIN(sir_flevsi):
    case BIN(sir_fgevsi):
        fop_p = TRUE;
        /* fall through */
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
        rv_p = imm2_p = TRUE;
        v1_p = v2_p = imm1_p = FALSE;
        goto op2;
    case BIN(sir_feqvis):
    case BIN(sir_fneqvis):
    case BIN(sir_fltvis):
    case BIN(sir_fgtvis):
    case BIN(sir_flevis):
    case BIN(sir_fgevis):
        fop_p = TRUE;
        /* fall through */
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
        rv_p = imm1_p = TRUE;
        v1_p = v2_p = imm2_p = FALSE;
        goto op2;
    case BIN(sir_feqvss):
    case BIN(sir_fneqvss):
    case BIN(sir_fltvss):
    case BIN(sir_fgtvss):
    case BIN(sir_flevss):
    case BIN(sir_fgevss):
        fop_p = TRUE;
        /* fall through */
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
        rv_p = TRUE;
        v1_p = v2_p = imm1_p = imm2_p = FALSE;
    op2:
        gen_insn_stat(buff, insn_id);
        start = rv_p ? 1 : 0;
        stack_ops_num = 0;
        if (v2_p) {
            O("  val2 = ep[%d];\n", (int)(EPDISP)operands[v1_p || imm1_p ? start + 1 : start]);
        } else if (imm2_p) {
            if (fop_p)
                O("  d = %.*e;\n", DECIMAL_DIG, ((double *)operands)[v1_p || imm1_p ? start + 1 : start]);
            else
                O("  val2 = 0x%"PRIxVALUE ";\n", operands[v1_p || imm1_p ? start + 1 : start]);
        } else {
            O("  val2 = sp[-1];\n");
            stack_ops_num++;
        }
        if (v1_p) {
            O("  val = ep[%d];\n", (int)(EPDISP)operands[start]);
        } else if (imm1_p) {
            if (fop_p)
                O("  d = %.*e;\n", DECIMAL_DIG, ((double *)operands)[start]);
            else
                O("  val = 0x%"PRIxVALUE ";\n", operands[start]);
        } else {
            O("  val = sp[-%d];\n", v2_p || imm2_p ? 1 : 2);
            stack_ops_num++;
        }
        if (check_p) {
            if (fop_p) {
                if (!imm1_p) gen_float_guard(buff, "val");
                if (!imm2_p) gen_float_guard(buff, "val2");
            } else if (imm1_p) {
                O("  if (!(val2 & 0x%x)) goto restart;\n", (int)RUBY_FIXNUM_FLAG);
            } else if (imm2_p) {
                O("  if (!(val & 0x%x)) goto restart;\n", (int)RUBY_FIXNUM_FLAG);
            } else {
                O("  if (!(val2 & val & 0x%x)) goto restart;\n", (int)RUBY_FIXNUM_FLAG);
            }
        }
        switch (insn_id) {
        case BIN(sir_iplussss):
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
        case BIN(sir_siplussss):
        case BIN(sir_siplussvv):
        case BIN(sir_siplussvi):
        case BIN(sir_siplussiv):
        case BIN(sir_siplussvs):
        case BIN(sir_siplusssv):
        case BIN(sir_siplusssi):
        case BIN(sir_siplussis):
            O("  if (__builtin_expect (__builtin_add_overflow((long)val, (long)val2-1, &r), 0)) goto restart;\n");
            break;
        case BIN(sir_iminussss):
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
        case BIN(sir_siminussss):
        case BIN(sir_siminussvv):
        case BIN(sir_siminussvi):
        case BIN(sir_siminussiv):
        case BIN(sir_siminussvs):
        case BIN(sir_siminusssv):
        case BIN(sir_siminusssi):
        case BIN(sir_siminussis):
            O("  if (__builtin_expect (__builtin_sub_overflow((long)val, (long)val2-1, &r), 0)) goto restart;\n");
            break;
        case BIN(sir_imultsss):
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
        case BIN(sir_simultsss):
        case BIN(sir_simultsvv):
        case BIN(sir_simultsvi):
        case BIN(sir_simultsiv):
        case BIN(sir_simultsvs):
        case BIN(sir_simultssv):
        case BIN(sir_simultssi):
        case BIN(sir_simultsis):
            O("  {\n");
            O("     SIGNED_VALUE mul, x = (SIGNED_VALUE)val >> 1, y = (SIGNED_VALUE)val2 >> 1;\n");
            O("     if (__builtin_mul_overflow(x, y, &mul) || (r = (VALUE)mul << 1) >> 1 != (VALUE)mul) goto restart;\n");
            O("     r++;\n");
            O("  }\n");
            break;
        case BIN(sir_idivsss):
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
        case BIN(sir_sidivsss):
        case BIN(sir_sidivsvv):
        case BIN(sir_sidivsvi):
        case BIN(sir_sidivsiv):
        case BIN(sir_sidivsvs):
        case BIN(sir_sidivssv):
        case BIN(sir_sidivssi):
        case BIN(sir_sidivsis):
            O("  {\n");
            O("     SIGNED_VALUE div, mod, x = (SIGNED_VALUE)val >> 1, y = (SIGNED_VALUE)val2 >> 1;\n");
            O("     if (x == %lld && y == -1) goto restart;\n", (long long)FIXNUM_MIN);
            O("     div = x / y; mod = x %% y;\n");
            O("     if (y > 0 ? mod < 0 : mod > 0) div -= 1;\n");
            O("     r = ((VALUE)div << 1) + 1;\n");
            O("  }\n");
            break;
        case BIN(sir_imodsss):
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
        case BIN(sir_simodsss):
        case BIN(sir_simodsvv):
        case BIN(sir_simodsvi):
        case BIN(sir_simodsiv):
        case BIN(sir_simodsvs):
        case BIN(sir_simodssv):
        case BIN(sir_simodssi):
        case BIN(sir_simodsis):
            O("  {\n");
            O("     SIGNED_VALUE mod, x = (SIGNED_VALUE)val >> 1, y = (SIGNED_VALUE)val2 >> 1;\n");
            O("     if (x == %lld && y == -1) {\n", (long long)FIXNUM_MIN);
            O("       r = 1;\n");
            O("     } else {\n");
            O("       mod = x %% y;\n");
            O("       if (y > 0 ? mod < 0 : mod > 0) mod += y;\n");
            O("       r = ((VALUE)mod << 1) + 1;\n");
            O("     }\n");
            O("  }\n");
            break;
        case BIN(sir_iorsss):
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
        case BIN(sir_siorsss):
        case BIN(sir_siorsvv):
        case BIN(sir_siorsvi):
        case BIN(sir_siorsiv):
        case BIN(sir_siorsvs):
        case BIN(sir_siorssv):
        case BIN(sir_siorssi):
        case BIN(sir_siorsis):
            O("  r = val | val2;\n");
            break;
        case BIN(sir_iandsss):
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
        case BIN(sir_siandsss):
        case BIN(sir_siandsvv):
        case BIN(sir_siandsvi):
        case BIN(sir_siandsiv):
        case BIN(sir_siandsvs):
        case BIN(sir_siandssv):
        case BIN(sir_siandssi):
        case BIN(sir_siandsis):
            O("  r = (val & val2) | 1;\n");
            break;
        case BIN(sir_ieqsss):
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
        case BIN(sir_sieqsss):
        case BIN(sir_sieqsvv):
        case BIN(sir_sieqsvi):
        case BIN(sir_sieqsiv):
        case BIN(sir_sieqsvs):
        case BIN(sir_sieqssv):
        case BIN(sir_sieqssi):
        case BIN(sir_sieqsis):
            O("  r = val == val2 ? 0x%x : 0x%x;\n", (int)Qtrue, (int)Qfalse);
            break;
        case BIN(sir_ineqsss):
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
        case BIN(sir_sineqsss):
        case BIN(sir_sineqsvv):
        case BIN(sir_sineqsvi):
        case BIN(sir_sineqsiv):
        case BIN(sir_sineqsvs):
        case BIN(sir_sineqssv):
        case BIN(sir_sineqssi):
        case BIN(sir_sineqsis):
            O("  r = val != val2 ? 0x%x : 0x%x;\n", (int)Qtrue, (int)Qfalse);
            break;
        case BIN(sir_iltsss):
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
        case BIN(sir_siltsss):
        case BIN(sir_siltsvv):
        case BIN(sir_siltsvi):
        case BIN(sir_siltsiv):
        case BIN(sir_siltsvs):
        case BIN(sir_siltssv):
        case BIN(sir_siltssi):
        case BIN(sir_siltsis):
            O("  r = (SIGNED_VALUE)val < (SIGNED_VALUE)val2 ? 0x%x : 0x%x;\n", (int)Qtrue, (int)Qfalse);
            break;
        case BIN(sir_igtsss):
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
        case BIN(sir_sigtsss):
        case BIN(sir_sigtsvv):
        case BIN(sir_sigtsvi):
        case BIN(sir_sigtsiv):
        case BIN(sir_sigtsvs):
        case BIN(sir_sigtssv):
        case BIN(sir_sigtssi):
        case BIN(sir_sigtsis):
            O("  r = (SIGNED_VALUE)val > (SIGNED_VALUE)val2 ? 0x%x : 0x%x;\n", (int)Qtrue, (int)Qfalse);
            break;
        case BIN(sir_ilesss):
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
        case BIN(sir_silesss):
        case BIN(sir_silesvv):
        case BIN(sir_silesvi):
        case BIN(sir_silesiv):
        case BIN(sir_silesvs):
        case BIN(sir_silessv):
        case BIN(sir_silessi):
        case BIN(sir_silesis):
            O("  r = (SIGNED_VALUE)val <= (SIGNED_VALUE)val2 ? 0x%x : 0x%x;\n", (int)Qtrue, (int)Qfalse);
            break;
        case BIN(sir_igesss):
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
        case BIN(sir_sigesss):
        case BIN(sir_sigesvv):
        case BIN(sir_sigesvi):
        case BIN(sir_sigesiv):
        case BIN(sir_sigesvs):
        case BIN(sir_sigessv):
        case BIN(sir_sigessi):
        case BIN(sir_sigesis):
            O("  r = (SIGNED_VALUE)val >= (SIGNED_VALUE)val2 ? 0x%x : 0x%x;\n", (int)Qtrue, (int)Qfalse);
            break;
#ifdef FLONUM_INSNS
        case BIN(sir_fplussss):
        case BIN(sir_fplussvv):
        case BIN(sir_fplussvi):
        case BIN(sir_fplussiv):
        case BIN(sir_fplussvs):
        case BIN(sir_fplusssv):
        case BIN(sir_fplusssi):
        case BIN(sir_fplussis):
        case BIN(sir_sfplussss):
        case BIN(sir_sfplussvv):
        case BIN(sir_sfplussvi):
        case BIN(sir_sfplussiv):
        case BIN(sir_sfplussvs):
        case BIN(sir_sfplusssv):
        case BIN(sir_sfplusssi):
        case BIN(sir_sfplussis):
            fn = "plus";
            goto fop;
        case BIN(sir_fminussss):
        case BIN(sir_fminussvv):
        case BIN(sir_fminussvi):
        case BIN(sir_fminussiv):
        case BIN(sir_fminussvs):
        case BIN(sir_fminusssv):
        case BIN(sir_fminusssi):
        case BIN(sir_fminussis):
        case BIN(sir_sfminussss):
        case BIN(sir_sfminussvv):
        case BIN(sir_sfminussvi):
        case BIN(sir_sfminussiv):
        case BIN(sir_sfminussvs):
        case BIN(sir_sfminusssv):
        case BIN(sir_sfminusssi):
        case BIN(sir_sfminussis):
            fn = "minus";
            goto fop;
        case BIN(sir_fmultsss):
        case BIN(sir_fmultsvv):
        case BIN(sir_fmultsvi):
        case BIN(sir_fmultsiv):
        case BIN(sir_fmultsvs):
        case BIN(sir_fmultssv):
        case BIN(sir_fmultssi):
        case BIN(sir_fmultsis):
        case BIN(sir_sfmultsss):
        case BIN(sir_sfmultsvv):
        case BIN(sir_sfmultsvi):
        case BIN(sir_sfmultsiv):
        case BIN(sir_sfmultsvs):
        case BIN(sir_sfmultssv):
        case BIN(sir_sfmultssi):
        case BIN(sir_sfmultsis):
            fn = "mult";
            goto fop;
        case BIN(sir_fdivsss):
        case BIN(sir_fdivsvv):
        case BIN(sir_fdivsvi):
        case BIN(sir_fdivsiv):
        case BIN(sir_fdivsvs):
        case BIN(sir_fdivssv):
        case BIN(sir_fdivssi):
        case BIN(sir_fdivsis):
        case BIN(sir_sfdivsss):
        case BIN(sir_sfdivsvv):
        case BIN(sir_sfdivsvi):
        case BIN(sir_sfdivsiv):
        case BIN(sir_sfdivsvs):
        case BIN(sir_sfdivssv):
        case BIN(sir_sfdivssi):
        case BIN(sir_sfdivsis):
            fn = "div";
            goto fop;
        case BIN(sir_fmodsss):
        case BIN(sir_fmodsvv):
        case BIN(sir_fmodsvi):
        case BIN(sir_fmodsiv):
        case BIN(sir_fmodsvs):
        case BIN(sir_fmodssv):
        case BIN(sir_fmodssi):
        case BIN(sir_fmodsis):
        case BIN(sir_sfmodsss):
        case BIN(sir_sfmodsvv):
        case BIN(sir_sfmodsvi):
        case BIN(sir_sfmodsiv):
        case BIN(sir_sfmodsvs):
        case BIN(sir_sfmodssv):
        case BIN(sir_sfmodssi):
        case BIN(sir_sfmodsis):
            fn = "mod";
            goto fop;
        case BIN(sir_feqsss):
        case BIN(sir_feqsvv):
        case BIN(sir_feqsvi):
        case BIN(sir_feqsiv):
        case BIN(sir_feqsvs):
        case BIN(sir_feqssv):
        case BIN(sir_feqssi):
        case BIN(sir_feqsis):
        case BIN(sir_feqvvv):
        case BIN(sir_feqvvi):
        case BIN(sir_feqviv):
        case BIN(sir_feqvvs):
        case BIN(sir_feqvsv):
        case BIN(sir_feqvsi):
        case BIN(sir_feqvis):
        case BIN(sir_feqvss):
        case BIN(sir_sfeqsss):
        case BIN(sir_sfeqsvv):
        case BIN(sir_sfeqsvi):
        case BIN(sir_sfeqsiv):
        case BIN(sir_sfeqsvs):
        case BIN(sir_sfeqssv):
        case BIN(sir_sfeqssi):
        case BIN(sir_sfeqsis):
            fn = "eq";
            goto fop;
        case BIN(sir_fneqsss):
        case BIN(sir_fneqsvv):
        case BIN(sir_fneqsvi):
        case BIN(sir_fneqsiv):
        case BIN(sir_fneqsvs):
        case BIN(sir_fneqssv):
        case BIN(sir_fneqssi):
        case BIN(sir_fneqsis):
        case BIN(sir_fneqvvv):
        case BIN(sir_fneqvvi):
        case BIN(sir_fneqviv):
        case BIN(sir_fneqvvs):
        case BIN(sir_fneqvsv):
        case BIN(sir_fneqvsi):
        case BIN(sir_fneqvis):
        case BIN(sir_fneqvss):
        case BIN(sir_sfneqsss):
        case BIN(sir_sfneqsvv):
        case BIN(sir_sfneqsvi):
        case BIN(sir_sfneqsiv):
        case BIN(sir_sfneqsvs):
        case BIN(sir_sfneqssv):
        case BIN(sir_sfneqssi):
        case BIN(sir_sfneqsis):
            fn = "neq";
            goto fop;
        case BIN(sir_fltsss):
        case BIN(sir_fltsvv):
        case BIN(sir_fltsvi):
        case BIN(sir_fltsiv):
        case BIN(sir_fltsvs):
        case BIN(sir_fltssv):
        case BIN(sir_fltssi):
        case BIN(sir_fltsis):
        case BIN(sir_fltvvv):
        case BIN(sir_fltvvi):
        case BIN(sir_fltviv):
        case BIN(sir_fltvvs):
        case BIN(sir_fltvsv):
        case BIN(sir_fltvsi):
        case BIN(sir_fltvis):
        case BIN(sir_fltvss):
        case BIN(sir_sfltsss):
        case BIN(sir_sfltsvv):
        case BIN(sir_sfltsvi):
        case BIN(sir_sfltsiv):
        case BIN(sir_sfltsvs):
        case BIN(sir_sfltssv):
        case BIN(sir_sfltssi):
        case BIN(sir_sfltsis):
            fn = "lt";
            goto fop;
        case BIN(sir_fgtsss):
        case BIN(sir_fgtsvv):
        case BIN(sir_fgtsvi):
        case BIN(sir_fgtsiv):
        case BIN(sir_fgtsvs):
        case BIN(sir_fgtssv):
        case BIN(sir_fgtssi):
        case BIN(sir_fgtsis):
        case BIN(sir_fgtvvv):
        case BIN(sir_fgtvvi):
        case BIN(sir_fgtviv):
        case BIN(sir_fgtvvs):
        case BIN(sir_fgtvsv):
        case BIN(sir_fgtvsi):
        case BIN(sir_fgtvis):
        case BIN(sir_fgtvss):
        case BIN(sir_sfgtsss):
        case BIN(sir_sfgtsvv):
        case BIN(sir_sfgtsvi):
        case BIN(sir_sfgtsiv):
        case BIN(sir_sfgtsvs):
        case BIN(sir_sfgtssv):
        case BIN(sir_sfgtssi):
        case BIN(sir_sfgtsis):
            fn = "gt";
            goto fop;
        case BIN(sir_flesss):
        case BIN(sir_flesvv):
        case BIN(sir_flesvi):
        case BIN(sir_flesiv):
        case BIN(sir_flesvs):
        case BIN(sir_flessv):
        case BIN(sir_flessi):
        case BIN(sir_flesis):
        case BIN(sir_flevvv):
        case BIN(sir_flevvi):
        case BIN(sir_fleviv):
        case BIN(sir_flevvs):
        case BIN(sir_flevsv):
        case BIN(sir_flevsi):
        case BIN(sir_flevis):
        case BIN(sir_flevss):
        case BIN(sir_sflesss):
        case BIN(sir_sflesvv):
        case BIN(sir_sflesvi):
        case BIN(sir_sflesiv):
        case BIN(sir_sflesvs):
        case BIN(sir_sflessv):
        case BIN(sir_sflessi):
        case BIN(sir_sflesis):
            fn = "le";
            goto fop;
        case BIN(sir_fgesss):
        case BIN(sir_fgesvv):
        case BIN(sir_fgesvi):
        case BIN(sir_fgesiv):
        case BIN(sir_fgesvs):
        case BIN(sir_fgessv):
        case BIN(sir_fgessi):
        case BIN(sir_fgesis):
        case BIN(sir_fgevvv):
        case BIN(sir_fgevvi):
        case BIN(sir_fgeviv):
        case BIN(sir_fgevvs):
        case BIN(sir_fgevsv):
        case BIN(sir_fgevsi):
        case BIN(sir_fgevis):
        case BIN(sir_fgevss):
        case BIN(sir_sfgesss):
        case BIN(sir_sfgesvv):
        case BIN(sir_sfgesvi):
        case BIN(sir_sfgesiv):
        case BIN(sir_sfgesvs):
        case BIN(sir_sfgessv):
        case BIN(sir_sfgessi):
        case BIN(sir_sfgesis):
            fn = "ge";
        fop:
            if (imm1_p)
                O("  r = mirjit_fr%sf(d, val2);\n", fn);
            else if (imm2_p)
                O("  r = mirjit_f%sf(val, d);\n", fn);
            else
                O("  r = mirjit_f%s(val, val2);\n", fn);
            break;
#endif
        case BIN(sir_siarefsss):
        case BIN(sir_siarefsvv):
        case BIN(sir_siarefsvi):
        case BIN(sir_siarefsiv):
        case BIN(sir_siarefsvs):
        case BIN(sir_siarefssv):
        case BIN(sir_siarefssi):
        case BIN(sir_siarefsis):
            gen_class_of(buff, "r", "val");
            O("  if (!(val2 & 0x%x) || r != 0x%"PRIxVALUE ") goto restart;\n", (int)RUBY_FIXNUM_FLAG, rb_cArray);
            /* fall through */
        case BIN(sir_iarefsss):
        case BIN(sir_iarefsvv):
        case BIN(sir_iarefsvi):
        case BIN(sir_iarefsiv):
        case BIN(sir_iarefsvs):
        case BIN(sir_iarefssv):
        case BIN(sir_iarefssi):
        case BIN(sir_iarefsis):
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
            abort();
            assert(FALSE);
        }
        if (stack_ops_num != 0) O("  sp -= %d;\n", stack_ops_num);
        if (rv_p)
            O("  ep[%d] = r;\n", (int)(EPDISP)operands[0]);
        else
            O("  *sp++ = r;\n");
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    case BIN(sir_siasetsis):
        siaset_p = TRUE;
        /* fall through */
    case BIN(sir_iasetsis):
        gen_insn_stat(buff, insn_id);
        stack_ops_num = 2;
        O("  r = sp[-2]; val2 = sp[-1];\n");
        O("  val = 0x%"PRIxVALUE ";\n", operands[0]);
        goto iaset;
    case BIN(sir_siasetsiv):
        siaset_p = TRUE;
        /* fall through */
    case BIN(sir_iasetsiv):
        gen_insn_stat(buff, insn_id);
        stack_ops_num = 1;
        O("  r = sp[-1];\n");
        O("  val = 0x%"PRIxVALUE ";\n", operands[0]);
        O("  val2 = ep[%d];\n", (int)(EPDISP)operands[1]);
        goto iaset;
    case BIN(sir_siasetviv):
        siaset_p = TRUE;
        /* fall through */
    case BIN(sir_iasetviv):
        gen_insn_stat(buff, insn_id);
        stack_ops_num = 0;
        O("  r = ep[%d];\n", (int)(EPDISP)operands[0]);
        O("  val = 0x%"PRIxVALUE ";\n", operands[1]);
        O("  val2 = ep[%d];\n", (int)(EPDISP)operands[2]);
        goto iaset;
    case BIN(sir_siasetvis):
        siaset_p = TRUE;
        /* fall through */
    case BIN(sir_iasetvis):
        gen_insn_stat(buff, insn_id);
        stack_ops_num = 1;
        O("  r = ep[%d];\n", (int)(EPDISP)operands[0]);
        O("  val = 0x%"PRIxVALUE ";\n", operands[1]);
        O("  val2 = sp[-1];\n");
        goto iaset;
    case BIN(sir_siasetsss):
        siaset_p = TRUE;
        /* fall through */
    case BIN(sir_iasetsss):
        gen_insn_stat(buff, insn_id);
        stack_ops_num = 3;
        O("  r = sp[-3]; val = sp[-2]; val2 = sp[-1];\n");
        goto iaset;
    case BIN(sir_siasetsvv):
        siaset_p = TRUE;
        /* fall through */
    case BIN(sir_iasetsvv):
        gen_insn_stat(buff, insn_id);
        stack_ops_num = 1;
        O("  r = sp[-1];\n");
        O("  val = ep[%d];\n", (int)(EPDISP)operands[0]);
        O("  val2 = ep[%d];\n", (int)(EPDISP)operands[1]);
        goto iaset;
    case BIN(sir_siasetsvs):
        siaset_p = TRUE;
        /* fall through */
    case BIN(sir_iasetsvs):
        gen_insn_stat(buff, insn_id);
        stack_ops_num = 2;
        O("  r = sp[-2]; val2 = sp[-1];\n");
        O("  val = ep[%d];\n", (int)(EPDISP)operands[0]);
        goto iaset;
    case BIN(sir_siasetssv):
        siaset_p = TRUE;
        /* fall through */
    case BIN(sir_iasetssv):
        gen_insn_stat(buff, insn_id);
        stack_ops_num = 2;
        O("  r = sp[-2]; val = sp[-1];\n");
        O("  val2 = ep[%d];\n", (int)(EPDISP)operands[0]);
        goto iaset;
    case BIN(sir_siasetvss):
        siaset_p = TRUE;
        /* fall through */
    case BIN(sir_iasetvss):
        gen_insn_stat(buff, insn_id);
        stack_ops_num = 2;
        O("  r = ep[%d];\n", (int)(EPDISP)operands[0]);
        O("  val = sp[-2]; val2 = sp[-1];\n");
        goto iaset;
    case BIN(sir_siasetvvv):
        siaset_p = TRUE;
        /* fall through */
    case BIN(sir_iasetvvv):
        gen_insn_stat(buff, insn_id);
        stack_ops_num = 0;
        O("  r = ep[%d];\n", (int)(EPDISP)operands[0]);
        O("  val = ep[%d];\n", (int)(EPDISP)operands[1]);
        O("  val2 = ep[%d];\n", (int)(EPDISP)operands[2]);
        goto iaset;
    case BIN(sir_siasetvvs):
        siaset_p = TRUE;
        /* fall through */
    case BIN(sir_iasetvvs):
        gen_insn_stat(buff, insn_id);
        stack_ops_num = 1;
        O("  r = ep[%d];\n", (int)(EPDISP)operands[0]);
        O("  val = ep[%d];\n", (int)(EPDISP)operands[1]);
        O("  val2 = sp[-1];\n");
        goto iaset;
    case BIN(sir_siasetvsv):
        siaset_p = TRUE;
        /* fall through */
    case BIN(sir_iasetvsv):
        gen_insn_stat(buff, insn_id);
        stack_ops_num = 1;
        O("  r = ep[%d];\n", (int)(EPDISP)operands[0]);
        O("  val = sp[-1];\n");
        O("  val2 = ep[%d];\n", (int)(EPDISP)operands[1]);
    iaset: /* r[val] = val2; */
        if (siaset_p) {
            gen_class_of(buff, "t", "r");
            O("  if (!(val & 0x%x) || t != 0x%"PRIxVALUE ") goto restart;\n", (int)RUBY_FIXNUM_FLAG, rb_cArray);
        }
        /* It is not worth to cancel sharing here as most probably it is done in the interpreter: */
        O("  if (__builtin_expect(*((VALUE *)((char *)r + %d)) & 0x%"PRIxVALUE ", 0)) goto restart;\n",
          (int)offsetof(struct RBasic, flags), (VALUE)ELTS_SHARED); /* check shared array */
        gen_array_aref_len(buff, "pv", "t", "r"); /* t is len */
        /* rb_ary_store(r, FIX2LONG(val), val2); */
        O("  val = (SIGNED_VALUE)val >> 1;\n"); /* offset = FIX2LONG(val) */
        O("  if ((SIGNED_VALUE)val < 0) {\n");
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
        gen_insn_stat(buff, insn_id);
        O("  if (*(rb_atomic_t *)((char*)ec+%d) & ~(VALUE)*(rb_atomic_t *)((char*)ec+%d)) goto restart;\n",
          (int)offsetof(rb_execution_context_t, interrupt_flag), (int)offsetof(rb_execution_context_t, interrupt_mask));
        if (v2_p)
            O("  val2 = ep[%d];\n", (int)(EPDISP)operands[v1_p ? 3 : 2]);
        else if (imm2_p)
            O("  val2 = 0x%"PRIxVALUE ";\n", operands[v1_p ? 3 : 2]);
        else
            O("  val2 = *--sp;\n");
        if (v1_p)
            O("  val = ep[%d];\n", (int)(EPDISP)operands[2]);
        else
            O("  val = *--sp;\n");
        O("  if ((long)val %s (long)val2)\n", op);
        O("    pc += %d;\n", insn_len(insn_id) + (int)(OFFSET)operands[0]); /* true label */
        O("  else\n");
        O("    pc += %d;\n", insn_len(insn_id) + (int)(OFFSET)operands[1]); /* false label */
        break;
    case BIN(sir_jump): // ??? test only for backward jumps/branches
        gen_insn_stat(buff, insn_id);
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
        gen_insn_stat(buff, insn_id);
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
    case BIN(leave):
        gen_insn_stat(buff, insn_id);
        O("  flags = *ep;\n");
        O("  if (flags & 0x%"PRIxVALUE ") goto restart;\n", (VALUE)VM_FRAME_FLAG_FINISH);
        if (OPT_CHECKED_RUN)
            O("  if (__builtin_expect(sp - 1 != cfp->__bp__, 0)) goto restart;\n");
        gen_pop_frame(buff, TRUE);
        break;
    case BIN(sir_iseq_send_without_block): { // ??? visibility
        SEND_INFO si = (SEND_INFO)operands[0];
        struct rb_iseq_bbv *bbv_ref = (struct rb_iseq_bbv *)operands[1];
        rb_iseq_t *iseq = bbv_ref->iseq;
        struct rb_iseq_constant_body *body = ISEQ_BODY(iseq);
        int argc = si->argc;
        const struct rb_callable_method_entry_struct *cme = si->cme;
        int local_size = body->local_table_size - argc;
        int stack_max = body->stack_max;

        assert(callable_method_entry_p(cme) && argc == param_size);
        gen_insn_stat(buff, insn_id);
        O("  {\n");
        O("    VALUE klass, *argv, recv = sp[-%d];\n", argc + 1);
        gen_class_of(buff, "klass", "recv");
        O("    if (__builtin_expect(0x%"PRIxVALUE " != klass, 0)) goto restart;\n", si->klass);
        O("    argv = (VALUE *)sp - %d;\n", argc);
        O("    VALUE specval = 0x%"PRIxVALUE ";\n", (VALUE)VM_BLOCK_HANDLER_NONE);
        O("    VALUE *start_pc = (VALUE *)0x%"PRIxVALUE ";\n", (VALUE)(body->bbv_code + bbv_ref->bbv_start));
        gen_push_frame(buff, local_size, stack_max, (int)insn_len(insn_id), (VALUE)cme,
                       (VALUE)(VM_FRAME_MAGIC_METHOD | VM_ENV_FLAG_LOCAL),
                       (VALUE)iseq, "start_pc");
        O("  }\n");
        break;
    }
        // ??? general rtl insns
    case BIN(sir_getivar):
    case BIN(sir_setivar): {
        shape_id_t shape_id = (lindex_t)operands[1];
        uint32_t index = (uint32_t)operands[0];

        gen_insn_stat(buff, insn_id);
        O("  val = cfp->self;\n");
        O("  {\n");
        O("    shape_id_t obj_shape_id;\n");
        gen_obj_shape(buff, "  obj_shape_id", "val");
        O("    if (obj_shape_id != 0x%"PRIxVALUE ") goto restart;\n", (VALUE)shape_id);
        O("  }\n");
        if (insn_id == BIN(sir_setivar)) gen_obj_frozen(buff, "val");
        O("  if (*((VALUE *)((char *)val + %d)) & 0x%"PRIxVALUE ") {\n", /* if (RB_FL_ANY_RAW(obj, ROBJECT_EMBED)) */
          (int)offsetof(struct RBasic, flags), (VALUE)ROBJECT_EMBED);
        O("    pv = &((VALUE *)((char *)val + %d))[%u];\n",
          (int)offsetof(struct RObject, as.ary), (unsigned)index);
        O("  } else {\n");
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
            O("  *sp++= r;\n");
            O("  pc += %d;\n", insn_len(insn_id));
        }
        break;
    }
    case BIN(sir_send_ivget):
    case BIN(sir_send_ivset): {
        shape_id_t shape_id = (lindex_t)operands[0];
        unsigned index = (unsigned)(lindex_t)operands[1];
        gen_insn_stat(buff, insn_id);
        O("  val = sp[%d];\n", insn_id == BIN(sir_send_ivset) ? -2 : -1); /* recv */
        O("  {\n");
        O("    shape_id_t obj_shape_id;\n");
        gen_obj_shape(buff, "  obj_shape_id", "val");
        O("    if (obj_shape_id != 0x%"PRIxVALUE ") goto restart;\n", (VALUE)shape_id);
        O("  }\n");
        if (insn_id == BIN(sir_send_ivset)) {
            O("  r = sp[-1];\n"); /* val */
            gen_obj_frozen(buff, "val");
        }
        O("  if (*((VALUE *)((char *)val + %d)) & 0x%"PRIxVALUE ") {\n", /* if (RB_FL_ANY_RAW(obj, ROBJECT_EMBED)) */
          (int)offsetof(struct RBasic, flags), (VALUE)ROBJECT_EMBED);
        O("    pv = &((VALUE *)((char *)val + %d))[%u];\n",
          (int)offsetof(struct RObject, as.ary), (unsigned)index);
        O("  } else {\n");
        O("    pv = &(*(VALUE **)((char *)val + %d))[%u];\n",
          (int)offsetof(struct RObject, as.heap.ivptr), (unsigned)index);
        O("  }\n");
        if (insn_id == BIN(sir_send_ivget)) {
            O("  r = *pv;\n");
            O("  sp[-1] = r;\n");
            O("  pc += %d;\n", insn_len(insn_id));
        } else {
            O("  sp--;\n");
            O("  *pv = r;\n");
            O("  sp[-1] = r;\n");
            O("  pc += %d;\n", insn_len(insn_id));
            gen_write_barrier(buff, "val", "r");
        }
        break;
    }
#ifdef MIRJIT_ITER_INSNS
    case BIN(sir_iter_start): {
        gen_insn_stat(buff, insn_id);
        int (*f)(VALUE, VALUE *) = (void *)operands[0];
        O("  val = sp[-1];\n");
        O("  if (!((int (*)(VALUE, VALUE *))0x%"PRIxVALUE ")(val, &val2)) goto restart;\n", (VALUE)f); /* f(arg, index)*/
        O("  *sp++ = val2;\n");
        O("  *sp++ = 0x%"PRIxVALUE ";\n", LONG2FIX(0)); /* state */
        O("  *sp++ = 0x%"PRIxVALUE ";\n", Qnil); /* res */
        O("  pc += %d;\n", insn_len(insn_id));
        break;
    }
    case BIN(sir_iter_body): {
        struct rb_iseq_bbv *bbv_ref = (struct rb_iseq_bbv *)operands[0];
        rb_iseq_t *iseq = bbv_ref->iseq;
        struct rb_iseq_constant_body * body = ISEQ_BODY(iseq);
        int param_size = body->param.size;
        int lead_num = body->param.lead_num;
        //VALUE *start_pc = body->bbv_code + bbv_ref->bbv_start;
        int local_size = body->local_table_size - param_size;
        int stack_max = body->stack_max;
        const struct rb_callable_method_entry_struct *cme = bbv_ref->cme;
        VALUE (*f)(VALUE, VALUE, VALUE, VALUE *) = (void *)operands[1];
        long argc = 1;

        gen_insn_stat(buff, insn_id);
        O("  {\n");
        O("    VALUE self, recv, index, state, arg, ary;\n");
        O("    recv = sp[-4];\n");
        O("    index = sp[-3];\n");
        O("    state = sp[-2];\n");
        O("    if ((index = ((VALUE (*)(VALUE, VALUE, VALUE, VALUE *))0x%"PRIxVALUE ")(recv, index, state, &arg)) == 0x%"
          PRIxVALUE ") {\n", (VALUE)f, (VALUE)Qundef);
        O("      sp -= 3;\n");
        O("      sp[-1] = arg;\n"); /* result */
        O("      pc += %d;\n", insn_len(insn_id));
        O("      if (state & 2) {\n"); /* state 1 or 3: switch to safe code */
        O("        pc = rb_sir_get_jump_safe_pc(cfp, pc);\n"); /*safe pc */
        O("      }\n");
        O("      goto exit;\n");
        O("    }\n");
        O("    if (state <= 0x%"PRIxVALUE ") {\n", LONG2FIX(1)); /* right from iter start */
        O("      sp[-2] = state + 4;\n");  /* change state += 2 */
        O("    }\n");
        O("    sp[-3] = index;\n");
        O("    VALUE *old_sp = sp;\n");
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
            O("      sp += lead_num + 1;\n");
            O("    } else {\n");
        } else {
            O("    {\n");
            O("      sp[1] = arg;\n");
            O("      sp += 2;\n");
        }
        O("    }\n");
        O("    VALUE *argv = old_sp;\n");
        O("    VALUE specval = (VALUE)cfp->ep | 1;\n");
        O("    recv = self;\n"); /* self */
        O("    VALUE *start_pc = (VALUE *)0x%"PRIxVALUE ";\n", (VALUE)(body->bbv_code + bbv_ref->bbv_start));
        gen_push_frame(buff, local_size, stack_max, 0, (VALUE)cme,
                       (VALUE)VM_FRAME_MAGIC_METHOD, (VALUE)iseq, "start_pc");
        O("  }\n");
        break;
    }
#endif
    case BIN(sir_cfunc_send): {
        SEND_INFO si = (SEND_INFO)operands[0];
        rb_iseq_t *block_iseq = (rb_iseq_t *)operands[1];
        int argc = si->argc;
        const struct rb_callable_method_entry_struct *cme = si->cme;
        const rb_method_cfunc_t *cfunc = vm_method_cfunc_entry(cme);
        rb_cfunc_invoker_t invoker = cfunc->invoker;
        rb_cfunc_func_t func = cfunc->func;

    /*  if (klass != CLASS_OF(recv)) goto sir_cfunc_send_exit;
        ???RUBY_DTRACE_CMETHOD_ENTRY_HOOK(ec, cme->owner, cme->def->original_id);
        ???EXEC_EVENT_HOOK(ec, RUBY_EVENT_C_CALL, recv, cme->def->original_id, si->mid, cme->owner, Qundef);
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

        gen_insn_stat(buff, insn_id);
        O("  {\n");
        O("    VALUE klass, *argv = sp - %d, recv = sp[-%d];\n", argc, argc + 1);
        gen_class_of(buff, "klass", "recv");
        O("    if (__builtin_expect(0x%"PRIxVALUE " != klass, 0)) goto restart;\n", si->klass);
        O("    argv = (VALUE *)sp - %d;\n", argc);
        if (block_iseq == NULL) {
            O("    VALUE specval = 0x%"PRIxVALUE ";\n", (VALUE)VM_BLOCK_HANDLER_NONE);
        } else {
            O("    VALUE *captured = &cfp->self;\n");
            O("    *(VALUE *)((char*)captured + %d) = 0x%"PRIxVALUE ";\n",
              (int)offsetof(struct rb_captured_block, code.iseq), (VALUE)block_iseq);
            O("    VALUE specval = (VALUE)captured | 1;\n");
        }
        O("    VALUE *old_cfp = cfp;\n");
        gen_push_frame(buff, 0, 0, (int)insn_len(insn_id), (VALUE)cme,
                       (VALUE)(VM_FRAME_MAGIC_CFUNC | VM_FRAME_FLAG_CFRAME | VM_ENV_FLAG_LOCAL),
                       (VALUE)0, "(VALUE *)0");
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
    case BIN(specialize_bbv): abort();
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

#ifdef FLONUM_INSNS
static sir_inline DBL mirjit_float(VALUE v) {
    if (RB_LIKELY(FLONUM_P(v))) return RFLOAT_VALUE(v);
    assert(!SPECIAL_CONST_P(v) && RBASIC_CLASS(v) == rb_cFloat);
    return RFLOAT(v)->float_value;
}

static VALUE mirjit_fplus(VALUE v1, VALUE v2){ return DBL2NUM(mirjit_float(v1) + mirjit_float(v2)); }
static VALUE mirjit_fplusf(VALUE v, DBL d){ return DBL2NUM(mirjit_float(v) + d); }
static VALUE mirjit_frplusf(DBL d, VALUE v){ return DBL2NUM(d + mirjit_float(v)); }
static VALUE mirjit_fminus(VALUE v1, VALUE v2){ return DBL2NUM(mirjit_float(v1) - mirjit_float(v2)); }
static VALUE mirjit_fminusf(VALUE v, DBL d){ return DBL2NUM(mirjit_float(v) - d); }
static VALUE mirjit_frminusf(DBL d, VALUE v){ return DBL2NUM(d - mirjit_float(v)); }
static VALUE mirjit_fmult(VALUE v1, VALUE v2){ return DBL2NUM(mirjit_float(v1) * mirjit_float(v2)); }
static VALUE mirjit_fmultf(VALUE v, DBL d){ return DBL2NUM(mirjit_float(v) * d); }
static VALUE mirjit_frmultf(DBL d, VALUE v){ return DBL2NUM(d * mirjit_float(v)); }
static VALUE mirjit_fdiv(VALUE v1, VALUE v2){ return DBL2NUM(ruby_float_div(mirjit_float(v1), mirjit_float(v2))); }
static VALUE mirjit_fdivf(VALUE v, DBL d){ return DBL2NUM(ruby_float_div(mirjit_float(v), d)); }
static VALUE mirjit_frdivf(DBL d, VALUE v){ return DBL2NUM(ruby_float_div(d, mirjit_float(v))); }
static VALUE mirjit_fmod(VALUE v1, VALUE v2){ return DBL2NUM(ruby_float_mod(mirjit_float(v1), mirjit_float(v2))); }
static VALUE mirjit_fmodf(VALUE v, DBL d){ return DBL2NUM(ruby_float_mod(mirjit_float(v), d)); }
static VALUE mirjit_frmodf(DBL d, VALUE v){ return DBL2NUM(ruby_float_mod(d, mirjit_float(v))); }
static VALUE mirjit_feq(VALUE v1, VALUE v2){ return RBOOL(mirjit_float(v1) == mirjit_float(v2)); }
static VALUE mirjit_feqf(VALUE v, DBL d){ return RBOOL(mirjit_float(v) == d); }
static VALUE mirjit_freqf(DBL d, VALUE v){ return RBOOL(d == mirjit_float(v)); }
static VALUE mirjit_fneq(VALUE v1, VALUE v2){ return RBOOL(mirjit_float(v1) != mirjit_float(v2)); }
static VALUE mirjit_fneqf(VALUE v, DBL d){ return RBOOL(mirjit_float(v) != d); }
static VALUE mirjit_frneqf(DBL d, VALUE v){ return RBOOL(d != mirjit_float(v)); }
static VALUE mirjit_flt(VALUE v1, VALUE v2){ return RBOOL(mirjit_float(v1) < mirjit_float(v2)); }
static VALUE mirjit_fltf(VALUE v, DBL d){ return RBOOL(mirjit_float(v) < d); }
static VALUE mirjit_frltf(DBL d, VALUE v){ return RBOOL(d < mirjit_float(v)); }
static VALUE mirjit_fgt(VALUE v1, VALUE v2){ return RBOOL(mirjit_float(v1) > mirjit_float(v2)); }
static VALUE mirjit_fgtf(VALUE v, DBL d){ return RBOOL(mirjit_float(v) > d); }
static VALUE mirjit_frgtf(DBL d, VALUE v){ return RBOOL(d < mirjit_float(v)); }
static VALUE mirjit_fle(VALUE v1, VALUE v2){ return RBOOL(mirjit_float(v1) <= mirjit_float(v2)); }
static VALUE mirjit_flef(VALUE v, DBL d){ return RBOOL(mirjit_float(v) <= d); }
static VALUE mirjit_frlef(DBL d, VALUE v){ return RBOOL(d <= mirjit_float(v)); }
static VALUE mirjit_fge(VALUE v1, VALUE v2){ return RBOOL(mirjit_float(v1) >= mirjit_float(v2)); }
static VALUE mirjit_fgef(VALUE v, DBL d){ return RBOOL(mirjit_float(v) >= d); }
static VALUE mirjit_frgef(DBL d, VALUE v){ return RBOOL(d >= mirjit_float(v)); }
#endif

#ifdef MIRJIT_STAT
long mirjit_run_nums, mirjit_restart_nums;
long mirjit_profile[VM_INSTRUCTION_SIZE];
long mirjit_cancel[VM_INSTRUCTION_SIZE];
#endif

typedef struct resolver_el {const char *name; void *addr;} resolver_el_t;
static resolver_el_t resolver_els[] = {
    {"rb_gc_writebarrier", rb_gc_writebarrier},
    {"rb_check_array_type", rb_check_array_type},
    {"rb_check_cfp_consistency", rb_check_cfp_consistency},
    {"rb_sir_get_jump_safe_pc", rb_sir_get_jump_safe_pc},
#ifdef FLONUM_INSNS
    {"mirjit_fplus", mirjit_fplus},
    {"mirjit_fplusf", mirjit_fplusf},
    {"mirjit_frplusf", mirjit_frplusf},
    {"mirjit_fminus", mirjit_fminus},
    {"mirjit_fminusf", mirjit_fminusf},
    {"mirjit_frminusf", mirjit_frminusf},
    {"mirjit_fmult", mirjit_fmult},
    {"mirjit_fmultf", mirjit_fmultf},
    {"mirjit_frmultf", mirjit_frmultf},
    {"mirjit_fdiv", mirjit_fdiv},
    {"mirjit_fdivf", mirjit_fdivf},
    {"mirjit_frdivf", mirjit_frdivf},
    {"mirjit_fmod", mirjit_fmod},
    {"mirjit_fmodf", mirjit_fmodf},
    {"mirjit_frmodf", mirjit_frmodf},
    {"mirjit_feq", mirjit_feq},
    {"mirjit_feqf", mirjit_feqf},
    {"mirjit_freqf", mirjit_freqf},
    {"mirjit_fneq", mirjit_fneq},
    {"mirjit_fneqf", mirjit_fneqf},
    {"mirjit_frneqf", mirjit_frneqf},
    {"mirjit_flt", mirjit_flt},
    {"mirjit_fltf", mirjit_fltf},
    {"mirjit_frltf", mirjit_frltf},
    {"mirjit_fgt", mirjit_fgt},
    {"mirjit_fgtf", mirjit_fgtf},
    {"mirjit_frgtf", mirjit_frgtf},
    {"mirjit_fle", mirjit_fle},
    {"mirjit_flef", mirjit_flef},
    {"mirjit_frlef", mirjit_frlef},
    {"mirjit_fge", mirjit_fge},
    {"mirjit_fgef", mirjit_fgef},
    {"mirjit_frgef", mirjit_frgef},
#endif
    {"memcpy", memcpy},
#ifdef MIRJIT_STAT
    {"mirjit_run_nums", &mirjit_run_nums},
    {"mirjit_restart_nums", &mirjit_restart_nums},
    {"mirjit_profile", &mirjit_profile},
    {"mirjit_cancel", &mirjit_cancel},
    {"rb_vm_insn_addr2insn", rb_vm_insn_addr2insn},
#endif
};

DEF_HTAB(resolver_el_t);
static HTAB(resolver_el_t) *resolver_tab;

static htab_hash_t resolver_hash(resolver_el_t el, void *arg) {
    return (htab_hash_t)st_hash(el.name, strlen(el.name), 42);
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
    if (strcmp(name, "printf") == 0) return printf;
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
gen_bbv_code(VARR(char) *buff, struct rb_iseq_bbv *bbv, int rejit_p) {
    int len, pc, next_pc, mirjit_debug_level = rb_mirjit_debug_level(), insns_num = 0, bbv_pc = -1;
    int finish_jit_code_p;
    rb_iseq_t *iseq = bbv->iseq;
    struct rb_iseq_constant_body *body = ISEQ_BODY(iseq);
    enum ruby_vminsn_type insn_id, next_insn_id;

    finish_jit_code_p = FALSE;
    assert(bbv->bbv_start < bbv->bbv_bound);
    if (rejit_p) {
        assert(BIN(sir_rejit_bbv) == rb_mirjit_insn_decode(&body->bbv_code[bbv->bbv_start]));
        body->bbv_code[bbv->bbv_start] = (VALUE)sir_dispatch_table[BIN(sir_bbv_start)];
        for (pc = bbv->bbv_start; pc < bbv->bbv_bound; pc += insn_len(insn_id)) {
            insn_id = rb_mirjit_insn_decode(&body->bbv_code[pc]);
            if (insn_id == BIN(sir_bbv_cont))
                body->bbv_code[pc] = (VALUE)sir_dispatch_table[BIN(sir_bbv_cont)];
        }
        if (mirjit_debug_level >= 1) {
            fprintf (stderr, "## Rejitting BBV 0x%llx\n", (unsigned long long)bbv);
            rb_sir_print_bbv(bbv);
        }
    }
    for (pc = bbv->bbv_start, insn_id = rb_mirjit_insn_decode(&body->bbv_code[pc]);
         pc < bbv->bbv_bound;
         pc = next_pc, insn_id = next_insn_id) {
        len = insn_len(insn_id);
        next_pc = pc + len;
        next_insn_id = next_pc < bbv->bbv_bound ? rb_mirjit_insn_decode(&body->bbv_code[next_pc]) : 0;
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
            O("  DBL d;\n");
            O("  VALUE *ep /*__mirc_attribute__ ((antialias (val)))*/ = cfp->ep;\n");
            O("  VALUE *sp /*__mirc_attribute__ ((antialias (val)))*/ = cfp->sp;\n");
#if defined(__x86_64__) && defined(JRET)
            /* skip sir_bbv_{start, cont} */
            O("  pc += %d;\n", insn_len(BIN(sir_bbv_start)));
#endif
#ifdef MIRJIT_STAT
            O("  mirjit_run_nums++;\n");
#endif
            break;
        default:
            if (!gen_insn_code(buff, body->bbv_code + pc, insn_id) || insn_id == BIN(leave)) {
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
#ifdef MIRJIT_STAT
            O("  goto exit;\n");
#endif
            O(" restart:\n");
#ifdef MIRJIT_STAT
            O("  mirjit_cancel[rb_vm_insn_addr2insn((void*)*pc)]++;\n");
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
            if (mirjit_debug_level >= 1) {
                fprintf(stderr, "C code for BBV 0x%llx @ %d:\n%s",
                        (unsigned long long)bbv, bbv_pc - bbv->bbv_start, VARR_ADDR(char, buff));
            }
            curr_input.code = (uint8_t *)VARR_ADDR(char, buff);
            curr_input.code_len = strlen((char *)curr_input.code);
            curr_input.curr_char = 0;
            double start_time = real_usec_time();
            if (!c2mir_compile(ctx, &curr_input.options, t_getc, &curr_input, "jit_c_code.c", NULL)) {
                if (mirjit_debug_level >= 1) {
                    fprintf(stderr, "Failure in compilation of C code for BBV 0x%llx @ %d\n",
                            (unsigned long long)bbv, bbv_pc - bbv->bbv_start);
                }
            } else {
                if (mirjit_debug_level >= 1) {
                    fprintf(stderr, "Finish C to MIR compilation -- compilation time %.0f usec\n",
                            real_usec_time() - start_time);
                    MIR_gen_set_debug_file(ctx, 0, stderr);
                    MIR_gen_set_debug_level(ctx, 0, mirjit_debug_level - 1);
                }
                MIR_module_t module = DLIST_TAIL(MIR_module_t, *MIR_get_module_list(ctx));
                MIR_load_module(ctx, module);
                MIR_link(ctx, MIR_set_lazy_gen_interface, import_resolver);
                MIR_item_t func = DLIST_TAIL(MIR_item_t, module->items);
                while(func != NULL && func->item_type != MIR_func_item)
                    func = DLIST_PREV(MIR_item_t, func);
                assert(func != NULL && func->item_type == MIR_func_item);
                void *thunk = MIR_gen(ctx, 0, func);
                VALUE *pc_addr = &body->bbv_code[bbv_pc];
                if (rb_mirjit_get_pc_dispatch_addr(pc_addr) == NULL) /* set up only original dispatch addr */
                    rb_mirjit_set_pc_dispatch_addr(pc_addr, (void*)*pc_addr);
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
    gen_bbv_code(jit_c_code, bbv, FALSE);
}

void
rb_rejit_bbv(struct rb_iseq_bbv *bbv) {
    gen_bbv_code(jit_c_code, bbv, TRUE);
}

void
rb_mirjit_mark_bbv_change(struct rb_iseq_bbv *bbv) {
    rb_iseq_t *iseq = bbv->iseq;
    struct rb_iseq_constant_body *body = ISEQ_BODY(iseq);
    enum ruby_vminsn_type insn_id;

    insn_id = rb_mirjit_insn_decode(&body->bbv_code[bbv->bbv_start]);
    if (insn_id == BIN(sir_rejit_bbv)) return;
    assert(insn_id == BIN(sir_bbv_start));
    body->bbv_code[bbv->bbv_start] = (VALUE)sir_dispatch_table[BIN(sir_rejit_bbv)];
    if (rb_mirjit_debug_level() >= 1)
        fprintf(stderr, "## Mark BBV 0x%llx for rejitting\n", (unsigned long long)bbv);
}

static struct rb_mirjit_options mirjit_options;

int
rb_mirjit_enabled_p(void) { return mirjit_options.mirjit_enabled_p; }

int
rb_mirjit_debug_level(void) { return mirjit_options.debug_level; }

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
    pc_dispatch_addr_map = rb_st_init_numtable();
}

#ifdef MIRJIT_STAT
struct profile {int insn; long count;};

static int
profile_cmp(const void *e1, const void *e2) {
    const struct profile *p1 = e1, *p2 = e2;
    if (p1->count < p2->count) return 1;
    if (p1->count > p2->count) return -1;
    return 0;
}
#endif

void
rb_mirjit_finish(void) {
    if (ctx == NULL) return; /* not initialized */
#ifdef MIRJIT_STAT
    fprintf(stderr, "+++++++++++MIRJIT INSN PROFILE++++++++++\n");
    struct profile profile[VM_INSTRUCTION_SIZE];
    for (int i = 0; i < VM_INSTRUCTION_SIZE; i++) {
        profile[i].insn = i;
        profile[i].count = mirjit_profile[i];
    }
    qsort(profile, VM_INSTRUCTION_SIZE, sizeof(struct profile), profile_cmp);
    for (int i = 0; i < VM_INSTRUCTION_SIZE; i++) {
        if (profile[i].count == 0) break;
        if (mirjit_cancel[profile[i].insn] == 0)
            fprintf(stderr, "%-25ld:%s\n", profile[i].count, insn_name(profile[i].insn));
        else
            fprintf(stderr, "%-12ld !%-11ld:%s\n",
                    profile[i].count, mirjit_cancel[profile[i].insn], insn_name(profile[i].insn));
    }
    fprintf(stderr, "MIRJIT CODE RUNS=%ld, RESTARTS=%ld\n", mirjit_run_nums, mirjit_restart_nums);
#endif
    resolver_finish();
    VARR_DESTROY(char, jit_c_code);
    MIR_gen_finish(ctx);
    c2mir_finish(ctx);
    MIR_finish(ctx);
    rb_st_free_table(pc_dispatch_addr_map);
}

#endif /* #if defined(HAVE_LIBMIR) && defined(HAVE_PTHREAD_H) */
