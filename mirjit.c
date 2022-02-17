#include "sir.h"
#include "internal/compile.h"
#include "internal/class.h"
#include "vm_callinfo.h"
#define YJIT_STATS 0 /* used in vm_insnhelper.h */
#include "vm_insnhelper.h"

#include "builtin.h"
#include "insns.inc"
#include "insns_info.inc"

#include "mirjit.h"

#include "mir.h"
#include "mir-gen.h"
#include "c2mir.h"

// Return time in milliseconds as a double.
#ifndef _WIN32
#include <sys/time.h>
static double real_usec_time (void) {
  struct timeval tv;

  gettimeofday (&tv, NULL);
  return tv.tv_usec + tv.tv_sec * 1000000.0;
}
#else
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
static double real_usec_time (void) {
  LARGE_INTEGER freq, count;

  if (QueryPerformanceFrequency (&freq) && QueryPerformanceCounter (&count))
    return (double) count.QuadPart / ((double) freq.QuadPart * 1.0E-6);
  return 0.0;
}
#endif

static const char *header = "typedef void *CALL_DATA;\n"
    "typedef void *IC;\n"
    "typedef void *IVC;\n"
    "typedef void *ISEQ;\n"
    "typedef void *rb_insn_func_t;\n"
    "typedef void *RB_BUILTIN;\n"
#if SIZEOF_LONG == SIZEOF_VOIDP
    "typedef unsigned long VALUE;\n"
    "typedef unsigned long ID;\n"
#elif SIZEOF_LONG_LONG == SIZEOF_VOIDP
    "typedef unsigned LONG_LONG VALUE;\n"
    "typedef unsigned LONG_LONG ID;\n"
#else
# error ---->> ruby requires sizeof(void*) == sizeof(long) or sizeof(LONG_LONG) to be compiled. <<----
#endif
    "typedef long OFFSET;\n"
    "typedef double DBL;\n"
    "typedef unsigned long lindex_t;\n"
    "typedef unsigned long rb_num_t;\n";

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

#define TEMP_STR_SIZE 512
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
#define MIRJIT_CONST_VAL_PTR(type, id, init_expr) (intptr_t)id

#define vm_method_cfunc_entry mirjit_method_cfunc_entry

static void
gen_insn_code(VARR(char) *buff, const VALUE *insn_pc,
              enum ruby_vminsn_type insn_id, char *insn_used_p) {
    int proto_gen_p;
    const VALUE *operands = &insn_pc[1];
    const VALUE *next_insn_pc = insn_pc + insn_len(insn_id);

    if ((proto_gen_p = !insn_used_p[insn_id])) {
        insn_used_p[insn_id] = TRUE;
    }
#include "mirjit_insn_compile.inc"
}

static void
gen_bbv_code(VARR(char) *buff, struct rb_iseq_bbv *bbv) {
    int len;
    rb_iseq_t *iseq = bbv->iseq;
    struct rb_iseq_constant_body *body = iseq->body;
    char insn_used_p[VM_INSTRUCTION_SIZE];

    fmt_output(buff, "%s", header);
    fmt_output(buff, "VALUE *jit_bbv%llx(void *ec, void *cfp, VALUE *pc) {\n",
               (long long unsigned)bbv);
    memset(insn_used_p, 0, VM_INSTRUCTION_SIZE);
    for (int pc = bbv->bbv_start; pc < bbv->bbv_bound; pc += len) {
	enum ruby_vminsn_type insn_id = rb_vm_insn_addr2insn((void *)body->bbv_code[pc]);
        len = insn_len(insn_id);
        fmt_output(buff, "l%d:\n", (int)(pc - bbv->bbv_start));
        switch (insn_id) {
#if 0
        case BIN(bbv_jit_call):
            break;
#endif
        default:
            gen_insn_code(buff, body->bbv_code + pc, insn_id, insn_used_p);
            break;
        }
    }
    fmt_output(buff, "  exit: return pc;\n}\n");
    void *func = NULL;
    body->bbv_code[bbv->bbv_start] = (VALUE)func;
}

#include "mirjit_code_env.inc"

static struct rb_mirjit_options mirjit_options;

int
rb_mirjit_enabled_p(void) { return mirjit_options.mirjit_enabled_p; }

int
rb_mirjit_debug_p(void) { return mirjit_options.debug_p; }

void
rb_mirjit_init(struct rb_mirjit_options *options) {
    mirjit_options = *options;
}

void
rb_mirjit_finish(void) {
}
