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
#if defined(__x86_64__)
    "register void *ec asm(\"r13\");\n"
    "register VALUE *pc asm(\"r14\") __mirc_attribute__ ((antialias (val)));\n"
    "register void *cfp asm(\"r15\");\n"
#endif
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

#define TEMP_STR_SIZE 1024
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

static int
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

static void *import_resolver(const char *name) {
    void *addr = rb_mirjit_import_resolver(name);
    assert(addr != NULL);
    return addr;
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
    char insn_used_p[VM_INSTRUCTION_SIZE];

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
            fmt_output(buff, "%s", header);
#if !defined(__x86_64__)
            fmt_output(buff, "VALUE *jit_bbv%d(void *ec, void *cfp, VALUE *pc) {\n", pc);
#else
            fmt_output(buff, "void jit_bbv%d(void) {\n", pc);
#ifdef JRET
            fmt_output(buff, "  pc += 2; *(VALUE **)cfp = pc;\n"); // skip sir_bbv_{start, cont}
#endif
#endif
            fmt_output(buff, "  VALUE exit_flag = 0;\n");
            memset(insn_used_p, 0, VM_INSTRUCTION_SIZE);
            break;
        default:
            if (!gen_insn_code(buff, body->bbv_code + pc, insn_id, insn_used_p)) {
                finish_jit_code_p = TRUE;
            } else {
                insns_num++;
                fmt_output(buff, "l%d:;\n", (int)(pc - bbv->bbv_start));
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
                        fmt_output(buff, "  if ((VALUE)pc == 0x%llx) goto l%d;\n",
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
#if defined(__x86_64__)
#ifdef JRET
            fmt_output(buff, "  exit: __builtin_jret (*(void **)pc);\n}\n");
#else
            fmt_output(buff, "  exit: return;\n}\n");
#endif
#else
            fmt_output(buff, "  exit: return pc;\n}\n");
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
#if 1
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

#include "mirjit_code_env.inc"

static struct rb_mirjit_options mirjit_options;

int
rb_mirjit_enabled_p(void) { return mirjit_options.mirjit_enabled_p; }

int
rb_mirjit_debug_p(void) { return mirjit_options.debug_p; }

static size_t curr_input_byte_num;

static int read_byte(MIR_context_t ctx) {
  if (curr_input_byte_num >= sizeof(mir_code)) return EOF;
  return mir_code[curr_input_byte_num++];
}

void
rb_mirjit_init(struct rb_mirjit_options *options) {
    double start_time = real_usec_time();
    mirjit_options = *options;
    int mirjit_debug_p = rb_mirjit_debug_p();
    MIR_module_t module;
    ctx = MIR_init();
    MIR_read_with_func(ctx, read_byte);
    module = DLIST_HEAD(MIR_module_t, *MIR_get_module_list(ctx));
    MIR_load_module(ctx, module);
    if (mirjit_debug_p) {
#if 0
        MIR_output_module (ctx, stderr, module);
#endif
        fprintf(stderr, "Finish of MIR reading from memory -- read time %.0f usec\n",
                real_usec_time() - start_time);
    }
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
    rb_mirjit_resolver_init();
}

void
rb_mirjit_finish(void) {
    rb_mirjit_resolver_finish();
    VARR_DESTROY(char, jit_c_code);
    MIR_gen_finish(ctx);
    c2mir_finish(ctx);
    MIR_finish(ctx);
}

#endif /* #ifdef HAVE_LIBMIR */
