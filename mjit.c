/**********************************************************************

  mjit.c - MRI method JIT compiler

  Copyright (C) 2017 Vladimir Makarov <vmakarov@redhat.com>.

**********************************************************************/

/* We utilize widely used C compilers (GCC and LLVM Clang) to
   implement MJIT.  We feed them a C code generated from ISEQ.  The
   industrial C compilers are slower than regular JIT engines.
   Generated code performance of the used C compilers has a higher
   priority over the compilation speed.

   So our major goal is to minimize the ISEQ compilation time when we
   use widely optimization level (-O2).  It is achieved by

   o Minimizing C code header needed for RTL insns (a special Ruby
     script is used for this)
   o Using a precompiled version of the header
   o Keeping all files in `/tmp`.  On modern Linux `/tmp` is a file
     system in memory. So it is pretty fast
   o Implementing MJIT as a multi-threaded code because we want to
     compile ISEQs in parallel with iseq execution to speed up Ruby
     code execution.  MJIT has a several threads (*workers*) to do
     parallel compilations:
      o One worker prepares a precompiled code of the minimized
        header. It starts at the MRI execution start
      o One or more workers generate PIC object files of ISEQs
      o They start when the precompiled header is ready
      o They take one JIT unit from a priority queue unless it is empty.
      o They translate the JIT unit ISEQ into C-code using the precompiled
        header, call CC and load PIC code when it is ready
      o A worker don't start processing another JIT unit until it finishes
        processing the current unit.
      o Currently MJIT put ISEQ in the queue when ISEQ is called or right
        after generating ISEQ for AOT
      o MJIT can reorder ISEQs in the queue if some ISEQ has been called
        many times and its compilation did not start yet or we need the
        ISEQ code for AOT
      o MRI reuses the machine code if it already exists for ISEQ
      o The machine code we generate can stop and switch to the ISEQ
        interpretation if some condition is not satisfied as the machine
        code can be speculative or some exception raises
      o Speculative machine code can be canceled, and a new
        mutated machine code can be queued for creation.  It can
        happen when insn speculation was wrong.  There is a constraint
        on the mutation number.  The last mutation will contain the
        code without speculation

   Here is a diagram showing the MJIT organization:

       _______     _________________
      |header |-->| minimized header|
      |_______|   |_________________|
                    |                         MRI building
      --------------|----------------------------------------
                    |                         MRI execution
     	            |
       _____________|_____
      |             |     |
      |          ___V__   |  CC      ____________________
      |         |      |----------->| precompiled header |
      |         |      |  |         |____________________|
      |         |      |  |              |
      |         | MJIT |  |              |
      |         |      |  |              |
      |         |      |  |          ____V___  CC  __________
      |         |______|----------->| C code |--->| .so file |
      |                   |         |________|    |__________|
      |                   |                              |
      |                   |                              |
      | MRI machine code  |<-----------------------------
      |___________________|             loading


   We don't use SIGCHLD signal and WNOHANG waitpid in MJIT as it
   might mess with ruby code dealing with signals.  Also as SIGCHLD
   signal can be delivered to non-main thread, the stack might have a
   constraint.  So the correct version of code based on SIGCHLD and
   WNOHANG waitpid would be very complicated.  */

#include <stdlib.h>
#include <unistd.h>
#include <stdarg.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <dlfcn.h>
#include "internal.h"
#include "vm_core.h"
#include "iseq.h"
#include "insns.inc"
#include "insns_info.inc"
#include "mjit.h"
#include "vm_insnhelper.h"
#include "version.h"

extern rb_serial_t ruby_vm_global_method_state;
extern rb_serial_t ruby_vm_global_constant_state;

/* Numbers of the interpreted insns and JIT executed insns when
   MJIT_INSN_STATISTICS is non-zero.  */
unsigned long byte_code_insns_num;
RUBY_SYMBOL_EXPORT_BEGIN
unsigned long jit_insns_num;
RUBY_SYMBOL_EXPORT_END

/* Return time in milliseconds as a double.  */
static double
real_ms_time(void) {
    struct timeval  tv;

    gettimeofday(&tv, NULL);
    return tv.tv_usec / 1000.0 + tv.tv_sec * 1000.0;
}

/* A copy of MJIT portion of MRI options since MJIT initialization.  We
   need them as MJIT threads still can work when the most MRI data were
   freed. */
RUBY_SYMBOL_EXPORT_BEGIN
struct mjit_options mjit_opts;
RUBY_SYMBOL_EXPORT_END

/* Default level of details in the debug info.  */
static int debug_level = 3;

/* The MJIT start time.  */
static double mjit_time_start;

/* Return time in milliseconds as a double relative to MJIT start.  */
static double
relative_ms_time(void) {
    return real_ms_time() - mjit_time_start;
}

/* TRUE if MJIT is initialized and will be used.  */
RUBY_SYMBOL_EXPORT_BEGIN
int mjit_init_p = FALSE;
RUBY_SYMBOL_EXPORT_END

/* PID of major MRI thread which is a client of MJIT. */
static pthread_t client_pid;


/*  A compilation unit is called a JIT unit.  Only one ISEQ can belong
    to a unit.  The unit has a status described by the following
    enumeration.

    The diagram shows the possible status transitions:

    NOT_FORMED -> IN_QUEUE -> IN_GENERATION -> FAILED
                     ^             |            ^
                     |             |            |   load_unit
                     |             |            |
                     |              -------> SUCCESS ----> LOADED
                     |                          |            |
                     |                          V            |
                      ------------------<--------------------
                                change in global speculation
*/

enum unit_status {
    /* The unit is not in the queue.  An ISEQ can be added to it.  */
    UNIT_NOT_FORMED,
    /* The unit is in the queue for compilation.  */
    UNIT_IN_QUEUE,
    /* The unit is being processed by a MJIT worker (C code
       generation, the C code compilation, and the object code
       load). */
    UNIT_IN_GENERATION,
    /* Unit compilation or its load failed.  */
    UNIT_FAILED,
    /* Unit compilation successfully finished.  */
    UNIT_SUCCESS,
    /* The unit ISEQ machine code was successfully loaded.  */
    UNIT_LOADED,
};

/* State of global speculation.
   TODO: Fine grain flags for different bop redefinitions.  */
struct global_spec_state {
    /* The following flags reflect presence of tracing and basic
       operation redefinitions.  */
    unsigned int trace_p:1;
    unsigned int bop_redefined_p:1;
};

/* The unit structure.  */
struct rb_mjit_unit {
    int num; /* unit order number */
    enum unit_status status;
    /* Name of the C code file of the unit ISEQ.  Defined for status
       UNIT_IN_GENERATION.  */
    char *cfname;
    /* Name of the object file of the unit ISEQ.  Defined for status
       UNIT_IN_GENERATION and UNIT_SUCCESS.  */
    char *ofname;
    /* PID of C compiler processing the unit.  Defined for status
       UNIT_IN_GENERATION. */
    pid_t pid;
    /* Units in lists are linked with the following members.  */
    struct rb_mjit_unit *next, *prev;
    /* Dlopen handle of the loaded object file.  Defined for status
       UNIT_LOADED.  */
    void *handle;
    /* ISEQ in the unit.  We have at most one unit iseq.  */
    struct rb_mjit_unit_iseq *unit_iseq;
    /* If the flag is TRUE, we should compile the unit first.  */
    char high_priority_p;
    /* If the flag is TRUE, the corresponding iseq was freed during
       compilation.  */
    char freed_p;
    /* Overall byte code size of ISEQ in the unit in VALUEs.  */
    size_t iseq_size;
    /* The following member is used to generate a code with global
       speculation.  */
    struct global_spec_state spec_state;
    /* The relative time when a worker started to process the unit.
       It is used in the debug mode.  */
    double time_start;
};

/* Info about insn resulted into a mutation.  */
struct mjit_mutation_insns {
    enum ruby_vminsn_type insn;
    size_t pc; /* the relative insn pc.  */
};

/* The structure describing an ISEQ in the unit.  We create at most
   one such structure for a particular iseq.  */
struct rb_mjit_unit_iseq {
    /* Unique order number of unit ISEQ.  */
    int num;
    rb_iseq_t *iseq;
    /* The ISEQ byte code size in VALUEs.  */
    size_t iseq_size;
    struct rb_mjit_unit *unit;
    /* All unit iseqs are chained by the following field.  */
    struct rb_mjit_unit_iseq *next;
    /* The fields used for profiling only:  */
    char *label; /* Name of the ISEQ */
    /* -1 means we speculate that self has few instance variables.
       Positive means we speculate that self has > IVAR_SPEC instance
       variables and IVAR_SPEC > ROBJECT_EMBED_LEN_MAX.  Zero means we
       know nothing about the number.  */
    size_t ivar_spec;
    /* Serial number of self for speculative translations for nonzero
       ivar_spec. */
    rb_serial_t ivar_serial;
    /* True if we use C vars for temporary Ruby variables during last
       iseq translation.  */
    char use_temp_vars_p;
    char used_code_p;
    /* The following flag reflects speculation about equality of ep
       and bp which we used during last iseq translation.  */
    char ep_neq_bp_p;
    /* Number of JIT code mutations (and cancellations).  */
    int jit_mutations_num;
    /* Array of structures describing insns which initiated mutations.
       The array has JIT_MUTATIONS_NUM defined elements.  */
    struct mjit_mutation_insns *mutation_insns;
    /* See the corresponding fields in iseq_constant_body.  The values
       are saved for GCed iseqs.  */
    unsigned long resume_calls, stop_calls, jit_calls, failed_jit_calls;
};

/* Defined in the client thread before starting MJIT threads:  */
/* Used C compiler path.  */
static char *cc_path;
/* Name of the header file.  */
static char *header_fname;
/* Name of the precompiled header file.  */
static char *pch_fname;

/* Return length of NULL-terminated array ARGS excluding the NULL
   marker.  */
static size_t
args_len(char *const *args) {
    size_t i;

    for (i = 0; (args[i]) != NULL;i++)
	;
    return i;
}

/* Concatenate NUM passed NULL-terminated arrays of strings, put the
   result (with NULL end marker) into the heap, and return the
   result.  */
static char **
form_args(int num, ...) {
    va_list argp, argp2;
    size_t len, disp;
    int i;
    char **args, **res;

    va_start(argp, num);
    va_copy(argp2, argp);
    for (i = len = 0; i < num; i++) {
	args = va_arg(argp, char **);
	len += args_len(args);
    }
    va_end(argp);
    if ((res = xmalloc((len + 1) * sizeof(char *))) == NULL)
	return NULL;
    for (i = disp = 0; i < num; i++) {
	args = va_arg(argp2, char **);
	len = args_len(args);
	memmove(res + disp, args, len * sizeof(char *));
	disp += len;
    }
    res[disp] = NULL;
    va_end(argp2);
    return res;
}

/* Make and return copy of STR in the heap.  Return NULL in case of a
   failure.  */
static char *
get_string(const char *str) {
    char *res;

    if ((res = xmalloc(strlen(str) + 1)) != NULL)
	strcpy(res, str);
    return res;
}

/* Return an unique file name in /tmp with PREFIX and SUFFIX and
   number ID.  Use getpid if ID == 0.  The return file name exists
   until the next function call.  */
static char *
get_uniq_fname(unsigned long id, const char *prefix, const char *suffix) {
    char str[70];

    sprintf(str, "/tmp/%sp%luu%lu%s", prefix, (unsigned long) getpid(), id, suffix);
    return get_string(str);
}

/* Maximum length for C function name generated for an ISEQ.  */
#define MAX_MJIT_FNAME_LEN 30

/* Put C function name of unit iseq UI into HOLDER.  Return
   HOLDER.  */
static char *
get_unit_iseq_fname(struct rb_mjit_unit_iseq *ui, char *holder) {
    sprintf(holder, "_%d", ui->num);
    return holder;
}

/* A mutex for conitionals and critical sections.  */
static pthread_mutex_t mjit_engine_mutex;
/* A thread conditional to wake up workers if at the end of PCH thread.  */
static pthread_cond_t mjit_pch_wakeup;
/* A thread conditional to wake up the client if there is a change in
   executed unit status.  */
static pthread_cond_t mjit_client_wakeup;
/* A thread conditional to wake up a worker if there we have something
   to add or we need to stop MJIT engine.  */
static pthread_cond_t mjit_worker_wakeup;
/* A thread conditional to wake up workers if at the end of GC.  */
static pthread_cond_t mjit_gc_wakeup;
/* Stop generation of a JITted code.  */
static int stop_mjit_generation_p;
/* True when GC is working.  */
static int in_gc;
/* Number of units being currently translated.  */
static int units_in_translation;

/* Doubly linked list of units.  */
struct rb_mjit_unit_list {
    struct rb_mjit_unit *head, *tail;
    int length; /* the list length */
};

/* The unit queue.  The client and MJIT threads work on the queue.
   So code using the following variable should be synced.  */
static struct rb_mjit_unit_list unit_queue;

/* The client and MJIT threads work on the list of active units
   (doubly linked list).  So code using the following variable should
   be synced.  All units with status UNIT_LOADED are in this list.
   They have also non-null handles.  */
static struct rb_mjit_unit_list active_units;

/* The client and MJIT threads work on the list of obsolete units
   (doubly linked list).  So code using the following variable should
   be synced.  All units in this list have status UNIT_FAILED.  They
   have also non-null handles.  */
static struct rb_mjit_unit_list obsolete_units;

/* The following functions are low level (ignoring thread
   synchronization) functions working with the lists.  */

/* Make non-zero to check consistency of MJIT lists.  */
#define MJIT_CHECK_LISTS 0

#if MJIT_CHECK_LISTS

/* Check double linked LIST consistency.  */
static void
check_list(struct rb_mjit_unit_list *list) {
    if (list->head == NULL)
	assert(list->tail == NULL);
    else {
	struct rb_mjit_unit *u, *pu;

	assert(list->tail != NULL);
	assert(list->head->prev == NULL);
	assert(list->tail->next == NULL);
	for (pu = NULL, u = list->head; u != NULL; pu = u, u = u->next) {
	    assert(u->prev == pu);
	}
	for (u = NULL, pu = list->tail; pu != NULL; u = pu, pu = pu->prev) {
	    assert(pu->next == u);
	}
    }
}

#endif

/* Initiate LIST.  */
static void
init_list(struct rb_mjit_unit_list *list) {
    list->tail = list->head = NULL;
    list->length = 0;
}

/* Add unit U to the tail of doubly linked LIST.  It should be not in
   the list before.  */
static void
add_to_list(struct rb_mjit_unit *u, struct rb_mjit_unit_list *list) {
    u->next = u->prev = NULL;
    if (list->head == NULL)
	list->head = list->tail = u;
    else {
	list->tail->next = u;
	u->prev = list->tail;
	list->tail = u;
    }
    list->length++;
#if MJIT_CHECK_LISTS
    check_list(list);
#endif
}

/* Remove unit U from the doubly linked LIST.  It should be in the
   list before.  */
static void
remove_from_list(struct rb_mjit_unit *u, struct rb_mjit_unit_list *list) {
    if (u == list->head)
	list->head = u->next;
    else
	u->prev->next = u->next;
    if (u == list->tail)
	list->tail = u->prev;
    else
	u->next->prev = u->prev;
    list->length--;
#if MJIT_CHECK_LISTS
    check_list(list);
#endif
}

/* Remove and return the best unit from doubly linked LIST.  The best
   is the first high priority unit or the unit whose iseq has the
   biggest number of calls so far.  */
static struct rb_mjit_unit *
get_from_list(struct rb_mjit_unit_list *list) {
    struct rb_mjit_unit *u;
    struct rb_mjit_unit *best_u = NULL;
    struct rb_mjit_unit_iseq *ui;
    unsigned long calls_num, best_calls_num;

#if MJIT_CHECK_LISTS
    check_list(list);
#endif
    for (u = list->head; u != NULL; u = u->next) {
	if (u->high_priority_p) {
	    best_u = u;
	    u->high_priority_p = FALSE;
	    break;
	}
	calls_num = 0;
	ui = u->unit_iseq;
	assert(ui != NULL);
	if (ui->iseq != NULL)
	    calls_num += ui->iseq->body->resume_calls + ui->iseq->body->stop_calls;
	if (best_u == NULL || calls_num > best_calls_num) {
	    best_u = u;
	    best_calls_num = calls_num;
	}
    }
    if (best_u != NULL)
	remove_from_list(best_u, list);
    return best_u;
}

/* Print ARGS according to FORMAT to stderr.  */
static void
va_list_log(const char *format, va_list args) {
    char str[256];

    vsprintf(str, format, args);
    /* Use one call for non-interrupted output:  */
    fprintf(stderr, "%s%s: time - %.3f ms\n",
	    pthread_self() == client_pid ? "" : "+++",
	    str, relative_ms_time());
}

/* Print the arguments according to FORMAT to stderr only if MJIT
   verbose option value is more or equal to LEVEL.  */
static void
verbose(int level, const char *format, ...) {
    va_list args;

    va_start(args, format);
    if (mjit_opts.verbose >= level)
	va_list_log(format, args);
    va_end(args);
}

/* Print the arguments according to FORMAT to stderr only if the
   message LEVEL is not greater to the current debug level.  */
static void
debug(int level, const char *format, ...) {
    va_list args;

    if (! mjit_opts.debug || ! mjit_opts.verbose)
	return;
    va_start(args, format);
    if (debug_level >= level)
	va_list_log(format, args);
    va_end(args);
}

/* Start a critical section.  Use message MSG to print debug info at
   LEVEL.  */
static inline void
CRITICAL_SECTION_START(int level, const char *msg) {
    int err_code;

    debug(level, "Locking %s", msg);
    if ((err_code = pthread_mutex_lock(&mjit_engine_mutex)) != 0) {
	fprintf(stderr, "%sCannot lock MJIT mutex %s: time - %.3f ms\n",
		pthread_self() == client_pid ? "" : "++", msg,
		relative_ms_time());
	fprintf(stderr, "%serror: %s\n",
		pthread_self() == client_pid ? "" : "++", strerror(err_code));
    }
    debug(level, "Locked %s", msg);
}

/* Finish the current critical section.  Use message MSG to print
   debug info at LEVEL. */
static inline void
CRITICAL_SECTION_FINISH(int level, const char *msg) {
    debug(level, "Unlocked %s", msg);
    pthread_mutex_unlock(&mjit_engine_mutex);
}

/* XXX_COMMONN_ARGS define the command line arguments of XXX C
   compiler used by MJIT.

   XXX_EMIT_PCH_ARGS define additional options to generate the
   precomiled header.

   XXX_USE_PCH_ARAGS define additional options to use the precomiled
   header.  */
static const char *GCC_COMMON_ARGS_DEBUG[] = {"gcc", "-O0", "-g", "-Wfatal-errors", "-fPIC", "-shared", "-w", "-pipe", "-nostartfiles", "-nodefaultlibs", "-nostdlib", NULL};
static const char *GCC_COMMON_ARGS[] = {"gcc", "-O2", "-Wfatal-errors", "-fPIC", "-shared", "-w", "-pipe", "-nostartfiles", "-nodefaultlibs", "-nostdlib", NULL};
static const char *GCC_USE_PCH_ARGS[] = {"-I/tmp", NULL};
static const char *GCC_EMIT_PCH_ARGS[] = {NULL};

#ifdef __MACH__

static const char *LLVM_COMMON_ARGS_DEBUG[] = {"clang", "-O0", "-g", "-dynamic", "-I/usr/local/include", "-L/usr/local/lib", "-w", "-bundle", NULL};
static const char *LLVM_COMMON_ARGS[] = {"clang", "-O2", "-dynamic", "-I/usr/local/include", "-L/usr/local/lib", "-w", "-bundle", NULL};

#else

static const char *LLVM_COMMON_ARGS_DEBUG[] = {"clang", "-O0", "-g", "-fPIC", "-shared", "-I/usr/local/include", "-L/usr/local/lib", "-w", "-bundle", NULL};
static const char *LLVM_COMMON_ARGS[] = {"clang", "-O2", "-fPIC", "-shared", "-I/usr/local/include", "-L/usr/local/lib", "-w", "-bundle", NULL};

#endif /* #if __MACH__ */

static const char *LLVM_USE_PCH_ARGS[] = {"-include-pch", NULL, "-Wl,-undefined", "-Wl,dynamic_lookup", NULL};
static const char *LLVM_EMIT_PCH_ARGS[] = {"-emit-pch", NULL};

/* All or most code for execution of any byte code insn is contained
   in the corresponding C function (see rtl_exec.c).  The following
   structure describes how to use the function (mostly its parameters
   passing) to implement the insn.  */
struct insn_fun_features {
    /* Pass argument th.  */
    char th_p;
    /* True if the first insn operand is a continuation insn (see
       comments of insns.def).  We don't pass such operands.  */
    char skip_first_p;
    /* Just go to spec section if the function returns non-zero.  */
    char op_end_p;
    /* Pass structure calling and call function mjit_call_method
       afterwards.  */
    char call_p;
    /* Defined only for call insns.  True if the call recv should be
       present on the stack.  */
    char recv_p;
    /* Jump the dest (1st of 2nd insn operand) if the function returns
       non-zero.  */
    char jmp_p;
    /* It is a bcmp insn, call function mjit_bcmp_end if it is
       necessary.  */
    char bcmp_p;
    /* A value passed to function jmp_bcmp_end.  */
    char jmp_true_p;
    /* Use a separate code to generate C code for the insn.  */
    char special_p;
    /* Flag of an insn which can become a speculative one.  */
    char changing_p;
    /* Flag of a speculative insn.  */
    char speculative_p;
    /* Flag of a simple insn whose all operands are given by one
       operand number.  */
    char simple_p;
};

/* Initiate S with no speculation.  */
static void
init_global_spec_state(struct global_spec_state *s) {
    s->trace_p = s->bop_redefined_p = TRUE;
}

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

/* Return TRUE if speculations described by S can be used in
   CURR_STATE.  */
static int
valid_global_spec_state_p(const struct global_spec_state *s,
			  const struct global_spec_state *curr_state) {
    return ((s->trace_p || ! curr_state->trace_p)
	    && (s->bop_redefined_p || ! curr_state->bop_redefined_p));
}

/*-------All the following code is executed in the worker threads only-----------*/

/* Return features of C function corresponding to the byte code INSN
   through F.  */
static void
get_insn_fun_features(VALUE insn, struct insn_fun_features *f) {
    f->th_p = f->skip_first_p = f->op_end_p = f->bcmp_p = FALSE;
    f->call_p = f->recv_p = f->jmp_p = f->jmp_true_p = FALSE;
    f->special_p = f->changing_p = f->speculative_p = f->simple_p = FALSE;
    switch (insn) {
    case BIN(const2var):
    case BIN(const_ld_val):
    case BIN(const_cached_val_ld):
    case BIN(special2var):
    case BIN(var2special):
    case BIN(define_class):
    case BIN(defined_p):
    case BIN(val_defined_p):
	f->th_p = TRUE;
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
	f->th_p = f->call_p = TRUE;
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
    case BIN(plusf):
    case BIN(minusi):
    case BIN(minusf):
    case BIN(multi):
    case BIN(multf):
    case BIN(divi):
    case BIN(divf):
    case BIN(modi):
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
	/* fall through: */
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
    case BIN(uplusf):
    case BIN(uminusi):
    case BIN(uminusf):
    case BIN(umulti):
    case BIN(umultf):
    case BIN(udivi):
    case BIN(udivf):
    case BIN(umodi):
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
	f->th_p = f->op_end_p = TRUE;
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
	/* fall through: */
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
	f->simple_p = f->th_p = f->op_end_p = TRUE;
	break;
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
    case BIN(fplus):
    case BIN(fminus):
    case BIN(fmult):
    case BIN(fdiv):
    case BIN(fmod):
    case BIN(feq):
    case BIN(fne):
    case BIN(flt):
    case BIN(fgt):
    case BIN(fle):
    case BIN(fge):
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
    case BIN(fplusf):
    case BIN(fminusf):
    case BIN(fmultf):
    case BIN(fdivf):
    case BIN(fmodf):
    case BIN(feqf):
    case BIN(fnef):
    case BIN(fltf):
    case BIN(fgtf):
    case BIN(flef):
    case BIN(fgef):
	f->speculative_p = TRUE;
	break;
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
    case BIN(sfplus):
    case BIN(sfminus):
    case BIN(sfmult):
    case BIN(sfdiv):
    case BIN(sfmod):
    case BIN(sfeq):
    case BIN(sfne):
    case BIN(sflt):
    case BIN(sfgt):
    case BIN(sfle):
    case BIN(sfge):
	f->simple_p = f->speculative_p = TRUE;
	break;
    case BIN(indset):
    case BIN(indseti):
    case BIN(indsets):
	f->changing_p = TRUE;
	/* fall through: */
    case BIN(uindset):
    case BIN(uindseti):
    case BIN(uindsets):
	f->th_p = f->op_end_p = TRUE;
	break;
    case BIN(aindset):
    case BIN(hindset):
    case BIN(aindseti):
    case BIN(hindseti):
    case BIN(hindsets):
	f->speculative_p = TRUE;
	break;
    case BIN(trace):
    case BIN(goto):
    case BIN(get_inline_cache):
    case BIN(case_dispatch):
	f->special_p = TRUE;
	break;
    case BIN(bt):
    case BIN(bf):
    case BIN(bnil):
    case BIN(bkw):
	f->th_p = f->jmp_p = TRUE;
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
	/* fall through: */
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
	f->th_p = f->jmp_p = f->jmp_true_p = f->bcmp_p = f->skip_first_p = TRUE;
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
	/* fall through: */
	f->changing_p = TRUE;
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
	f->th_p = f->jmp_p = f->bcmp_p = f->skip_first_p = TRUE;
	break;
    case BIN(ibteq):
    case BIN(ibtne):
    case BIN(ibtlt):
    case BIN(ibtgt):
    case BIN(ibtle):
    case BIN(ibtge):
    case BIN(fbteq):
    case BIN(fbtne):
    case BIN(fbtlt):
    case BIN(fbtgt):
    case BIN(fbtle):
    case BIN(fbtge):
    case BIN(ibteqi):
    case BIN(ibtnei):
    case BIN(ibtlti):
    case BIN(ibtgti):
    case BIN(ibtlei):
    case BIN(ibtgei):
    case BIN(fbteqf):
    case BIN(fbtnef):
    case BIN(fbtltf):
    case BIN(fbtgtf):
    case BIN(fbtlef):
    case BIN(fbtgef):
	f->jmp_true_p = TRUE;
	/* fall through: */
    case BIN(ibfeq):
    case BIN(ibfne):
    case BIN(ibflt):
    case BIN(ibfgt):
    case BIN(ibfle):
    case BIN(ibfge):
    case BIN(fbfeq):
    case BIN(fbfne):
    case BIN(fbflt):
    case BIN(fbfgt):
    case BIN(fbfle):
    case BIN(fbfge):
    case BIN(ibfeqi):
    case BIN(ibfnei):
    case BIN(ibflti):
    case BIN(ibfgti):
    case BIN(ibflei):
    case BIN(ibfgei):
    case BIN(fbfeqf):
    case BIN(fbfnef):
    case BIN(fbfltf):
    case BIN(fbfgtf):
    case BIN(fbflef):
    case BIN(fbfgef):
	f->jmp_p = f->bcmp_p = f->skip_first_p = f->speculative_p = TRUE;
	break;
    case BIN(nop):
    case BIN(temp_ret):
    case BIN(loc_ret):
    case BIN(val_ret):
    case BIN(raise_except):
    case BIN(raise_except_val):
    case BIN(ret_to_loc):
    case BIN(ret_to_temp):
    case BIN(call_block):
	f->call_p = f->special_p = TRUE;
	break;
    case BIN(var2ivar):
    case BIN(val2ivar):
    case BIN(ivar2var):
    case BIN(var2var):
    case BIN(var_swap):
    case BIN(temp2temp):
    case BIN(temp_swap):
    case BIN(temp_reverse):
    case BIN(loc2loc):
    case BIN(loc2temp):
    case BIN(temp2loc):
    case BIN(uploc2temp):
    case BIN(uploc2var):
    case BIN(val2temp):
    case BIN(val2loc):
    case BIN(str2var):
    case BIN(set_inline_cache):
    case BIN(specialobj2var):
    case BIN(self2var):
    case BIN(global2var):
    case BIN(cvar2var):
    case BIN(iseq2var):
    case BIN(var2uploc):
    case BIN(val2uploc):
    case BIN(var2const):
    case BIN(var2global):
    case BIN(var2cvar):
    case BIN(make_range):
    case BIN(make_array):
    case BIN(make_hash):
    case BIN(new_array_min):
    case BIN(new_array_max):
    case BIN(clone_array):
    case BIN(spread_array):
    case BIN(splat_array):
    case BIN(concat_array):
    case BIN(check_match):
    case BIN(regexp_match1):
    case BIN(to_string):
    case BIN(concat_strings):
    case BIN(to_regexp):
    case BIN(str_freeze_call):
    case BIN(freeze_string):
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

/* If SET_P is FALSE, return empty string.  Otherwise, return string
   representing C code setting cfp pc to PC.  Use BUF as the string
   container.  */
static const char *
generate_set_pc(int set_p, char *buf, const VALUE *pc) {
    if (! set_p)
	return "";
    sprintf(buf, "  cfp->pc = (void *) 0x%"PRIxVALUE ";\n", (VALUE) pc);
    return buf;
}

/* Return number of mutations which insn with POS from UI iseq
   caused.  */
static int
get_insn_mutation_num(struct rb_mjit_unit_iseq *ui, size_t pos) {
    int i, num = 0;
    
    for (i = 0; i < ui->jit_mutations_num; i++)
	if (ui->mutation_insns[i].insn != BIN(nop) && pos == ui->mutation_insns[i].pc)
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

/* Output C code implementing an iseq UI insn starting with position
   POS to file F.  Generate the code according to TCP.  */
static int
translate_iseq_insn(FILE *f, size_t pos, struct rb_mjit_unit_iseq *ui,
		    struct translation_control *tcp) {
    rb_iseq_t *iseq = ui->iseq;
    const VALUE *code = iseq->body->rtl_encoded;
    VALUE insn, op;
    int len, i, ivar_p, const_p, insn_mutation_num;
    const char *types;
    const char *iname;
    struct insn_fun_features features;
    struct rb_call_cache local_cc;
    struct iseq_inline_cache_entry local_ic;
    CALL_CACHE cc = NULL;
    CALL_INFO ci = NULL;
    char buf[150];
    
    insn_mutation_num = get_insn_mutation_num(ui, pos);
    insn = code[pos];
#if OPT_DIRECT_THREADED_CODE || OPT_CALL_THREADED_CODE
    insn = rb_vm_insn_addr2insn((void *) insn);
#endif
    if (tcp->safe_p || insn_mutation_num != 0)
	insn = get_safe_insn(insn);
    len = insn_len(insn);
    types = insn_op_types(insn);
    iname = insn_name(insn);
    fprintf(f, "l%ld:\n", pos);
    if (mjit_opts.debug)
	fprintf(f, "  /* %s:%u */\n", ui->label, rb_iseq_line_no(iseq, pos));
#if MJIT_INSN_STATISTICS
    fprintf(f, "  jit_insns_num++;\n");
#endif
    get_insn_fun_features(insn, &features);
    ivar_p = const_p = FALSE;
    if (! features.speculative_p
	&& (insn != BIN(trace) || ui->unit->spec_state.trace_p))
	fprintf(f, "%s", generate_set_pc(TRUE, buf, &code[pos] + len));
    if (features.call_p && ! features.special_p) {
	/* CD is always the 1st operand.  */
	cc = &((CALL_DATA) code[pos + 1])->call_cache;
	ci = &((CALL_DATA) code[pos + 1])->call_info;
	/* Remember cc can change in the interpreter thread in
	   parallel.  TODO: Make the following atomic.  */
	local_cc = *cc;
    } else if (insn == BIN(ivar2var) || insn == BIN(var2ivar) || insn == BIN(val2ivar)) {
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
	    fprintf(f, "  ruby_vm_check_ints(th);\n");
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
		fprintf(f, ")) {\n  %s", generate_set_pc(TRUE, buf, &code[pos]));
		fprintf(f, "    %sgoto stop_spec;\n  }\n", set_failed_insn_str(buf, pos));
	    }
	    fprintf(f, "  goto l%ld;\n", dest);
	    break;
	}
	case BIN(trace):
	    fprintf(f, "  if (%s_f(th, cfp, %"PRIdVALUE ")) goto cancel;\n", iname, code[pos + 1]);
	    break;
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
	    fprintf(f, "  %s_f(th, cfp, %s, &val);\n  return val;\n",
		    iname, get_op_str(buf, code[pos + 1], tcp));
	    break;
	case BIN(loc_ret):
	    fprintf(f, "  %s_f(th, cfp, %s, &val);\n  return val;\n",
		    iname, get_op_str(buf, code[pos + 1], tcp));
	    break;
	case BIN(val_ret):
	    fprintf(f, "  %s_f(th, cfp, %"PRIuVALUE ", &val);\n  return val;\n",
		    iname, code[pos + 1]);
	    break;
	case BIN(raise_except):
	    fprintf(f, "  val = %s_f(th, cfp, %s, %"PRIuVALUE ");\n",
		    iname, get_op_str(buf, code[pos + 1], tcp), code[pos + 2]);
	    fprintf(f, "  th->errinfo = val; rb_threadptr_tag_jump(th, th->state);\n");
	    break;
	case BIN(raise_except_val):
	    fprintf(f, "  val = %s_f(th, cfp, 0x%"PRIxVALUE ", %"PRIuVALUE ");\n",
		    iname, code[pos + 1], code[pos + 2]);
	    fprintf(f, "  th->errinfo = val; rb_threadptr_tag_jump(th, th->state);\n");
	    break;
	case BIN(ret_to_loc):
	case BIN(ret_to_temp):
	    fprintf(f, "  %s_f(th, cfp, (sindex_t) %"PRIdVALUE ", %s);\n  return RUBY_Qnil;\n",
		    iname, code[pos + 1], get_op_str(buf, code[pos + 2], tcp));
	    break;
	case BIN(call_block):
	    /* Generate copying temps to the stack.  */
	    if (tcp->use_temp_vars_p)
		generate_param_setup(f, code, pos, TRUE);
	    fprintf(f, "  val = %s_f(th, cfp, (void *) 0x%"PRIxVALUE ", (sindex_t) %"PRIdVALUE ");\n",
	            iname, code[pos + 1], code[pos + 2]);
	    fprintf(f, "  if (mjit_call_block_end(th, cfp, val, %s)) {\n",
	            get_op_str(buf, code[pos + 2], tcp));
	    fprintf(f, "    %sgoto stop_spec;\n  }\n", set_failed_insn_str(buf, pos));
	    break;
	default:
	    break;
	}
    } else if (!tcp->safe_p && features.call_p && insn_mutation_num == 0
	       && ruby_vm_global_method_state == local_cc.method_state
	       && (local_cc.call == vm_call_cfunc
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
	
	if (tcp->use_temp_vars_p)
	    /* Generate copying temps to the stack.  */
	    generate_param_setup(f, code, pos, features.recv_p);
	rec = (self_p ? "&cfp->self" : get_op_str(buf, recv_op, tcp));
	fprintf(f, "  if (mjit_check_cc_attr_p(*%s, %llu, %llu)) {\n",
		rec, (unsigned long long) local_cc.method_state,
		(unsigned long long) local_cc.class_serial);
	fprintf(f, "  %s", generate_set_pc(TRUE, buf, &code[pos]));
	fprintf(f, "    %sgoto stop_spec;\n  }\n", set_failed_insn_str(buf, pos));
	rec = (self_p ? "&cfp->self" : get_op_str(buf, recv_op, tcp));
	if (local_cc.call == vm_call_cfunc) {
	    fprintf(f, "  if (mjit_call_cfunc(th, cfp, %llu, (void *) 0x%"PRIxVALUE
		    ", %u, %d, 0x%x, (void *) 0x%"PRIxVALUE
		    ", %ld, (void *) 0x%"PRIxVALUE ", *%s, %d, %d",
		    (unsigned long long) ci->mid, (VALUE) local_cc.me,
		    iseq->body->temp_vars_num, ci->orig_argc, ci->flag, (VALUE) &((struct rb_call_data_with_kwarg *)ci)->kw_arg,
		    call_start, block_iseq, rec, !features.recv_p, simple_p);
	} else {
	    const rb_iseq_t *callee_iseq = rb_iseq_check(local_cc.me->def->body.iseq.iseqptr);
	    struct rb_iseq_constant_body *callee_body = callee_iseq->body;

	    fprintf(f, "  if (mjit_iseq_call(th, cfp, (void *) 0x%"PRIxVALUE ", (void *) 0x%"PRIxVALUE
		    ", (void *) 0x%"PRIxVALUE ", %d, (void *) 0x%"PRIxVALUE
		    ", %d, %d, %d, %d, %u, %d, 0x%x, %ld, (void *) 0x%"PRIxVALUE
		    ", *%s, %d, %d",
		    (VALUE) local_cc.me, (VALUE) callee_iseq,
		    (VALUE) callee_body, callee_body->except_p, (VALUE) callee_body->rtl_encoded,
		    callee_body->type, callee_body->param.size, callee_body->local_table_size,
		    iseq->body->temp_vars_num, callee_body->stack_max,
		    ci->orig_argc, ci->flag, call_start, block_iseq,
		    rec, !features.recv_p, simple_p);
	}
	fprintf(f, ", %s)) {\n", get_op_str(buf, call_start, tcp));
	fprintf(f, "    %sgoto stop_spec;\n  }\n", set_failed_insn_str(buf, pos));
    } else if (!tcp->safe_p && features.call_p && insn_mutation_num == 0
	       && ruby_vm_global_method_state == local_cc.method_state
	       && (local_cc.call == vm_call_ivar || local_cc.call == vm_call_attrset)
	       && local_cc.aux.index > 0) {
	ptrdiff_t call_start = code[pos + 2];
	long call_ivar_obj_op = features.recv_p ? code[pos + 2] : code[pos + 3];
	const char *rec = (insn == BIN(simple_call_self)
			   ? "&cfp->self" : get_op_str(buf, call_ivar_obj_op, tcp));
	assert(insn == BIN(simple_call_recv) || insn == BIN(simple_call) || insn == BIN(simple_call_self) || insn == BIN(call_super));
	fprintf(f, "  if (mjit_check_cc_attr_p(*%s, %llu, %llu) || ",
		rec, (unsigned long long) local_cc.method_state,
		(unsigned long long) local_cc.class_serial);
	if (local_cc.call == vm_call_ivar) {
	    fprintf(f, "mjit_call_ivar(*%s, %u, ", rec, (unsigned) local_cc.aux.index);
	    fprintf(f, "%s)) {\n", get_op_str(buf, call_start, tcp));
	} else {
	    fprintf(f, "mjit_call_setivar(*%s, %u, ", rec, (unsigned) local_cc.aux.index);
	    fprintf(f, "*%s)) {\n", get_op_str(buf, call_start - 1, tcp));
	}
	fprintf(f, "  %s", generate_set_pc(TRUE, buf, &code[pos]));
	fprintf(f, "    %sgoto stop_spec;\n  }\n", set_failed_insn_str(buf, pos));
    } else if (!tcp->safe_p && ivar_p && insn_mutation_num == 0) {
	assert(insn == BIN(ivar2var) || insn == BIN(var2ivar) || insn == BIN(val2ivar));
	if (ui->ivar_spec != 0) {
	    if (insn == BIN(ivar2var))
		fprintf(f, "  mjit_ivar2var_no_check(cfp, self, %d, %llu, %s);\n",
			ui->ivar_spec != (size_t) -1, (unsigned long long) local_ic.ic_value.index,
			get_op_str(buf, code[pos + 1], tcp));
	    else {
		fprintf(f, "  mjit_%s_no_check(cfp, self, %d, %llu, ",
			iname, ui->ivar_spec != (size_t) -1, (unsigned long long) local_ic.ic_value.index);
		if (insn == BIN(var2ivar))
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
		if (insn == BIN(var2ivar))
		    fprintf (f, "%s", get_op_str(buf, code[pos + 3], tcp));
		else
		    fprintf (f, "0x%"PRIxVALUE, code[pos + 3]);
	    }
	    fprintf(f, ")) {\n  %s", generate_set_pc(TRUE, buf, &code[pos]));
	    fprintf(f, "    %sgoto stop_spec;\n  }\n", set_failed_insn_str(buf, pos));
	}
    } else if (!tcp->safe_p && const_p && insn_mutation_num == 0 && insn == BIN(const_cached_val_ld)
	       && local_ic.ic_serial == ruby_vm_global_constant_state) {
	assert(insn == BIN(const_cached_val_ld));
	fprintf(f, "  if (mjit_const_cached_val_ld(cfp, %llu, %llu, 0x%"PRIxVALUE ", %s, %ld",
		(unsigned long long) local_ic.ic_serial,
		(unsigned long long) local_ic.ic_cref, local_ic.ic_value.value,
		get_op_str(buf, code[pos + 1], tcp), code[pos + 1]);
	fprintf(f, ")) {\n  %s", generate_set_pc(TRUE, buf, &code[pos]));
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
	if (features.th_p)
	    fprintf(f, "th, ");
	fprintf(f, "cfp");
	if (features.call_p)
	    fprintf(f, ", &calling");
	for (i = (features.jmp_p ? 2 : 1) + (features.skip_first_p ? 1 : 0); i < len; i++) {
	    op = code[pos + i];
	    if (types[i - 1] != TS_VARIABLE)
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
		fprintf(f, "(void *) 0x%"PRIxVALUE, op);
		break;
	    case TS_CDHASH:
	    case TS_VALUE:
	    case TS_ISEQ:
	    case TS_CALLINFO:
	    case TS_CALLCACHE:
	    case TS_CALLDATA:
	    case TS_GENTRY:
		fprintf(f, "(void *) 0x%"PRIxVALUE, op);
		break;
	    case TS_VARIABLE:
		break;
	    case TS_INSN:
		/* An insn operand should be never processed.  */
	    default:
		fprintf(stderr, "Unknown %d operand %c of %s\n", i, types[i - 1], iname);
		break;
	    }
	}
	if (features.op_end_p)
	    fprintf(f, ")) {\n    %sgoto stop_spec;\n  }\n", set_failed_insn_str(buf, pos));
	else if (features.speculative_p) {
	    if (features.jmp_p)
		fprintf(f, ", &val, &new_insn);\n  if (val == RUBY_Qundef) {\n");
	    else
		fprintf(f, ", &new_insn)) {\n");
	    fprintf(f, "  %s", generate_set_pc(TRUE, buf, &code[pos]));
	    fprintf(f, "    vm_change_insn(cfp->iseq, (void *) 0x%"PRIxVALUE ", new_insn);\n",
		    (VALUE) &code[pos]);
	    fprintf(f, "    %sgoto stop_spec;\n  }\n", set_failed_insn_str(buf, pos));
	    if (features.jmp_p) {
		unsigned long dest = pos + len + code[pos + 2];
		fprintf(f, "  if (flag) goto l%ld;\n", dest);
	    }
	} else if (! features.jmp_p)
	    fprintf(f, ");\n");
	else if (! features.bcmp_p)
	    fprintf(f, "))\n    goto l%ld;\n", pos + len + code[pos + 1]);
	else {
            unsigned long dest = pos + len + code[pos + 2];
	    
	    fprintf(f, ", &val);\n  if (val == RUBY_Qundef) {\n");
	    fprintf(f, "    if (flag)%s", generate_set_pc(TRUE, buf, &code[dest]));
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
		fprintf(f, " {\n  %s", generate_set_pc(TRUE, buf, &code[pos]));
		fprintf(f, "    %sgoto stop_spec;\n  }\n", set_failed_insn_str(buf, pos));
		fprintf(f, "  if (mjit_call_iseq_normal(th, cfp, &calling, (void *) 0x%"PRIxVALUE ", %d, %d, %s)) {\n",
			code[pos + 1], callee_iseq->body->param.size, callee_iseq->body->local_table_size,
			get_op_str(buf, call_start, tcp));
		fprintf(f, "    %sgoto stop_spec;\n  }\n", set_failed_insn_str(buf, pos));
	    } else {
		fprintf(f, "  if (mjit_call_method(th, cfp, &calling, (void *) 0x%"PRIxVALUE ", %s)) {\n",
			code[pos + 1], get_op_str(buf, call_start, tcp));
		fprintf(f, "    goto cancel;\n  }\n");
	    }
        }
    }
    return len;
}

/* Translate iseq of unit U into C code and output it to the
   corresponding file.  Add include directives with INCLUDE_FNAME
   unless it is NULL.  Return 0 for a success.  Otherwise return IO
   error code.  */
static int
translate_unit_iseq(struct rb_mjit_unit *u, const char *include_fname) {
    struct rb_mjit_unit_iseq *ui;
    int fd, err, ep_neq_bp_p;
    FILE *f = fopen(u->cfname, "w");
    char mjit_fname_holder[MAX_MJIT_FNAME_LEN];

    if (f == NULL)
	return errno;
    if (include_fname != NULL) {
	const char *s;

	fprintf(f, "#include \"");
	for (s = pch_fname; strcmp(s, ".gch") != 0; s++)
	    fprintf(f, "%c", *s);
	fprintf(f, "\"\n");
    }
#if MJIT_INSN_STATISTICS
    fprintf(f, "extern unsigned long jit_insns_num;\n");
#endif
    setup_global_spec_state(&u->spec_state);
    ep_neq_bp_p = FALSE;
    ui = u->unit_iseq;
    assert (ui != NULL);
    if (ui->ep_neq_bp_p) {
	ep_neq_bp_p = TRUE;
    }
    fprintf(f, "static const char mjit_profile_p = %d;\n", mjit_opts.profile);
    fprintf(f, "static const char mjit_trace_p = %u;\n", u->spec_state.trace_p);
    fprintf(f, "static const char mjit_bop_redefined_p = %u;\n", u->spec_state.bop_redefined_p);
    fprintf(f, "static const char mjit_ep_neq_bp_p = %d;\n", ep_neq_bp_p);
    if (ui->iseq != NULL) {
	struct rb_iseq_constant_body *body;
	size_t i, size = ui->iseq_size;
	struct translation_control tc;
	
       	body = ui->iseq->body;
	tc.safe_p = ui->jit_mutations_num >= mjit_opts.max_mutations;
	tc.use_temp_vars_p = ui->use_temp_vars_p;
	/* If the current iseq contains a block, we should not use C
	   vars for local Ruby vars because a binding can be created
	   in a block and used inside for access to a variable of the
	   current iseq.  The current frame local vars will be saved
	   as bp and ep equality is changed into their inequality
	   after the binding call.  */
	tc.use_local_vars_p = ! body->parent_iseq_p && ! ep_neq_bp_p && tc.use_temp_vars_p;
	debug(3, "translating %d(0x%lx)", ui->num, (long unsigned) ui->iseq);
	fprintf(f, "VALUE %s(rb_thread_t *th, rb_control_frame_t *cfp) {\n",
		get_unit_iseq_fname(ui, mjit_fname_holder));
	fprintf(f, "  struct rb_calling_info calling;\n  VALUE val; int flag;\n");
	fprintf(f, "  enum ruby_vminsn_type new_insn;\n");
	fprintf(f, "  static const char mutation_num = %d;\n", ui->jit_mutations_num);
	fprintf(f, "  size_t failed_insn_pc;\n");
	fprintf(f, "  VALUE self = cfp->self;\n");
	if (tc.use_local_vars_p)
	    for (i = 0; i < body->local_table_size; i++)
		fprintf(f, "  VALUE v%lu;\n", i);
	if (tc.use_temp_vars_p)
	    for (i = 0; i <= body->temp_vars_num; i++)
		fprintf(f, "  VALUE t%lu;\n", i);
	if (! ep_neq_bp_p) {
	    fprintf(f, "  if (cfp->bp != cfp->ep) {\n");
	    fprintf(f, "    mjit_ep_eq_bp_fail(cfp->iseq); return RUBY_Qundef;\n  }\n");
	}
	if (ui->ivar_spec != 0) {
	    fprintf(f, "  if (mjit_check_self_p(self, %llu, %lu)) {\n",
		    (unsigned long long) ui->ivar_serial, ui->ivar_spec);
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
	for (i = 0; i < size;)
	    i += translate_iseq_insn(f, i, ui, &tc);
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
	fprintf(f, "  return RUBY_Qundef;\n}\n");
    }
    fd = fileno(f);
    fsync(fd);
    err = ferror(f);
    fclose(f);
    return err;
}

/* Start an OS process of executable PATH with arguments ARGV.  Return
   PID of the process.  */
static pid_t
start_process(const char *path, char *const argv[]) {
  pid_t pid;

  if (mjit_opts.verbose >= 2) {
      int i;
      const char *arg;

      fprintf(stderr, "++Starting process: %s", path);
      for (i = 0; (arg = argv[i]) != NULL; i++)
	  fprintf(stderr, " %s", arg);
      fprintf(stderr, ": time - %.3f ms\n", relative_ms_time());
  }
  if ((pid = vfork()) == 0) {
      if (mjit_opts.verbose) {
	  /* CC can be started in a thread using a file which has been
	     already removed while MJIT is finishing.  Discard the
	     messages about missing files.  */
	  FILE *f = fopen("/dev/null", "w");

	  dup2(fileno(f), STDERR_FILENO);
	  dup2(fileno(f), STDOUT_FILENO);
      }
      pid = execvp(path, argv); /* Pid will be negative on an error */
      /* Even if we successfully found CC to compile PCH we still can
	 fail with loading the CC in very rare cases for some reasons.
	 Stop the forked process in this case.  */
      debug(1, "Error in execvp: %s", path);
      _exit(1);
  }
  return pid;
}

/* Status of the the precompiled header creation.  The status is
   shared by the workers and the pch thread.  */
static enum {PCH_NOT_READY, PCH_FAILED,  PCH_SUCCESS} pch_status;

/* The function producing the pre-compiled header.  It is executed in
   a separate thread started by pthread_create. */
static void *
make_pch(void *arg) {
    int stat, exit_code, ok_p;
    pid_t pid;
    static const char *input[] = {NULL, NULL};
    static const char *output[] = {"-o",  NULL, NULL};
    char **args;

    if (pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, NULL) != 0) {
	fprintf(stderr, "+++Cannot enable cancelation in pch-thread: time - %.3f ms\n",
		relative_ms_time());
    }
    verbose(2, "Creating precompiled header");
    input[0] = header_fname;
    output[1] = pch_fname;
    if (mjit_opts.llvm)
	args = form_args(4, (mjit_opts.debug ? LLVM_COMMON_ARGS_DEBUG : LLVM_COMMON_ARGS),
			 LLVM_EMIT_PCH_ARGS, input, output);
    else
	args = form_args(4, (mjit_opts.debug ? GCC_COMMON_ARGS_DEBUG : GCC_COMMON_ARGS),
			 GCC_EMIT_PCH_ARGS, input, output);
    if (args == NULL)
	pid = -1;
    else
	pid = start_process(cc_path, args);
    ok_p = pid > 0;
    if (ok_p) {
	for (;;) {
	    waitpid(pid, &stat, 0);
	    if (WIFEXITED(stat)) {
		exit_code = WEXITSTATUS(stat);
		break;
	    } else if (WIFSIGNALED(stat)) {
		exit_code = -1;
		break;
	    }
	}
	ok_p = exit_code == 0;
    }
    free(args);
    CRITICAL_SECTION_START(3, "in make_pch");
    if (ok_p) {
	verbose(1, "Precompiled header was succesfully created");
	pch_status = PCH_SUCCESS;
    } else {
	if (mjit_opts.warnings || mjit_opts.verbose)
	    fprintf(stderr, "MJIT warning: making precompiled header failed\n");
	pch_status = PCH_FAILED;
    }
    debug(3, "Sending a wakeup signal to workers in make_pch");
    if (pthread_cond_broadcast(&mjit_pch_wakeup) != 0) {
	fprintf(stderr, "++Cannot send client wakeup signal in make_pch: time - %.3f ms\n",
		relative_ms_time());
    }
    CRITICAL_SECTION_FINISH(3, "in make_pch");
    return NULL;
}

/* This function is executed in a worker thread.  The function creates
   a C file for iseqs in the unit U and starts a C compiler to
   generate an object file of the C file.  Return TRUE in a success
   case.  */
static int
start_unit(struct rb_mjit_unit *u) {
    int fail_p;
    pid_t pid;
    static const char *input[] = {NULL, NULL};
    static const char *output[] = {"-o",  NULL, NULL};
    char **args;

    verbose(3, "Starting unit %d compilation", u->num);
    if ((u->cfname = get_uniq_fname(u->num, "_mjit", ".c")) == NULL) {
	u->status = UNIT_FAILED;
	return FALSE;
    }
    if ((u->ofname = get_uniq_fname(u->num, "_mjit", ".so")) == NULL) {
	u->status = UNIT_FAILED;
	free(u->cfname); u->cfname = NULL;
	return FALSE;
    }
    if (mjit_opts.debug)
	u->time_start = real_ms_time();
    CRITICAL_SECTION_START(3, "in worker to wait GC finish");
    while (in_gc) {
	debug(3, "Waiting wakeup from GC");
	pthread_cond_wait(&mjit_gc_wakeup, &mjit_engine_mutex);
    }
    units_in_translation++;
    CRITICAL_SECTION_FINISH(3, "in worker to wait GC finish");
    fail_p = translate_unit_iseq(u, mjit_opts.llvm ? NULL : header_fname);
    CRITICAL_SECTION_START(3, "in worker to wakeup client for GC");
    units_in_translation--;
    debug(3, "Sending wakeup signal to client in a mjit-worker for GC");
    if (pthread_cond_signal(&mjit_client_wakeup) != 0) {
	fprintf(stderr, "+++Cannot send wakeup signal to client in mjit-worker: time - %.3f ms\n",
		relative_ms_time());
    }
    CRITICAL_SECTION_FINISH(3, "in worker to wakeup client for GC");

    if (fail_p) {
	pid = -1;
    } else {
	input[0] = u->cfname;
	output[1] = u->ofname;
	if (mjit_opts.llvm) {
	    LLVM_USE_PCH_ARGS[1] = pch_fname;
	    args = form_args(4, (mjit_opts.debug ? LLVM_COMMON_ARGS_DEBUG : LLVM_COMMON_ARGS),
			     LLVM_USE_PCH_ARGS, input, output);
	} else {
	    args = form_args(4, (mjit_opts.debug ? GCC_COMMON_ARGS_DEBUG : GCC_COMMON_ARGS),
			     GCC_USE_PCH_ARGS, input, output);
	}
	if (args == NULL)
	    pid = -1;
	else {
	    pid = start_process(cc_path, args);
	    free(args);
	}
    }
    if (pid < 0) {
        debug(1, "Failed starting unit %d compilation", u->num);
	u->status = UNIT_FAILED;
	if (! mjit_opts.save_temps) {
	    remove(u->cfname);
	    free(u->cfname); u->cfname = NULL;
	    remove(u->ofname);
	    free(u->ofname); u->ofname = NULL;
	}
	return FALSE;
    } else {
	debug(2, "Success in starting unit %d compilation", u->num);
	u->pid = pid;
	return TRUE;
    }
}

static void discard_unit(struct rb_mjit_unit *u);

/* The function should be called after successul creation of the
   object file for iseq of unit U.  The function loads the object
   file.  */
static void
load_unit(struct rb_mjit_unit *u) {
    struct rb_mjit_unit_iseq *ui;
    void *addr;
    char mjit_fname_holder[MAX_MJIT_FNAME_LEN];
    const char *fname, *err_name;
    void *handle;

    assert(u->status == UNIT_SUCCESS);
    handle = dlopen(u->ofname, RTLD_NOW);
    if (mjit_opts.save_temps) {
	CRITICAL_SECTION_START(3, "in load_unit to setup MJIT code");
    } else {
	const char *ofname = u->ofname;
	
	if (ofname != NULL)
	    remove(ofname);
	CRITICAL_SECTION_START(3, "in load_unit to setup MJIT code");
	if (u->ofname != NULL)
	    free(u->ofname);
	u->ofname = NULL;
    }
    if (handle != NULL)
	verbose(2, "Success in loading code of unit %d",	u->num);
    else if (mjit_opts.warnings || mjit_opts.verbose)
	fprintf(stderr, "MJIT warning: failure in loading code of unit %d(%s)\n", u->num, dlerror());
    ui = u->unit_iseq;
    if (ui->iseq == NULL) {
	/* Garbage collected */
	if (handle != NULL)
	    dlclose(handle);
    } else {
	assert(ui != NULL);
	u->handle = handle;
	if (handle == NULL) {
	    addr = (void *) NOT_ADDED_JIT_ISEQ_FUN;
	    discard_unit(u);
	} else {
	    fname = get_unit_iseq_fname(ui, mjit_fname_holder);
	    addr = dlsym(handle, fname);
	    if ((err_name = dlerror ()) != NULL) {
		debug(0, "Failure (%s) in setting address of iseq %d(%s)", err_name, ui->num, fname);
		addr = (void *) NOT_ADDED_JIT_ISEQ_FUN;
		add_to_list(u, &obsolete_units);
		u->status = UNIT_FAILED;
	    } else {
		debug(2, "Success in setting address of iseq %d(%s)(%s) 0x%"PRIxVALUE,
		      ui->num, fname, ui->label, addr);
		add_to_list(u, &active_units);
		u->status = UNIT_LOADED;
	    }
	}
	/* Usage of jit_code might be not in a critical section.  */
	VM_ATOMIC_SET(ui->iseq->body->jit_code, addr);
    }
    CRITICAL_SECTION_FINISH(3, "in load_unit to setup MJIT code");
}

/* Maximum number of worker threads.  As worker can process only one
   unit at a time, the number also represents the maximal number of C
   compiler processes started by MJIT and running at any given
   time.  */
#define MAX_WORKERS_NUM 100
/* The default number of worker threads.  */
#define DEFAULT_WORKERS_NUM 1

/* Set to TRUE to finish workers.  */
static int finish_workers_p;
/* A number of teh finished workers so far.  */
static int finished_workers;

/* The function implementing a worker. It is executed in a separate
   thread started by pthread_create. */
static void *
worker(void *arg) {
    if (pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, NULL) != 0) {
	fprintf(stderr, "+++Cannot enable cancelation in worker: time - %.3f ms\n",
		relative_ms_time());
    }
    CRITICAL_SECTION_START(3, "in worker to wakeup from pch");
    while (pch_status == PCH_NOT_READY) {
	debug(3, "Waiting wakeup from make_pch");
	pthread_cond_wait(&mjit_pch_wakeup, &mjit_engine_mutex);
    }
    CRITICAL_SECTION_FINISH(3, "in worker to wakeup from pch");
    if (pch_status == PCH_FAILED) {
	mjit_init_p = FALSE;
	CRITICAL_SECTION_START(3, "in worker to update finished_workers");
	finished_workers++;
	debug(3, "Sending wakeup signal to client in a mjit-worker");
	if (pthread_cond_signal(&mjit_client_wakeup) != 0) {
	    fprintf(stderr, "+++Cannot send wakeup signal to client in mjit-worker: time - %.3f ms\n",
		    relative_ms_time());
	}
	CRITICAL_SECTION_FINISH(3, "in worker to update finished_workers");
	return NULL;
    }
    CRITICAL_SECTION_START(3, "in worker to start the unit");
    for (;;) {
	int stat, exit_code;
	struct rb_mjit_unit *u;

	if (finish_workers_p) {
	    finished_workers++;
	    break;
	}
	u = get_from_list(&unit_queue);
	if (u != NULL)
	    u->status = UNIT_IN_GENERATION;
	CRITICAL_SECTION_FINISH(3, "in worker to start the unit");
	if (u != NULL) {
	    start_unit(u);
	    waitpid(u->pid, &stat, 0);
	    exit_code = -1;
	    if (WIFEXITED(stat)) {
		exit_code = WEXITSTATUS(stat);
	    }
	    CRITICAL_SECTION_START(3, "in worker to setup status");
	    u->status = exit_code != 0 ? UNIT_FAILED : UNIT_SUCCESS;
	    if (exit_code == 0)
		verbose(3, "Success in compilation of unit %d", u->num);
	    else if (mjit_opts.warnings || mjit_opts.verbose)
		fprintf(stderr, "MJIT warning: failure in compilation of unit %d\n", u->num);
	    CRITICAL_SECTION_FINISH(3, "in worker to setup status");
	    if (! mjit_opts.save_temps) {
		remove(u->cfname);
		free(u->cfname); u->cfname = NULL;
	    }
	    CRITICAL_SECTION_START(3, "in worker to check global speculation status");
	    if (exit_code != 0 || u->freed_p) {
		discard_unit(u);
	    } else {
		struct global_spec_state curr_state;
		int recompile_p;
		
		setup_global_spec_state(&curr_state);
		recompile_p = ! valid_global_spec_state_p(&u->spec_state, &curr_state);
		if (recompile_p) {
		    add_to_list(u, &unit_queue);
		    u->status = UNIT_IN_QUEUE;
		    verbose(3, "Global speculation changed -- put unit %d back into the queue", u->num);
		} else {
		    CRITICAL_SECTION_FINISH(3, "in worker to check global speculation status");
		    debug(2, "Start loading unit %d", u->num);
		    load_unit(u);
		    CRITICAL_SECTION_START(3, "in worker for a worker wakeup");
		}
	    }
	    debug(3, "Sending wakeup signal to client in a mjit-worker");
	    if (pthread_cond_signal(&mjit_client_wakeup) != 0) {
		fprintf(stderr, "+++Cannot send wakeup signal to client in mjit-worker: time - %.3f ms\n",
			relative_ms_time());
	    }
	} else {
	    debug(3, "Waiting wakeup from client");
	    CRITICAL_SECTION_START(3, "in worker for a worker wakeup");
	    while (unit_queue.head == NULL && ! finish_workers_p) {
		pthread_cond_wait(&mjit_worker_wakeup, &mjit_engine_mutex);
		debug(3, "Getting wakeup from client");
	    }
	}
    }
    CRITICAL_SECTION_FINISH(3, "in worker to finish");
    debug(1, "Finishing worker");
    return NULL;
}

/*-------All the following code is executed in the client thread only-----------*/
/* PID of thread creating the pre-compiled header.  */
static pthread_t pch_pid;
/* The current number of worker threads. */
static int workers_num;
/* Only first workers_num elements are defined.  The element contains
   PID of worker thread with its number equal to the index.  */
static pthread_t worker_pids[MAX_WORKERS_NUM];

/* Singly linked list of allocated but marked free unit structures.  */
static struct rb_mjit_unit *free_unit_list;
/* Singly linked list of all allocated but unit iseq structures.  */
static struct rb_mjit_unit_iseq *unit_iseq_list;
/* The number of so far processed ISEQs.  */
static int curr_unit_iseq_num;
/* The unit currently being formed.  */
static struct rb_mjit_unit *curr_unit;
/* The number of so far created units.  */
static int curr_unit_num;

/* Initialize code for work with workers.  */
static void
init_workers(void) {
    workers_num = 0;
    free_unit_list = NULL;
    unit_iseq_list = NULL;
    curr_unit_iseq_num = 0;
    init_list(&unit_queue);
    init_list(&active_units);
    init_list(&obsolete_units);
    curr_unit = NULL;
    curr_unit_num = 0;
    pch_status = PCH_NOT_READY;
}

/* Update initial values of UI ivar_spec, ivar_serial, and
   use_temp_vars_p.  */
static void
update_unit_iseq_info_from_insns(struct rb_mjit_unit_iseq *ui) {
    rb_iseq_t *iseq = ui->iseq;
    struct rb_iseq_constant_body *body = iseq->body;
    size_t pos, ic_disp, size = ui->iseq_size;
    rb_serial_t ivar_serial;
    int ivar_spec_update_p;
    size_t ivar_access_num, index, max_ivar_spec_index;
    const VALUE *code = iseq->body->rtl_encoded;
    VALUE insn;
    IC ic;
    
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
	    ui->use_temp_vars_p = FALSE;
	    break;
	case BIN(ivar2var):
	    ic_disp = 3;
	case BIN(val2ivar):
	case BIN(var2ivar):
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
	}
	pos += insn_len(insn);
    }
    if (ivar_spec_update_p && ivar_access_num > 2 && body->in_type_object_p) {
	/* We have enough ivar accesses to make whole function
	   speculation about them.  */
	ui->ivar_spec = (max_ivar_spec_index >= ROBJECT_EMBED_LEN_MAX
			 ? max_ivar_spec_index : (size_t) -1);
	ui->ivar_serial = ivar_serial;
    }
    if (body->except_p)
	ui->use_temp_vars_p = FALSE;
}

/* Return a free unit iseq.  It can allocate a new structure if there
   are no free unit iseqs.  */
static struct rb_mjit_unit_iseq *
create_unit_iseq(rb_iseq_t *iseq) {
    int i;
    struct rb_mjit_unit_iseq *ui;

    ui = xmalloc(sizeof(struct rb_mjit_unit_iseq));
    if (ui == NULL)
	return NULL;
    ui->num = curr_unit_iseq_num++;
    ui->iseq = iseq;
    iseq->body->unit_iseq = ui;
    ui->unit = NULL;
    ui->next = unit_iseq_list;
    unit_iseq_list = ui;
    ui->iseq_size = iseq->body->rtl_size;
    ui->label = NULL;
    if (mjit_opts.debug || mjit_opts.profile) {
	ui->label = get_string(RSTRING_PTR(iseq->body->location.label));
	ui->resume_calls = ui->stop_calls = ui->jit_calls = ui->failed_jit_calls = 0;
    }
    ui->ep_neq_bp_p = FALSE;
    ui->ivar_spec = 0;
    ui->ivar_serial = 0;
    ui->use_temp_vars_p = TRUE;
    ui->used_code_p = FALSE;
    ui->jit_mutations_num = 0;
    ui->mutation_insns = xmalloc(sizeof (struct mjit_mutation_insns) * (mjit_opts.max_mutations + 1));
    for (i = 0; i <= mjit_opts.max_mutations; i++)
	ui->mutation_insns[i].insn = BIN(nop);
    update_unit_iseq_info_from_insns(ui);
    return ui;
}

/* Return a free unit iseq.  */
static struct rb_mjit_unit *
create_unit(void) {
    struct rb_mjit_unit *u;

    if (free_unit_list == NULL) {
	u = xmalloc(sizeof(struct rb_mjit_unit));
	if (u == NULL)
	    return NULL;
    } else {
	u = free_unit_list;
	free_unit_list = free_unit_list->next;
    }
    u->status = UNIT_NOT_FORMED;
    u->num = curr_unit_num++;
    u->iseq_size = 0;
    u->cfname = u->ofname = NULL;
    u->handle = NULL;
    u->high_priority_p = FALSE;
    u->freed_p = FALSE;
    u->next = NULL;
    u->unit_iseq = NULL;
    init_global_spec_state(&u->spec_state);
    return u;
}

/* Mark the unit U free.  Unload the JITted code.  Remove the object
   file if it exists.  */
static void
free_unit(struct rb_mjit_unit *u) {
    verbose(3, "Removing unit %d", u->num);
    if (u->handle != NULL) {
	dlclose(u->handle);
	u->handle = NULL;
    }
    if (u->status == UNIT_SUCCESS && ! mjit_opts.save_temps) {
	remove(u->ofname);
    }
    if (u->ofname != NULL) {
	free(u->ofname);
	u->ofname = NULL;
    }
    u->next = free_unit_list;
    free_unit_list = u;
}

/* Mark the unit from LIST as free.  */
static void
free_units(struct rb_mjit_unit *list) {
  struct rb_mjit_unit *u, *next;

  for (u = list; u != NULL; u = next) {
      next = u->next;
      free_unit(u);
  }
}

/* Free memory for all marked free units and unit iseqs.  */
static void
finish_units(void) {
  struct rb_mjit_unit *u, *next;
  struct rb_mjit_unit_iseq *ui, *ui_next;

  for (u = free_unit_list; u != NULL; u = next) {
      next = u->next;
      free(u);
  }
  for (ui = unit_iseq_list; ui != NULL; ui = ui_next) {
      ui_next = ui->next;
      free(ui->mutation_insns);
      free(ui);
  }
  free_unit_list = NULL;
  unit_iseq_list = NULL;
}

/* Free memory allocated for all units and unit iseqs.  */
static void
finish_workers(void){
    free_units(obsolete_units.head);
    free_units(active_units.head);
    free_units(unit_queue.head);
    if (curr_unit != NULL)
	free_unit(curr_unit);
    finish_units();
}

/* Add the unit iseq UI to the unit U.  */
static void
add_iseq_to_unit(struct rb_mjit_unit *u, struct rb_mjit_unit_iseq *ui) {
    assert(u->unit_iseq == NULL);
    ui->unit = u;
    u->unit_iseq = ui;
    u->iseq_size += ui->iseq_size;
    debug(1, "iseq %d is added to unit %d (size %lu)",
	  ui->num, u->num, (long unsigned) u->iseq_size);
}

static void
finish_forming_unit(struct rb_mjit_unit *u) {
    assert(u != NULL);
    add_to_list(u, &unit_queue);
    u->status = UNIT_IN_QUEUE;
    debug(2, "Finish forming unit %d (size = %lu)",
	  u->num, (long unsigned) u->iseq_size);
}

/* Add the current unit to the queue.  */
static void
finish_forming_curr_unit(void) {
    if (curr_unit == NULL)
	return;
    finish_forming_unit(curr_unit);
    curr_unit = NULL;
}

/* Create unit and iseq unit for ISEQ.  Reuse the iseq unit if it
   already exists.  */
static void
create_iseq_unit(rb_iseq_t *iseq) {
    struct rb_mjit_unit_iseq *ui;
    
    if (curr_unit == NULL && (curr_unit = create_unit()) == NULL)
	return;
    if ((ui = iseq->body->unit_iseq) == NULL
	&& (ui = create_unit_iseq(iseq)) == NULL)
	return;
    add_iseq_to_unit(curr_unit, ui);
}

/* Detach the iseq unit from unit U and free the unit.  */
static void
discard_unit(struct rb_mjit_unit *u) {
    struct rb_mjit_unit_iseq *ui = u->unit_iseq;

    assert(ui->unit == u);
    ui->unit = NULL;
    free_unit(u);
}


/* MJIT info related to an existing continutaion.  */
struct mjit_cont {
    rb_thread_t *th; /* continuation thread */
    struct mjit_cont *prev, *next; /* used to form lists */
};

/* Double linked list of registered continuations.  */
struct mjit_cont *first_cont;
/* List of unused mjit_cont structures.  */
struct mjit_cont *free_conts;

/* Initiate continuation info in MJIT.  */
static void
init_conts(void) {
    free_conts = first_cont = NULL;
}

/* Create and return continuation info in MJIT.  Include it to the
   continuation list.  TH is the continuation thread.  */
static struct mjit_cont *
create_cont(rb_thread_t *th) {
    struct mjit_cont *cont;
    
    if (free_conts != NULL) {
	cont = free_conts;
	free_conts = free_conts->next;
    } else if ((cont = xmalloc(sizeof(struct mjit_cont))) == NULL) {
	/* We can not generate and use code without the continuation
	   info.  */
	stop_mjit_generation_p = TRUE;
	return NULL;
    }
    cont->th = th;
    if (first_cont == NULL) {
	cont->next = cont->prev = NULL;
    } else {
	cont->prev = NULL;
	cont->next = first_cont;
	first_cont->prev = cont;
    }
    first_cont = cont;
    return cont;
}

/* Remove continuation info CONT from the continuation list.  Include
   it into the free list.  */
static void
free_cont(struct mjit_cont *cont) {
    if (cont == first_cont) {
	first_cont = cont->next;
	if (first_cont != NULL)
	    first_cont->prev = NULL;
    } else {
	cont->prev->next = cont->next;
	if (cont->next != NULL)
	    cont->next->prev = cont->prev;
    }
    cont->next = free_conts;
    free_conts = cont;
}

/* Finish work with continuation info (free all continuation
   structures).  */
static void
finish_conts(void) {
    struct mjit_cont *cont, *next;
    
    for (cont = first_cont; cont != NULL; cont = next) {
	next = cont->next;
	free_cont(cont);
    }
    for (cont = free_conts; cont != NULL; cont = next) {
	next = cont->next;
	free(cont);
    }
}

/* Default permitted number of units with a JIT code kept in
   memory.  */
#define DEFAULT_CACHE_SIZE 1000
/* Minimum value for JIT cache size.  */
#define MIN_CACHE_SIZE 10

/* Clear used_code_p field for unit iseqs of units in LIST.  */
static void
mark_unit_iseqs(struct rb_mjit_unit_list *list) {
    struct rb_mjit_unit *u;
    
    for (u = list->head; u != NULL; u = u->next) {
	assert(u->handle != NULL && u->unit_iseq != NULL);
	u->unit_iseq->used_code_p = FALSE;
    }
}

/* Set up field used_code_p for unit iseqs whose iseq on the stack of
   thread TH.  */
static void
mark_thread_unit_iseqs(rb_thread_t *th) { 
    rb_iseq_t *iseq;
    rb_control_frame_t *fp;
    struct rb_mjit_unit_iseq *ui;
    rb_control_frame_t *last_cfp = th->cfp;
    rb_control_frame_t *end_marker_cfp;
    ptrdiff_t i, size;

    if (th->stack == NULL)
	return;
    end_marker_cfp = RUBY_VM_END_CONTROL_FRAME(th);
    size = end_marker_cfp - last_cfp;
    for (i = 0, fp = end_marker_cfp - 1; i < size; i++, fp = RUBY_VM_NEXT_CONTROL_FRAME(fp))
	if (fp->pc && (iseq = fp->iseq) != NULL
	    && imemo_type((VALUE) iseq) == imemo_iseq
	    && (ui = iseq->body->unit_iseq) != NULL) {
	    ui->used_code_p = TRUE;
	}
}

/* Unload JIT code of some units to satisfy the maximum permitted
   number of units with a loaded code.  */
static void
unload_units(void) {
    rb_vm_t *vm = GET_THREAD()->vm;
    rb_thread_t *th = 0;
    struct rb_mjit_unit *u, *next, *best_u;
    struct rb_mjit_unit_iseq *ui, *best_ui;
    unsigned long overall_calls, best_overall_calls;
    struct mjit_cont *cont;
    int n, units_num = active_units.length + obsolete_units.length;
    
    list_for_each(&vm->living_threads, th, vmlt_node) {
	mark_thread_unit_iseqs(th);
    }
    for (cont = first_cont; cont != NULL; cont = cont->next) {
	mark_thread_unit_iseqs(cont->th);
    }
    for (u = obsolete_units.head; u != NULL; u = next) {
	next = u->next;
	assert(u->unit_iseq != NULL && u->unit_iseq->unit != u);
	/* We can not just remove obsolete unit code.  Although it
	   will be never used, it might be still on the stack.  For
	   example, obsolete code might be still on the stack for
	   previous recursive calls.  */
	if (u->unit_iseq->used_code_p)
	    continue;
	verbose(2, "Unloading obsolete unit %d(%s)\n", u->num, u->unit_iseq->label);
	remove_from_list(u, &obsolete_units);
	/* ??? provide more parallelism */
	free_unit(u);
    }
    /* Remove 1/10 units more to decrease unloading calls.  */
    n = active_units.length / 10;
    for (; active_units.length + obsolete_units.length > mjit_opts.max_cache_size - n;) {
	best_u = NULL;
	best_ui = NULL;
	for (u = active_units.head; u != NULL; u = u->next) {
	    ui = u->unit_iseq;
	    assert(ui != NULL && ui->iseq != NULL && ui->unit == u && u->handle != NULL);
	    overall_calls = ui->iseq->body->resume_calls + ui->iseq->body->stop_calls;
	    if (ui->used_code_p
		|| (best_u != NULL && best_overall_calls < overall_calls))
		continue;
	    best_u = u;
	    best_ui = ui;
	    best_overall_calls = overall_calls;
	}
	if (best_u == NULL)
	    break;
	verbose(2, "Unloading unit %d(%s) (calls=%lu)",
		best_u->num, best_ui->label, best_overall_calls);
	assert(best_ui->iseq != NULL);
	best_ui->iseq->body->jit_code
	    = (void *) (ptrdiff_t) (mjit_opts.aot
				    ? NOT_READY_AOT_ISEQ_FUN
				    : NOT_READY_JIT_ISEQ_FUN);
	best_ui->iseq->body->stop_calls += best_ui->iseq->body->resume_calls;
	best_ui->iseq->body->resume_calls = 0;
	remove_from_list(best_u, &active_units);
	if (! mjit_opts.aot) {
	    discard_unit(best_u);
	} else {
	    assert(best_u->handle != NULL);
	    /* ??? provide more parallelism */
	    dlclose(best_u->handle);
	    best_u->handle = NULL;
	    finish_forming_unit(best_u);
	}
    }
    verbose(1, "Too many JIT code -- %d units unloaded",
	    units_num - (active_units.length + obsolete_units.length));
}

RUBY_SYMBOL_EXPORT_BEGIN
/* Add ISEQ to be JITed in parallel with the current thread.  Add it
   to the current unit.  Add the current unit to the queue.  Unload
   some units if there are too many of them.  */
void
mjit_add_iseq_to_process(rb_iseq_t *iseq) {
    struct rb_mjit_unit_iseq *ui;
    
    if (!mjit_init_p || stop_mjit_generation_p)
	return;
    create_iseq_unit(iseq);
    if ((ui = iseq->body->unit_iseq) == NULL)
	/* Failure in creating the unit iseq.  */
	return;
    verbose(2, "Adding iseq %s", ui->label);
    CRITICAL_SECTION_START(3, "in add_iseq_to_process");
    finish_forming_curr_unit();
    if (active_units.length + obsolete_units.length >= mjit_opts.max_cache_size) {
	/* Unload some units.  */
	mark_unit_iseqs(&obsolete_units);
	mark_unit_iseqs(&active_units);
	unload_units();
    }
    debug(3, "Sending wakeup signal to workers in mjit_add_iseq_to_process");
    if (pthread_cond_broadcast(&mjit_worker_wakeup) != 0) {
      fprintf(stderr, "Cannot send wakeup signal to workers in add_iseq_to_process: time - %.3f ms\n",
	      relative_ms_time());
    }
    CRITICAL_SECTION_FINISH(3, "in add_iseq_to_process");
}

/* Redo ISEQ.  It means canceling the current JIT code and adding ISEQ
   to the queue for processing again.  SPEC_FAIL_P flags that iseq is
   canceling because of a particular insn speculation failed.  */
void
mjit_redo_iseq(rb_iseq_t *iseq, int spec_fail_p) {
    struct rb_mjit_unit_iseq *ui = iseq->body->unit_iseq;
    struct rb_mjit_unit *u = ui->unit;

    verbose(2, "Start redoing iseq #%d in unit %d", ui->num, u->num);
    CRITICAL_SECTION_START(3, "in redo iseq");
    if (u->status != UNIT_LOADED || ui->jit_mutations_num >= mjit_opts.max_mutations) {
      CRITICAL_SECTION_FINISH(3, "in redo iseq");
      return;
    }
    assert(u->handle != NULL);
    verbose(3, "Iseq #%d (so far mutations=%d) in unit %d is canceled",
	    ui->num, ui->jit_mutations_num, u->num);
    if (spec_fail_p)
	ui->jit_mutations_num++;
    verbose(3, "Code of unit %d is removed", u->num);
    remove_from_list(u, &active_units);
    add_to_list(u, &obsolete_units);
    u->status = UNIT_FAILED;
    iseq->body->jit_code
	= (void *) (ptrdiff_t) (mjit_opts.aot
				? NOT_READY_AOT_ISEQ_FUN
				: NOT_READY_JIT_ISEQ_FUN);
    CRITICAL_SECTION_FINISH(3, "in redo iseq 2");
    mjit_add_iseq_to_process(iseq);
}

/* Increase unit U priority.  Finish the current unit if
   necessary.  */
static void
increase_unit_priority(struct rb_mjit_unit *u) {
    CRITICAL_SECTION_START(3, "in increase_unit_priority");
    verbose(3, "Increasing priority for unit %d", u->num);
    u->high_priority_p = TRUE;
    if (u == curr_unit) {
	finish_forming_curr_unit();
    }
    debug(3, "Sending wakeup signal to workers in increase_unit_priority");
    if (pthread_cond_broadcast(&mjit_worker_wakeup) != 0) {
	fprintf(stderr, "Cannot send wakeup signal to workers in increase_unit_priority: time - %.3f ms\n",
		relative_ms_time());
    }
    CRITICAL_SECTION_FINISH(3, "in increase_unit_priority");
}

/* Wait for finishing ISEQ generation.  To decrease the wait, increase
   ISEQ priority.  Return the code address, NULL in a failure
   case.  */
mjit_fun_t
mjit_get_iseq_fun(const rb_iseq_t *iseq) {
    enum unit_status status;
    struct rb_mjit_unit_iseq *ui;
    struct rb_mjit_unit *u;

    if (!mjit_init_p)
	return NULL;
    if (stop_mjit_generation_p) {
	if (mjit_opts.aot)
	    /* Speed up the calls -- see mjit_execute_iseq_0.  */
	    iseq->body->jit_code = (void *) NOT_READY_JIT_ISEQ_FUN;
	return NULL;
    }
    ui = iseq->body->unit_iseq;
    u = ui->unit;
    increase_unit_priority(u);
    CRITICAL_SECTION_START(3, "in mjit_add_iseq_to_process for a client wakeup");
    while ((status = u->status) != UNIT_LOADED && status != UNIT_FAILED) {
	debug(3, "Waiting wakeup from a worker in mjit_add_iseq_to_process");
	pthread_cond_wait(&mjit_client_wakeup, &mjit_engine_mutex);
	debug(3, "Getting wakeup from a worker in mjit_add_iseq_to_process");
    }
    CRITICAL_SECTION_FINISH(3, "in mjit_add_iseq_to_process for a client wakeup");
    if (u->status == UNIT_FAILED)
	return NULL;
    return ui->iseq->body->jit_code;
}

/* Called when an ISEQ insn with a relative PC caused MUTATION_NUM-th
   mutation.  We just collect this info in mutation_insns array. */
void
mjit_store_failed_spec_insn(rb_iseq_t *iseq, size_t pc, int mutation_num)  {
    struct rb_mjit_unit_iseq *ui = iseq->body->unit_iseq;
    VALUE insn;

    insn = iseq->body->rtl_encoded[pc];
#if OPT_DIRECT_THREADED_CODE || OPT_CALL_THREADED_CODE
    insn = rb_vm_insn_addr2insn((void *) insn);
#endif
    ui->mutation_insns[mutation_num].pc = pc;
    ui->mutation_insns[mutation_num].insn = (enum ruby_vminsn_type)insn;
}

/* It is called when our whole function speculation about ivar
   accesses during ISEQ execution failed.  */
void
mjit_ivar_spec_fail(rb_iseq_t *iseq) {
    if (iseq->body->jit_code >= (void *) LAST_JIT_ISEQ_FUN) {
	iseq->body->unit_iseq->ivar_spec = 0;
	mjit_redo_iseq(iseq, FALSE);
    }
}

/* It is called when our speculation about ep and bp equality during
   ISEQ execution failed.  */
void
mjit_ep_eq_bp_fail(rb_iseq_t *iseq) {
    if (iseq->body->jit_code >= (void *) LAST_JIT_ISEQ_FUN) {
	iseq->body->unit_iseq->ep_neq_bp_p = TRUE;
	mjit_redo_iseq(iseq, FALSE);
    }
}

RUBY_SYMBOL_EXPORT_END

/* Iseqs can be garbage collected.  This function should call when it
   happens.  It removes unit iseq from the unit.  */
void
mjit_free_iseq(const rb_iseq_t *iseq) {
    struct rb_mjit_unit_iseq *ui;
    struct rb_mjit_unit *u;
    
    if (!mjit_init_p || (ui = iseq->body->unit_iseq) == NULL)
	return;
    CRITICAL_SECTION_START(3, "to clear iseq in mjit_free_iseq");
    u = ui->unit;
    if (u == NULL) {
	/* Was obsoleted */
	CRITICAL_SECTION_FINISH(3, "to clear iseq in mjit_free_iseq");
	return;
    }
    ui->iseq = NULL;
    if (u->status == UNIT_IN_QUEUE) {
	remove_from_list(u, &unit_queue);
    } else if (u->status == UNIT_LOADED) {
	remove_from_list(u, &active_units);
    } else if (u->status == UNIT_IN_GENERATION) {
	u->freed_p = TRUE;
    }
    if (u->status != UNIT_IN_GENERATION)
	discard_unit(u);
    CRITICAL_SECTION_FINISH(3, "to clear iseq in mjit_free_iseq");
    if (mjit_opts.debug || mjit_opts.profile) {
	ui->resume_calls = iseq->body->resume_calls;
	ui->stop_calls = iseq->body->stop_calls;
	ui->jit_calls = iseq->body->jit_calls;
	ui->failed_jit_calls = iseq->body->failed_jit_calls;
    }
}

/* Mark all frames of thread TH as canceled.  */
static void
cancel_thread_frames(rb_thread_t *th) {
    rb_iseq_t *iseq;
    rb_control_frame_t *fp;
    rb_control_frame_t *last_cfp = th->cfp;
    rb_control_frame_t *end_marker_cfp;
    ptrdiff_t i, size;

    if (th->stack == NULL)
	return;
    end_marker_cfp = RUBY_VM_END_CONTROL_FRAME(th);
    size = end_marker_cfp - last_cfp;
    for (i = 0, fp = end_marker_cfp - 1; i < size; i++, fp = RUBY_VM_NEXT_CONTROL_FRAME(fp))
	if (fp->pc && (iseq = fp->iseq) != NULL && imemo_type((VALUE) iseq) == imemo_iseq
	    && iseq->body->jit_code >= (void *) LAST_JIT_ISEQ_FUN) {
	    fp->ep[0] |= VM_FRAME_FLAG_CANCEL;
	}
}

/* Mark all JIT code being executed for cancellation.  Redo JIT code
   with invalid speculation. It happens when a global speculation
   fails.  For example, a basic operation is redefined or tracing
   starts.  */
void
mjit_cancel_all(void)
{
    rb_vm_t *vm = GET_THREAD()->vm;
    rb_thread_t *th = 0;
    struct global_spec_state curr_state;
    struct rb_mjit_unit *u, *next;
    struct rb_mjit_unit_iseq *ui;
    struct mjit_cont *cont;

    if (!mjit_init_p)
	return;
    list_for_each(&vm->living_threads, th, vmlt_node) {
	cancel_thread_frames(th);
    }
    for (cont = first_cont; cont != NULL; cont = cont->next) {
	cancel_thread_frames(cont->th);
    }
    CRITICAL_SECTION_START(3, "mjit_cancel_all");
    verbose(1, "Cancel all wrongly speculative JIT code");
    setup_global_spec_state(&curr_state);
    for (u = active_units.head; u != NULL; u = next) {
	next = u->next;
	if (u->status == UNIT_LOADED
	    && ! valid_global_spec_state_p(&u->spec_state, &curr_state)) {
	    verbose(2, "Global speculation changed -- recompiling unit %d", u->num);
	    ui = u->unit_iseq;
	    assert(ui != NULL);
	    remove_from_list(u, &active_units);
	    add_to_list(u, &obsolete_units);
	    u->status = UNIT_FAILED;
	    assert(ui->iseq != NULL);
	    ui->iseq->body->jit_code = (void *) NOT_READY_JIT_ISEQ_FUN;
	    create_iseq_unit(ui->iseq);
	    finish_forming_curr_unit();
	}
    }
    debug(3, "Sending wakeup signal to workers in mjit_cancel_all");
    if (pthread_cond_broadcast(&mjit_worker_wakeup) != 0) {
        fprintf(stderr, "Cannot send wakeup signal to workers in mjit_cancel_all: time - %.3f ms\n",
		relative_ms_time());
    }
    CRITICAL_SECTION_FINISH(3, "mjit_cancel_all");
}

/* Wait until workers don't compile any iseq.  It is called at the
   start of GC.  */
void
mjit_gc_start(void) {
    if (!mjit_init_p)
	return;
    debug(4, "mjit_gc_start");
    CRITICAL_SECTION_START(4, "mjit_gc_start");
    while (units_in_translation != 0) {
	debug(4, "Waiting wakeup from a worker for GC");
	pthread_cond_wait(&mjit_client_wakeup, &mjit_engine_mutex);
	debug(4, "Getting wakeup from a worker for GC");
    }
    in_gc = TRUE;
    CRITICAL_SECTION_FINISH(4, "mjit_gc_start");
}

/* Send a signal to workers to continue iseq compilations.  It is
   called at the end of GC.  */
void
mjit_gc_finish(void) {
    if (!mjit_init_p)
	return;
    debug(4, "mjit_gc_finish");
    CRITICAL_SECTION_START(4, "mjit_gc_finish");
    in_gc = FALSE;
    debug(4, "Sending wakeup signal to workers after GC");
    if (pthread_cond_broadcast(&mjit_gc_wakeup) != 0) {
        fprintf(stderr, "Cannot send wakeup signal to workers in mjit_gc_finish: time - %.3f ms\n",
		relative_ms_time());
    }
    CRITICAL_SECTION_FINISH(4, "mjit_gc_finish");
}

/* Register a new continuation with thread TH.  Return MJIT info about
   the continuation.  */
struct mjit_cont *
mjit_cont_new(rb_thread_t *th) {
    return create_cont(th);
}

/* Unregister continuation CONT.  */
void
mjit_cont_free(struct mjit_cont *cont) {
    if (cont != NULL)
	free_cont(cont);
}

/* A name of the header file included in any C file generated by MJIT for iseqs.  */
#define RUBY_MJIT_HEADER_FNAME ("rb_mjit_min_header-" RUBY_VERSION ".h")
/* GCC and LLVM executable paths.  TODO: The paths should absolute
   ones to prevent changing C compiler for security reasons.  */
#define GCC_PATH "gcc"
#define LLVM_PATH "clang"

/* The default number of permitted ISEQ MJIT code mutations.  */
#define DEFAULT_MUTATIONS_NUM 2

/* This is called after each fork in the child in to switch off MJIT
   engine in the child as it does not inherit MJIT threads.  */
static void child_after_fork(void) {
    verbose(3, "Switching off MJIT in a forked child");
    mjit_init_p = FALSE;
    /* TODO: Should we initiate MJIT in the forked Ruby.  */
}

/* Initialize MJIT.  Start a thread creating the precompiled
   header.  Create worker threads processing units.  The function
   should be called first for using MJIT.  If everything is
   successfull, MJIT_INIT_P will be TRUE.  */
void
mjit_init(struct mjit_options *opts) {
    pthread_attr_t attr;
    int init_state;
    pthread_t pid;
    const char *path;
    FILE *f;

    stop_mjit_generation_p = FALSE;
    in_gc = FALSE;
    units_in_translation = 0;
    mjit_opts = *opts;
    if (mjit_opts.threads <= 0)
	mjit_opts.threads = DEFAULT_WORKERS_NUM;
    if (mjit_opts.threads > MAX_WORKERS_NUM)
	mjit_opts.threads = MAX_WORKERS_NUM;
    if (mjit_opts.max_mutations <= 0)
	mjit_opts.max_mutations = DEFAULT_MUTATIONS_NUM;
    if (mjit_opts.max_cache_size <= 0)
	mjit_opts.max_cache_size = DEFAULT_CACHE_SIZE;
    if (mjit_opts.max_cache_size < MIN_CACHE_SIZE)
	mjit_opts.max_cache_size = MIN_CACHE_SIZE;
    mjit_time_start = real_ms_time();
    client_pid = pthread_self();
    if (mjit_init_p)
	return;
    pthread_atfork(NULL, NULL, child_after_fork);
    debug(2, "Start initializing MJIT");
    finish_workers_p = FALSE;
    finished_workers = 0;
    header_fname = xmalloc(strlen(BUILD_DIR) + 2 + strlen(RUBY_MJIT_HEADER_FNAME));
    if (header_fname == NULL)
	return;
    strcpy(header_fname, BUILD_DIR);
    strcat(header_fname, "/");
    strcat(header_fname, RUBY_MJIT_HEADER_FNAME);
    if ((f = fopen(header_fname, "r")) == NULL) {
	free(header_fname);
	header_fname = xmalloc(strlen(DEST_INCDIR) + 2 + strlen(RUBY_MJIT_HEADER_FNAME));
	if (header_fname == NULL)
	    return;
	strcpy(header_fname, DEST_INCDIR);
	strcat(header_fname, "/");
	strcat(header_fname, RUBY_MJIT_HEADER_FNAME);
	if ((f = fopen(header_fname, "r")) == NULL) {
	    free(header_fname); header_fname = NULL;
	    return;
	}
    }
    fclose(f);
#ifdef __MACH__
    if (! mjit_opts.llvm) {
	if (mjit_opts.warnings || mjit_opts.verbose)
	    fprintf(stderr, "MJIT warning: we use only clang on Mac OS X\n");
	mjit_opts.llvm = 1;
    }
#endif
    path = mjit_opts.llvm ? LLVM_PATH : GCC_PATH;
    cc_path = xmalloc(strlen(path) + 1);
    if (cc_path == NULL) {
	free(header_fname); header_fname = NULL;
	return;
    }
    strcpy(cc_path, path);
    if ((pch_fname = get_uniq_fname(0, "_mjit_h", ".h.gch")) == NULL) {
	free(header_fname); header_fname = NULL;
	free(cc_path); cc_path = NULL;
	return;
    }
    init_conts();
    init_workers();
    pthread_mutex_init(&mjit_engine_mutex, NULL);
    init_state = 0;
    if (pthread_cond_init(&mjit_pch_wakeup, NULL) == 0) {
      init_state = 1;
      if (pthread_cond_init(&mjit_client_wakeup, NULL) == 0) {
	init_state = 2;
	if (pthread_cond_init(&mjit_worker_wakeup, NULL) == 0) {
	    init_state = 3;
	    if (pthread_cond_init(&mjit_gc_wakeup, NULL) == 0) {
		init_state = 4;
		if (pthread_attr_init(&attr) == 0
		    && pthread_attr_setscope(&attr, PTHREAD_SCOPE_SYSTEM) == 0
		    && pthread_create(&pch_pid, &attr, make_pch, NULL) == 0) {
		    int i;
		    
		    /* Use the detached threads not to fiddle with major code
		       processing SICHLD.  */
		    pthread_detach(pch_pid);
		    init_state = 5;
		    for (i = 0; i < mjit_opts.threads; i++) {
			if (pthread_create(&pid, &attr, worker, NULL) != 0)
			    break;
			worker_pids[workers_num++] = pid;
			pthread_detach(pid);
		    }
		    if (i == mjit_opts.threads)
			init_state = 6;
		}
	    }
	}
      }
    }
    switch (init_state) {
    case 5:
	mjit_init_p = TRUE;
	/* Fall through: */
    case 4:
	pthread_cond_destroy(&mjit_gc_wakeup);
	/* Fall through: */
    case 3:
	pthread_cond_destroy(&mjit_worker_wakeup);
	/* Fall through: */
    case 2:
	pthread_cond_destroy(&mjit_client_wakeup);
	/* Fall through: */
    case 1:
	pthread_cond_destroy(&mjit_pch_wakeup);
	/* Fall through: */
    case 0:
	free(header_fname); header_fname = NULL;
	free(cc_path); cc_path = NULL;
	free(pch_fname); pch_fname = NULL;
	pthread_mutex_destroy(&mjit_engine_mutex);
	verbose(1, "Failure in MJIT initialization");
	return;
    }
    mjit_init_p = TRUE;
    verbose(1, "Successful MJIT initialization (workers = %d)", mjit_opts.threads);
}

/* Return number of all unit iseqs.  */
static int
get_unit_iseqs_num(void) {
    int n = 0;
    struct rb_mjit_unit_iseq *ui;
    
    for (ui = unit_iseq_list; ui != NULL; ui = ui->next)
	n++;
    return n;
}

/* Used to sort unit iseqs to put most frequently called ones first.
   If the call numbers are equal put bigger iseqs first.  */ 
static int
unit_iseq_compare(const void *el1, const void *el2) {
    const struct rb_mjit_unit_iseq *ui1 = *(struct rb_mjit_unit_iseq * const *) el1;
    const struct rb_mjit_unit_iseq *ui2 = *(struct rb_mjit_unit_iseq * const *) el2;
    unsigned long overall_calls1 = (ui1->iseq == NULL
				    ? ui1->resume_calls + ui1->stop_calls
				    : ui1->iseq->body->resume_calls + ui1->iseq->body->stop_calls);
    unsigned long overall_calls2 = (ui2->iseq == NULL
				    ? ui2->resume_calls + ui2->stop_calls
				    : ui2->iseq->body->resume_calls + ui2->iseq->body->stop_calls);

    if (overall_calls2 < overall_calls1) return -1;
    if (overall_calls1 < overall_calls2) return 1;
    return (int)((long) ui2->iseq_size - (long) ui1->iseq_size);
}

/* Allocate and return a new array of done unit iseqs sorted
   according criteria described in unit_iseq_compare.  The array has
   null end marker.  */
static struct rb_mjit_unit_iseq **
get_sorted_unit_iseqs(void) {
    int n, unit_iseqs_num = get_unit_iseqs_num();
    struct rb_mjit_unit_iseq **unit_iseqs = xmalloc(sizeof(struct rb_mjit_unit_iseq *) * (unit_iseqs_num + 1));
    struct rb_mjit_unit_iseq *ui;
    
    if (unit_iseqs == NULL)
	return NULL;
    n = 0;
    for (ui = unit_iseq_list; ui != NULL; ui = ui->next)
	unit_iseqs[n++] = ui;
    unit_iseqs[n] = NULL;
    assert(n == unit_iseqs_num);
    qsort(unit_iseqs, unit_iseqs_num, sizeof(struct rb_mjit_unit_iseq *), unit_iseq_compare);
    return unit_iseqs;
}

/* Print statistics about ISEQ calls to stderr.  Statistics is printed
   only for JITed iseqs.  */
static void
print_statistics(void) {
    int n;
    struct rb_mjit_unit_iseq *ui, **unit_iseqs;
    unsigned long long all_calls = 0;
    unsigned long long all_jit_calls = 0;

    if ((unit_iseqs = get_sorted_unit_iseqs()) == NULL)
	return;

    fprintf(stderr, "Name                                           Unit Iseq  Size     Calls/JIT Calls               Spec Fails\n");
    for (n = 0; (ui = unit_iseqs[n]) != NULL; n++) {
	int i;
	unsigned long overall_calls
	    = (ui->iseq == NULL
	       ? ui->resume_calls + ui->stop_calls
	       : ui->iseq->body->resume_calls + ui->iseq->body->stop_calls);
	unsigned long jit_calls
	    = (ui->iseq == NULL ? ui->jit_calls : ui->iseq->body->jit_calls);
	unsigned long failed_jit_calls
	    = (ui->iseq == NULL ? ui->jit_calls : ui->iseq->body->failed_jit_calls);
	
	all_calls += overall_calls;
	all_jit_calls += jit_calls;
	fprintf(stderr, "%-45s %5d %4d %5lu %9lu/%-9lu (%-5.2f%%)   %9lu (%6.2f%%)",
		ui->label, ui->unit->num, ui->num, ui->iseq_size,
		overall_calls, jit_calls, overall_calls == 0 ? 0.0 : jit_calls * 100.0 / overall_calls,
		failed_jit_calls, jit_calls == 0 ? 0.0 : failed_jit_calls * 100.0 / jit_calls);
	/* Print insns whose failed spec variant caused mutations.  */
	for (i = 0; i < ui->jit_mutations_num; i++)
	    if (ui->mutation_insns[i].insn == BIN(nop))
		fprintf (stderr, " -");
	    else
		fprintf (stderr, " %s", insn_name((VALUE) ui->mutation_insns[i].insn));
	fprintf(stderr, "\n");
    }
    fprintf(stderr, "Calls summary (calls / JIT calls): %9llu/%-9llu (%-5.2f%%)\n",
	    all_calls, all_jit_calls, all_calls == 0 ? 0.0 : all_jit_calls * 100.0 / all_calls);
    free(unit_iseqs);
}

/* Finish the threads processing units and creating PCH, finalize
   and free MJIT data.  It should be called last during MJIT
   life.  */
void
mjit_finish(void) {
    if (!mjit_init_p)
	return;
    debug(1, "Initiate finishing MJIT");
    debug(2, "Canceling pch and worker threads");
    /* As our threads are detached, we could just cancel them.  But it
       is a bad idea because OS processes (C compiler) started by
       threads can produce temp files.  And even if the temp files are
       removed, the used C compiler still complaint about their
       absence.  So wait for a clean finish of the threads.  */
    CRITICAL_SECTION_START(3, "in mjit_finish to wakeup from pch");
    while (pch_status == PCH_NOT_READY) {
	debug(3, "Waiting wakeup from make_pch");
	pthread_cond_wait(&mjit_pch_wakeup, &mjit_engine_mutex);
    }
    CRITICAL_SECTION_FINISH(3, "in mjit_finish to wakeup from pch");
    finish_workers_p = TRUE;
    while (finished_workers < workers_num) {
	debug(3, "Sending cancel signal to workers");
	CRITICAL_SECTION_START(3, "in mjit_finish");
	if (pthread_cond_broadcast(&mjit_worker_wakeup) != 0) {
	    fprintf(stderr, "Cannot send wakeup signal to workers in mjit_finish: time - %.3f ms\n",
		    relative_ms_time());
	}
	CRITICAL_SECTION_FINISH(3, "in mjit_finish");
    }
    pthread_mutex_destroy(&mjit_engine_mutex);
    pthread_cond_destroy(&mjit_pch_wakeup);
    pthread_cond_destroy(&mjit_client_wakeup);
    pthread_cond_destroy(&mjit_worker_wakeup);
    pthread_cond_destroy(&mjit_gc_wakeup);
    if (! mjit_opts.save_temps)
	remove(pch_fname);
    if (mjit_opts.profile)
	print_statistics();
#if MJIT_INSN_STATISTICS
    fprintf(stderr, "Executed insns (all/jit): %8lu/%-8lu (%5.2f%%)\n",
	    byte_code_insns_num + jit_insns_num, jit_insns_num,
	    (byte_code_insns_num + jit_insns_num) == 0
	    ? 0.0 : jit_insns_num * 100.0 / (byte_code_insns_num + jit_insns_num));
#endif
    free(header_fname); header_fname = NULL;
    free(cc_path); cc_path = NULL;
    free(pch_fname); pch_fname = NULL;
    finish_workers();
    finish_conts();
    mjit_init_p = FALSE;
    verbose(1, "Successful MJIT finish");
}
