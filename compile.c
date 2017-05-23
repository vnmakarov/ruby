/**********************************************************************

  compile.c - ruby node tree -> VM instruction sequence

  $Author$
  created at: 04/01/01 03:42:15 JST

  Copyright (C) 2004-2007 Koichi Sasada

  Modified by Vladimir Makarov <vmakarov@redhat.com> in 2017 to
  generate RTL insns

**********************************************************************/

/* During insns generation we use location indexes which are local
   variable index if the index is less than current local table size
   or, otherwise, n-th temporary on the stack where n is the location
   index minus the local table size.  We call the location index an
   offset if it was decreased by the local table size.

   We never generate speculative and unchanging insns here (see
   comments in insns.def).  They are created only during execution of
   ISEQs.  */

#include "internal.h"
#include "ruby/re.h"
#include "encindex.h"
#include <math.h>

#include "vm_core.h"
#include "iseq.h"

#include "insns.inc"
#include "insns_info.inc"
#include "id_table.h"
#include "gc.h"

/* The interface to MJIT is needed for AOT.  */
#include "mjit.h"

#ifdef HAVE_DLADDR
# include <dlfcn.h>
#endif

#undef RUBY_UNTYPED_DATA_WARNING
#define RUBY_UNTYPED_DATA_WARNING 0

#define FIXNUM_INC(n, i) ((n)+(INT2FIX(i)&~FIXNUM_FLAG))
#define FIXNUM_OR(n, i) ((n)|INT2FIX(i))

typedef struct iseq_link_element {
    enum {
	ISEQ_ELEMENT_NONE,
	ISEQ_ELEMENT_LABEL,
	ISEQ_ELEMENT_INSN,
    } type;
    struct iseq_link_element *next;
    struct iseq_link_element *prev;
} LINK_ELEMENT;

typedef struct iseq_link_anchor {
    LINK_ELEMENT anchor;
    LINK_ELEMENT *last;
} LINK_ANCHOR;

typedef enum {
    LABEL_RESCUE_NONE,
    LABEL_RESCUE_BEG,
    LABEL_RESCUE_END,
    LABEL_RESCUE_TYPE_MAX
} LABEL_RESCUE_TYPE;

typedef struct iseq_label_data {
    LINK_ELEMENT link;
    int label_no;
    int position;
    int sc_state;
    int sp;
    int refcnt;
    unsigned int set: 1;
    unsigned int rescued: 2;
} LABEL;

typedef struct iseq_insn_data {
    LINK_ELEMENT link;
    enum ruby_vminsn_type insn_id;
    unsigned int line_no;
    int operand_size;
    int sc_state;
    /* Used to prevent repeated insn processing for some
       algorithms.  */
    int check;
    VALUE *operands;
} INSN;

struct ensure_range {
    LABEL *begin;
    LABEL *end;
    struct ensure_range *next;
};

struct iseq_compile_data_ensure_node_stack {
    NODE *ensure_node;
    struct iseq_compile_data_ensure_node_stack *prev;
    struct ensure_range *erange;
};

/**
 * debug function(macro) interface depend on CPDEBUG
 * if it is less than 0, runtime option is in effect.
 *
 * debug level:
 *  0: no debug output
 *  1: show node type
 *  2: show node important parameters
 *  ...
 *  5: show other parameters
 * 10: show every AST array
 */

#ifndef CPDEBUG
#define CPDEBUG 0
#endif

#if CPDEBUG >= 0
#define compile_debug CPDEBUG
#else
#define compile_debug ISEQ_COMPILE_DATA(iseq)->option->debug_level
#endif

#if CPDEBUG

#define compile_debug_print_indent(level) \
    ruby_debug_print_indent((level), compile_debug, gl_node_level * 2)

#define debugp(header, value) (void) \
  (compile_debug_print_indent(1) && \
   ruby_debug_print_value(1, compile_debug, (header), (value)))

#define debugi(header, id)  (void) \
  (compile_debug_print_indent(1) && \
   ruby_debug_print_id(1, compile_debug, (header), (id)))

#define debugp_param(header, value)  (void) \
  (compile_debug_print_indent(1) && \
   ruby_debug_print_value(1, compile_debug, (header), (value)))

#define debugp_verbose(header, value)  (void) \
  (compile_debug_print_indent(2) && \
   ruby_debug_print_value(2, compile_debug, (header), (value)))

#define debugp_verbose_node(header, value)  (void) \
  (compile_debug_print_indent(10) && \
   ruby_debug_print_value(10, compile_debug, (header), (value)))

#define debug_node_start(node)  ((void) \
  (compile_debug_print_indent(1) && \
   (ruby_debug_print_node(1, CPDEBUG, "", (NODE *)(node)), gl_node_level)), \
   gl_node_level++)

#define debug_node_end()  gl_node_level --

#else

static inline ID
r_id(ID id)
{
    return id;
}

static inline VALUE
r_value(VALUE value)
{
    return value;
}

#define debugi(header, id)                 r_id(id)
#define debugp(header, value)              r_value(value)
#define debugp_verbose(header, value)      r_value(value)
#define debugp_verbose_node(header, value) r_value(value)
#define debugp_param(header, value)        r_value(value)
#define debug_node_start(node)             ((void)0)
#define debug_node_end()                   ((void)0)
#endif

#if CPDEBUG > 1 || CPDEBUG < 0
#define printf ruby_debug_printf
#define debugs if (compile_debug_print_indent(1)) ruby_debug_printf
#define debug_compile(msg, v) ((void)(compile_debug_print_indent(1) && fputs((msg), stderr)), (v))
#else
#define debugs                             if(0)printf
#define debug_compile(msg, v) (v)
#endif

/* We have special call insns which can put SELF to a reserved stack
   slot by themself.  Use non-zero to switch off such call
   generation.  */
#define USE_SELF_LD 0

/* Different macros to convert to/from lindex_t.  */
#define INT2LINT(x) ((long) (x))
#define LINT2INT(x) ((int) (x))
#define FIXNUM2LINT(x) ((int) (x))

#define LVAR_ERRINFO (1)

/* create new label */
#define NEW_LABEL(l) new_label_body(iseq, (l))

#define iseq_path(iseq) ((iseq)->body->location.path)
#define iseq_absolute_path(iseq) ((iseq)->body->location.absolute_path)

#define NEW_ISEQ(node, name, type, line_no) \
  new_child_iseq(iseq, (node), rb_fstring(name), 0, (type), (line_no))

#define NEW_CHILD_ISEQ(node, name, type, line_no) \
  new_child_iseq(iseq, (node), rb_fstring(name), iseq, (type), (line_no))

/* add instructions */
#define ADD_SEQ(seq1, seq2) \
  APPEND_LIST((seq1), (seq2))

/* add an instruction */
#define ADD_INSN(seq, line, insn) \
  ADD_ELEM((seq), (LINK_ELEMENT *) new_insn_body(iseq, (line), BIN(insn), 0))

/* add an instruction with some operands (1, 2, 3, 5) */
#define ADD_INSN1(seq, line, insn, op1) \
  ADD_ELEM((seq), (LINK_ELEMENT *) \
           new_insn_body(iseq, (line), BIN(insn), 1, (VALUE)(op1)))

#define LABEL_REF(label) ((label)->refcnt++)

/* add an instruction with label operand (alias of ADD_INSN1) */
#define ADD_INSNL(seq, line, insn, label) (ADD_INSN1(seq, line, insn, label), LABEL_REF(label))

#define ADD_INSN2(seq, line, insn, op1, op2) \
  ADD_ELEM((seq), (LINK_ELEMENT *) \
           new_insn_body(iseq, (line), BIN(insn), 2, (VALUE)(op1), (VALUE)(op2)))

#define ADD_INSN3(seq, line, insn, op1, op2, op3) \
  ADD_ELEM((seq), (LINK_ELEMENT *) \
           new_insn_body(iseq, (line), BIN(insn), 3, (VALUE)(op1), (VALUE)(op2), (VALUE)(op3)))

/* Specific Insn factory */
#define ADD_SEND(seq, line, id, argc) \
  ADD_SEND_R((seq), (line), (id), (argc), NULL, (VALUE)INT2FIX(0), NULL)

#define ADD_SEND_WITH_FLAG(seq, line, id, argc, flag) \
  ADD_SEND_R((seq), (line), (id), (argc), NULL, (VALUE)(flag), NULL)

#define ADD_SEND_WITH_BLOCK(seq, line, id, argc, block) \
  ADD_SEND_R((seq), (line), (id), (argc), (block), (VALUE)INT2FIX(0), NULL)

#define ADD_CALL_RECEIVER(seq, line) \
  ADD_INSN((seq), (line), putself)

#define ADD_CALL(seq, line, id, argc) \
  ADD_SEND_R((seq), (line), (id), (argc), NULL, (VALUE)INT2FIX(VM_CALL_FCALL), NULL)

#define ADD_CALL_WITH_BLOCK(seq, line, id, argc, block) \
  ADD_SEND_R((seq), (line), (id), (argc), (block), (VALUE)INT2FIX(VM_CALL_FCALL), NULL)

#define ADD_SEND_R(seq, line, id, argc, block, flag, keywords) \
  ADD_ELEM((seq), (LINK_ELEMENT *) new_insn_send(iseq, (line), (id), (VALUE)(argc), (block), (VALUE)(flag), (keywords)))

#define ADD_TRACE(seq, line, event) \
  do { \
      if ((event) == RUBY_EVENT_LINE && ISEQ_COVERAGE(iseq) && \
	  (line) > 0 && \
	  (line) != ISEQ_COMPILE_DATA(iseq)->last_coverable_line) { \
	  RARRAY_ASET(ISEQ_COVERAGE(iseq), (line) - 1, INT2FIX(0)); \
	  ISEQ_COMPILE_DATA(iseq)->last_coverable_line = (line); \
	  ADD_INSN1((seq), (line), trace, INT2FIX(RUBY_EVENT_COVERAGE)); \
      } \
      if (ISEQ_COMPILE_DATA(iseq)->option->trace_instruction) { \
	  ADD_INSN1((seq), (line), trace, INT2FIX(event)); \
      } \
  } while (0)

/* add label */
#define ADD_LABEL(seq, label) \
  ADD_ELEM((seq), (LINK_ELEMENT *) (label))

#define APPEND_LABEL(seq, before, label) \
  APPEND_ELEM((seq), (before), (LINK_ELEMENT *) (label))

#define ADD_CATCH_ENTRY(type, ls, le, iseqv, lc) do {				\
    VALUE _e = rb_ary_new3(5, (type),						\
			   (VALUE)(ls) | 1, (VALUE)(le) | 1,			\
			   (VALUE)(iseqv), (VALUE)(lc) | 1);			\
    if (ls) LABEL_REF(ls);							\
    if (le) LABEL_REF(le);							\
    if (lc) LABEL_REF(lc);							\
    rb_ary_push(ISEQ_COMPILE_DATA(iseq)->catch_table_ary, freeze_hide_obj(_e));	\
} while (0)

/* Different macros to create a specific insn with LINE and add it to
   SEQ for ISEQ (defined outside macro usage): */
#define ADD_RTL_GOTO(seq, line, label)				\
    (ADD_INSN1(seq, line, goto, label), LABEL_REF(label))
#define ADD_RTL_INSNL(seq, line, insn, label, op)		\
    (ADD_INSN2(seq, line, insn, label, op), LABEL_REF(label))
#define ADD_INSN4(seq, line, insn, op1, op2, op3, op4)	\
  ADD_ELEM((seq), (LINK_ELEMENT *) \
           new_insn_body(iseq, (line), BIN(insn), 4, (VALUE)(op1), (VALUE)(op2), (VALUE)(op3), (VALUE)(op4)))

#define ADD_INSN5(seq, line, insn, op1, op2, op3, op4, op5)	\
  ADD_ELEM((seq), (LINK_ELEMENT *) \
           new_insn_body(iseq, (line), BIN(insn), 5, (VALUE)(op1), (VALUE)(op2), (VALUE)(op3), (VALUE)(op4), (VALUE)(op5)))

#define ADD_INSN6(seq, line, insn, op1, op2, op3, op4, op5, op6)	\
  ADD_ELEM((seq), (LINK_ELEMENT *) \
           new_insn_body(iseq, (line), BIN(insn), 6, (VALUE)(op1), (VALUE)(op2), (VALUE)(op3), (VALUE)(op4), (VALUE)(op5), (VALUE)(op6)))

#define ADD_INSN7(seq, line, insn, op1, op2, op3, op4, op5, op6, op7)	\
  ADD_ELEM((seq), (LINK_ELEMENT *) \
           new_insn_body(iseq, (line), BIN(insn), 7, (VALUE)(op1), (VALUE)(op2), (VALUE)(op3), (VALUE)(op4), \
			 (VALUE)(op5), (VALUE)(op6), (VALUE)(op7)))

#define ADD_RTL_CALL_RECEIVER(seq, line, res)	\
  ADD_INSN1((seq), (line), self2var, res)

#define ADD_RTL_SEND_R(seq, line, id, argc, block, self_p, start, flag, keywords) \
  ADD_ELEM((seq), (LINK_ELEMENT *) new_rtl_insn_send(iseq, (line), (id), (VALUE)(argc), (block), (self_p), (start), (VALUE)(flag), (keywords)))

#define ADD_RTL_SEND_RECV(seq, line, id, argc, block, recv, start, flag, keywords) \
  ADD_ELEM((seq), (LINK_ELEMENT *) new_rtl_insn_send_recv(iseq, (line), (id), (VALUE)(argc), (block), (recv), (start), (VALUE)(flag), (keywords)))

#define ADD_RTL_VMCORE_SEND(seq, line, id, block, start)	\
  ADD_ELEM((seq), (LINK_ELEMENT *) new_rtl_vmcore_send(iseq, (line), (id), (block), (start)))

/* compile node */
#define COMPILE(anchor, desc, node, res, curr_temp_vars) \
  (debug_compile("== " desc "\n", \
                 iseq_compile_each(iseq, (anchor), (node), 0, (res), (curr_temp_vars))))

/* Compile NODE, this node's value will be popped (not used).  RES and
   CURR_TEMP_VARS are inout arguments.  See their meaning in
   iseq_compile_each comment.  */
#define COMPILE_POPPED(anchor, desc, node, res, curr_temp_vars)    \
  (debug_compile("== " desc "\n", \
                 iseq_compile_each(iseq, (anchor), (node), 1, (res), (curr_temp_vars))))

/* As above but with explicit popped argument. */
#define COMPILE_(anchor, desc, node, popped, res, curr_temp_vars)  \
  (debug_compile("== " desc "\n", \
                 iseq_compile_each(iseq, (anchor), (node), (popped), (res), (curr_temp_vars))))

/* See res and curr_temp_vars meaning in iseq_compile_each comment.  */
#define COMPILE_RECV(anchor, desc, node, res, curr_temp_vars)	\
    (private_recv_p(node) ? \
     (ADD_INSN(anchor, nd_line(node), putself), VM_CALL_FCALL) : \
     (COMPILE(anchor, desc, node->nd_recv, (res), (curr_temp_vars)), 0))

#define OPERAND_AT(insn, idx) \
  (((INSN*)(insn))->operands[(idx)])

#define INSN_OF(insn) \
  (((INSN*)(insn))->insn_id)

#define IS_INSN(link) ((link)->type == ISEQ_ELEMENT_INSN)
#define IS_LABEL(link) ((link)->type == ISEQ_ELEMENT_LABEL)

#define IS_INSN_ID(iobj, insn) (INSN_OF(iobj) == BIN(insn))

/* error */
typedef void (*compile_error_func)(rb_iseq_t *, int, const char *, ...);

static void
append_compile_error(rb_iseq_t *iseq, int line, const char *fmt, ...)
{
    VALUE err_info = ISEQ_COMPILE_DATA(iseq)->err_info;
    VALUE file = iseq->body->location.path;
    VALUE err = err_info;
    va_list args;

    va_start(args, fmt);
    err = rb_syntax_error_append(err, file, line, -1, NULL, fmt, args);
    va_end(args);
    if (NIL_P(err_info)) {
	RB_OBJ_WRITE(iseq, &ISEQ_COMPILE_DATA(iseq)->err_info, err);
	rb_set_errinfo(err);
    }
}

static void
compile_bug(rb_iseq_t *iseq, int line, const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    rb_report_bug_valist(iseq->body->location.path, line, fmt, args);
    va_end(args);
    abort();
}

NOINLINE(static compile_error_func prepare_compile_error(rb_iseq_t *iseq));

static compile_error_func
prepare_compile_error(rb_iseq_t *iseq)
{
    if (compile_debug) return &compile_bug;
    return &append_compile_error;
}

#define COMPILE_ERROR prepare_compile_error(iseq)

#define ERROR_ARGS_AT(n) iseq, nd_line(n),
#define ERROR_ARGS ERROR_ARGS_AT(node)

#define EXPECT_NODE(prefix, node, ndtype) \
do { \
    NODE *error_node = (node); \
    enum node_type error_type = nd_type(error_node); \
    if (error_type != (ndtype)) { \
	compile_bug(ERROR_ARGS_AT(error_node) \
		    prefix ": " #ndtype " is expected, but %s", \
		    ruby_node_name(error_type)); \
    } \
} while (0)

#define EXPECT_NODE_NONULL(prefix, parent, ndtype) \
do { \
    compile_bug(ERROR_ARGS_AT(parent) \
		prefix ": must be " #ndtype ", but 0"); \
} while (0)

#define UNKNOWN_NODE(prefix, node) \
do { \
    NODE *error_node = (node); \
    compile_bug(ERROR_ARGS_AT(error_node) prefix ": unknown node (%s)", \
		ruby_node_name(nd_type(error_node))); \
} while (0)

#define COMPILE_OK 1
#define COMPILE_NG 0

#define CHECK(sub) if (!(sub)) {BEFORE_RETURN;return COMPILE_NG;}
#define BEFORE_RETURN

/* leave name uninitialized so that compiler warn if INIT_ANCHOR is
 * missing */
#define DECL_ANCHOR(name) \
    LINK_ANCHOR name[1] = {{{0,},}}
#define INIT_ANCHOR(name) \
    (name->last = &name->anchor)

static inline VALUE
freeze_hide_obj(VALUE obj)
{
    OBJ_FREEZE(obj);
    RBASIC_CLEAR_CLASS(obj);
    return obj;
}

#include "optinsn.inc"
#if OPT_INSTRUCTIONS_UNIFICATION
#include "optunifs.inc"
#endif

/* for debug */
#if CPDEBUG < 0
#define ISEQ_ARG iseq,
#define ISEQ_ARG_DECLARE rb_iseq_t *iseq,
#else
#define ISEQ_ARG
#define ISEQ_ARG_DECLARE
#endif

#if CPDEBUG
#define gl_node_level ISEQ_COMPILE_DATA(iseq)->node_level
#endif

static void dump_disasm_list(LINK_ELEMENT *elem);

static int insn_data_length(INSN *iobj);
static INSN *new_insn_body(rb_iseq_t *iseq, int line_no, enum ruby_vminsn_type insn_id, int argc, ...);
static LABEL *new_label_body(rb_iseq_t *iseq, long line);

static int iseq_compile_each(rb_iseq_t *iseq, LINK_ANCHOR *const anchor, NODE * n, int, int *, int *);

static int iseq_setup(rb_iseq_t *iseq, LINK_ANCHOR *const anchor);
static int iseq_optimize(rb_iseq_t *iseq, LINK_ANCHOR *const anchor);
static int iseq_insns_unification(rb_iseq_t *iseq, LINK_ANCHOR *const anchor);

static int iseq_set_local_table(rb_iseq_t *iseq, const ID *tbl);
static int iseq_set_exception_local_table(rb_iseq_t *iseq);

static int iseq_set_arguments(rb_iseq_t *iseq, LINK_ANCHOR *const anchor, NODE * node,
			      int *result, int *curr_temp_vars_num);

static int iseq_set_sequence_stackcaching(rb_iseq_t *iseq, LINK_ANCHOR *const anchor);
static int iseq_set_sequence(rb_iseq_t *iseq, LINK_ANCHOR *const anchor);
static int iseq_set_exception_table(rb_iseq_t *iseq);
static int iseq_set_optargs_table(rb_iseq_t *iseq);

/*
 * To make Array to LinkedList, use link_anchor
 */

static void
verify_list(ISEQ_ARG_DECLARE const char *info, LINK_ANCHOR *const anchor)
{
#if CPDEBUG
    int flag = 0;
    LINK_ELEMENT *list, *plist;

    if (!compile_debug) return;

    list = anchor->anchor.next;
    plist = &anchor->anchor;
    while (list) {
	if (plist != list->prev) {
	    flag += 1;
	}
	plist = list;
	list = list->next;
    }

    if (anchor->last != plist && anchor->last != 0) {
	flag |= 0x70000;
    }

    if (flag != 0) {
	rb_bug("list verify error: %08x (%s)", flag, info);
    }
#endif
}
#if CPDEBUG < 0
#define verify_list(info, anchor) verify_list(iseq, (info), (anchor))
#endif

/*
 * elem1, elem2 => elem1, elem2, elem
 */
static void
ADD_ELEM(ISEQ_ARG_DECLARE LINK_ANCHOR *const anchor, LINK_ELEMENT *elem)
{
    elem->prev = anchor->last;
    anchor->last->next = elem;
    anchor->last = elem;
    verify_list("add", anchor);
}

/*
 * elem1, before, elem2 => elem1, before, elem, elem2
 */
static void
APPEND_ELEM(ISEQ_ARG_DECLARE LINK_ANCHOR *const anchor, LINK_ELEMENT *before, LINK_ELEMENT *elem)
{
    elem->prev = before;
    elem->next = before->next;
    elem->next->prev = elem;
    before->next = elem;
    if (before == anchor->last) anchor->last = elem;
    verify_list("add", anchor);
}
#if CPDEBUG < 0
#define ADD_ELEM(anchor, elem) ADD_ELEM(iseq, (anchor), (elem))
#define APPEND_ELEM(anchor, before, elem) APPEND_ELEM(iseq, (anchor), (before), (elem))
#endif

/*
 * elem1, before, ... => elem1, before, elem, ...
 */
static void
INSERT_ELEM_AFTER(ISEQ_ARG_DECLARE LINK_ANCHOR *anchor, LINK_ELEMENT *before, LINK_ELEMENT *elem)
{
    elem->prev = before;
    elem->next = before->next;
    if (elem->next != NULL)
      elem->next->prev = elem;
    before->next = elem;
    if (before == anchor->last) anchor->last = elem;
    verify_list("add", anchor);
}

static int
iseq_add_mark_object(const rb_iseq_t *iseq, VALUE v)
{
    if (!SPECIAL_CONST_P(v)) {
	rb_iseq_add_mark_object(iseq, v);
    }
    return COMPILE_OK;
}

#define ruby_sourcefile		RSTRING_PTR(iseq->body->location.path)

static int
iseq_add_mark_object_compile_time(const rb_iseq_t *iseq, VALUE v)
{
    if (!SPECIAL_CONST_P(v)) {
	rb_ary_push(ISEQ_COMPILE_DATA(iseq)->mark_ary, v);
    }
    return COMPILE_OK;
}

static int
validate_label(st_data_t name, st_data_t label, st_data_t arg)
{
    rb_iseq_t *iseq = (rb_iseq_t *)arg;
    LABEL *lobj = (LABEL *)label;
    if (!lobj->link.next) {
	do {
	    COMPILE_ERROR(iseq, lobj->position,
			  "%"PRIsVALUE": undefined label",
			  rb_id2str((ID)name));
	} while (0);
    }
    return ST_CONTINUE;
}

static void
validate_labels(rb_iseq_t *iseq, st_table *labels_table)
{
    st_foreach(labels_table, validate_label, (st_data_t)iseq);
    st_free_table(labels_table);
    if (!NIL_P(ISEQ_COMPILE_DATA(iseq)->err_info)) {
	rb_exc_raise(ISEQ_COMPILE_DATA(iseq)->err_info);
    }
}

/* Increase *TEMP_VARS_NUM by INCR and update temp_vars_num in ISEQ
   body if necessary.  */
static inline void
increment_temps_var(rb_iseq_t *iseq, int *temp_vars_num, int incr)
{
  (*temp_vars_num) += incr;
  if (*temp_vars_num > (int) iseq->body->temp_vars_num)
    iseq->body->temp_vars_num = *temp_vars_num;
}

/* Update *TEMP_VARS_NUM which means reserving a temporary on the
   stack.  Return its location index.  */
static inline int
get_temp_stack_slot(rb_iseq_t *iseq, int *temp_vars_num)
{
  increment_temps_var(iseq, temp_vars_num, 1);
  /* local size + 1 includes me and specval */
  return *temp_vars_num + (int) iseq->body->local_table_size;
}

/* Location indexes denoting some preferable locations for the insn
   result in a temporary (on the stack) or anywhere (local or
   temporary variable).  */
static const int stack_result = INT_MIN + 1;
static const int anywhere_result = INT_MIN;

/* Take a preferable location index from RESULT if it is not NULL and
   return the location index and also pass it through RESULT.  Update
   *TEMP_VARS_NUM if we reserved a new temporary on the stack.  */
static inline int
setup_result_var_number(rb_iseq_t *iseq, int *result, int *temp_vars_num)
{
  int new_p = result == NULL || *result == stack_result || *result == anywhere_result;
  int res = (new_p ? get_temp_stack_slot(iseq, temp_vars_num) : *result);

  if (result != NULL)
    *result = res;
  return res;
}

static void REMOVE_ELEM(LINK_ELEMENT *elem);

/* Return true if ELEM is labeled and there is at least one jump to
   the label.  */
static inline int
prev_used_label_p(LINK_ELEMENT *elem)
{
  return (elem->prev != NULL && elem->prev->type == ISEQ_ELEMENT_LABEL
	  && ((LABEL *) elem->prev)->refcnt != 0);
}

VALUE
rb_iseq_compile_node(rb_iseq_t *iseq, NODE *node)
{
    int result = anywhere_result, temp_vars_num = 0;
    int event = 0, labeled_ret_p = FALSE;

    DECL_ANCHOR(ret);
    INIT_ANCHOR(ret);

    if (node == 0) {
	COMPILE(ret, "nil", node, NULL, &temp_vars_num);
	iseq_set_local_table(iseq, 0);
    }
    else if (nd_type(node) == NODE_SCOPE) {
	/* iseq type of top, method, class, block */
	iseq_set_local_table(iseq, node->nd_tbl);
	iseq_set_arguments(iseq, ret, node->nd_args, &result, &temp_vars_num);
	temp_vars_num = 0;
	result = anywhere_result;

	switch (iseq->body->type) {
	  case ISEQ_TYPE_BLOCK:
	    {
		LABEL *start = ISEQ_COMPILE_DATA(iseq)->start_label = NEW_LABEL(0);
		LABEL *end = ISEQ_COMPILE_DATA(iseq)->end_label = NEW_LABEL(0);

		start->rescued = LABEL_RESCUE_BEG;
		end->rescued = LABEL_RESCUE_END;

		start->sp = end->sp = temp_vars_num;

		ADD_TRACE(ret, FIX2INT(iseq->body->location.first_lineno), RUBY_EVENT_B_CALL);
		ADD_LABEL(ret, start);
		CHECK(COMPILE(ret, "block body", node->nd_body, &result, &temp_vars_num));
		ADD_LABEL(ret, end);
		labeled_ret_p = TRUE;
		event = RUBY_EVENT_B_RETURN;

		/* wide range catch handler must put at last */
		ADD_CATCH_ENTRY(CATCH_TYPE_REDO, start, end, 0, start);
		ADD_CATCH_ENTRY(CATCH_TYPE_NEXT, start, end, 0, end);
		break;
	    }
	  case ISEQ_TYPE_CLASS:
	    {
		ADD_TRACE(ret, FIX2INT(iseq->body->location.first_lineno), RUBY_EVENT_CLASS);
		CHECK(COMPILE(ret, "scoped node", node->nd_body, &result, &temp_vars_num));
		event = RUBY_EVENT_END;
		break;
	    }
	  case ISEQ_TYPE_METHOD:
	    {
		ADD_TRACE(ret, FIX2INT(iseq->body->location.first_lineno), RUBY_EVENT_CALL);
		CHECK(COMPILE(ret, "scoped node", node->nd_body, &result, &temp_vars_num));
		event = RUBY_EVENT_RETURN;
		break;
	    }
	  default: {
	    COMPILE(ret, "scoped node", node->nd_body, &result, &temp_vars_num);
	    break;
	  }
	}
    }
    else if (RB_TYPE_P((VALUE)node, T_IMEMO)) {
	const struct vm_ifunc *ifunc = (struct vm_ifunc *)node;
	int temp_res = stack_result;
	/* user callback */
	(*ifunc->func)(iseq, ret, ifunc->data);
	/* Stack top and reserving slot for the result */
	result = setup_result_var_number(iseq, &temp_res, &temp_vars_num);
    }
    else {
	switch (iseq->body->type) {
	  case ISEQ_TYPE_METHOD:
	  case ISEQ_TYPE_CLASS:
	  case ISEQ_TYPE_BLOCK:
	  case ISEQ_TYPE_EVAL:
	  case ISEQ_TYPE_MAIN:
	  case ISEQ_TYPE_TOP:
	    COMPILE_ERROR(ERROR_ARGS "compile/should not be reached: %s:%d",
			  __FILE__, __LINE__);
	    return COMPILE_NG;
	  case ISEQ_TYPE_RESCUE:
	    {
		int temp_res = stack_result;

		iseq_set_exception_local_table(iseq);
		CHECK(COMPILE(ret, "rescue", node, &temp_res, &temp_vars_num));
	    }
	    break;
	  case ISEQ_TYPE_ENSURE:
	    {
		int temp_res = stack_result;

		iseq_set_exception_local_table(iseq);
		CHECK(COMPILE_POPPED(ret, "ensure", node, &temp_res, &temp_vars_num));
	    }
	    break;
	  case ISEQ_TYPE_DEFINED_GUARD:
	    iseq_set_exception_local_table(iseq);
	    CHECK(COMPILE(ret, "defined guard", node, &result, &temp_vars_num));
	    break;
	  default:
	    compile_bug(ERROR_ARGS "unknown scope");
	}
    }

    if (iseq->body->type == ISEQ_TYPE_RESCUE || iseq->body->type == ISEQ_TYPE_ENSURE) {
	ADD_INSN2(ret, 0, raise_except, INT2LINT(LVAR_ERRINFO + VM_ENV_DATA_SIZE - 1),
		  INT2FIX(0) /* continue throw */);
    }
    else {
	int ls = iseq->body->local_table_size;
	int line = event == 0 ? ISEQ_COMPILE_DATA(iseq)->last_line : nd_line(node);

	if (result == anywhere_result) {
	    /* The result did not changed since its initialization.  */
	    ADD_INSN2(ret, line, val_ret, Qnil, INT2FIX(event));
	} else if (! labeled_ret_p && ret->last
		   && ret->last->type == ISEQ_ELEMENT_INSN
		   && IS_INSN_ID((INSN *) ret->last, val2temp)
		   && LINT2INT(((INSN *) ret->last)->operands[0]) == ls - result) {
	    LINK_ELEMENT *last = ret->last;

	    ADD_INSN2(ret, line, val_ret,
		      ((INSN *) last)->operands[1], INT2FIX(event));
	    if (! prev_used_label_p (last))
		REMOVE_ELEM(last);
	} else {
	    if (result > ls)
		ADD_INSN2(ret, line, temp_ret,
			  INT2LINT(ls - result), INT2FIX(event));
	    else
		ADD_INSN2(ret, line, loc_ret,
			  INT2LINT(ls - result), INT2FIX(event));
	}
    }

#if SUPPORT_JOKE
    if (ISEQ_COMPILE_DATA(iseq)->labels_table) {
	st_table *labels_table = ISEQ_COMPILE_DATA(iseq)->labels_table;
	ISEQ_COMPILE_DATA(iseq)->labels_table = 0;
	validate_labels(iseq, labels_table);
    }
#endif
    return iseq_setup(iseq, ret);
}

int
rb_iseq_translate_threaded_code(rb_iseq_t *iseq)
{
#if OPT_DIRECT_THREADED_CODE || OPT_CALL_THREADED_CODE
    const void * const *table = rb_vm_get_insns_address_table();
    unsigned int i;
    VALUE *encoded = (VALUE *)iseq->body->iseq_encoded;

    for (i = 0; i < iseq->body->iseq_size; /* */ ) {
	int insn = (int)iseq->body->iseq_encoded[i];
	int len = insn_len(insn);

	encoded[i] = (VALUE)table[insn];
	if (len > 1) {
	  const char *types = insn_op_types(insn);
	  char type = types[0];
	  if (type == TS_INSN)
	    encoded[i + 1] = (VALUE)table[(int)iseq->body->iseq_encoded[i + 1]];
	}
	i += len;
    }
#endif
    return COMPILE_OK;
}

VALUE *
rb_iseq_original_iseq(const rb_iseq_t *iseq) /* cold path */
{
    VALUE *original_code;

    if (ISEQ_ORIGINAL_ISEQ(iseq)) return ISEQ_ORIGINAL_ISEQ(iseq);
    original_code = ISEQ_ORIGINAL_ISEQ_ALLOC(iseq, iseq->body->iseq_size);
    MEMCPY(original_code, iseq->body->iseq_encoded, VALUE, iseq->body->iseq_size);

#if OPT_DIRECT_THREADED_CODE || OPT_CALL_THREADED_CODE
    {
	unsigned int i;

	for (i = 0; i < iseq->body->iseq_size; /* */ ) {
	    const void *addr = (const void *)original_code[i];
	    const int insn = rb_vm_insn_addr2insn(addr);
	    int len = insn_len(insn);

	    original_code[i] = insn;
	    if (len > 1) {
		const char *types = insn_op_types(insn);
		char type = types[0];
		if (type == TS_INSN)
		    /* Change also an insn which is the operand of
		       another insn.  It is always the first
		       operand.  */
		  original_code[i + 1] = rb_vm_insn_addr2insn((const void *)original_code[i + 1]);
	    }
	    i += len;
	}
    }
#endif
    return original_code;
}

/*********************************************/
/* definition of data structure for compiler */
/*********************************************/

/*
 * On 32-bit SPARC, GCC by default generates SPARC V7 code that may require
 * 8-byte word alignment. On the other hand, Oracle Solaris Studio seems to
 * generate SPARCV8PLUS code with unaligned memory access instructions.
 * That is why the STRICT_ALIGNMENT is defined only with GCC.
 */
#if defined(__sparc) && SIZEOF_VOIDP == 4 && defined(__GNUC__)
  #define STRICT_ALIGNMENT
#endif

#ifdef STRICT_ALIGNMENT
  #if defined(HAVE_TRUE_LONG_LONG) && SIZEOF_LONG_LONG > SIZEOF_VALUE
    #define ALIGNMENT_SIZE SIZEOF_LONG_LONG
  #else
    #define ALIGNMENT_SIZE SIZEOF_VALUE
  #endif
  #define PADDING_SIZE_MAX    ((size_t)((ALIGNMENT_SIZE) - 1))
  #define ALIGNMENT_SIZE_MASK PADDING_SIZE_MAX
  /* Note: ALIGNMENT_SIZE == (2 ** N) is expected. */
#else
  #define PADDING_SIZE_MAX 0
#endif /* STRICT_ALIGNMENT */

#ifdef STRICT_ALIGNMENT
/* calculate padding size for aligned memory access */
static size_t
calc_padding(void *ptr, size_t size)
{
    size_t mis;
    size_t padding = 0;

    mis = (size_t)ptr & ALIGNMENT_SIZE_MASK;
    if (mis > 0) {
        padding = ALIGNMENT_SIZE - mis;
    }
/*
 * On 32-bit sparc or equivalents, when a single VALUE is requested
 * and padding == sizeof(VALUE), it is clear that no padding is needed.
 */
#if ALIGNMENT_SIZE > SIZEOF_VALUE
    if (size == sizeof(VALUE) && padding == sizeof(VALUE)) {
        padding = 0;
    }
#endif

    return padding;
}
#endif /* STRICT_ALIGNMENT */

static void *
compile_data_alloc(rb_iseq_t *iseq, size_t size)
{
    void *ptr = 0;
    struct iseq_compile_data_storage *storage =
	ISEQ_COMPILE_DATA(iseq)->storage_current;
#ifdef STRICT_ALIGNMENT
    size_t padding = calc_padding((void *)&storage->buff[storage->pos], size);
#else
    const size_t padding = 0; /* expected to be optimized by compiler */
#endif /* STRICT_ALIGNMENT */

    if (size >= INT_MAX - padding) rb_memerror();
    if (storage->pos + size + padding > storage->size) {
	unsigned int alloc_size = storage->size;

	while (alloc_size < size + PADDING_SIZE_MAX) {
	    if (alloc_size >= INT_MAX / 2) rb_memerror();
	    alloc_size *= 2;
	}
	storage->next = (void *)ALLOC_N(char, alloc_size +
					SIZEOF_ISEQ_COMPILE_DATA_STORAGE);
	storage = ISEQ_COMPILE_DATA(iseq)->storage_current = storage->next;
	storage->next = 0;
	storage->pos = 0;
	storage->size = alloc_size;
#ifdef STRICT_ALIGNMENT
        padding = calc_padding((void *)&storage->buff[storage->pos], size);
#endif /* STRICT_ALIGNMENT */
    }

#ifdef STRICT_ALIGNMENT
    storage->pos += (int)padding;
#endif /* STRICT_ALIGNMENT */

    ptr = (void *)&storage->buff[storage->pos];
    storage->pos += (int)size;
    return ptr;
}

static INSN *
compile_data_alloc_insn(rb_iseq_t *iseq)
{
    return (INSN *)compile_data_alloc(iseq, sizeof(INSN));
}

static LABEL *
compile_data_alloc_label(rb_iseq_t *iseq)
{
    return (LABEL *)compile_data_alloc(iseq, sizeof(LABEL));
}

/*
 * elemX, elem1, elemY => elemX, elem2, elemY
 */
static void
REPLACE_ELEM(LINK_ELEMENT *elem1, LINK_ELEMENT *elem2)
{
    elem2->prev = elem1->prev;
    elem2->next = elem1->next;
    if (elem1->prev) {
	elem1->prev->next = elem2;
    }
    if (elem1->next) {
	elem1->next->prev = elem2;
    }
}

static void
REMOVE_ELEM(LINK_ELEMENT *elem)
{
    elem->prev->next = elem->next;
    if (elem->next) {
	elem->next->prev = elem->prev;
    }
}

static LINK_ELEMENT *
FIRST_ELEMENT(LINK_ANCHOR *const anchor)
{
    return anchor->anchor.next;
}

static LINK_ELEMENT *
POP_ELEMENT(ISEQ_ARG_DECLARE LINK_ANCHOR *const anchor)
{
    LINK_ELEMENT *elem = anchor->last;
    anchor->last = anchor->last->prev;
    anchor->last->next = 0;
    verify_list("pop", anchor);
    return elem;
}
#if CPDEBUG < 0
#define POP_ELEMENT(anchor) POP_ELEMENT(iseq, (anchor))
#endif

/*
 * anc1: e1, e2, e3
 * anc2: e4, e5
 *#=>
 * anc1: e1, e2, e3, e4, e5
 * anc2: e4, e5 (broken)
 */
static void
APPEND_LIST(ISEQ_ARG_DECLARE LINK_ANCHOR *const anc1, LINK_ANCHOR *const anc2)
{
    if (anc2->anchor.next) {
	anc1->last->next = anc2->anchor.next;
	anc2->anchor.next->prev = anc1->last;
	anc1->last = anc2->last;
    }
    verify_list("append", anc1);
}
#if CPDEBUG < 0
#define APPEND_LIST(anc1, anc2) APPEND_LIST(iseq, (anc1), (anc2))
#endif

#if CPDEBUG
static void
debug_list(ISEQ_ARG_DECLARE LINK_ANCHOR *const anchor)
{
    LINK_ELEMENT *list = FIRST_ELEMENT(anchor);
    printf("----\n");
    printf("anch: %p, frst: %p, last: %p\n", &anchor->anchor,
	   anchor->anchor.next, anchor->last);
    while (list) {
	printf("curr: %p, next: %p, prev: %p, type: %d\n", list, list->next,
	       list->prev, FIX2INT(list->type));
	list = list->next;
    }
    printf("----\n");

    dump_disasm_list(anchor->anchor.next);
    verify_list("debug list", anchor);
}
#if CPDEBUG < 0
#define debug_list(anc) debug_list(iseq, (anc))
#endif
#endif

static LABEL *
new_label_body(rb_iseq_t *iseq, long line)
{
    LABEL *labelobj = compile_data_alloc_label(iseq);

    labelobj->link.type = ISEQ_ELEMENT_LABEL;
    labelobj->link.next = 0;

    labelobj->label_no = ISEQ_COMPILE_DATA(iseq)->label_no++;
    labelobj->sc_state = 0;
    labelobj->sp = -1;
    labelobj->refcnt = 0;
    labelobj->set = 0;
    labelobj->rescued = LABEL_RESCUE_NONE;
    return labelobj;
}

static INSN *
new_insn_core(rb_iseq_t *iseq, int line_no,
	      int insn_id, int argc, VALUE *argv)
{
    INSN *iobj = compile_data_alloc_insn(iseq);
    /* printf("insn_id: %d, line: %d\n", insn_id, line_no); */

    iobj->link.type = ISEQ_ELEMENT_INSN;
    iobj->link.next = 0;
    iobj->insn_id = insn_id;
    iobj->line_no = line_no;
    iobj->operands = argv;
    iobj->operand_size = argc;
    iobj->sc_state = 0;
    iobj->check = 0;
    return iobj;
}

static INSN *
new_insn_body(rb_iseq_t *iseq, int line_no, enum ruby_vminsn_type insn_id, int argc, ...)
{
    VALUE *operands = 0;
    va_list argv;
    if (argc > 0) {
	int i;
	va_init_list(argv, argc);
	operands = (VALUE *)compile_data_alloc(iseq, sizeof(VALUE) * argc);
	for (i = 0; i < argc; i++) {
	    VALUE v = va_arg(argv, VALUE);
	    operands[i] = v;
	}
	va_end(argv);
    }
    return new_insn_core(iseq, line_no, insn_id, argc, operands);
}

/* Return a new specialized move insn FROM->TO (both are variable
   offsets) with LINE_NO for ISEQ.  */
static INSN *
new_local_move(rb_iseq_t *iseq, int line_no, int to, int from) {
    enum ruby_vminsn_type insn_id;
    if (to < 0) {
	if (from < 0) {
	    insn_id = BIN(temp2temp);
	} else
	    insn_id = BIN(loc2temp);
    } else if (from < 0) {
	insn_id = BIN(temp2loc);
    } else {
	insn_id = BIN(loc2loc);
    }
    return new_insn_body(iseq, line_no, insn_id, 2, (VALUE) INT2LINT(to), (VALUE) INT2LINT(from));
}

/* Return a new call data for a call with given features as
   parameters.  */
static struct rb_call_data *
new_calldata(rb_iseq_t *iseq, ID mid, int argc, int call_start, unsigned int flag, struct rb_call_info_kw_arg *kw_arg, int has_blockiseq)
{
    size_t size = kw_arg != NULL ? sizeof(struct rb_call_data_with_kwarg) : sizeof(struct rb_call_data);
    struct rb_call_data *cd = (struct rb_call_data *)compile_data_alloc(iseq, size);
    struct rb_call_data_with_kwarg *cd_kw = (struct rb_call_data_with_kwarg *)cd;

    cd->call_info.mid = mid;
    cd->call_info.flag = flag;
    cd->call_info.orig_argc = argc;
    cd->call_start = INT2LINT(call_start);

    if (kw_arg) {
	cd->call_info.flag |= VM_CALL_KWARG;
	cd_kw->kw_arg = kw_arg;
	cd->call_info.orig_argc += kw_arg->keyword_len;
	iseq->body->cd_kw_size++;
    }
    else {
	iseq->body->cd_size++;
    }

    if (!(cd->call_info.flag & (VM_CALL_ARGS_SPLAT | VM_CALL_ARGS_BLOCKARG)) &&
	kw_arg == NULL && !has_blockiseq) {
	cd->call_info.flag |= VM_CALL_ARGS_SIMPLE;
    }
    return cd;
}

/* Return a new specialized call insn with call data given as
   parameters.  True SELF_P means we need a call which put self first
   at CALL_START (a location index of temporary at the stack).  */
static INSN *
new_rtl_insn_send(rb_iseq_t *iseq, int line_no, ID id, VALUE argc, const rb_iseq_t *blockiseq,
		  int self_p, int call_start, VALUE flag, struct rb_call_info_kw_arg *keywords)
{
    CALL_DATA cd = new_calldata(iseq, id, FIX2INT(argc),
				(int) iseq->body->local_table_size - call_start,
				FIX2INT(flag), keywords, blockiseq != NULL);
    int simple_p = (cd->call_info.flag & VM_CALL_ARGS_BLOCKARG) == 0 && blockiseq == NULL;
    VALUE *operands = (VALUE *)compile_data_alloc(iseq, sizeof(VALUE) * (simple_p ? 3 : 4));
    operands[0] = (VALUE)cd;
    operands[1] = INT2LINT((int) iseq->body->local_table_size - call_start);
    if (simple_p) {
	return new_insn_core(iseq, line_no, self_p ? BIN(simple_call_self) : BIN(simple_call), 2, operands);
    } else {
	operands[2] = (VALUE)blockiseq;
	return new_insn_core(iseq, line_no, self_p ? BIN(call_self) : BIN(call), 3, operands);
    }
}

/* Return a new specialized call insn with call data given as
   parameters.  The call insn should put RECV first at CALL_START (a
   location index of temporary at the stack).  */
static INSN *
new_rtl_insn_send_recv(rb_iseq_t *iseq, int line_no, ID id, VALUE argc, const rb_iseq_t *blockiseq,
		       int recv, int call_start, VALUE flag, struct rb_call_info_kw_arg *keywords)
{
    CALL_DATA cd = new_calldata(iseq, id, FIX2INT(argc),
				(int) iseq->body->local_table_size - call_start,
				FIX2INT(flag), keywords, blockiseq != NULL);
    int simple_p = (cd->call_info.flag & VM_CALL_ARGS_BLOCKARG) == 0 && blockiseq == NULL;
    int self_p = recv == anywhere_result;
    int no_recv_p = self_p || call_start == recv;
    VALUE *operands = (VALUE *)compile_data_alloc(iseq, sizeof(VALUE) * ((simple_p ? 2 : 3) + !no_recv_p));
    operands[0] = (VALUE)cd;
    operands[1] = INT2LINT((int) iseq->body->local_table_size - call_start);
    if (simple_p) {
	if (no_recv_p)
	    return new_insn_core(iseq, line_no, self_p ?  BIN(simple_call_self) : BIN(simple_call), 2, operands);
	operands[2] = INT2LINT((int) iseq->body->local_table_size - recv);
	return new_insn_core(iseq, line_no, BIN(simple_call_recv), 3, operands);
    } else {
	operands[2] = (VALUE)blockiseq;
	if (no_recv_p)
	    return new_insn_core(iseq, line_no, self_p ?  BIN(call_self) : BIN(call), 3, operands);
	operands[3] = INT2LINT((int) iseq->body->local_table_size - recv);
	return new_insn_core(iseq, line_no, BIN(call_recv), 4, operands);
    }
}

/* Return a new vmcore_call insn with given parameters.  */
static INSN *
new_rtl_vmcore_send(rb_iseq_t *iseq, int line_no, ID id, const rb_iseq_t *blockiseq, int call_start)
{
    CALL_DATA cd = new_calldata(iseq, id, 0,
				(int) iseq->body->local_table_size - call_start,
				VM_CALL_FCALL, NULL, blockiseq != NULL);
    VALUE *operands = (VALUE *)compile_data_alloc(iseq, sizeof(VALUE) * 3);
    operands[0] = (VALUE)cd;
    operands[1] = INT2LINT((int) iseq->body->local_table_size - call_start);
    operands[2] = (VALUE)blockiseq;
    return new_insn_core(iseq, line_no, BIN(vmcore_call), 3, operands);
}

static rb_iseq_t *
new_child_iseq(rb_iseq_t *iseq, NODE *node,
	       VALUE name, rb_iseq_t *parent, enum iseq_type type, int line_no)
{
    rb_iseq_t *ret_iseq;

    debugs("[new_child_iseq]> ---------------------------------------\n");
    ret_iseq = rb_iseq_new_with_opt(node, name,
				    iseq_path(iseq), iseq_absolute_path(iseq),
				    INT2FIX(line_no), parent, type, ISEQ_COMPILE_DATA(iseq)->option);
    debugs("[new_child_iseq]< ---------------------------------------\n");
    iseq_add_mark_object(iseq, (VALUE)ret_iseq);
    return ret_iseq;
}

static int
iseq_setup(rb_iseq_t *iseq, LINK_ANCHOR *const anchor)
{
    /* debugs("[compile step 2] (iseq_array_to_linkedlist)\n"); */

    if (compile_debug > 5)
	dump_disasm_list(FIRST_ELEMENT(anchor));

    debugs("[compile step 3.1 (iseq_optimize)]\n");
    iseq_optimize(iseq, anchor);

    if (compile_debug > 5)
	dump_disasm_list(FIRST_ELEMENT(anchor));

    if (ISEQ_COMPILE_DATA(iseq)->option->instructions_unification) {
	debugs("[compile step 3.2 (iseq_insns_unification)]\n");
	iseq_insns_unification(iseq, anchor);
	if (compile_debug > 5)
	    dump_disasm_list(FIRST_ELEMENT(anchor));
    }

    if (ISEQ_COMPILE_DATA(iseq)->option->stack_caching) {
	debugs("[compile step 3.3 (iseq_set_sequence_stackcaching)]\n");
	iseq_set_sequence_stackcaching(iseq, anchor);
	if (compile_debug > 5)
	    dump_disasm_list(FIRST_ELEMENT(anchor));
    }

    debugs("[compile step 4.1 (iseq_set_sequence)]\n");
    if (!iseq_set_sequence(iseq, anchor)) return COMPILE_NG;
    if (compile_debug > 5)
	dump_disasm_list(FIRST_ELEMENT(anchor));

    debugs("[compile step 4.2 (iseq_set_exception_table)]\n");
    if (!iseq_set_exception_table(iseq)) return COMPILE_NG;

    debugs("[compile step 4.3 (set_optargs_table)] \n");
    if (!iseq_set_optargs_table(iseq)) return COMPILE_NG;

    debugs("[compile step 5 (iseq_translate_threaded_code)] \n");
    if (!rb_iseq_translate_threaded_code(iseq)) return COMPILE_NG;

    if (compile_debug > 1) {
	VALUE str = rb_iseq_disasm(iseq);
	printf("%s\n", StringValueCStr(str));
    }
    debugs("[compile step: finish]\n");

    /* ISEQ is ready now.  It can be compiled ahead of time.  */
    mjit_aot_process(iseq);

    return COMPILE_OK;
}

static int
iseq_set_exception_local_table(rb_iseq_t *iseq)
{
    /* TODO: every id table is same -> share it.
     * Current problem is iseq_free().
     */
    ID id_dollar_bang;
    ID *ids = (ID *)ALLOC_N(ID, 1);

    CONST_ID(id_dollar_bang, "#$!");
    iseq->body->local_table_size = 1;
    ids[0] = id_dollar_bang;
    iseq->body->local_table = ids;
    return COMPILE_OK;
}

static int
get_lvar_level(const rb_iseq_t *iseq)
{
    int lev = 0;
    while (iseq != iseq->body->local_iseq) {
	lev++;
	iseq = iseq->body->parent_iseq;
    }
    return lev;
}

static int
get_dyna_var_idx_at_raw(const rb_iseq_t *iseq, ID id)
{
    unsigned int i;

    for (i = 0; i < iseq->body->local_table_size; i++) {
	if (iseq->body->local_table[i] == id) {
	    return (int)i;
	}
    }
    return -1;
}

static int
get_local_var_idx(const rb_iseq_t *iseq, ID id)
{
    int idx = get_dyna_var_idx_at_raw(iseq->body->local_iseq, id);

    if (idx < 0) {
	rb_bug("get_local_var_idx: %d", idx);
    }

    return idx - VM_ENV_DATA_SIZE + 1;
}

/* Find local var with ID in ISEQ of in its parent iseqs.  Return the
   number of variable in a frame of the found iseq.  Return the found
   iseq, its level, and local table size through ID_ISEQ, LEVEL, and
   LS.  */
static int
get_dyna_var_idx(rb_iseq_t *iseq, ID id, int *level, int *ls, rb_iseq_t **id_iseq)
{
    int lv = 0, idx = -1;

    while (iseq) {
	idx = get_dyna_var_idx_at_raw(iseq, id);
	if (idx >= 0) {
	    break;
	}
	iseq = iseq->body->parent_iseq;
	lv++;
    }

    if (idx < 0) {
	rb_bug("get_dyna_var_idx: -1");
    }

    *level = lv;
    *ls = iseq->body->local_table_size;
    if (id_iseq != NULL)
	*id_iseq = iseq;
    return idx - VM_ENV_DATA_SIZE + 1;
}

static void
iseq_calc_param_size(rb_iseq_t *iseq)
{
    if (iseq->body->param.flags.has_opt ||
	iseq->body->param.flags.has_post ||
	iseq->body->param.flags.has_rest ||
	iseq->body->param.flags.has_block ||
	iseq->body->param.flags.has_kw ||
	iseq->body->param.flags.has_kwrest) {

	if (iseq->body->param.flags.has_block) {
	    iseq->body->param.size = iseq->body->param.block_start + 1;
	}
	else if (iseq->body->param.flags.has_kwrest) {
	    iseq->body->param.size = iseq->body->param.keyword->rest_start + 1;
	}
	else if (iseq->body->param.flags.has_kw) {
	    iseq->body->param.size = iseq->body->param.keyword->bits_start + 1;
	}
	else if (iseq->body->param.flags.has_post) {
	    iseq->body->param.size = iseq->body->param.post_start + iseq->body->param.post_num;
	}
	else if (iseq->body->param.flags.has_rest) {
	    iseq->body->param.size = iseq->body->param.rest_start + 1;
	}
	else if (iseq->body->param.flags.has_opt) {
	    iseq->body->param.size = iseq->body->param.lead_num + iseq->body->param.opt_num;
	}
	else {
	    rb_bug("unreachable");
	}
    }
    else {
	iseq->body->param.size = iseq->body->param.lead_num;
    }
}

/* Set up param structure of ISEQ body from ARGS.  Compile optional
   values using RESULT as a preferable location index and add the
   insns to OPT_ARGS.  Use and update CURR_TEMP_VARS_NUM as the
   current number of reserverd temporaries.  */
static void
iseq_set_arguments_keywords(rb_iseq_t *iseq, LINK_ANCHOR *const optargs, const struct rb_args_info *args,
			    int *result, int *curr_temp_vars_num)
{
    NODE *node = args->kw_args;
    struct rb_iseq_param_keyword *keyword;
    const VALUE default_values = rb_ary_tmp_new(1);
    const VALUE complex_mark = rb_str_tmp_new(0);
    int kw = 0, rkw = 0, di = 0, i;

    iseq->body->param.flags.has_kw = TRUE;
    iseq->body->param.keyword = keyword = ZALLOC_N(struct rb_iseq_param_keyword, 1);
    keyword->bits_start = get_dyna_var_idx_at_raw(iseq, args->kw_rest_arg->nd_vid);

    while (node) {
	NODE *val_node = node->nd_body->nd_value;
	VALUE dv;

	if (val_node == (NODE *)-1) {
	    ++rkw;
	}
	else {
	    switch (nd_type(val_node)) {
	      case NODE_LIT:
		dv = val_node->nd_lit;
		iseq_add_mark_object(iseq, dv);
		break;
	      case NODE_NIL:
		dv = Qnil;
		break;
	      case NODE_TRUE:
		dv = Qtrue;
		break;
	      case NODE_FALSE:
		dv = Qfalse;
		break;
	      default:
         	/* nd_type(node) == NODE_KW_ARG */
		COMPILE_POPPED(optargs, "kwarg", node, result, curr_temp_vars_num);
		dv = complex_mark;
	    }

	    keyword->num = ++di;
	    rb_ary_push(default_values, dv);
	}

	kw++;
	node = node->nd_next;
    }

    keyword->num = kw;

    if (args->kw_rest_arg->nd_cflag != 0) {
	keyword->rest_start =  get_dyna_var_idx_at_raw(iseq, args->kw_rest_arg->nd_cflag);
	iseq->body->param.flags.has_kwrest = TRUE;
    }
    keyword->required_num = rkw;
    keyword->table = &iseq->body->local_table[keyword->bits_start - keyword->num];

    {
	VALUE *dvs = ALLOC_N(VALUE, RARRAY_LEN(default_values));

	for (i = 0; i < RARRAY_LEN(default_values); i++) {
	    VALUE dv = RARRAY_AREF(default_values, i);
	    if (dv == complex_mark) dv = Qundef;
	    dvs[i] = dv;
	}

	keyword->default_values = dvs;
    }
}

/* As above but used for setting all ISEQ args.  Return the
   compilation status.  */
static int
iseq_set_arguments(rb_iseq_t *iseq, LINK_ANCHOR *const optargs, NODE *node_args,
		   int *result, int *curr_temp_vars_num)
{
    debugs("iseq_set_arguments: %s\n", node_args ? "" : "0");

    if (node_args) {
	struct rb_args_info *args = node_args->nd_ainfo;
	ID rest_id = 0;
	int last_comma = 0;
	ID block_id = 0;

	EXPECT_NODE("iseq_set_arguments", node_args, NODE_ARGS);

	iseq->body->param.lead_num = (int)args->pre_args_num;
	if (iseq->body->param.lead_num > 0) iseq->body->param.flags.has_lead = TRUE;
	debugs("  - argc: %d\n", iseq->body->param.lead_num);

	rest_id = args->rest_arg;
	if (rest_id == 1) {
	    last_comma = 1;
	    rest_id = 0;
	}
	block_id = args->block_arg;

	if (args->first_post_arg) {
	    iseq->body->param.post_start = get_dyna_var_idx_at_raw(iseq, args->first_post_arg);
	    iseq->body->param.post_num = args->post_args_num;
	    iseq->body->param.flags.has_post = TRUE;
	}

	if (args->opt_args) {
	    NODE *node = args->opt_args;
	    LABEL *label;
	    VALUE labels = rb_ary_tmp_new(1);
	    VALUE *opt_table;
	    int i = 0, j, temp_vars_num;

	    while (node) {
		label = NEW_LABEL(nd_line(node));
		rb_ary_push(labels, (VALUE)label | 1);
		ADD_LABEL(optargs, label);
		label->sp = temp_vars_num = *curr_temp_vars_num;
		COMPILE_POPPED(optargs, "optarg", node->nd_body, result, &temp_vars_num);
		node = node->nd_next;
		i += 1;
	    }

	    /* last label */
	    label = NEW_LABEL(nd_line(node_args));
	    rb_ary_push(labels, (VALUE)label | 1);
	    ADD_LABEL(optargs, label);
	    label->sp = *curr_temp_vars_num;

	    opt_table = ALLOC_N(VALUE, i+1);

	    MEMCPY(opt_table, RARRAY_CONST_PTR(labels), VALUE, i+1);
	    for (j = 0; j < i+1; j++) {
		opt_table[j] &= ~1;
	    }
	    rb_ary_clear(labels);

	    iseq->body->param.flags.has_opt = TRUE;
	    iseq->body->param.opt_num = i;
	    iseq->body->param.opt_table = opt_table;
	}

	if (args->kw_args) {
	    iseq_set_arguments_keywords(iseq, optargs, args, result, curr_temp_vars_num);
	}
	else if (args->kw_rest_arg) {
	    struct rb_iseq_param_keyword *keyword = ZALLOC_N(struct rb_iseq_param_keyword, 1);
	    keyword->rest_start = get_dyna_var_idx_at_raw(iseq, args->kw_rest_arg->nd_vid);
	    iseq->body->param.keyword = keyword;
	    iseq->body->param.flags.has_kwrest = TRUE;
	}

	if (args->pre_init) { /* m_init */
	    COMPILE_POPPED(optargs, "init arguments (m)", args->pre_init, result, curr_temp_vars_num);
	}
	if (args->post_init) { /* p_init */
	    COMPILE_POPPED(optargs, "init arguments (p)", args->post_init, result, curr_temp_vars_num);
	}

	if (rest_id) {
	    iseq->body->param.rest_start = get_dyna_var_idx_at_raw(iseq, rest_id);
	    iseq->body->param.flags.has_rest = TRUE;
	    assert(iseq->body->param.rest_start != -1);

	    if (iseq->body->param.post_start == 0) { /* TODO: why that? */
		iseq->body->param.post_start = iseq->body->param.rest_start + 1;
	    }
	}

	if (block_id) {
	    iseq->body->param.block_start = get_dyna_var_idx_at_raw(iseq, block_id);
	    iseq->body->param.flags.has_block = TRUE;
	}

	iseq_calc_param_size(iseq);

	if (iseq->body->type == ISEQ_TYPE_BLOCK) {
	    if (iseq->body->param.flags.has_opt    == FALSE &&
		iseq->body->param.flags.has_post   == FALSE &&
		iseq->body->param.flags.has_rest   == FALSE &&
		iseq->body->param.flags.has_kw     == FALSE &&
		iseq->body->param.flags.has_kwrest == FALSE) {

		if (iseq->body->param.lead_num == 1 && last_comma == 0) {
		    /* {|a|} */
		    iseq->body->param.flags.ambiguous_param0 = TRUE;
		}
	    }
	}
    }

    return COMPILE_OK;
}

static int
iseq_set_local_table(rb_iseq_t *iseq, const ID *tbl)
{
    unsigned int size;

    if (tbl) {
	size = (unsigned int)*tbl;
	tbl++;
    }
    else {
	size = 0;
    }

    if (size > 0) {
	ID *ids = (ID *)ALLOC_N(ID, size);
	MEMCPY(ids, tbl, ID, size);
	iseq->body->local_table = ids;
    }

    iseq->body->local_table_size = size;

    debugs("iseq_set_local_table: %u\n", iseq->body->local_table_size);
    return COMPILE_OK;
}

static int
cdhash_cmp(VALUE val, VALUE lit)
{
    if (val == lit) return 0;
    if (SPECIAL_CONST_P(lit)) {
	return val != lit;
    }
    if (SPECIAL_CONST_P(val) || BUILTIN_TYPE(val) != BUILTIN_TYPE(lit)) {
	return -1;
    }
    if (BUILTIN_TYPE(lit) == T_STRING) {
	return rb_str_hash_cmp(lit, val);
    }
    return !rb_eql(lit, val);
}

static st_index_t
cdhash_hash(VALUE a)
{
    if (SPECIAL_CONST_P(a)) return (st_index_t)a;
    if (RB_TYPE_P(a, T_STRING)) return rb_str_hash(a);
    {
	VALUE hval = rb_hash(a);
	return (st_index_t)FIX2LONG(hval);
    }
}

static const struct st_hash_type cdhash_type = {
    cdhash_cmp,
    cdhash_hash,
};

struct cdhash_set_label_struct {
    VALUE hash;
    int pos;
    int len;
};

static int
cdhash_set_label_i(VALUE key, VALUE val, void *ptr)
{
    struct cdhash_set_label_struct *data = (struct cdhash_set_label_struct *)ptr;
    LABEL *lobj = (LABEL *)(val & ~1);
    rb_hash_aset(data->hash, key, INT2FIX(lobj->position - (data->pos+data->len)));
    return ST_CONTINUE;
}


static inline VALUE
get_ivar_ic_value(rb_iseq_t *iseq,ID id)
{
    VALUE val;
    struct rb_id_table *tbl = ISEQ_COMPILE_DATA(iseq)->ivar_cache_table;
    if (tbl) {
	if (rb_id_table_lookup(tbl,id,&val)) {
	    return val;
	}
    }
    else {
	tbl = rb_id_table_create(1);
	ISEQ_COMPILE_DATA(iseq)->ivar_cache_table = tbl;
    }
    val = INT2FIX(iseq->body->is_size++);
    rb_id_table_insert(tbl,id,val);
    return val;
}

/**
  ruby insn object list -> raw instruction sequence
 */
static int
iseq_set_sequence(rb_iseq_t *iseq, LINK_ANCHOR *const anchor)
{
    struct iseq_line_info_entry *line_info_table;
    unsigned int last_line = 0;
    LINK_ELEMENT *list;
    VALUE *generated_iseq;

    int insn_num, code_index, line_info_index, line = 0;
    int stack_max = 0;

    /* fix label position */
    list = FIRST_ELEMENT(anchor);
    insn_num = code_index = 0;
    while (list) {
	switch (list->type) {
	  case ISEQ_ELEMENT_INSN:
	    {
		INSN *iobj = (INSN *)list;
		line = iobj->line_no;
		code_index += insn_data_length(iobj);
		insn_num++;
		break;
	    }
	  case ISEQ_ELEMENT_LABEL:
	    {
		LABEL *lobj = (LABEL *)list;
		lobj->position = code_index;
		lobj->set = TRUE;
		break;
	    }
	  case ISEQ_ELEMENT_NONE:
	    {
		/* ignore */
		break;
	    }
	  default:
	    dump_disasm_list(FIRST_ELEMENT(anchor));
	    dump_disasm_list(list);
	    COMPILE_ERROR(iseq, line, "error: set_sequence");
	    return COMPILE_NG;
	}
	list = list->next;
    }

    /* make instruction sequence */
    generated_iseq = ALLOC_N(VALUE, code_index);
    line_info_table = ALLOC_N(struct iseq_line_info_entry, insn_num);
    iseq->body->is_entries = ZALLOC_N(union iseq_inline_storage_entry, iseq->body->is_size);
    iseq->body->cd_entries = (struct rb_call_data *)ruby_xmalloc(sizeof(struct rb_call_data) * iseq->body->cd_size +
								 sizeof(struct rb_call_data_with_kwarg) * iseq->body->cd_kw_size);
    ISEQ_COMPILE_DATA(iseq)->cd_index = ISEQ_COMPILE_DATA(iseq)->cd_kw_index = 0;

    list = FIRST_ELEMENT(anchor);
    line_info_index = code_index = 0;

    while (list) {
	switch (list->type) {
	  case ISEQ_ELEMENT_INSN:
	    {
		int j, len, insn;
		const char *types;
		VALUE *operands;
		INSN *iobj = (INSN *)list;

		/* fprintf(stderr, "insn: %-16s, sp: %d\n", insn_name(iobj->insn_id), sp); */
		operands = iobj->operands;
		insn = iobj->insn_id;
		generated_iseq[code_index] = insn;
		types = insn_op_types(insn);
		len = insn_len(insn);

		/* operand check */
		if (iobj->operand_size != len - 1) {
		    /* printf("operand size miss! (%d, %d)\n", iobj->operand_size, len); */
		    dump_disasm_list(list);
		    xfree(generated_iseq);
		    xfree(line_info_table);
		    COMPILE_ERROR(iseq, iobj->line_no,
				  "operand size miss! (%d for %d)",
				  iobj->operand_size, len - 1);
		    return COMPILE_NG;
		}

		for (j = 0; types[j]; j++) {
		    char type = types[j];
		    /* printf("--> [%c - (%d-%d)]\n", type, k, j); */
		    switch (type) {
		      case TS_OFFSET:
			{
			    /* label(destination position) */
			    LABEL *lobj = (LABEL *)operands[j];
			    if (!lobj->set) {
				COMPILE_ERROR(iseq, iobj->line_no,
					      "unknown label");
				return COMPILE_NG;
			    }
			    if (lobj->sp == -1) {
				lobj->sp = 0;
			    }
			    generated_iseq[code_index + 1 + j] = lobj->position - (code_index + len);
			    break;
			}
		      case TS_CDHASH:
			{
			    VALUE map = operands[j];
			    struct cdhash_set_label_struct data;
                            data.hash = map;
                            data.pos = code_index;
                            data.len = len;
			    rb_hash_foreach(map, cdhash_set_label_i, (VALUE)&data);

			    rb_hash_rehash(map);
			    freeze_hide_obj(map);
			    generated_iseq[code_index + 1 + j] = map;
			    break;
			}
		      case TS_INSN:
			generated_iseq[code_index + 1 + j] = operands[j];
			break;
		      case TS_LINDEX:
		      case TS_SINDEX:
		      case TS_RINDEX: {
			  int op;

			  generated_iseq[code_index + 1 + j] = operands[j];
			  if ((op = LINT2INT(operands[j])) < 0) {
			      assert(op != stack_result && op != anywhere_result);
			      if (-op > stack_max)
				  stack_max = -op;
			      if ((unsigned) -op > iseq->body->temp_vars_num)
				  iseq->body->temp_vars_num = -op;
			  }
			  break;
		      }
		      case TS_NUM:	/* ulong */
			generated_iseq[code_index + 1 + j] = FIX2LONG(operands[j]);
			break;
		      case TS_ISEQ:	/* iseq */
			{
			    VALUE v = operands[j];
			    generated_iseq[code_index + 1 + j] = v;
			    break;
			}
		      case TS_VALUE:	/* VALUE */
			{
			    VALUE v = operands[j];
			    generated_iseq[code_index + 1 + j] = v;
			    /* to mark ruby object */
			    iseq_add_mark_object(iseq, v);
			    break;
			}
		      case TS_IC: /* inline cache */
			{
			    unsigned int ic_index = FIX2UINT(operands[j]);
			    IC ic = (IC)&iseq->body->is_entries[ic_index];
			    if (UNLIKELY(ic_index >= iseq->body->is_size)) {
				rb_bug("iseq_set_sequence: ic_index overflow: index: %d, size: %d", ic_index, iseq->body->is_size);
			    }
			    generated_iseq[code_index + 1 + j] = (VALUE)ic;
			    break;
			}
		      case TS_CALLINFO: /* call info */
			{
			    /* TS_CALLINFO is not be used for RTL insns.  */
			    break;
			}
		      case TS_CALLCACHE:
			{
			    /* TS_CALLCACHE is not be used for RTL insns.  */
			    break;
			}
		      case TS_CALLDATA: /* call data */
			{
			    struct rb_call_data *base_cd = (struct rb_call_data *)operands[j];
			    struct rb_call_data *cd;

			    if (base_cd->call_info.flag & VM_CALL_KWARG) {
				struct rb_call_data_with_kwarg *cd_kw_entries = (struct rb_call_data_with_kwarg *)&iseq->body->cd_entries[iseq->body->cd_size];
				struct rb_call_data_with_kwarg *cd_kw = &cd_kw_entries[ISEQ_COMPILE_DATA(iseq)->cd_kw_index++];
				cd_kw->call_info = ((struct rb_call_data_with_kwarg *)base_cd)->call_info;
				cd_kw->kw_arg = ((struct rb_call_data_with_kwarg *)base_cd)->kw_arg;
				cd = (struct rb_call_data *)cd_kw;
				assert(ISEQ_COMPILE_DATA(iseq)->cd_kw_index <= iseq->body->cd_kw_size);
			    }
			    else {
				cd = &iseq->body->cd_entries[ISEQ_COMPILE_DATA(iseq)->cd_index++];
				cd->call_info = base_cd->call_info;
				assert(ISEQ_COMPILE_DATA(iseq)->cd_index <= iseq->body->cd_size);
			    }
			    cd->call_start = base_cd->call_start;
			    cd->call_cache.method_state = cd->call_cache.class_serial = 0;

			    generated_iseq[code_index + 1 + j] = (VALUE)cd;
			    break;
			}
		      case TS_ID: /* ID */
			generated_iseq[code_index + 1 + j] = SYM2ID(operands[j]);
			break;
		      case TS_GENTRY:
			{
			    struct rb_global_entry *entry =
				(struct rb_global_entry *)(operands[j] & (~1));
			    generated_iseq[code_index + 1 + j] = (VALUE)entry;
			}
			break;
		      case TS_FUNCPTR:
			generated_iseq[code_index + 1 + j] = operands[j];
			break;
		      default:
			xfree(generated_iseq);
			xfree(line_info_table);
			COMPILE_ERROR(iseq, iobj->line_no,
				      "unknown operand type: %c", type);
			return COMPILE_NG;
		    }
		}
		if (last_line != iobj->line_no) {
		    line_info_table[line_info_index].line_no = last_line = iobj->line_no;
		    line_info_table[line_info_index].position = code_index;
		    line_info_index++;
		}
		code_index += len;
		break;
	    }
	  case ISEQ_ELEMENT_LABEL:
	    {
		LABEL *lobj = (LABEL *)list;
		// assert(lobj->sp >= 0);
		if (lobj->sp == -1) {
		    lobj->sp = 0;
		}
		break;
	    }
	  default:
	    /* ignore */
	    break;
	}
	list = list->next;
    }

    iseq->body->iseq_encoded = (void *)generated_iseq;
    iseq->body->iseq_size = code_index;
    iseq->body->stack_max = stack_max;
    REALLOC_N(line_info_table, struct iseq_line_info_entry, line_info_index);
    iseq->body->line_info_table = line_info_table;
    iseq->body->line_info_size = line_info_index;

    return COMPILE_OK;
}

static int
label_get_position(LABEL *lobj)
{
    return lobj->position;
}

static int
label_get_sp(LABEL *lobj)
{
    return lobj->sp;
}

static int
iseq_set_exception_table(rb_iseq_t *iseq)
{
    const VALUE *tptr, *ptr;
    unsigned int tlen, i;
    struct iseq_catch_table_entry *entry;

    tlen = (int)RARRAY_LEN(ISEQ_COMPILE_DATA(iseq)->catch_table_ary);
    tptr = RARRAY_CONST_PTR(ISEQ_COMPILE_DATA(iseq)->catch_table_ary);

    if (tlen > 0) {
	struct iseq_catch_table *table = xmalloc(iseq_catch_table_bytes(tlen));
	table->size = tlen;

	for (i = 0; i < table->size; i++) {
	    ptr = RARRAY_CONST_PTR(tptr[i]);
	    entry = &table->entries[i];
	    entry->type = (enum catch_type)(ptr[0] & 0xffff);
	    entry->start = label_get_position((LABEL *)(ptr[1] & ~1));
	    entry->end = label_get_position((LABEL *)(ptr[2] & ~1));
	    entry->iseq = (rb_iseq_t *)ptr[3];

	    /* register iseq as mark object */
	    if (entry->iseq != 0) {
		iseq_add_mark_object(iseq, (VALUE)entry->iseq);
	    }

	    /* stack depth */
	    if (ptr[4]) {
		LABEL *lobj = (LABEL *)(ptr[4] & ~1);
		entry->cont = label_get_position(lobj);
		entry->sp = label_get_sp(lobj);

	    }
	    else {
		entry->cont = 0;
	    }
	}
	iseq->body->catch_table = table;
	RB_OBJ_WRITE(iseq, &ISEQ_COMPILE_DATA(iseq)->catch_table_ary, 0); /* free */
    }
    else {
	iseq->body->catch_table = NULL;
    }

    return COMPILE_OK;
}

/*
 * set optional argument table
 *   def foo(a, b=expr1, c=expr2)
 *   =>
 *    b:
 *      expr1
 *    c:
 *      expr2
 */
static int
iseq_set_optargs_table(rb_iseq_t *iseq)
{
    int i;
    VALUE *opt_table = (VALUE *)iseq->body->param.opt_table;

    if (iseq->body->param.flags.has_opt) {
	for (i = 0; i < iseq->body->param.opt_num + 1; i++) {
	    opt_table[i] = label_get_position((LABEL *)opt_table[i]);
	}
    }
    return COMPILE_OK;
}

/* Return the order number of the operand containing a destination of
   branch insn INSN_ID.  */
static int
destination_offset(enum ruby_vminsn_type insn_id)
{
    switch (insn_id) {
    case BIN(bteq):
    case BIN(bteqi):
    case BIN(bteqf):
    case BIN(bfeq):
    case BIN(bfeqi):
    case BIN(bfeqf):
    case BIN(btne):
    case BIN(btnei):
    case BIN(btnef):
    case BIN(bfne):
    case BIN(bfnei):
    case BIN(bfnef):
    case BIN(btlt):
    case BIN(btlti):
    case BIN(btltf):
    case BIN(bflt):
    case BIN(bflti):
    case BIN(bfltf):
    case BIN(btgt):
    case BIN(btgti):
    case BIN(btgtf):
    case BIN(bfgt):
    case BIN(bfgti):
    case BIN(bfgtf):
    case BIN(btle):
    case BIN(btlei):
    case BIN(btlef):
    case BIN(bfle):
    case BIN(bflei):
    case BIN(bflef):
    case BIN(btge):
    case BIN(btgei):
    case BIN(btgef):
    case BIN(bfge):
    case BIN(bfgei):
    case BIN(bfgef):
	 /* It is always the 2nd operand for combined branch insns */
	return 1;
    default:
	return 0;
    }
}

static LINK_ELEMENT *
get_destination_insn(INSN *iobj)
{
    LABEL *lobj = (LABEL *)OPERAND_AT(iobj,
				      destination_offset(iobj->insn_id));
    LINK_ELEMENT *list;

    list = lobj->link.next;
    while (list) {
	if (IS_INSN(list)) {
	    break;
	}
	list = list->next;
    }
    return list;
}

static LINK_ELEMENT *
get_next_insn(INSN *iobj)
{
    LINK_ELEMENT *list = iobj->link.next;

    while (list) {
	if (IS_INSN(list)) {
	    return list;
	}
	list = list->next;
    }
    return 0;
}

static LINK_ELEMENT *
get_prev_insn(INSN *iobj)
{
    LINK_ELEMENT *list = iobj->link.prev;

    while (list) {
	if (IS_INSN(list)) {
	    return list;
	}
	list = list->prev;
    }
    return 0;
}

static void
unref_destination(INSN *iobj, int pos)
{
    LABEL *lobj = (LABEL *)OPERAND_AT(iobj, pos);
    --lobj->refcnt;
    if (!lobj->refcnt) REMOVE_ELEM(&lobj->link);
}

static void
replace_destination(INSN *dobj, INSN *nobj)
{
    VALUE n = OPERAND_AT(nobj, destination_offset(nobj->insn_id));
    int dop = destination_offset(dobj->insn_id);
    LABEL *dl = (LABEL *)OPERAND_AT(dobj, dop);
    LABEL *nl = (LABEL *)n;
    --dl->refcnt;
    ++nl->refcnt;
    OPERAND_AT(dobj, dop) = n;
    if (!dl->refcnt) REMOVE_ELEM(&dl->link);
}

static int
remove_unreachable_chunk(rb_iseq_t *iseq, LINK_ELEMENT *i)
{
    int removed = 0;
    while (i) {
	if (IS_INSN(i)) {
	    struct rb_iseq_constant_body *body = iseq->body;
	    VALUE insn = INSN_OF(i);
	    int pos, len = insn_len(insn);
	    for (pos = 0; pos < len; ++pos) {
		switch (insn_op_types(insn)[pos]) {
		  case TS_OFFSET:
		    unref_destination((INSN *)i, pos);
		    break;
		  case TS_CALLINFO:
		    /* CALLINFO is not used for RTL insns.  */
		    break;
		  case TS_CALLDATA:
		    if (((struct rb_call_data *)OPERAND_AT(i, pos))->call_info.flag & VM_CALL_KWARG)
			--(body->cd_kw_size);
		    else
			--(body->cd_size);
		    break;
		}
	    }
	}
	else if (IS_LABEL(i)) {
	    if (((LABEL *)i)->refcnt > 0) break;
	}
	else break;
	REMOVE_ELEM(i);
	removed = 1;
	i = i->next;
    }
    return removed;
}

/* Return a branch insn with opposite condition than insn INSN_ID.  If
   there is no such insn, return nop.  */
static enum ruby_vminsn_type
get_branch_reverse_id(enum ruby_vminsn_type insn_id)
{
    switch (insn_id) {
      case BIN(bt): return BIN(bf);
      case BIN(bf): return BIN(bt);
      case BIN(bteq): return BIN(bfeq);
      case BIN(bteqi): return BIN(bfeqi);
      case BIN(bteqf): return BIN(bfeqf);
      case BIN(bfeq): return BIN(bteq);
      case BIN(bfeqi): return BIN(bteqi);
      case BIN(bfeqf): return BIN(bteqf);
      case BIN(btne): return BIN(bfne);
      case BIN(btnei): return BIN(bfnei);
      case BIN(btnef): return BIN(bfnef);
      case BIN(bfne): return BIN(btne);
      case BIN(bfnei): return BIN(btnei);
      case BIN(bfnef): return BIN(btnef);
      case BIN(btlt): return BIN(bflt);
      case BIN(btlti): return BIN(bflti);
      case BIN(btltf): return BIN(bfltf);
      case BIN(bflt): return BIN(btlt);
      case BIN(bflti): return BIN(btlti);
      case BIN(bfltf): return BIN(btltf);
      case BIN(btgt): return BIN(bfgt);
      case BIN(btgti): return BIN(bfgti);
      case BIN(btgtf): return BIN(bfgtf);
      case BIN(bfgt): return BIN(btgt);
      case BIN(bfgti): return BIN(btgti);
      case BIN(bfgtf): return BIN(btgtf);
      case BIN(btle): return BIN(bfle);
      case BIN(btlei): return BIN(bflei);
      case BIN(btlef): return BIN(bflef);
      case BIN(bfle): return BIN(btle);
      case BIN(bflei): return BIN(btlei);
      case BIN(bflef): return BIN(btlef);
      case BIN(btge): return BIN(bfge);
      case BIN(btgei): return BIN(bfgei);
      case BIN(btgef): return BIN(bfgef);
      case BIN(bfge): return BIN(btge);
      case BIN(bfgei): return BIN(btgei);
      case BIN(bfgef): return BIN(btgef);
      default: return BIN(nop);
    }
}

/* Return TRUE if ID is any branch insn.  */
static int
branch_insn_p(enum ruby_vminsn_type id)
{
    return (get_branch_reverse_id(id) != BIN(nop)
	    || id == BIN(goto)
	    || id == BIN(bkw)
	    || id == BIN(bnil));
}

/* Return TRUE if insn INSN_ID is a combined branch insn (compare and
   branch).  */
static int
combined_branch_p(enum ruby_vminsn_type insn_id)
{
    switch (insn_id) {
    case BIN(bteq):
    case BIN(bfeq):
    case BIN(bteqi):
    case BIN(bfeqi):
    case BIN(bteqf):
    case BIN(bfeqf):
    case BIN(btne):
    case BIN(bfne):
    case BIN(btnei):
    case BIN(bfnei):
    case BIN(btnef):
    case BIN(bfnef):
    case BIN(btlt):
    case BIN(bflt):
    case BIN(btlti):
    case BIN(bflti):
    case BIN(btltf):
    case BIN(bfltf):
    case BIN(btgt):
    case BIN(bfgt):
    case BIN(btgti):
    case BIN(bfgti):
    case BIN(btgtf):
    case BIN(bfgtf):
    case BIN(btle):
    case BIN(bfle):
    case BIN(btlei):
    case BIN(bflei):
    case BIN(btlef):
    case BIN(bflef):
    case BIN(btge):
    case BIN(bfge):
    case BIN(btgei):
    case BIN(bfgei):
    case BIN(btgef):
    case BIN(bfgef):
    case BIN(bt_match):
	return TRUE;
    default:
	return FALSE;
    }
}

/* Return combined compare insn INSN_ID and branch (on true of TRUE_P)
   insn.  If INSN_ID is not a compare insn or there is no a combined
   insn, return nop.  */
static enum ruby_vminsn_type
get_combined_branch_id(enum ruby_vminsn_type insn_id, int true_p)
{
    switch (insn_id) {
      case BIN(eq): return true_p ? BIN(bteq) : BIN(bfeq);
      case BIN(eqi): return true_p ? BIN(bteqi) : BIN(bfeqi);
      case BIN(eqf): return true_p ? BIN(bteqf) : BIN(bfeqf);
      case BIN(ne): return true_p ? BIN(btne) : BIN(bfne);
      case BIN(nei): return true_p ? BIN(btnei) : BIN(bfnei);
      case BIN(nef): return true_p ? BIN(btnef) : BIN(bfnef);
      case BIN(lt): return true_p ? BIN(btlt) : BIN(bflt);
      case BIN(lti): return true_p ? BIN(btlti) : BIN(bflti);
      case BIN(ltf): return true_p ? BIN(btltf) : BIN(bfltf);
      case BIN(gt): return true_p ? BIN(btgt) : BIN(bfgt);
      case BIN(gti): return true_p ? BIN(btgti) : BIN(bfgti);
      case BIN(gtf): return true_p ? BIN(btgtf) : BIN(bfgtf);
      case BIN(le): return true_p ? BIN(btle) : BIN(bfle);
      case BIN(lei): return true_p ? BIN(btlei) : BIN(bflei);
      case BIN(lef): return true_p ? BIN(btlef) : BIN(bflef);
      case BIN(ge): return true_p ? BIN(btge) : BIN(bfge);
      case BIN(gei): return true_p ? BIN(btgei) : BIN(bfgei);
      case BIN(gef): return true_p ? BIN(btgef) : BIN(bfgef);
      case BIN(check_match): return true_p ? BIN(bt_match) : BIN(nop);
      default: return BIN(nop);
    }
}

/* Each pass of an insn in iseq_peephole_optimize has a unique value.
   The current pass value is in the following variable.  */
static int peephole_pass = 0;

static int
iseq_peephole_optimize(rb_iseq_t *iseq, LINK_ELEMENT *list, const int do_tailcallopt)
{
    INSN *iobj = (INSN *)list;
    enum ruby_vminsn_type id;
    INSN *niobj, *diobj, *piobj;

    /* We don't remove code after return insns as in stack-based VM
       because we can have more one (specialized) return insns in the
       iseq.  */
 again:
    if (IS_INSN_ID(iobj, goto)) {
	/*
	 *  useless jump elimination:
	 *     jump LABEL1
	 *     ...
	 *   LABEL1:
	 *     jump LABEL2
	 *
	 *   => in this case, first jump instruction should jump to
	 *      LABEL2 directly
	 */
	diobj = (INSN *)get_destination_insn(iobj);
	niobj = (INSN *)get_next_insn(iobj);

	if (diobj != NULL && diobj == niobj) {
	    /*
	     *   jump LABEL
	     *  LABEL:
	     * =>
	     *   LABEL:
	     */
	    unref_destination(iobj, 0);
	    REMOVE_ELEM(&iobj->link);
	}
	else if (diobj != NULL && iobj != diobj && IS_INSN_ID(diobj, goto) &&
		 OPERAND_AT(iobj, destination_offset(iobj->insn_id))
		 != OPERAND_AT(diobj, destination_offset(diobj->insn_id))) {
	    replace_destination(iobj, diobj);
	    remove_unreachable_chunk(iseq, iobj->link.next);
	    goto again;
	}
	else if (diobj != NULL
		 && (IS_INSN_ID(diobj, temp_ret)
		     || IS_INSN_ID(diobj, loc_ret)
		     || IS_INSN_ID(diobj, val_ret))) {
	    /*
	     *  jump LABEL
	     *  ...
	     * LABEL:
	     *  ret
	     * =>
	     *  ret
	     *  ...
	     * LABEL:
	     *  ret
	     */
	    INSN *riobj = new_insn_core(iseq, iobj->line_no, diobj->insn_id,
					diobj->operand_size, diobj->operands);
	    unref_destination(iobj, 0);
	    /* replace */
	    REPLACE_ELEM((LINK_ELEMENT *) iobj, (LINK_ELEMENT *) riobj);
	    iobj = riobj;
	    if ((IS_INSN_ID(iobj, temp_ret) || IS_INSN_ID(iobj, loc_ret))
		&& (piobj = (INSN *)get_prev_insn(iobj)) != 0
		&& (IS_INSN_ID(piobj, val2loc)
		    || IS_INSN_ID(piobj, val2temp))
		&& piobj->operands[0] == iobj->operands[0]) {
		VALUE *operands = (VALUE *)compile_data_alloc(iseq, sizeof(VALUE) * 2);

		operands[0] = piobj->operands[1];
		operands[1] = iobj->operands[1];
		riobj = new_insn_core(iseq, iobj->line_no, BIN(val_ret), 2, operands);
		REPLACE_ELEM((LINK_ELEMENT *) iobj, (LINK_ELEMENT *) riobj);
		iobj = riobj;
		REMOVE_ELEM(&piobj->link);
	    }
	}
	/*
	 * useless jump elimination (if/unless destination):
	 *   if   L1
	 *   jump L2
	 * L1:
	 *   ...
	 * L2:
	 *
	 * ==>
	 *   unless L2
	 * L1:
	 *   ...
	 * L2:
	 */
	else if (niobj != NULL
		 && (piobj = (INSN *) get_prev_insn(iobj)) != 0
		 && (id = get_branch_reverse_id(piobj->insn_id)) != BIN(nop)
		 && niobj == (INSN *) get_destination_insn(piobj))
	    {
		piobj->insn_id = id;
		if (combined_branch_p(id)) {
		    /* Change continuation insn too.  */
		    enum ruby_vminsn_type id = (enum ruby_vminsn_type) OPERAND_AT(piobj, 0);

		    assert(id == BIN(cont_btcmp) || id == BIN(cont_bfcmp));
		    OPERAND_AT(piobj, 0) = id == BIN(cont_bfcmp) ? BIN(cont_btcmp) : BIN(cont_bfcmp);
		}
		replace_destination(piobj, iobj);
		REMOVE_ELEM(&iobj->link);
	    }
	else if (remove_unreachable_chunk(iseq, iobj->link.next)) {
	    goto again;
	}
    }

    if (branch_insn_p(iobj->insn_id)) {
	/*
	 *   if L1
	 *   ...
	 * L1:
	 *   jump L2
	 * =>
	 *   if L2
	 */
	peephole_pass++;
	for (;;) {
	    diobj = (INSN *)get_destination_insn(iobj);
	    if (diobj == NULL || diobj->check == peephole_pass)
		break;
	    diobj->check = peephole_pass;
	    if (!IS_INSN_ID(diobj, goto)
		&& (iobj->insn_id != diobj->insn_id
		    || (!IS_INSN_ID(iobj, bt)
			&& !IS_INSN_ID(iobj, bf)
			&& !IS_INSN_ID(iobj, bnil))
		    || OPERAND_AT(iobj, 1) != OPERAND_AT(diobj, 1)))
		break;
	    replace_destination(iobj, diobj);
	}
    }

    if ((IS_INSN_ID(iobj, bt) || IS_INSN_ID(iobj, bf))
	&& (piobj = (INSN *) get_prev_insn(iobj)) != 0
	&& ((id = get_combined_branch_id(piobj->insn_id,
					 IS_INSN_ID(iobj, bt)))
	    != BIN(nop))
	&& ! prev_used_label_p((LINK_ELEMENT *) iobj)) {
	/* cmp res, op1, op2, ...
	   bt|bf L1, res
	   =>
	   btcmp|bfcmp L1, res, op1, op2, ...
	*/
	if (piobj->insn_id != BIN(check_match) && OPERAND_AT(piobj, 2) == OPERAND_AT(iobj, 1)) {
	    VALUE dst;
	    int i;
	    enum ruby_vminsn_type old_id = iobj->insn_id;

	    dst = OPERAND_AT(iobj, destination_offset(old_id));
	    iobj->insn_id = id;
	    /* ??? Free iobj->operands  */
	    iobj->operands = (VALUE *)compile_data_alloc(iseq, sizeof(VALUE) * 6);
	    iobj->operand_size = 6;
	    iobj->operands[0] = old_id == BIN(bt) ? BIN(cont_btcmp) : BIN(cont_bfcmp);
	    iobj->operands[1] = dst;
	    for (i = 0; i < 4; i++)
		iobj->operands[i+2] = piobj->operands[i + 1];
	    REMOVE_ELEM(&piobj->link);
	}
	else if (0&&IS_INSN_ID(piobj, check_match)
		 && OPERAND_AT(piobj, 0) == OPERAND_AT(iobj, 1)) {
	    VALUE dst;
	    int i;

	    dst = OPERAND_AT(iobj, destination_offset(iobj->insn_id));
	    iobj->insn_id = id;
	    /* ??? Free iobj->operands  */
	    iobj->operands = (VALUE *)compile_data_alloc(iseq, sizeof(VALUE) * 5);
	    iobj->operand_size = 5;
	    iobj->operands[0] = dst;
	    for (i = 0; i < 4; i++)
		iobj->operands[i+1] = piobj->operands[i];
	    REMOVE_ELEM(&piobj->link);
	}
    }

    if (0 && (IS_INSN_ID(iobj, var2ivar))
	&& ! prev_used_label_p((LINK_ELEMENT *) iobj)
	&& (piobj = (INSN *) get_prev_insn(iobj)) != 0
	&& IS_INSN_ID(piobj, val2temp)
	&& ! prev_used_label_p((LINK_ELEMENT *) piobj)
	&& iobj->operands[2] == piobj->operands[0]) {
      iobj->insn_id = BIN(val2ivar);
      iobj->operands[2] = piobj->operands[1];
      REMOVE_ELEM(&piobj->link);
    }

    if (0 && (IS_INSN_ID(iobj, var2uploc))
	&& ! prev_used_label_p((LINK_ELEMENT *) iobj)
	&& (piobj = (INSN *) get_prev_insn(iobj)) != 0
	&& IS_INSN_ID(piobj, val2temp)
	&& ! prev_used_label_p((LINK_ELEMENT *) piobj)
	&& iobj->operands[1] == piobj->operands[0]) {
      iobj->insn_id = BIN(val2uploc);
      iobj->operands[1] = piobj->operands[1];
      REMOVE_ELEM(&piobj->link);
    }

    if (0&&(IS_INSN_ID(iobj, temp_ret) || IS_INSN_ID(iobj, loc_ret)
	 || IS_INSN_ID(iobj, val_ret))
	&& (piobj = (INSN *) get_prev_insn(iobj)) != 0
	&& IS_INSN_ID(piobj, trace)
	&& ! prev_used_label_p((LINK_ELEMENT *) piobj)
	/* For correct line reporting.  Can we rid off it???  */
	&& iobj->line_no == piobj->line_no) {
	iobj->operands[1] = INT2FIX(FIX2INT(iobj->operands[1]) | FIX2INT(piobj->operands[0]));
	REMOVE_ELEM(&piobj->link);
    } else if (0&&IS_INSN_ID(iobj, trace) && (piobj = (INSN *) get_prev_insn(iobj)) != 0
	       && IS_INSN_ID(piobj, trace)
	       && ! prev_used_label_p((LINK_ELEMENT *) piobj)
	       /* For correct line reporting.  Can we rid off it???  */
	       && iobj->line_no == piobj->line_no) {
	iobj->operands[0] = INT2FIX(FIX2INT(iobj->operands[0]) | FIX2INT(piobj->operands[0]));
	REMOVE_ELEM(&piobj->link);
    }

    if (IS_INSN_ID(iobj, make_array) ||
	IS_INSN_ID(iobj, clone_array) ||
	IS_INSN_ID(iobj, spread_array) ||
	IS_INSN_ID(iobj, concat_array) ||
	IS_INSN_ID(iobj, splat_array)) {
	/* At this stage we have only one use of the result.  It can
	 * change in the future if we implement (speculative) common
	 * sub-expression elimination.
	 *
	 *  make_array op, ...
	 *  splat_array res, op
	 * =>
	 *  make_array res, ...
	 */
	LINK_ELEMENT *next = iobj->link.next;
	if (IS_INSN(next) && IS_INSN_ID(next, splat_array)
	    && ((INSN *) next)->operands[1] == iobj->operands[0]) {
	    if (iobj->operands[0] == ((INSN *) next)->operands[0])
		/* remove splat_array following always-array insn */
		REMOVE_ELEM(next);
	    else
		REPLACE_ELEM(next, (LINK_ELEMENT *) new_local_move(iseq, ((INSN *) next)->line_no,
								   LINT2INT(((INSN *) next)->operands[0]),
								   LINT2INT(iobj->operands[0])));
	}
    }

    if (do_tailcallopt &&
	(IS_INSN_ID(iobj, simple_call) ||
	 IS_INSN_ID(iobj, simple_call_self) ||
	 IS_INSN_ID(iobj, simple_call_recv) ||
	 IS_INSN_ID(iobj, call_self) ||
	 IS_INSN_ID(iobj, call) ||
	 IS_INSN_ID(iobj, call_super))) {
	/*
	 *  send ...
	 *  ret
	 * =>
	 *  send ..., ... | VM_CALL_TAILCALL, ...
	 *  ret # unreachable
	 */
	INSN *piobj = NULL;
	if (iobj->link.next) {
	    LINK_ELEMENT *next = iobj->link.next;
	    do {
		if (!IS_INSN(next)) {
		    next = next->next;
		    continue;
		}
		switch (INSN_OF(next)) {
		  case BIN(nop):
		  /*case BIN(trace):*/
		    next = next->next;
		    break;
		  case BIN(goto):
		    /* if cond
		     *   return tailcall
		     * end
		     */
		    next = get_destination_insn((INSN *)next);
		    break;
		  case BIN(temp_ret):
		    piobj = iobj;
		  default:
		    next = NULL;
		    break;
		}
	    } while (next);
	}

	if (piobj) {
	    struct rb_call_data *cd = (struct rb_call_data *)piobj->operands[0];
	    cd->call_info.flag |= VM_CALL_TAILCALL;
	}
    }

#define IS_TRACE_LINE(insn)	    \
	(IS_INSN_ID(insn, trace) && \
	 OPERAND_AT(insn, 0) == INT2FIX(RUBY_EVENT_LINE))
    if (IS_TRACE_LINE(iobj) && iobj->link.prev && IS_INSN(iobj->link.prev)) {
	INSN *piobj = (INSN *)iobj->link.prev;
	if (IS_TRACE_LINE(piobj)) {
	    REMOVE_ELEM(iobj->link.prev);
	}
    }

    return COMPILE_OK;
}

static int
iseq_specialized_instruction(rb_iseq_t *iseq, INSN *iobj)
{
    if (IS_INSN_ID(iobj, make_array) && iobj->link.next &&
	/* The array elements should be on the stack  */
	IS_INSN(iobj->link.next) && (long) iobj->operands[1] < 0) {
	/*
	 *   [a, b, ...].max/min -> a, b, c, new_array_max/min
	 */
	INSN *niobj = (INSN *)iobj->link.next;
	if (IS_INSN_ID(niobj, simple_call)) {
	    struct rb_call_data *cd = (struct rb_call_data *)OPERAND_AT(niobj, 0);
	    if ((cd->call_info.flag & VM_CALL_ARGS_SIMPLE) && cd->call_info.orig_argc == 0) {
		switch (cd->call_info.mid) {
		  case idMax:
		    iobj->insn_id = BIN(new_array_max);
		    REMOVE_ELEM(&niobj->link);
		    return COMPILE_OK;
		  case idMin:
		    iobj->insn_id = BIN(new_array_min);
		    REMOVE_ELEM(&niobj->link);
		    return COMPILE_OK;
		}
	    }
	}
    }

    return COMPILE_OK;
}

static inline int
tailcallable_p(rb_iseq_t *iseq)
{
    switch (iseq->body->type) {
      case ISEQ_TYPE_TOP:
      case ISEQ_TYPE_EVAL:
      case ISEQ_TYPE_MAIN:
	/* not tail callable because cfp will be over popped */
      case ISEQ_TYPE_RESCUE:
      case ISEQ_TYPE_ENSURE:
	/* rescue block can't tail call because of errinfo */
	return FALSE;
      default:
	return TRUE;
    }
}

static int
iseq_optimize(rb_iseq_t *iseq, LINK_ANCHOR *const anchor)
{
    LINK_ELEMENT *list;
    const int do_peepholeopt = ISEQ_COMPILE_DATA(iseq)->option->peephole_optimization;
    const int do_tailcallopt = tailcallable_p(iseq) &&
	ISEQ_COMPILE_DATA(iseq)->option->tailcall_optimization;
    const int do_si = ISEQ_COMPILE_DATA(iseq)->option->specialized_instruction;
    const int do_ou = ISEQ_COMPILE_DATA(iseq)->option->operands_unification;
    int rescue_level = 0;
    int tailcallopt = do_tailcallopt;

    list = FIRST_ELEMENT(anchor);

    while (list) {
	if (IS_INSN(list)) {
	    if (do_peepholeopt) {
		iseq_peephole_optimize(iseq, list, tailcallopt);
	    }
	    if (do_si) {
		iseq_specialized_instruction(iseq, (INSN *)list);
	    }
	    if (do_ou) {
		insn_operands_unification((INSN *)list);
	    }
	}
	if (IS_LABEL(list)) {
	    switch (((LABEL *)list)->rescued) {
	      case LABEL_RESCUE_BEG:
		rescue_level++;
		tailcallopt = FALSE;
		break;
	      case LABEL_RESCUE_END:
		if (!--rescue_level) tailcallopt = do_tailcallopt;
		break;
	    }
	}
	list = list->next;
    }
    return COMPILE_OK;
}

#if OPT_INSTRUCTIONS_UNIFICATION
static INSN *
new_unified_insn(rb_iseq_t *iseq,
		 int insn_id, int size, LINK_ELEMENT *seq_list)
{
    INSN *iobj = 0;
    LINK_ELEMENT *list = seq_list;
    int i, argc = 0;
    VALUE *operands = 0, *ptr = 0;


    /* count argc */
    for (i = 0; i < size; i++) {
	iobj = (INSN *)list;
	argc += iobj->operand_size;
	list = list->next;
    }

    if (argc > 0) {
	ptr = operands =
	    (VALUE *)compile_data_alloc(iseq, sizeof(VALUE) * argc);
    }

    /* copy operands */
    list = seq_list;
    for (i = 0; i < size; i++) {
	iobj = (INSN *)list;
	MEMCPY(ptr, iobj->operands, VALUE, iobj->operand_size);
	ptr += iobj->operand_size;
	list = list->next;
    }

    return new_insn_core(iseq, iobj->line_no, insn_id, argc, operands);
}
#endif

/*
 * This scheme can get more performance if do this optimize with
 * label address resolving.
 * It's future work (if compile time was bottle neck).
 */
static int
iseq_insns_unification(rb_iseq_t *iseq, LINK_ANCHOR *const anchor)
{
#if OPT_INSTRUCTIONS_UNIFICATION
    LINK_ELEMENT *list;
    INSN *iobj, *niobj;
    int id, k;
    intptr_t j;

    list = FIRST_ELEMENT(anchor);
    while (list) {
	if (IS_INSN(list)) {
	    iobj = (INSN *)list;
	    id = iobj->insn_id;
	    if (unified_insns_data[id] != 0) {
		const int *const *entry = unified_insns_data[id];
		for (j = 1; j < (intptr_t)entry[0]; j++) {
		    const int *unified = entry[j];
		    LINK_ELEMENT *li = list->next;
		    for (k = 2; k < unified[1]; k++) {
			if (!IS_INSN(li) ||
			    ((INSN *)li)->insn_id != unified[k]) {
			    goto miss;
			}
			li = li->next;
		    }
		    /* matched */
		    niobj =
			new_unified_insn(iseq, unified[0], unified[1] - 1,
					 list);

		    /* insert to list */
		    niobj->link.prev = (LINK_ELEMENT *)iobj->link.prev;
		    niobj->link.next = li;
		    if (li) {
			li->prev = (LINK_ELEMENT *)niobj;
		    }

		    list->prev->next = (LINK_ELEMENT *)niobj;
		    list = (LINK_ELEMENT *)niobj;
		    break;
		  miss:;
		}
	    }
	}
	list = list->next;
    }
#endif
    return COMPILE_OK;
}

#if OPT_STACK_CACHING

/* TODO: Remove OPT_STACK_CACHING code everywhere.  */

#define SC_INSN(insn, stat) sc_insn_info[(insn)][(stat)]
#define SC_NEXT(insn)       sc_insn_next[(insn)]

#include "opt_sc.inc"

static int
insn_set_sc_state(rb_iseq_t *iseq, INSN *iobj, int state)
{
    int nstate;
    int insn_id;

    insn_id = iobj->insn_id;
    iobj->insn_id = SC_INSN(insn_id, state);
    nstate = SC_NEXT(iobj->insn_id);

    if (insn_id == BIN(jump) ||
	insn_id == BIN(branchif) || insn_id == BIN(branchunless)
	|| branch_insn_p(insn_id)) {
	LABEL *lobj = (LABEL *)OPERAND_AT(iobj,
					  destination_offset(iobj->insn_id));

	if (lobj->sc_state != 0) {
	    if (lobj->sc_state != nstate) {
		dump_disasm_list((LINK_ELEMENT *)iobj);
		dump_disasm_list((LINK_ELEMENT *)lobj);
		printf("\n-- %d, %d\n", lobj->sc_state, nstate);
		COMPILE_ERROR(iseq, iobj->line_no,
			      "insn_set_sc_state error\n");
		return COMPILE_NG;
	    }
	}
	else {
	    lobj->sc_state = nstate;
	}
	if (insn_id == BIN(jump)) {
	    nstate = SCS_XX;
	}
    }
    else if (insn_id == BIN(leave)) {
	nstate = SCS_XX;
    }

    return nstate;
}

static int
label_set_sc_state(LABEL *lobj, int state)
{
    if (lobj->sc_state != 0) {
	if (lobj->sc_state != state) {
	    state = lobj->sc_state;
	}
    }
    else {
	lobj->sc_state = state;
    }

    return state;
}


#endif

static int
iseq_set_sequence_stackcaching(rb_iseq_t *iseq, LINK_ANCHOR *const anchor)
{
#if OPT_STACK_CACHING
    LINK_ELEMENT *list;
    int state, insn_id;

    /* initialize */
    state = SCS_XX;
    list = FIRST_ELEMENT(anchor);
    /* dump_disasm_list(list); */

    /* for each list element */
    while (list) {
      redo_point:
	switch (list->type) {
	  case ISEQ_ELEMENT_INSN:
	    {
		INSN *iobj = (INSN *)list;
		insn_id = iobj->insn_id;

		/* dump_disasm_list(list); */

		switch (insn_id) {
		  case BIN(nop):
		    {
			/* exception merge point */
			if (state != SCS_AX) {
			    INSN *rpobj =
				new_insn_body(iseq, 0, BIN(reput), 0);

			    /* replace this insn */
			    REPLACE_ELEM(list, (LINK_ELEMENT *)rpobj);
			    list = (LINK_ELEMENT *)rpobj;
			    goto redo_point;
			}
			break;
		    }
		  case BIN(swap):
		    {
			if (state == SCS_AB || state == SCS_BA) {
			    state = (state == SCS_AB ? SCS_BA : SCS_AB);

			    REMOVE_ELEM(list);
			    list = list->next;
			    goto redo_point;
			}
			break;
		    }
		  case BIN(pop):
		    {
			switch (state) {
			  case SCS_AX:
			  case SCS_BX:
			    state = SCS_XX;
			    break;
			  case SCS_AB:
			    state = SCS_AX;
			    break;
			  case SCS_BA:
			    state = SCS_BX;
			    break;
			  case SCS_XX:
			    goto normal_insn;
			  default:
			    COMPILE_ERROR(iseq, iobj->line_no,
					  "unreachable");
			    return COMPILE_NG;
			}
			/* remove useless pop */
			REMOVE_ELEM(list);
			list = list->next;
			goto redo_point;
		    }
		  default:;
		    /* none */
		}		/* end of switch */
	      normal_insn:
		state = insn_set_sc_state(iseq, iobj, state);
		break;
	    }
	  case ISEQ_ELEMENT_LABEL:
	    {
		LABEL *lobj;
		lobj = (LABEL *)list;

		state = label_set_sc_state(lobj, state);
	    }
	  default:
	    break;
	}
	list = list->next;
    }
#endif
    return COMPILE_OK;
}


/* Return a new specialized insn assigning value VAL to a variable
   given by offset TO.  */
static INSN *
new_value_load(rb_iseq_t *iseq, int line_no, lindex_t to, VALUE val) {
    enum ruby_vminsn_type insn_id;
    if (LINT2INT(to) < 0)
	insn_id = BIN(val2temp);
    else
	insn_id = BIN(val2loc);
    return new_insn_body(iseq, line_no, insn_id, 2, (VALUE) to, val);
}

/* Add a new specialized value load insn to RET for ISEQ.  */
static void
add_value_load(rb_iseq_t *iseq, LINK_ANCHOR *ret, int line_no, lindex_t to, VALUE val) {
    ADD_ELEM(ret, (LINK_ELEMENT *) new_value_load(iseq, line_no, to, val));
}

/* Generate ISEQ insns of NODE representing dstr fragments.  Put the
   insns into RET.  Return the fragments number through CNTP.  Update
   the number of reserved temporaries in CURR_TEMP_VARS_NUM and result
   location index in RESULT.  Return the compilation status.  */
static int
compile_dstr_fragments(rb_iseq_t *iseq, LINK_ANCHOR *const ret, NODE *node, int *cntp,
		       int *result, int *curr_temp_vars_num)
{
    NODE *list = node->nd_next;
    VALUE lit = node->nd_lit;
    int cnt = 0;
    int ls = iseq->body->local_table_size;

    debugp_param("nd_lit", lit);
    if (!NIL_P(lit)) {
	cnt++;
	if (!RB_TYPE_P(lit, T_STRING)) {
	    compile_bug(ERROR_ARGS "dstr: must be string: %s",
			rb_builtin_type_name(TYPE(lit)));
	}
	lit = node->nd_lit = rb_fstring(lit);
	add_value_load(iseq, ret, nd_line(node),
		       ls - get_temp_stack_slot(iseq, curr_temp_vars_num), lit);
    }

    while (list) {
	int op_result = stack_result;
	node = list->nd_head;
	if (nd_type(node) == NODE_STR) {
	    node->nd_lit = rb_fstring(node->nd_lit);
	    add_value_load(iseq, ret, nd_line(node),
			   ls - get_temp_stack_slot(iseq, curr_temp_vars_num), node->nd_lit);
	    lit = Qnil;
	}
	else {
	    CHECK(COMPILE(ret, "each string", node, &op_result, curr_temp_vars_num));
	}
	cnt++;
	list = list->nd_next;
    }
    *cntp = cnt;
    if (result != NULL)
      *result = ls + *curr_temp_vars_num - cnt + 1;

    return COMPILE_OK;
}

/* As above but compiling dstr NODE.  */
static int
compile_dstr(rb_iseq_t *iseq, LINK_ANCHOR *const ret, NODE * node, int *result, int *curr_temp_vars_num)
{
    int cnt;
    int ls = iseq->body->local_table_size;
    int op_result = stack_result;
    int temp_vars_num = *curr_temp_vars_num;

    CHECK(compile_dstr_fragments(iseq, ret, node, &cnt, &op_result, &temp_vars_num));
    ADD_INSN2(ret, nd_line(node), concat_strings, INT2LINT(ls - op_result), INT2FIX(cnt));
    if (result != NULL) {
	*result = op_result;
	increment_temps_var(iseq, curr_temp_vars_num, 1);
    }
    return COMPILE_OK;
}

static int
compile_dregx(rb_iseq_t *iseq, LINK_ANCHOR *const ret, NODE * node, int *result, int *curr_temp_vars_num)
{
    int cnt;
    int ls = iseq->body->local_table_size;
    int op_result = stack_result;
    int temp_vars_num = *curr_temp_vars_num;

    CHECK(compile_dstr_fragments(iseq, ret, node, &cnt, &op_result, &temp_vars_num));
    ADD_INSN3(ret, nd_line(node), to_regexp,
	      INT2LINT(ls - op_result), INT2FIX(node->nd_cflag), INT2FIX(cnt));
    if (result != NULL) {
	*result = op_result;
	increment_temps_var(iseq, curr_temp_vars_num, 1);
    }
    return COMPILE_OK;
}

/* Add a new specialized move insn to RET for ISEQ unless it is actually a nop.   */
static void
add_local_move(rb_iseq_t *iseq, LINK_ANCHOR *ret, int line_no, int to, int from) {
    if (to != from)
	ADD_ELEM(ret, (LINK_ELEMENT *) new_local_move(iseq, line_no, to, from));
}

/* Generate ISEQ insns of (inclusive if AGAIN) flip-flop NODE with
   THEN_LABEL and ELSE_LABEL.  Put the insns into RET.  Update the
   number of reserved temporaries in CURR_TEMP_VARS_NUM and result
   location index in RESULT.  Return the compilation status.  */
static int
compile_flip_flop(rb_iseq_t *iseq, LINK_ANCHOR *const ret, NODE *node, int again,
		  LABEL *then_label, LABEL *else_label,
		  int *result, int *curr_temp_vars_num)
{
    const int line = nd_line(node);
    LABEL *lend = NEW_LABEL(line);
    rb_num_t cnt = ISEQ_FLIP_CNT_INCREMENT(iseq->body->local_iseq)
	+ VM_SVAR_FLIPFLOP_START;
    VALUE key = INT2FIX(cnt);
    int res, temp_res, temp_vars_num;
    int ls = iseq->body->local_table_size;

    if (result == NULL || *result == stack_result || *result == anywhere_result) {
	int st_res = stack_result;
	res = setup_result_var_number(iseq, &st_res, curr_temp_vars_num);
    } else
	res = *result;
    temp_vars_num = *curr_temp_vars_num;

    {
	int st_res = stack_result;
	temp_res = setup_result_var_number(iseq, &st_res, &temp_vars_num);
	ADD_INSN3(ret, line, special2var, INT2LINT(ls - temp_res),
		  key, INT2FIX(0));
	ADD_RTL_INSNL(ret, line, bt, lend, INT2LINT(ls - temp_res));
    }

    /* *flip == 0 */
    temp_res = res;
    temp_vars_num = *curr_temp_vars_num;
    CHECK(COMPILE(ret, "flip2 beg", node->nd_beg, &temp_res, &temp_vars_num));
    if (result != NULL && temp_res != res)
	add_local_move(iseq, ret, line, ls - res, ls - temp_res);
    ADD_RTL_INSNL(ret, line, bf, else_label, INT2LINT(ls - temp_res));
    add_value_load(iseq, ret, line, ls - res, Qtrue);
    if (! again) {
	ADD_INSN2(ret, line, var2special, key, INT2LINT(ls - temp_res));
	ADD_RTL_GOTO(ret, line, then_label);
    }
    else {
	ADD_INSN2(ret, line, var2special, key, INT2LINT(ls - temp_res));
    }

    /* *flip == 1 */
    ADD_LABEL(ret, lend);
    lend->sp = *curr_temp_vars_num;
    temp_res = anywhere_result;
    temp_vars_num = *curr_temp_vars_num;
    CHECK(COMPILE(ret, "flip2 end", node->nd_end, &temp_res, &temp_vars_num));
    {
	int st_res = stack_result;
	ADD_RTL_INSNL(ret, line, bf, then_label, INT2LINT(ls - temp_res));
	temp_res = setup_result_var_number(iseq, &st_res, &temp_vars_num);
	add_value_load(iseq, ret, line, ls - temp_res, Qfalse);
	ADD_INSN2(ret, line, var2special, key, INT2LINT(ls - temp_res));
    }
    ADD_RTL_GOTO(ret, line, then_label);
    if (result != NULL)
	*result = res;
    return COMPILE_OK;
}

/* Generate ISEQ insns for a conditional branch on condition COND with
   THEN_LABEL and ELSE_LABEL.  Put the insns into RET.  Update the
   number of reserved temporaries in CURR_TEMP_VARS_NUM and result
   location index in RESULT.  Return the compilation status.  */
static int
compile_branch_condition(rb_iseq_t *iseq, LINK_ANCHOR *const ret, NODE * cond,
			 LABEL *then_label, LABEL *else_label,
			 int *result, int *curr_temp_vars_num)
{
    int temp_vars_num = *curr_temp_vars_num;
    switch (nd_type(cond)) {
      case NODE_AND:
	{
	    LABEL *label = NEW_LABEL(nd_line(cond));
	    label->sp = temp_vars_num;
	    CHECK(compile_branch_condition(iseq, ret, cond->nd_1st, label,
					   else_label, result, &temp_vars_num));
	    ADD_LABEL(ret, label);
	    temp_vars_num = *curr_temp_vars_num;
	    CHECK(compile_branch_condition(iseq, ret, cond->nd_2nd, then_label,
					   else_label, result, curr_temp_vars_num));
	    break;
	}
      case NODE_OR:
	{
	    LABEL *label = NEW_LABEL(nd_line(cond));
	    label->sp = temp_vars_num;
	    CHECK(compile_branch_condition(iseq, ret, cond->nd_1st, then_label,
					   label, result, &temp_vars_num));
	    ADD_LABEL(ret, label);
	    temp_vars_num = *curr_temp_vars_num;
	    compile_branch_condition(iseq, ret, cond->nd_2nd, then_label,
				     else_label, result, curr_temp_vars_num);
	    break;
	}
      case NODE_LIT:		/* NODE_LIT is always not true */
      case NODE_TRUE:
      case NODE_STR:
      case NODE_DSTR:
      case NODE_XSTR:
      case NODE_DXSTR:
      case NODE_DREGX:
      case NODE_DREGX_ONCE:
      case NODE_DSYM:
      case NODE_ARRAY:
      case NODE_ZARRAY:
      case NODE_HASH:
      case NODE_LAMBDA:
      case NODE_DEFN:
      case NODE_DEFS:
	/* printf("useless condition eliminate (%s)\n",  ruby_node_name(nd_type(cond))); */
	ADD_RTL_GOTO(ret, nd_line(cond), then_label);
	break;
      case NODE_FALSE:
      case NODE_NIL:
	/* printf("useless condition eliminate (%s)\n", ruby_node_name(nd_type(cond))); */
	ADD_RTL_GOTO(ret, nd_line(cond), else_label);
	break;
      case NODE_FLIP2:
	CHECK(compile_flip_flop(iseq, ret, cond, TRUE, then_label, else_label, result, curr_temp_vars_num));
	break;
      case NODE_FLIP3:
	CHECK(compile_flip_flop(iseq, ret, cond, FALSE, then_label, else_label, result, curr_temp_vars_num));
	break;
      default:
	{
	    int op_result = anywhere_result;
	    int ls = iseq->body->local_table_size;

	    CHECK(COMPILE(ret, "branch condition", cond, &op_result, &temp_vars_num));
	    ADD_RTL_INSNL(ret, nd_line(cond), bf, else_label, INT2LINT(ls - op_result));
	    ADD_RTL_GOTO(ret, nd_line(cond), then_label);
	    break;
	  }
    }
    return COMPILE_OK;
}

/* Generate ISEQ insns for args ROOT_NODE with keyword args.  Put the
   insns into RET.  Return keyword arg through and KW_ARG_PTR.  Update
   the number of reserved temporaries in CURR_TEMP_VARS_NUM and result
   location index in RESULT.  Return the compilation status.  */
static int
compile_array_keyword_arg(rb_iseq_t *iseq, LINK_ANCHOR *const ret,
			  const NODE * const root_node,
			  struct rb_call_info_kw_arg ** const kw_arg_ptr,
			  int *result, int *curr_temp_vars_num)
{
    if (kw_arg_ptr == NULL) return FALSE;

    if (nd_type(root_node) == NODE_HASH && root_node->nd_head && nd_type(root_node->nd_head) == NODE_ARRAY) {
	NODE *node = root_node->nd_head;

	while (node) {
	    NODE *key_node = node->nd_head;

	    assert(nd_type(node) == NODE_ARRAY);
	    if (key_node && nd_type(key_node) == NODE_LIT && RB_TYPE_P(key_node->nd_lit, T_SYMBOL)) {
		/* can be keywords */
	    }
	    else {
		return FALSE;
	    }
	    node = node->nd_next; /* skip value node */
	    node = node->nd_next;
	}

	/* may be keywords */
	node = root_node->nd_head;
	{
	    int len = (int)node->nd_alen / 2;
	    struct rb_call_info_kw_arg *kw_arg  = (struct rb_call_info_kw_arg *)ruby_xmalloc(sizeof(struct rb_call_info_kw_arg) + sizeof(VALUE) * (len - 1));
	    VALUE *keywords = kw_arg->keywords;
	    int i = 0;

	    assert(result != NULL && *result == stack_result);
	    kw_arg->keyword_len = len;

	    *kw_arg_ptr = kw_arg;

	    for (i=0; node != NULL; i++, node = node->nd_next->nd_next) {
		NODE *key_node = node->nd_head;
		NODE *val_node = node->nd_next->nd_head;
		int temp_res = stack_result;

		keywords[i] = key_node->nd_lit;
		COMPILE(ret, "keyword values", val_node, (i == 0 ? result : &temp_res), curr_temp_vars_num);
	    }
	    assert(i == len);
	    return TRUE;
	}
    }
    return FALSE;
}

static inline int
static_literal_node_p(NODE *node)
{
    node = node->nd_head;
    switch (nd_type(node)) {
      case NODE_LIT:
      case NODE_NIL:
      case NODE_TRUE:
      case NODE_FALSE:
	return TRUE;
      default:
	return FALSE;
    }
}

static inline VALUE
static_literal_value(NODE *node)
{
    node = node->nd_head;
    switch (nd_type(node)) {
      case NODE_NIL:
	return Qnil;
      case NODE_TRUE:
	return Qtrue;
      case NODE_FALSE:
	return Qfalse;
      default:
	return node->nd_lit;
    }
}

enum compile_array_type_t {
    COMPILE_ARRAY_TYPE_ARRAY,
    COMPILE_ARRAY_TYPE_HASH,
    COMPILE_ARRAY_TYPE_ARGS
};

/* Return a new specialized insn returning FROM from teh current frame
   to variable TO from a previous frame.  The insn is created with
   LINE_NO for ISEQ.  TO and FROM are offsets.  */
static INSN *
new_ret_to(rb_iseq_t *iseq, int line_no, lindex_t to, lindex_t from) {
    return new_insn_body(iseq, line_no, LINT2INT(to) < 0 ? BIN(ret_to_temp) : BIN(ret_to_loc), 2,
			 (VALUE) to, (VALUE) from);
}

/* Add a new specialized ret_to insn to RET for ISEQ.   */
static void
add_ret_to(rb_iseq_t *iseq, LINK_ANCHOR *ret, int line_no, lindex_t to, lindex_t from) {
    ADD_ELEM(ret, (LINK_ELEMENT *) new_ret_to(iseq, line_no, to, from));
}

/* Generate ISEQ insns for array ROOT_NODE representing different Ruby
   language constructions given by TYPE.  Put the insns into RET.
   Return keyword args (if any) through and KEYWORDS_PTR.  Update the
   number of reserved temporaries in CURR_TEMP_VARS_NUM and result
   location index in RESULT.  Return the compilation status.  True
   POPPED means the result will be not used.  */
static int
compile_array_(rb_iseq_t *iseq, LINK_ANCHOR *const ret, NODE* node_root,
	       enum compile_array_type_t type, struct rb_call_info_kw_arg **keywords_ptr, int popped,
	       int *result, int *curr_temp_vars_num)
{
    NODE *node = node_root;
    int line = (int)nd_line(node);
    int len = 0;
    int ls = iseq->body->local_table_size;

    if (nd_type(node) == NODE_ZARRAY) {
	switch (type) {
	  case COMPILE_ARRAY_TYPE_ARRAY:{
	    ADD_INSN3(ret, line, make_array,
		      INT2LINT(ls - setup_result_var_number(iseq, result, curr_temp_vars_num)),
		      INT2LINT(0), INT2FIX(0));
	    break;
	  }
	  case COMPILE_ARRAY_TYPE_HASH: {
	    ADD_INSN3(ret, line, make_hash,
		      INT2LINT(ls - setup_result_var_number(iseq, result, curr_temp_vars_num)),
		      INT2LINT(0), INT2FIX(0));
	    break;
	  }
	  case COMPILE_ARRAY_TYPE_ARGS: /* do nothing */ break;
	}
    }
    else {
	int opt_p = 1;
	int first = 1, i;
	int stack_p = result != NULL && *result == stack_result;
	int res, array_res = stack_result, array_temp_vars_num = *curr_temp_vars_num;

	if (type == COMPILE_ARRAY_TYPE_HASH) {
	    /* Reserve stack slot for VM_SPECIAL_OBJECT_VMCORE.  */
	    setup_result_var_number(iseq, &array_res, &array_temp_vars_num);
	    array_res = stack_result;
	}
	/* Reserve result on the stack as local as result can be used
	   in array calculation.  */
	setup_result_var_number(iseq, &array_res, &array_temp_vars_num);
	while (node) {
	    NODE *start_node = node, *end_node;
	    NODE *kw = 0;
	    const int max = 0x100;
	    int temp_vars_num = array_temp_vars_num - first;
	    int array_start = ls + temp_vars_num + 1;
	    DECL_ANCHOR(anchor);
	    INIT_ANCHOR(anchor);

	    res = array_res;
	    for (i=0; i<max && node; i++, len++, node = node->nd_next) {
		int temp_res = stack_result;

		if (CPDEBUG > 0) {
		    EXPECT_NODE("compile_array", node, NODE_ARRAY);
		}

		if (type != COMPILE_ARRAY_TYPE_ARRAY && !node->nd_head) {
		    kw = node->nd_next;
		    node = 0;
		    if (kw) {
			opt_p = 0;
			node = kw->nd_next;
			kw = kw->nd_head;
		    }
		    break;
		}
		if (opt_p && !static_literal_node_p(node)) {
		    opt_p = 0;
		}
		if (type == COMPILE_ARRAY_TYPE_ARGS && node->nd_next == NULL /* last node */
		    && compile_array_keyword_arg(iseq, anchor, node->nd_head, keywords_ptr,
						 &temp_res, &temp_vars_num)) {
		    len--;
		}
		else {
		    COMPILE_(anchor, "array element", node->nd_head, popped,
			     &temp_res, &temp_vars_num);
		}
	    }

	    if (opt_p && type != COMPILE_ARRAY_TYPE_ARGS) {
		VALUE ary = rb_ary_tmp_new(i);

		end_node = node;
		node = start_node;

		while (node != end_node) {
		    rb_ary_push(ary, static_literal_value(node));
		    node = node->nd_next;
		}
		while (node && node->nd_next &&
		       static_literal_node_p(node) &&
		       static_literal_node_p(node->nd_next)) {
		    VALUE elem[2];
		    elem[0] = static_literal_value(node);
		    elem[1] = static_literal_value(node->nd_next);
		    rb_ary_cat(ary, elem, 2);
		    node = node->nd_next->nd_next;
		    len++;
		}

		OBJ_FREEZE(ary);

		iseq_add_mark_object_compile_time(iseq, ary);

		if (first) {
		    first = 0;
		    if (type == COMPILE_ARRAY_TYPE_ARRAY) {
			ADD_INSN2(ret, line, clone_array, INT2LINT(ls - array_res), ary);
		    }
		    else { /* COMPILE_ARRAY_TYPE_HASH */
			ADD_INSN2(ret, line, specialobj2var,
				  INT2LINT(ls - array_res + 1),
				  INT2FIX(VM_SPECIAL_OBJECT_VMCORE));
			add_value_load(iseq, ret, line, ls - array_res, ary);
			ADD_RTL_SEND_R(ret, line, id_core_hash_from_ary,
				       INT2FIX(1), NULL, FALSE, array_res - 1,
				       (VALUE) INT2FIX(0), NULL);
			res = array_res - 1;
			array_temp_vars_num = res - ls + (node != NULL);
		    }
		}
		else {
		    int temp_vars_num = array_temp_vars_num;
		    int temp_res = stack_result;

		    if (type == COMPILE_ARRAY_TYPE_ARRAY) {
			add_value_load(iseq, ret, line,
				       ls - setup_result_var_number(iseq, &temp_res, &temp_vars_num),
				       ary);
			ADD_INSN3(ret, line, concat_array,
				  INT2LINT(ls - array_res),
				  INT2LINT(ls - array_res), INT2LINT(ls - temp_res));
		    }
		    else {
			int temp_res2 = stack_result;

			ADD_INSN2(ret, line, specialobj2var,
				  INT2LINT(ls - array_res + 1),
				  INT2FIX(VM_SPECIAL_OBJECT_VMCORE));
			add_value_load(iseq, ret, line,
				       ls - setup_result_var_number(iseq, &temp_res2, &temp_vars_num),
				       ary);
			ADD_RTL_SEND_R(ret, line, id_core_hash_merge_ary, INT2FIX(2), NULL, FALSE,
				       array_res - 1, (VALUE)INT2FIX(0), NULL);
			res = array_res - 1;
			array_temp_vars_num = res - ls + (node != NULL);
		    }
		}
	    }
	    else {
		switch (type) {
		  case COMPILE_ARRAY_TYPE_ARRAY:
		    ADD_INSN3(anchor, line, make_array,
			      INT2LINT(ls - array_start),
			      INT2LINT(ls - array_start), INT2FIX(i));
		    if (first) {
			first = 0;
			assert(array_start == array_res);
		    }
		    else {

			ADD_INSN3(anchor, line, concat_array,
				  INT2LINT(ls - array_res),
				  INT2LINT(ls - array_res), INT2LINT(ls - array_start));
		    }

		    APPEND_LIST(ret, anchor);
		    break;
		  case COMPILE_ARRAY_TYPE_HASH:
		    if (i > 0) {
			if (first) {
			    ADD_INSN3(anchor, line, make_hash,
				      INT2LINT(ls - array_res + 1),
				      INT2LINT(ls - array_start),
				      INT2FIX(i));
			    APPEND_LIST(ret, anchor);
			}
			else {
			    add_local_move(iseq, ret, line, ls - array_res, ls - array_res + 1);
			    ADD_INSN2(ret, line, specialobj2var,
				      INT2LINT(ls - array_res + 1),
				      INT2FIX(VM_SPECIAL_OBJECT_VMCORE));
			    APPEND_LIST(ret, anchor);
			    ADD_RTL_SEND_R(ret, line, id_core_hash_merge_ptr, INT2FIX(i + 1), NULL, FALSE,
					   array_res - 1, (VALUE)INT2FIX(0), NULL);
			}
		    }
		    if (kw) {
			VALUE nhash = (i > 0 || !first) ? INT2FIX(2) : INT2FIX(1);
			int temp_res = stack_result;
			int temp_vars_num = array_temp_vars_num - (i == 0 && first);

			add_local_move(iseq, ret, line, ls - array_res, ls - array_res + 1);
			ADD_INSN2(ret, line, specialobj2var,
				  INT2LINT(ls - array_res + 1),
				  INT2FIX(VM_SPECIAL_OBJECT_VMCORE));
			COMPILE(ret, "keyword splat", kw, &temp_res, &temp_vars_num);
			ADD_RTL_SEND_R(ret, line, id_core_hash_merge_kwd, nhash, NULL, FALSE,
				       array_res - 1, (VALUE)INT2FIX(0), NULL);
			if (nhash == INT2FIX(1)) {
			  ADD_RTL_SEND_R(ret, line, rb_intern("dup"), INT2FIX(0), NULL, FALSE,
					 array_res - 1, (VALUE)INT2FIX(0), NULL);
			}
			if (node != NULL)
			  add_local_move(iseq, ret, line, ls - array_res, ls - array_res + 1);
		    }
		    res = array_res - 1;
		    array_temp_vars_num = res - ls + (node != NULL);
		    first = 0;
		    break;
		  case COMPILE_ARRAY_TYPE_ARGS:
		    APPEND_LIST(ret, anchor);
		    array_temp_vars_num = temp_vars_num;
		    break;
		}
	    }
	}
	if (stack_p || (result != NULL && *result == anywhere_result)) {
	    *curr_temp_vars_num = array_temp_vars_num;
	    if (result != NULL)
		*result = res;
	}
	else if (!popped && result != NULL) {
	    /* Move */
	    add_local_move(iseq, ret, line, ls - *result, ls - res);
	}
    }
    return len;
}

/* See comments for compile_array_.  */
static VALUE
compile_array(rb_iseq_t *iseq, LINK_ANCHOR *const ret, NODE* node_root, enum compile_array_type_t type,
	      int *result, int *curr_temp_vars_num)
{
    return compile_array_(iseq, ret, node_root, type, NULL, 0, result, curr_temp_vars_num);
}

static VALUE
case_when_optimizable_literal(NODE *node)
{
    switch (nd_type(node)) {
      case NODE_LIT: {
	VALUE v = node->nd_lit;
	double ival;
	if (RB_TYPE_P(v, T_FLOAT) &&
	    modf(RFLOAT_VALUE(v), &ival) == 0.0) {
	    return FIXABLE(ival) ? LONG2FIX((long)ival) : rb_dbl2big(ival);
	}
	if (SYMBOL_P(v) || rb_obj_is_kind_of(v, rb_cNumeric)) {
	    return v;
	}
	break;
      }
      case NODE_NIL:
	return Qnil;
      case NODE_TRUE:
	return Qtrue;
      case NODE_FALSE:
	return Qfalse;
      case NODE_STR:
	return node->nd_lit = rb_fstring(node->nd_lit);
    }
    return Qundef;
}

/* Generate ISEQ insns of when conditionals VALS and label L1 of code
   in the match success case.  Put the insns into COND_SEQ. Update the
   number of reserved temporaries in CURR_TEMP_VARS_NUM and result
   location index in RESULT.  Return updated value
   ONLY_SPECIAL_LITERALS (using only special literals in
   when-clauses).  */
static int
when_vals(rb_iseq_t *iseq, LINK_ANCHOR *const cond_seq, NODE *vals,
	  LABEL *l1, int only_special_literals, VALUE literals,
	  int case_op, int *curr_temp_vars_num)
{
    int ls = iseq->body->local_table_size;

    while (vals) {
	NODE* val = vals->nd_head;
	VALUE lit = case_when_optimizable_literal(val);
	int temp_vars_num = *curr_temp_vars_num;
	int temp_res, res, when_op = anywhere_result;

	if (lit == Qundef) {
	    only_special_literals = 0;
	}
	else {
	    if (rb_hash_lookup(literals, lit) != Qnil) {
		rb_compile_warning(ruby_sourcefile, nd_line(val),
				   "duplicated when clause is ignored");
	    }
	    else {
		rb_hash_aset(literals, lit, (VALUE)(l1) | 1);
	    }
	}

	if (nd_type(val) == NODE_STR) {
	    val->nd_lit = rb_fstring(val->nd_lit);
	    debugp_param("nd_lit", val->nd_lit);
	    setup_result_var_number(iseq, &when_op, &temp_vars_num);
	    add_value_load(iseq, cond_seq, nd_line(val), ls - when_op, val->nd_lit);
	}
	else {
	    COMPILE(cond_seq, "when cond", val, &when_op, &temp_vars_num);
	}

	temp_res = stack_result;
	res = setup_result_var_number(iseq, &temp_res, &temp_vars_num);
	ADD_INSN4(cond_seq, nd_line(vals), check_match,
		  INT2LINT(ls - res), INT2LINT(ls - case_op), INT2LINT(ls - when_op),
		  INT2FIX(VM_CHECKMATCH_TYPE_CASE));
	ADD_RTL_INSNL(cond_seq, nd_line(vals), bt, l1, INT2LINT(ls - res));
	vals = vals->nd_next;
    }
    return only_special_literals;
}

/* Generate ISEQ insns for LHS part NODE of a multiple assignment with
   start RHS location index RHS_START.  Put the insns into RET.
   Update the number of reserved temporaries in CURR_TEMP_VARS_NUM.
   Return the compilation status.  */
static int
compile_massign_lhs(rb_iseq_t *iseq, LINK_ANCHOR *const ret, NODE *node, int *curr_temp_vars_num,
		    int rhs_start)
{
    int ls = iseq->body->local_table_size;

    switch (nd_type(node)) {
      case NODE_ATTRASGN: {
	INSN *iobj;
	struct rb_call_info *ci;
	int line = nd_line(node);

	CHECK(COMPILE_POPPED(ret, "masgn lhs (NODE_ATTRASGN)", node, NULL, curr_temp_vars_num));

	iobj = (INSN *)POP_ELEMENT(ret); /* pop send insn */
	ci = (struct rb_call_info *)iobj->operands[0];
	ci->orig_argc += 1;

	if (!(ci->flag & VM_CALL_ARGS_SPLAT))
	    add_local_move(iseq, ret, line, -*curr_temp_vars_num - 1 - ci->orig_argc, ls - rhs_start);
	else {
	    int res = ls + *curr_temp_vars_num;
	    --ci->orig_argc;
	    ADD_INSN3(ret, line, make_array, INT2LINT(ls - res), INT2LINT(ls - res), INT2FIX(1));
	    ADD_INSN3(ret, line, concat_array, INT2LINT(ls - res - 2), INT2LINT(ls - res), INT2LINT(ls - res + 1));
	}
	ADD_ELEM(ret, (LINK_ELEMENT *)iobj);
	break;
      }
      case NODE_MASGN: {
	DECL_ANCHOR(anchor);
	INIT_ANCHOR(anchor);
	CHECK(COMPILE_POPPED(anchor, "nest masgn lhs", node, NULL, curr_temp_vars_num));
	REMOVE_ELEM(FIRST_ELEMENT(anchor));
	ADD_SEQ(ret, anchor);
	break;
      }
      default: {
	INSN *iobj;
	DECL_ANCHOR(anchor);

	INIT_ANCHOR(anchor);
	CHECK(COMPILE_POPPED(anchor, "masgn lhs", node, NULL, curr_temp_vars_num));
	iobj = (INSN *) FIRST_ELEMENT(anchor);

	assert(IS_INSN_ID(iobj, val2temp) || IS_INSN_ID(iobj, val2loc));
	INSERT_ELEM_AFTER(anchor, (LINK_ELEMENT *) iobj,
			  (LINK_ELEMENT *) new_local_move(iseq, iobj->line_no,
							  LINT2INT(iobj->operands[0]),
							  ls - rhs_start));
	REMOVE_ELEM(FIRST_ELEMENT(anchor));
	ADD_SEQ(ret, anchor);
      }
    }

    return COMPILE_OK;
}

/* Generate optimized version of ISEQ insns for LHS part (LHSN) of a
   multiple assignment.  Put the insns into RET.  Update the number of
   reserved temporaries in CURR_TEMP_VARS_NUM and result location
   index in RESULT.  Return the compilation status.  */
static int
compile_massign_opt_lhs(rb_iseq_t *iseq, LINK_ANCHOR *const ret, NODE *lhsn,
			int *curr_temp_vars_num, int rhs_start)
{
    if (lhsn) {
	int temp_vars_num = *curr_temp_vars_num;
	CHECK(compile_massign_opt_lhs(iseq, ret, lhsn->nd_next, &temp_vars_num, rhs_start + 1));
	CHECK(compile_massign_lhs(iseq, ret, lhsn->nd_head, &temp_vars_num, rhs_start));
    }
    return COMPILE_OK;
}

/* Try to generate optimized version of ISEQ insns for multiple
   assignment NODE.  Put the insns into RET.  Update the number of
   reserved temporaries in CURR_TEMP_VARS_NUM and result location
   index in RESULT.  Return non-zero in a success case.  */
static int
compile_massign_opt(rb_iseq_t *iseq, LINK_ANCHOR *const ret,
		    NODE *rhsn, NODE *orig_lhsn, int *result, int *curr_temp_vars_num)
{
    VALUE mem[64];
    const int memsize = numberof(mem);
    int memindex = 0;
    int llen = 0, rlen = 0;
    int i, rhs_start, ls = iseq->body->local_table_size;
    NODE *lhsn = orig_lhsn;

#define MEMORY(v) { \
    int i; \
    if (memindex == memsize) return 0; \
    for (i=0; i<memindex; i++) { \
	if (mem[i] == (v)) return 0; \
    } \
    mem[memindex++] = (v); \
}

    if (rhsn == 0 || nd_type(rhsn) != NODE_ARRAY) {
	return 0;
    }

    while (lhsn) {
	NODE *ln = lhsn->nd_head;
	switch (nd_type(ln)) {
	  case NODE_LASGN:
	    MEMORY(ln->nd_vid);
	    break;
	  case NODE_DASGN:
	  case NODE_DASGN_CURR:
	  case NODE_IASGN:
	  case NODE_IASGN2:
	  case NODE_CVASGN:
	    MEMORY(ln->nd_vid);
	    break;
	  default:
	    return 0;
	}
	lhsn = lhsn->nd_next;
	llen++;
    }

    rhs_start = ls + *curr_temp_vars_num + 1;
    while (rhsn) {
	if (llen <= rlen) {
	    int temp_res = anywhere_result;
	    int temp_vars_num = *curr_temp_vars_num;
	    COMPILE_POPPED(ret, "masgn val (popped)", rhsn->nd_head,
			   &temp_res, &temp_vars_num);
	}
	else {
	    int temp_res = stack_result;

	    COMPILE(ret, "masgn val", rhsn->nd_head, &temp_res, curr_temp_vars_num);
	}
	rhsn = rhsn->nd_next;
	rlen++;
    }

    if (llen > rlen) {
	for (i=0; i<llen-rlen; i++) {
	    int temp_res = stack_result;
	    add_value_load(iseq, ret, nd_line(orig_lhsn),
			   ls - setup_result_var_number(iseq, &temp_res, curr_temp_vars_num), Qnil);
	}
    }

    compile_massign_opt_lhs(iseq, ret, orig_lhsn, curr_temp_vars_num, rhs_start);
    return 1;
}

/* Generate ISEQ insns for multiple assignment NODE.  Put the insns
   into RET.  Update the number of reserved temporaries in
   CURR_TEMP_VARS_NUM and result location index in RESULT.  Return the
   compilation status.  True POPPED means the result will be not
   used.  */
static int
compile_massign(rb_iseq_t *iseq, LINK_ANCHOR *const ret, NODE *node, int popped,
		int *result, int *curr_temp_vars_num)
{
    NODE *rhsn = node->nd_value;
    NODE *splatn = node->nd_args;
    NODE *curr, *lhsn = node->nd_head;
    int n, temp_res, rhs_start, rhs_result, lhs_splat = (splatn && (VALUE)splatn != (VALUE)-1) ? 1 : 0;
    int ls = iseq->body->local_table_size;
    int saved_temp_vars_num = *curr_temp_vars_num;

    if (!popped || splatn || !compile_massign_opt(iseq, ret, rhsn, lhsn, result, curr_temp_vars_num)) {
	int llen = 0;

	assert(saved_temp_vars_num == *curr_temp_vars_num);
	temp_res = stack_result;
	if (!rhsn) {
	    int temp_vars_num = *curr_temp_vars_num;
	    rhs_start = *curr_temp_vars_num;
	    rhs_result = ls + rhs_start;
	    COMPILE(ret, "normal masgn rhs", rhsn, &temp_res, &temp_vars_num);
	} else {
	    rhs_start = *curr_temp_vars_num + 1;
	    COMPILE(ret, "normal masgn rhs", rhsn, &temp_res, curr_temp_vars_num);
	    rhs_result = temp_res;
	}
	for (curr = lhsn ;curr; curr = curr->nd_next)
	    llen++;

	if (!popped && result != NULL) {
	    add_local_move(iseq, ret, nd_line(node), -*curr_temp_vars_num - 1, - *curr_temp_vars_num);

	    *result = ls + *curr_temp_vars_num;
	    (*curr_temp_vars_num)++;
	    rhs_start++;
	    rhs_result++;
	}

 	if (lhs_splat || llen != 0) {
	    ADD_INSN3(ret, nd_line(node), spread_array,
		      INT2LINT(ls - rhs_result), INT2FIX(llen), INT2FIX(lhs_splat));
	}

	n = 1;
	while (lhsn) {
	    int temp_vars_num = lhs_splat ? *curr_temp_vars_num : rhs_start + llen - n;
	    if (lhs_splat && nd_type(lhsn->nd_head) == NODE_MASGN)
		n++, temp_vars_num++;
	    CHECK(compile_massign_lhs(iseq, ret, lhsn->nd_head, &temp_vars_num,
				      ls + rhs_start + (0&&lhs_splat ? 0 : llen - n + lhs_splat)));
	    lhsn = lhsn->nd_next;
	    n++;
	}

	if (lhs_splat) {
	    if (nd_type(splatn) == NODE_POSTARG) {
		/*a, b, *r, p1, p2 */
		NODE *postn = splatn->nd_2nd;
		NODE *restn = splatn->nd_1st;
		int n, num = (int)postn->nd_alen;
		int flag = 0x02 | (((VALUE)restn == (VALUE)-1) ? 0x00 : 0x01);

		ADD_INSN3(ret, nd_line(node), spread_array,
			  INT2LINT(ls-rhs_result), INT2FIX(num), INT2FIX(flag));

		for (n = 0, curr = postn; curr; curr = curr->nd_next)
		    n++;

		if ((VALUE)restn != (VALUE)-1) {
		    int temp_vars_num;
		    temp_vars_num = *curr_temp_vars_num + n;
		    CHECK(compile_massign_lhs(iseq, ret, restn, &temp_vars_num, rhs_result + n));
		}
		n--;
		while (postn) {
		    int temp_vars_num;
		    temp_vars_num = *curr_temp_vars_num + n;
		    CHECK(compile_massign_lhs(iseq, ret, postn->nd_head, &temp_vars_num, rhs_result + n));
		    n--;
		    postn = postn->nd_next;
		}
	    }
	    else {
		/* a, b, *r */
		CHECK(compile_massign_lhs(iseq, ret, splatn, curr_temp_vars_num, rhs_result));
	    }
	}
    }
    return COMPILE_OK;
}

/* Generate ISEQ insns for scoped constant reference NODE.  Put the
   insns into PREF and BODY.  When we put the insns into pref, assign
   TRUE to *PREF_FLAG.  Update the number of reserved temporaries in
   CURR_TEMP_VARS_NUM and result location index in RESULT.  Return the
   compilation status.  */
static int
compile_colon2(rb_iseq_t *iseq, NODE * node,
	       LINK_ANCHOR *const pref, LINK_ANCHOR *const body,
	       int *result, int *curr_temp_vars_num, int *pref_flag)
{
    int ls = iseq->body->local_table_size;

    switch (nd_type(node)) {
      case NODE_CONST:{
	int res = setup_result_var_number(iseq, result, curr_temp_vars_num);

	debugi("compile_colon2 - colon", node->nd_vid);
	ADD_INSN3(body, nd_line(node), const_ld_val, ID2SYM(node->nd_vid),
		  INT2LINT(ls - res), Qnil);
	break;
      }
      case NODE_COLON3:{
	int res = setup_result_var_number(iseq, result, curr_temp_vars_num);

	debugi("compile_colon2 - colon3", node->nd_mid);
	ADD_INSN3(body, nd_line(node), const_ld_val, ID2SYM(node->nd_mid),
		  INT2LINT(ls - res), rb_cObject);
	break;
      }
      case NODE_COLON2: {
	int temp_vars_num = *curr_temp_vars_num;
	int head_op = anywhere_result;
	int res = setup_result_var_number(iseq, result, curr_temp_vars_num);

	compile_colon2(iseq, node->nd_head, pref, body, &head_op, &temp_vars_num, pref_flag);
	debugi("compile_colon2 - colon2", node->nd_mid);
	ADD_INSN3(body, nd_line(node), const2var, ID2SYM(node->nd_mid),
		  INT2LINT(ls - res), INT2LINT(ls - head_op));
	break;
      }
      default:
	*pref_flag = TRUE;
        CHECK(COMPILE(pref, "const colon2 prefix", node, result, curr_temp_vars_num));
	break;
    }
    return COMPILE_OK;
}

/* Generate ISEQ insns for class path CPATH.  Put the insns into RET.
   Update the number of reserved temporaries in CURR_TEMP_VARS_NUM and
   result location index in RESULT.  Return Qtrue for case Bar::Foo.
   Otherwise return Qfalse.  */
static VALUE
compile_cpath(LINK_ANCHOR *const ret, rb_iseq_t *iseq, NODE *cpath, int *result, int *curr_temp_vars_num)
{
    if (nd_type(cpath) == NODE_COLON3) {
	/* toplevel class ::Foo */
	add_value_load(iseq, ret, nd_line(cpath),
		       (int) iseq->body->local_table_size - setup_result_var_number(iseq, result, curr_temp_vars_num),
		       rb_cObject);
	return Qfalse;
    }
    else if (cpath->nd_head) {
	/* Bar::Foo */
	COMPILE(ret, "nd_else->nd_head", cpath->nd_head, result, curr_temp_vars_num);
	return Qfalse;
    }
    else {
	/* class at cbase Foo */
	ADD_INSN2(ret, nd_line(cpath), specialobj2var,
		  INT2LINT((int) iseq->body->local_table_size
			   - setup_result_var_number(iseq, result, curr_temp_vars_num)),
		  INT2FIX(VM_SPECIAL_OBJECT_CONST_BASE));
	return Qtrue;
    }
}
#define private_recv_p(node) (nd_type((node)->nd_recv) == NODE_SELF)

#define defined_expr defined_expr0

/* Generate ISEQ insns for defined expression NODE.  Put the insns
   into RET.  Update the number of reserved temporaries in
   CURR_TEMP_VARS_NUM and result location index in RESULT.  If
   LFINISH[1] is not set up yet, create a new label, assign it to
   LFINISH[1] and use it to short cut of the false result.  NEEDSTR is
   a parameter to (val_)defined_p insns or flag to use
   rb_iseq_defined_string call result as the result for simple NODE
   cases.  Return non-zero in a success case.  */
static int
defined_expr(rb_iseq_t *iseq, LINK_ANCHOR *const ret,
	     NODE *node, LABEL **lfinish, VALUE needstr, int *result, int *curr_temp_vars_num)
{
    int ls = iseq->body->local_table_size;
    enum defined_type expr_type = 0;
    enum node_type type;

    switch (type = nd_type(node)) {

	/* easy literals */
      case NODE_NIL:
	expr_type = DEFINED_NIL;
	break;
      case NODE_SELF:
	expr_type = DEFINED_SELF;
	break;
      case NODE_TRUE:
	expr_type = DEFINED_TRUE;
	break;
      case NODE_FALSE:
	expr_type = DEFINED_FALSE;
	break;

      case NODE_ARRAY:{
	NODE *vals = node;

	do {
	    int op_result = anywhere_result;
	    int temp_vars_num = *curr_temp_vars_num;

	    defined_expr(iseq, ret, vals->nd_head, lfinish, Qfalse, &op_result, &temp_vars_num);

	    if (!lfinish[1]) {
		lfinish[1] = NEW_LABEL(nd_line(node));
		lfinish[1]->sp = *curr_temp_vars_num;
	    }
	    ADD_RTL_INSNL(ret, nd_line(node), bf, lfinish[1], INT2LINT(ls - op_result));
	} while ((vals = vals->nd_next) != NULL);
      }
      case NODE_STR:
      case NODE_LIT:
      case NODE_ZARRAY:
      case NODE_AND:
      case NODE_OR:
      default:
	expr_type = DEFINED_EXPR;
	break;

	/* variables */
      case NODE_LVAR:
      case NODE_DVAR:
	expr_type = DEFINED_LVAR;
	break;

      case NODE_IVAR:{
	ADD_INSN5(ret, nd_line(node), val_defined_p,
		  INT2LINT(ls - setup_result_var_number(iseq, result, curr_temp_vars_num)),
		  Qnil, INT2FIX(DEFINED_IVAR), ID2SYM(node->nd_vid), needstr);
	return 1;
      }
      case NODE_GVAR:{
	ADD_INSN5(ret, nd_line(node), val_defined_p,
		  INT2LINT(ls - setup_result_var_number(iseq, result, curr_temp_vars_num)),
		  Qnil, INT2FIX(DEFINED_GVAR), ID2SYM(node->nd_entry->id), needstr);
	return 1;
      }
      case NODE_CVAR:{
	ADD_INSN5(ret, nd_line(node), val_defined_p,
		  INT2LINT(ls - setup_result_var_number(iseq, result, curr_temp_vars_num)),
		  Qnil, INT2FIX(DEFINED_CVAR), ID2SYM(node->nd_vid), needstr);
	return 1;
      }
      case NODE_CONST:{
	ADD_INSN5(ret, nd_line(node), val_defined_p,
		  INT2LINT(ls - setup_result_var_number(iseq, result, curr_temp_vars_num)),
		  Qnil, INT2FIX(DEFINED_CONST), ID2SYM(node->nd_vid), needstr);
	return 1;
      }
      case NODE_COLON2: {
	int op_result = anywhere_result;
	int temp_vars_num = *curr_temp_vars_num;

	if (!lfinish[1]) {
	    lfinish[1] = NEW_LABEL(nd_line(node));
	    lfinish[1]->sp = *curr_temp_vars_num;
	}
	defined_expr(iseq, ret, node->nd_head, lfinish, Qfalse, &op_result, &temp_vars_num);
	ADD_RTL_INSNL(ret, nd_line(node), bf, lfinish[1], INT2LINT(ls - op_result));

	op_result = anywhere_result;
	temp_vars_num = *curr_temp_vars_num;
	COMPILE(ret, "defined/colon2#nd_head", node->nd_head, &op_result, &temp_vars_num);
	if (rb_is_const_id(node->nd_mid)) {
	    ADD_INSN5(ret, nd_line(node), defined_p,
		      INT2LINT(ls - setup_result_var_number(iseq, result, curr_temp_vars_num)),
		      INT2LINT(ls - op_result),
		      INT2FIX(DEFINED_CONST), ID2SYM(node->nd_mid), needstr);
	}
	else {
	    ADD_INSN5(ret, nd_line(node), defined_p,
		      INT2LINT(ls - setup_result_var_number(iseq, result, curr_temp_vars_num)),
		      INT2LINT(ls - op_result),
		      INT2FIX(DEFINED_METHOD), ID2SYM(node->nd_mid), needstr);
	}
	return 1;
      }
      case NODE_COLON3:{
	ADD_INSN5(ret, nd_line(node), val_defined_p,
		  INT2LINT(ls - setup_result_var_number(iseq, result, curr_temp_vars_num)),
		  rb_cObject, INT2FIX(DEFINED_CONST), ID2SYM(node->nd_mid), needstr);
	return 1;
      }
	/* method dispatch */
      case NODE_CALL:
      case NODE_VCALL:
      case NODE_FCALL:
      case NODE_ATTRASGN:{
	int op_result = anywhere_result;
	int temp_vars_num = *curr_temp_vars_num;

	const int explicit_receiver =
	    (type == NODE_CALL ||
	     (type == NODE_ATTRASGN && !private_recv_p(node)));

	if (!lfinish[1]) {
	    lfinish[1] = NEW_LABEL(nd_line(node));
	    lfinish[1]->sp = *curr_temp_vars_num;
	}
	if (node->nd_args) {
	    defined_expr(iseq, ret, node->nd_args, lfinish, Qfalse, &op_result, &temp_vars_num);
	    ADD_RTL_INSNL(ret, nd_line(node), bf, lfinish[1], INT2LINT(ls - op_result));
	}
	if (explicit_receiver) {
	    op_result = anywhere_result;
	    temp_vars_num = *curr_temp_vars_num;
	    defined_expr(iseq, ret, node->nd_recv, lfinish, Qfalse, &op_result, &temp_vars_num);
	    ADD_RTL_INSNL(ret, nd_line(node), bf, lfinish[1], INT2LINT(ls - op_result));
	    op_result = anywhere_result;
	    COMPILE(ret, "defined/recv", node->nd_recv, &op_result, curr_temp_vars_num);
	    ADD_INSN5(ret, nd_line(node), defined_p,
		      INT2LINT(ls - setup_result_var_number(iseq, result, curr_temp_vars_num)),
		      INT2LINT(ls - op_result), INT2FIX(DEFINED_METHOD), ID2SYM(node->nd_mid),
		      needstr);
	}
	else {
	    int res = ls - setup_result_var_number(iseq, result, curr_temp_vars_num);

	    ADD_INSN1(ret, nd_line(node), self2var, INT2LINT(res));
	    ADD_INSN5(ret, nd_line(node), defined_p,
		      INT2LINT(res), INT2LINT(res), INT2FIX(DEFINED_FUNC),
		      ID2SYM(node->nd_mid), needstr);
	}
	return 1;
      }

      case NODE_YIELD:{
	ADD_INSN5(ret, nd_line(node), val_defined_p,
		  INT2LINT(ls - setup_result_var_number(iseq, result, curr_temp_vars_num)),
		  Qnil, INT2FIX(DEFINED_YIELD), 0, needstr);
	return 1;
      }
      case NODE_BACK_REF:
      case NODE_NTH_REF:{
	ADD_INSN5(ret, nd_line(node), val_defined_p,
		  INT2LINT(ls - setup_result_var_number(iseq, result, curr_temp_vars_num)),
		  Qnil, INT2FIX(DEFINED_REF),
		  INT2FIX((node->nd_nth << 1) | (type == NODE_BACK_REF)),
		  needstr);
	return 1;
      }
      case NODE_SUPER:
      case NODE_ZSUPER:{
	ADD_INSN5(ret, nd_line(node), val_defined_p,
		  INT2LINT(ls - setup_result_var_number(iseq, result, curr_temp_vars_num)),
		  Qnil, INT2FIX(DEFINED_ZSUPER), 0, needstr);
	return 1;
      }
      case NODE_OP_ASGN1:
      case NODE_OP_ASGN2:
      case NODE_OP_ASGN_OR:
      case NODE_OP_ASGN_AND:
      case NODE_MASGN:
      case NODE_LASGN:
      case NODE_DASGN:
      case NODE_DASGN_CURR:
      case NODE_GASGN:
      case NODE_IASGN:
      case NODE_CDECL:
      case NODE_CVDECL:
      case NODE_CVASGN:
	expr_type = DEFINED_ASGN;
	break;
    }

    if (expr_type) {
	if (needstr != Qfalse) {
	    VALUE str = rb_iseq_defined_string(expr_type);
	    add_value_load(iseq, ret, nd_line(node),
			   ls - setup_result_var_number(iseq, result, curr_temp_vars_num),
			   str);
	}
	else {
	    add_value_load(iseq, ret, nd_line(node),
			   ls - setup_result_var_number(iseq, result, curr_temp_vars_num),
			   Qtrue);
	}
	return 1;
    }
    return 0;
}

#undef defined_expr

/* Entry function to generate ISEQ insns for defined a expression.  It
   can add a rescue part insn.  See comments for the major function
   above.  */
static int
defined_expr(rb_iseq_t *iseq, LINK_ANCHOR *const ret,
	     NODE *node, LABEL **lfinish, VALUE needstr,
	     int *result, int *curr_temp_vars_num)
{
    LINK_ELEMENT *lcur = ret->last;
    int done = defined_expr0(iseq, ret, node, lfinish, needstr, result, curr_temp_vars_num);
    if (lfinish[1]) {
	int line = nd_line(node);
	LABEL *lstart = NEW_LABEL(line);
	LABEL *lend = NEW_LABEL(line);
	const rb_iseq_t *rescue = NEW_CHILD_ISEQ(NEW_NIL(),
						 rb_str_concat(rb_str_new2
							       ("defined guard in "),
							       iseq->body->location.label),
						 ISEQ_TYPE_DEFINED_GUARD, 0);
	lstart->rescued = LABEL_RESCUE_BEG;
	lend->rescued = LABEL_RESCUE_END;
	lstart->sp = *curr_temp_vars_num;
	lend->sp = *curr_temp_vars_num;
	APPEND_LABEL(ret, lcur, lstart);
	ADD_LABEL(ret, lend);
	ADD_CATCH_ENTRY(CATCH_TYPE_RESCUE, lstart, lend, rescue, lfinish[1]);
    }
    return done;
}

static VALUE
make_name_for_block(const rb_iseq_t *orig_iseq)
{
    int level = 1;
    const rb_iseq_t *iseq = orig_iseq;

    if (orig_iseq->body->parent_iseq != 0) {
	while (orig_iseq->body->local_iseq != iseq) {
	    if (iseq->body->type == ISEQ_TYPE_BLOCK) {
		level++;
	    }
	    iseq = iseq->body->parent_iseq;
	}
    }

    if (level == 1) {
	return rb_sprintf("block in %"PRIsVALUE, iseq->body->location.label);
    }
    else {
	return rb_sprintf("block (%d levels) in %"PRIsVALUE, level, iseq->body->location.label);
    }
}

static void
push_ensure_entry(rb_iseq_t *iseq,
		  struct iseq_compile_data_ensure_node_stack *enl,
		  struct ensure_range *er, NODE *node)
{
    enl->ensure_node = node;
    enl->prev = ISEQ_COMPILE_DATA(iseq)->ensure_node_stack;	/* prev */
    enl->erange = er;
    ISEQ_COMPILE_DATA(iseq)->ensure_node_stack = enl;
}

static void
add_ensure_range(rb_iseq_t *iseq, struct ensure_range *erange,
		 LABEL *lstart, LABEL *lend)
{
    struct ensure_range *ne =
	compile_data_alloc(iseq, sizeof(struct ensure_range));

    while (erange->next != 0) {
	erange = erange->next;
    }
    ne->next = 0;
    ne->begin = lend;
    ne->end = erange->end;
    erange->end = lstart;

    erange->next = ne;
}

/* Generate insns of ISEQ ensure part.  Put them into RET.  Update the
   number of reserved temporaries in CURR_TEMP_VARS_NUM and result
   location index in RESULT.  True IS_RETURN means it is an ensure
   part of the method.  */
static void
add_ensure_iseq(LINK_ANCHOR *const ret, rb_iseq_t *iseq, int is_return,
		int *result, int *curr_temp_vars_num)
{
    struct iseq_compile_data_ensure_node_stack *enlp =
	ISEQ_COMPILE_DATA(iseq)->ensure_node_stack;
    struct iseq_compile_data_ensure_node_stack *prev_enlp = enlp;
    DECL_ANCHOR(ensure);

    INIT_ANCHOR(ensure);
    while (enlp) {
	if (enlp->erange != 0) {
	    int temp_vars_num = *curr_temp_vars_num;
	    DECL_ANCHOR(ensure_part);
	    LABEL *lstart = NEW_LABEL(0);
	    LABEL *lend = NEW_LABEL(0);
	    INIT_ANCHOR(ensure_part);

	    lstart->sp = temp_vars_num;
	    lend->sp = temp_vars_num;

	    add_ensure_range(iseq, enlp->erange, lstart, lend);

	    ISEQ_COMPILE_DATA(iseq)->ensure_node_stack = enlp->prev;
	    ADD_LABEL(ensure_part, lstart);
	    COMPILE_POPPED(ensure_part, "ensure part", enlp->ensure_node, result, &temp_vars_num);
	    ADD_LABEL(ensure_part, lend);
	    ADD_SEQ(ensure, ensure_part);
	}
	else {
	    if (!is_return) {
		break;
	    }
	}
	enlp = enlp->prev;
    }
    ISEQ_COMPILE_DATA(iseq)->ensure_node_stack = prev_enlp;
    ADD_SEQ(ret, ensure);
}

/* Generate ISEQ insns for passing arguments ARGN and KEYWORDS.
   Update FLAG and number of reserved temporaries in
   CURR_TEMP_VARS_NUM.  Update argument start location index in
   RESULT.  Return argc as a fixnum.  */
static VALUE
setup_args(rb_iseq_t *iseq, LINK_ANCHOR *const args, NODE *argn, int dup_p,
	   unsigned int *flag, struct rb_call_info_kw_arg **keywords,
	   int *result, int *curr_temp_vars_num)
{
    VALUE argc = INT2FIX(0);
    int ls = iseq->body->local_table_size;

    if (argn) {
	switch (nd_type(argn)) {
	case NODE_BLOCK_PASS: {
	    int rest_res = stack_result;

	    /* Arguments are never asked to be assigned to a local variable.  */
	    assert(result == NULL || *result == anywhere_result || *result == stack_result);
	    argc = setup_args(iseq, args, argn->nd_head, FALSE, flag,
			      keywords, &rest_res, curr_temp_vars_num);
	    COMPILE(args, "block", argn->nd_body, result, curr_temp_vars_num);
	    *flag |= VM_CALL_ARGS_BLOCKARG;
	    break;
	}
	case NODE_SPLAT: {
	    int op_res = anywhere_result;
	    int arg_res, temp_vars_num = *curr_temp_vars_num;

	    assert(result == NULL || *result == anywhere_result || *result == stack_result);
	    COMPILE(args, "args (splat)", argn->nd_head, &op_res, &temp_vars_num);
	    arg_res = setup_result_var_number(iseq, result, curr_temp_vars_num);
	    ADD_INSN3(args, nd_line(argn), splat_array,
		      INT2LINT(ls - arg_res), INT2LINT(ls - op_res),
		      dup_p ? Qtrue : Qfalse);
	    argc = INT2FIX(1);
	    *flag |= VM_CALL_ARGS_SPLAT;
	    break;
	}
	case NODE_ARGSCAT:
	case NODE_ARGSPUSH: {
	    int arg_res, op_res = anywhere_result, rest_res = stack_result;
	    int temp_vars_num;

	    assert(result == NULL || *result == anywhere_result || *result == stack_result);
	    argc = setup_args(iseq, args, argn->nd_head, TRUE, flag, keywords, &rest_res, curr_temp_vars_num);
	    temp_vars_num = *curr_temp_vars_num;
	    COMPILE(args, "args (cat: splat)", argn->nd_body, &op_res, &temp_vars_num);
	    arg_res = setup_result_var_number(iseq, result, curr_temp_vars_num);
	    if (nd_type(argn) == NODE_ARGSCAT)
		ADD_INSN3(args, nd_line(argn), splat_array, INT2LINT(ls - arg_res),
			  INT2LINT(ls - op_res),
			  dup_p ? Qtrue : Qfalse);
	    else
		ADD_INSN3(args, nd_line(argn), make_array, INT2LINT(ls - arg_res),
			  INT2LINT(ls - op_res), INT2FIX(1));
	    if (nd_type(argn->nd_head) == NODE_SPLAT
		|| nd_type(argn->nd_head) == NODE_ARGSCAT
		|| nd_type(argn->nd_head) == NODE_ARGSPUSH)
		{
		    ADD_INSN3(args, nd_line(argn), concat_array,
			      INT2LINT(ls - rest_res), INT2LINT(ls - rest_res), INT2LINT(ls - arg_res));

		    if (result != NULL)
			*result = rest_res;
		    *curr_temp_vars_num = rest_res - ls;
		}
	    else
		argc = INT2FIX(FIX2INT(argc) + 1);
	    *flag |= VM_CALL_ARGS_SPLAT;

	    break;
	}
	case NODE_ARRAY: {
	    argc = INT2FIX(compile_array_(iseq, args, argn, COMPILE_ARRAY_TYPE_ARGS, keywords, FALSE,
					  result, curr_temp_vars_num));
	    break;
	}
	default: {
	    UNKNOWN_NODE("setup_arg", argn);
	}
	}
    }
    return argc;
}

static int
has_keyword_args_p(const NODE *argn) {
    const NODE *node;

    if (nd_type(argn) != NODE_ARRAY)
	return FALSE;
    for (node = argn; node->nd_next; node = node->nd_next)
	;
    /* node is the last element here.  */
    node = node->nd_head;
    if (node == NULL || nd_type(node) != NODE_HASH)
	return FALSE;
    node = node->nd_head;
    if (node == NULL || nd_type(node) != NODE_ARRAY)
	return FALSE;
    while (node) {
	NODE *key_node = node->nd_head;

	assert(nd_type(node) == NODE_ARRAY);
	if (key_node && nd_type(key_node) != NODE_LIT
	    || ! RB_TYPE_P(key_node->nd_lit, T_SYMBOL))
	    return FALSE;
	node = node->nd_next; /* skip value node */
	node = node->nd_next;
    }
    return TRUE;
}


static VALUE
build_postexe_iseq(rb_iseq_t *iseq, LINK_ANCHOR *const ret, NODE *body)
{
    int line = nd_line(body);
    const rb_iseq_t *block = NEW_CHILD_ISEQ(body, make_name_for_block(iseq->body->parent_iseq), ISEQ_TYPE_BLOCK, line);

    iseq_set_local_table(iseq, 0);
    ADD_RTL_VMCORE_SEND(ret, line, id_core_set_postexe, block, iseq->body->local_table_size + 1);
    return Qnil;
}

/* Generate ISEQ insns for capture assigns given by args NODE of a
   regexp match.  Use number of reserved temporaries in
   CURR_TEMP_VARS_NUM.  */
static void
compile_named_capture_assign(rb_iseq_t *iseq, LINK_ANCHOR *const ret, NODE *node,
			     int *curr_temp_vars_num)
{
    NODE *vars;
    LINK_ELEMENT *last;
    int line = nd_line(node);
    LABEL *fail_label = NEW_LABEL(line), *end_label = NEW_LABEL(line);
    int ls = iseq->body->local_table_size;
    int global_res = setup_result_var_number(iseq, NULL, curr_temp_vars_num);

    end_label->sp = fail_label->sp = *curr_temp_vars_num;
#if !(defined(NAMED_CAPTURE_BY_SVAR) && NAMED_CAPTURE_BY_SVAR-0)
    ADD_INSN2(ret, line, global2var, INT2LINT(ls - global_res),
	      ((VALUE)rb_global_entry(idBACKREF) | 1));
#else
    ADD_INSN3(ret, line, special2var, INT2LINT(ls - global_res),
	      INT2FIX(1) /* '~' */, INT2FIX(0));
#endif
    ADD_RTL_INSNL(ret, line, bf, fail_label, INT2LINT(ls - global_res));

    for (vars = node; vars; vars = vars->nd_next) {
	int temp_vars_num = *curr_temp_vars_num;
	int head_res, cap_res;

	last = ret->last;
	COMPILE_POPPED(ret, "capture", vars->nd_head, NULL, &temp_vars_num);
	last = last->next; /* val2loc res, :var */
	assert(((INSN*)last)->insn_id == BIN(val2temp)
	       || ((INSN*)last)->insn_id == BIN(val2loc));
	cap_res = LINT2INT(((INSN*)last)->operands[0]);
	temp_vars_num = *curr_temp_vars_num;
	head_res = setup_result_var_number(iseq, NULL, &temp_vars_num);
	((INSN*)last)->operands[0] = INT2LINT(ls - head_res);
	if (((INSN*)last)->insn_id == BIN(val2loc))
	    ((INSN*)last)->insn_id = BIN(val2temp);
	ADD_INSN5(ret, line, ind, BIN(cont_op2),
		  (VALUE) new_calldata(iseq, idAREF, 1, ls - head_res, 0, NULL, FALSE),
		  INT2LINT(cap_res), INT2LINT(ls - global_res), INT2LINT(ls - head_res));
    }
    ADD_RTL_GOTO(ret, line, end_label);
    ADD_LABEL(ret, fail_label);
    for (vars = node; vars; vars = vars->nd_next) {
	int temp_vars_num = *curr_temp_vars_num;

	last = ret->last;
	COMPILE_POPPED(ret, "capture", vars->nd_head, NULL, &temp_vars_num);
	last = last->next; /* val2loc res, :var */
	assert(((INSN*)last)->insn_id == BIN(val2temp)
	       || ((INSN*)last)->insn_id == BIN(val2loc));
	((INSN*)last)->operands[1] = Qnil;
    }
    ADD_LABEL(ret, end_label);
}

/* Return an optimized insn id for call of method MID with ARG_NUM
   number of arguments.  */
static enum ruby_vminsn_type
get_opt_id(ID mid, int arg_num)
{
    if (arg_num == 0)
	switch (mid) {
	  case idLength: return BIN(length);
	  case idSize: return BIN(size);
	  case idEmptyP: return BIN(empty_p);
	  case idSucc: return BIN(succ);
	  case idNot: return BIN(not);
	  default: return BIN(nop);
	}
    if (arg_num == 1)
	switch (mid) {
	  case idPLUS: return BIN(plus);
	  case idMINUS: return BIN(minus);
	  case idMULT: return BIN(mult);
	  case idEq: return BIN(eq);
	  case idNeq: return BIN(ne);
	  case idLT: return BIN(lt);
	  case idGT: return BIN(gt);
	  case idLE: return BIN(le);
	  case idGE: return BIN(ge);
	  case idDIV: return BIN(div);
	  case idMOD: return BIN(mod);
	  case idLTLT: return BIN(ltlt);
	  case idAREF: return BIN(ind);
	  case idASET:
	  default: return BIN(nop);
	}
    return BIN(nop);
}

/* Return a variant of insn INSN_ID with an immediate operand (fixnum if
   FIXNUM_P or flonum otherwise).  Return NOP if there is no such
   insn.  */
static enum ruby_vminsn_type
make_imm_id(enum ruby_vminsn_type insn_id, int fixnum_p)
{
    switch (insn_id) {
      case BIN(plus): return fixnum_p ? BIN(plusi) : BIN(plusf);
      case BIN(minus): return fixnum_p ? BIN(minusi) : BIN(minusf);
      case BIN(mult): return fixnum_p ? BIN(multi) : BIN(multf);
      case BIN(eq): return fixnum_p ? BIN(eqi) : BIN(eqf);
      case BIN(ne): return fixnum_p ? BIN(nei) : BIN(nef);
      case BIN(lt): return fixnum_p ? BIN(lti) : BIN(ltf);
      case BIN(gt): return fixnum_p ? BIN(gti) : BIN(gtf);
      case BIN(le): return fixnum_p ? BIN(lei) : BIN(lef);
      case BIN(ge): return fixnum_p ? BIN(gei) : BIN(gef);
      case BIN(div): return fixnum_p ? BIN(divi) : BIN(divf);
      case BIN(mod): return fixnum_p ? BIN(modi) : BIN(modf);
      case BIN(ltlt): return fixnum_p ? BIN(ltlti) : BIN(nop);
      case BIN(ind): return fixnum_p ? BIN(indi) : BIN(inds);
      default: return BIN(nop);
    }
}

/* Generate insns for operation ID with ARGC, PARENT_BLOCK, FLAG, and
   KEYWORDS whose operands at CALL_START on the stack (of
   temporaries).  If OPT_P, generate an optimized insn with 2 operands
   using places RECV (receiver) and OP (operand) in this case ignoring
   CALL_START.  The operation result will be passed through RESULT.
   Update number of reserved temporaries through
   CURR_TEMP_VARS_NUM.  */
static void
gen_op2(rb_iseq_t *iseq, int line, LINK_ANCHOR *ret, int opt_p, ID id,
	VALUE argc, int self_p, int call_start, const rb_iseq_t *parent_block,
	unsigned int flag, struct rb_call_info_kw_arg *keywords,
	int *result, int *curr_temp_vars_num, int recv, int op)
{
    enum ruby_vminsn_type imm_id, new_id = get_opt_id(id, 1);
    VALUE imm_val;
    int imm_p, remove_p;
    int ls = iseq->body->local_table_size;

    imm_val = Qfalse;
    remove_p = imm_p = FALSE;
    if (opt_p && ret->last
	&& IS_INSN(ret->last)
	&& ((INSN *) ret->last)->insn_id == BIN(loc2temp)
	&& LINT2INT(((INSN *) ret->last)->operands[0]) == ls - op) {
	remove_p = TRUE;
	op = ls - LINT2INT(((INSN *) ret->last)->operands[1]);
    } else if (opt_p && ret->last
	       && IS_INSN(ret->last)
	       && ((((INSN *) ret->last)->insn_id == BIN(val2loc)
		    || ((INSN *) ret->last)->insn_id == BIN(val2temp))
		   && (FIXNUM_P(((INSN *) ret->last)->operands[1])
		       || (new_id != BIN(ind)
			   && FLONUM_P(((INSN *) ret->last)->operands[1])))
		   || (new_id == BIN(ind)
		       && ((INSN *) ret->last)->insn_id == BIN(str2var)))
	       && LINT2INT(((INSN *) ret->last)->operands[0]) == ls - op) {
	imm_id = make_imm_id(new_id, FIXNUM_P(((INSN *) ret->last)->operands[1]));
	if (imm_id != BIN(nop)) {
	    imm_val = ((INSN *) ret->last)->operands[1];
	    new_id = imm_id;
	    remove_p = imm_p = TRUE;
	}
    }

    /* optimized insns: */
    if (opt_p) {
	int res = setup_result_var_number(iseq, result, curr_temp_vars_num);

	assert(new_id != BIN(nop));
	ADD_ELEM(ret,
		 (LINK_ELEMENT *)
		 new_insn_body
		 (iseq, line, new_id, 5, BIN(cont_op2),
		  (VALUE) new_calldata(iseq, id, FIX2INT(argc), -1 - *curr_temp_vars_num, 0, NULL, FALSE),
		  INT2LINT(ls - res), INT2LINT(ls - recv),
		  imm_p ? imm_val : (unsigned long long) INT2LINT(ls - op)));
	if (remove_p)
	    REMOVE_ELEM(ret->last->prev);
    }
    else {
	if (recv == anywhere_result){
	    ADD_RTL_SEND_R(ret, line, id, argc, parent_block, self_p, call_start,
			   INT2FIX(flag), keywords);
	}
	else {
	    assert(!self_p);
	    ADD_RTL_SEND_RECV(ret, line, id, argc, parent_block, recv, call_start, INT2FIX(flag), keywords);
	}
	if (result != NULL) {
	    *result = call_start;
	    /* Reserve slot for result  */
	    get_temp_stack_slot(iseq, curr_temp_vars_num);
	}
    }
}

/* Generate ISEQ insns with LINE to finish `ary[i] OP= op_value`
   compilation.  Ary is in location with index ARY_RESULT, I is in
   IND_RESULT, and op_value is in OP_RESULT.  For true OPT_P use aset
   insn and if CHECK_P is true, move op_value to the result.
   Otherwise, generate a call of idAset (with ARGC, FLAG, and SELF_P)
   and putting arguments on the stack (start location index is in
   CALL_START).  BOFF is a flag of block presence in the call.  Put
   the generated insns into RET.  Update the number of reserved
   temporaries in CURR_TEMP_VARS_NUM and result location index in
   RESULT.  */
static void
finish_op1_assign(rb_iseq_t *iseq, int line, LINK_ANCHOR *ret, int opt_p, int check_p,
		  int *result, int op_result, int ary_result, int ind_result,
		  int *curr_temp_vars_num,
		  unsigned int flag, int boff, int call_start, VALUE argc, int self_p)
{
    int ls = iseq->body->local_table_size;

    if (opt_p) {
	if (check_p) {
	    add_local_move(iseq, ret, line, ls - *result, ls - op_result);
	}
	ADD_INSN4(ret, line, indset,
		  new_calldata(iseq, idASET, 2, -1 - *curr_temp_vars_num, 0, NULL, FALSE),
		  INT2LINT(ls - ary_result), INT2LINT(ls - ind_result),  INT2LINT(ls - op_result));
	if (result != NULL)
	    *result = op_result;
    }
    else if (flag & VM_CALL_ARGS_SPLAT) {
	add_local_move(iseq, ret, line, ls - call_start + 1, ls - op_result);
	if (result != NULL)
	    *result = call_start - 1;
	ADD_INSN3(ret, line, make_array,
		  INT2LINT(ls - op_result), INT2LINT(ls - op_result),
		  INT2FIX(1));
	ADD_INSN3(ret, line, concat_array,
		  INT2LINT(ls - ind_result), INT2LINT(ls - ind_result), INT2LINT(ls - op_result));
	ADD_RTL_SEND_R(ret, line, idASET, argc,
		       NULL, self_p, call_start, INT2FIX(flag), NULL);
    } else {
	add_local_move(iseq, ret, line, ls - call_start + 1, ls - op_result);
	if (result != NULL)
	    *result = call_start - 1;
	if (boff > 0)
	    /* Put the block right after the call arguments by
	       swapping with the second copy of op value.  */
	    ADD_INSN2(ret, line, var_swap,
		      INT2LINT(ls - call_start - FIX2INT(argc) - 1),
		      INT2LINT(ls - call_start - FIX2INT(argc) - 2));
	ADD_RTL_SEND_R(ret, line, idASET, FIXNUM_INC(argc, 1),
		       NULL, self_p, call_start, INT2FIX(flag), NULL);
    }
}

/* The function is used to generate a result on joint point of CFG
   (e.g. in case of result for if-then-else).  Generate ISEQ insns for
   location given by RESULT to have the same value as locations
   RESULT1 or RESULT2.  Put the insns generated to have RESULT1 into
   LAST1 (use LINE1 for such insns) and RESULT2 to RET (use LINE2).
   Reserve a temporary, if it is necessary, and update RESULT (the
   preferable and final location) and CURR_TEMP_VARS_NUM (a number of
   temporary variables reserved before and after the call). */
static void
joint_result(rb_iseq_t *iseq, LINK_ANCHOR *ret, int line1, int line2,
	     int result1, LINK_ELEMENT *last1, int result2, int *result, int *curr_temp_vars_num)
{
    int res;
    int ls = iseq->body->local_table_size;
    int und1_p, und2_p;

    assert(result != NULL);

    und1_p = result1 == stack_result || result1 == anywhere_result;
    und2_p = result2 == stack_result || result2 == anywhere_result;
    if (*result == stack_result) {
	if (result1 == *curr_temp_vars_num + ls)
	    res = result1;
	else if (result2 == *curr_temp_vars_num + ls)
	    res = result2;
	else
	    res = setup_result_var_number(iseq, result, curr_temp_vars_num);
    }
    else if (result1 == *result && ! und1_p)
	res = result1;
    else if (! und1_p && result1 > ls) /* result1 is stack temporary */
	res = result1;
    else if (! und2_p && result2 > ls) /* result2 is stack temporary */
	res = result2;
    else
	res = setup_result_var_number(iseq, result, curr_temp_vars_num);
    if (res != result1)
	INSERT_ELEM_AFTER
	    (ret, last1,
	     und1_p
	     ? (LINK_ELEMENT *) new_value_load(iseq, line1, INT2LINT(ls - res), Qnil)
	     : (LINK_ELEMENT *) new_local_move(iseq, line1, ls - res, ls - result1));
    if (res != result2) {
	if (und2_p)
	    add_value_load(iseq, ret, line2, ls - res, Qnil);
	else
	    add_local_move(iseq, ret, line2, ls - res, ls - result2);
    }
    if (result != NULL) {
	*result = res;
	if (res - ls == *curr_temp_vars_num + 1)
	    /* Reserve result stack slot */
	    increment_temps_var(iseq, curr_temp_vars_num, 1);
    }
}

/* Make location given by RESULT to have the same value as location
   with index VAL_RESULT.  Generate a move insn with LINE for ISEQ, if
   it is necessary, add it to RET.  Reserve a temporary, if it is
   necessary, and update RESULT (the preferable and final location)
   and CURR_TEMP_VARS_NUM (a number of temporary variables reserved
   before and after the call).  */
static void
update_result(int *result, rb_iseq_t *iseq, LINK_ANCHOR *ret, int line, int val_result,
	      int *curr_temp_vars_num) {
    if (result != NULL) {
	int ls = iseq->body->local_table_size;
	if (*result == stack_result && ls >= val_result) {
	    int res = get_temp_stack_slot(iseq, curr_temp_vars_num);

	    add_local_move(iseq, ret, line, ls - res, ls - val_result);
	    val_result = res;
	}
	*result = val_result;
    }
}

/* Increase the reference count of a label given by VAL.  The function
   is passed to rb_hash_foreach to process hash table elements
   corresponding to when clauses in a case-stmt.  */
static int
cdhash_increase_label_refcnt(VALUE key, VALUE val, void *ptr)
{
    LABEL *lobj = (LABEL *)(val & ~1);
    lobj->refcnt++;
    return ST_CONTINUE;
}

static int
number_literal_p(NODE *n)
{
    return (n && nd_type(n) == NODE_LIT && RB_INTEGER_TYPE_P(n->nd_lit));
}

/* Compile each NODE for ISEQ passing the result through RET.
   Non-zero POPPED means the result will be not used.  Inout argument
   RESULT is used to pass the preferable and final location of the
   result.  Inout argument CURR_TEMP_VARS_NUM is used to pass a number
   of temporary variables reserved before and after the call.  Return
   the compilation status.  */
static int
iseq_compile_each(rb_iseq_t *iseq, LINK_ANCHOR *const ret, NODE * node, int popped,
		  int *result, int *curr_temp_vars_num)
{
    enum node_type type;
    LINK_ELEMENT *saved_last_element = 0;
    int line;
    int ls = iseq->body->local_table_size;

    if (node == 0) {
	debugs("node: NODE_NIL(implicit)\n");
	if (!popped)
	  add_value_load(iseq, ret, ISEQ_COMPILE_DATA(iseq)->last_line,
			 (int) iseq->body->local_table_size - setup_result_var_number(iseq, result, curr_temp_vars_num),
			 Qnil);
	return COMPILE_OK;
    }

    line = (int)nd_line(node);

    if (ISEQ_COMPILE_DATA(iseq)->last_line == line) {
	/* ignore */
    }
    else {
	if (node->flags & NODE_FL_NEWLINE) {
	    ISEQ_COMPILE_DATA(iseq)->last_line = line;
	    ADD_TRACE(ret, line, RUBY_EVENT_LINE);
	    saved_last_element = ret->last;
	}
    }

    debug_node_start(node);
#undef BEFORE_RETURN
#define BEFORE_RETURN debug_node_end()

    type = nd_type(node);

    switch (type) {
    case NODE_BLOCK:{
	int node_result, temp_vars_num;

	while (node && nd_type(node) == NODE_BLOCK) {
	    node_result = anywhere_result;
	    temp_vars_num = *curr_temp_vars_num;
	    CHECK(COMPILE_(ret, "BLOCK body", node->nd_head,
			   (node->nd_next ? 1 : popped),
			   (node->nd_next == 0
			    || (node->nd_next->nd_head != 0
				&& nd_type(node->nd_next->nd_head) == NODE_RESCUE)
			    ? &node_result : NULL),
			   &temp_vars_num));
	    node = node->nd_next;
	}
	if (node) {
	    node_result = anywhere_result;
	    temp_vars_num = 0;
	    CHECK(COMPILE_(ret, "BLOCK next", node->nd_next, popped, &node_result, &temp_vars_num));
	}
	if (result != NULL && node_result != anywhere_result) {
	    /* The result will be used.  */
	    if (node_result == *curr_temp_vars_num + ls + 1) {
		/* The result is on the stack reserve it.  */
		increment_temps_var(iseq, curr_temp_vars_num, 1);
		*result = node_result;
	    }
	    else if (*result == stack_result) {
		if (node_result == anywhere_result)
		    assert(((INSN *)ret->last)->insn_id == BIN(temp_ret)
			   || ((INSN *)ret->last)->insn_id == BIN(loc_ret)
			   || ((INSN *)ret->last)->insn_id == BIN(val_ret)
			   || ((INSN *)ret->last)->insn_id == BIN(raise_except)
			   || ((INSN *)ret->last)->insn_id == BIN(goto));
		else
		    add_local_move(iseq, ret, line,
				   ls - setup_result_var_number(iseq, result, curr_temp_vars_num),
				   ls - node_result);
	    } else
		*result = node_result;
	}
	break;
    }
    case NODE_IF:{
	DECL_ANCHOR(cond_seq);
	DECL_ANCHOR(then_seq);
	DECL_ANCHOR(else_seq);
	LABEL *then_label, *else_label, *end_label;
	int then_result = anywhere_result, else_result = anywhere_result;
	int cond_op = anywhere_result, temp_vars_num = *curr_temp_vars_num;
	LINK_ELEMENT *last;
	int line1, line2;

	INIT_ANCHOR(cond_seq);
	INIT_ANCHOR(then_seq);
	INIT_ANCHOR(else_seq);
	then_label = NEW_LABEL(line);
	else_label = NEW_LABEL(line);
	end_label = NEW_LABEL(line);

	compile_branch_condition(iseq, cond_seq, node->nd_cond,
				 then_label, else_label, &cond_op, &temp_vars_num);
	then_label->sp = else_label->sp = temp_vars_num;

	if (result != NULL) {
	    /* Set up preferences from result  */
	    then_result = *result;
	    else_result = then_result;
	}
	temp_vars_num = *curr_temp_vars_num;
	/* If we don't care about place or storing if-node result is, we should not care
	   about place or storing of if- or else- body result.  */
	CHECK(COMPILE_(then_seq, "then", node->nd_body, popped, result == NULL ? NULL : &then_result, &temp_vars_num));
	line1 = ISEQ_COMPILE_DATA(iseq)->last_line;
	temp_vars_num = *curr_temp_vars_num;
	CHECK(COMPILE_(else_seq, "else", node->nd_else, popped, result == NULL ? NULL : &else_result, &temp_vars_num));
	line2 = ISEQ_COMPILE_DATA(iseq)->last_line;

	ADD_SEQ(ret, cond_seq);

	ADD_LABEL(ret, then_label);
	ADD_SEQ(ret, then_seq);
	last = ret->last;
	ADD_RTL_GOTO(ret, line, end_label);

	ADD_LABEL(ret, else_label);
	ADD_SEQ(ret, else_seq);
	if (result != NULL)
	    joint_result(iseq, ret, line1, line2, then_result, last, else_result, result, curr_temp_vars_num);

	ADD_LABEL(ret, end_label);
	end_label->sp = *curr_temp_vars_num;

	break;
    }
    case NODE_CASE:{
	NODE *vals;
	NODE *tempnode = node;
	LABEL *endlabel, *elselabel;
	DECL_ANCHOR(head);
	DECL_ANCHOR(body_seq);
	DECL_ANCHOR(cond_seq);
	int only_special_literals = 1;
	VALUE literals = rb_hash_new();
	int case_op = stack_result; /* as it may change */
	int temp_vars_num, case_result;
	int start_vars_num = *curr_temp_vars_num;
	INIT_ANCHOR(head);
	INIT_ANCHOR(body_seq);
	INIT_ANCHOR(cond_seq);

	rb_hash_tbl_raw(literals)->type = &cdhash_type;

	if (node->nd_head == 0) {
	    CHECK(COMPILE_(ret, "when", node->nd_body, popped, result, curr_temp_vars_num));
	    break;
	}
	temp_vars_num = *curr_temp_vars_num;
	case_result = setup_result_var_number(iseq, result, curr_temp_vars_num);
	CHECK(COMPILE(head, "case base", node->nd_head, &case_op, &temp_vars_num));
	node = node->nd_body;
	type = nd_type(node);
	line = nd_line(node);

	if (type != NODE_WHEN) {
	    COMPILE_ERROR(ERROR_ARGS "NODE_CASE: unexpected node. must be NODE_WHEN, but %s", ruby_node_name(type));
	ng:
	    debug_node_end();
	    return COMPILE_NG;
	}

	endlabel = NEW_LABEL(line);
	elselabel = NEW_LABEL(line);
	endlabel->sp = elselabel->sp = temp_vars_num;

	ADD_SEQ(ret, head);	/* case VAL */

	while (type == NODE_WHEN) {
	    LABEL *l1;
	    int body_op = anywhere_result, when_op = anywhere_result;
	    int when_temp_vars_num = temp_vars_num;

	    l1 = NEW_LABEL(line);
	    ADD_LABEL(body_seq, l1);
	    l1->sp = temp_vars_num;

	    /* If we have a return BODY_OP can be the same as before
	       the call.  */
	    CHECK(COMPILE_(body_seq, "when body", node->nd_body, popped, &body_op, &when_temp_vars_num));
	    if (! popped && body_op != anywhere_result && case_result != body_op) {
		/* Make sure about the same place for result if we
		   have a result.  A return can make result
		   unavailable.  */
		add_local_move(iseq, body_seq, line, ls - case_result, ls - body_op);
	    }
	    ADD_RTL_GOTO(body_seq, line, endlabel);

	    vals = node->nd_head;
	    if (vals) {
		switch (nd_type(vals)) {
		case NODE_ARRAY:{
		    when_temp_vars_num = temp_vars_num;
		    only_special_literals = when_vals(iseq, cond_seq, vals, l1, only_special_literals, literals,
						      case_op, &when_temp_vars_num);
		    break;
		}
		case NODE_SPLAT:
		case NODE_ARGSCAT:
		case NODE_ARGSPUSH: {
		    int temp_res = stack_result;
		    int res;
		    when_temp_vars_num = temp_vars_num;
		    res = setup_result_var_number(iseq, &temp_res, &when_temp_vars_num);
		    only_special_literals = 0;
		    CHECK(COMPILE(cond_seq, "when/cond splat", vals, &when_op, &when_temp_vars_num));
		    {

			ADD_INSN4(cond_seq, nd_line(vals), check_match,
				  INT2LINT(ls - res), INT2LINT(ls - case_op), INT2LINT(ls - when_op),
				  INT2FIX(VM_CHECKMATCH_TYPE_CASE | VM_CHECKMATCH_ARRAY));
			ADD_RTL_INSNL(cond_seq, nd_line(vals), bt, l1, INT2LINT(ls - res));
		    }
		    break;
		}
		default:
		    UNKNOWN_NODE("NODE_CASE", vals);
		}
	    }
	    else {
		EXPECT_NODE_NONULL("NODE_CASE", node, NODE_ARRAY);
	    }

	    node = node->nd_next;
	    if (!node) {
		break;
	    }
	    type = nd_type(node);
	    line = nd_line(node);
	}
	/* else */
	if (node) {
	    int body_op = anywhere_result;

	    ADD_LABEL(cond_seq, elselabel);
	    temp_vars_num = start_vars_num;
	    CHECK(COMPILE_(cond_seq, "else", node, popped, &body_op, &temp_vars_num));
	    /* Break inside makes body_op unchanged */
	    if (!popped && body_op != anywhere_result && case_result != body_op) { /* Make sure about the same place for result */
		add_local_move(iseq, cond_seq, line, ls - case_result, ls - body_op);
	    }
	    ADD_RTL_GOTO(cond_seq, line, endlabel);
	}
	else {
	    debugs("== else (implicit)\n");
	    ADD_LABEL(cond_seq, elselabel);
	    add_value_load(iseq, cond_seq, nd_line(tempnode), ls - case_result, Qnil);
	    ADD_RTL_GOTO(cond_seq, nd_line(tempnode), endlabel);
	}

	if (only_special_literals) {
	    iseq_add_mark_object(iseq, literals);

	    ADD_INSN3(ret, nd_line(tempnode), case_dispatch,
		      INT2LINT(ls - case_op), literals, elselabel);
	    rb_hash_foreach(literals, cdhash_increase_label_refcnt, (VALUE)NULL);
	    LABEL_REF(elselabel);
	}

	ADD_SEQ(ret, cond_seq);
	ADD_SEQ(ret, body_seq);
	ADD_LABEL(ret, endlabel);
	break;
    }
    case NODE_WHEN:{
	NODE *vals;
	NODE *val;
	NODE *orig_node = node;
	LABEL *endlabel;
	int temp_vars_num, body_op, start_vars_num = *curr_temp_vars_num;
	int case_result = setup_result_var_number(iseq, result, curr_temp_vars_num);
	DECL_ANCHOR(body_seq);

	INIT_ANCHOR(body_seq);
	endlabel = NEW_LABEL(line);
	endlabel->sp = *curr_temp_vars_num;

	while (node && nd_type(node) == NODE_WHEN) {
	    LABEL *l1 = NEW_LABEL(line = nd_line(node));
	    int when_result, target_op, pattern_op;

	    ADD_LABEL(body_seq, l1);
	    l1->sp = start_vars_num;
	    body_op = anywhere_result;
	    temp_vars_num = start_vars_num;
	    CHECK(COMPILE_(body_seq, "when", node->nd_body, popped, &body_op, &temp_vars_num));
	    if (!popped && case_result != body_op) { /* Make sure about the same place for result */
		add_local_move(iseq, body_seq, line, ls - case_result, ls - body_op);
	    }
	    ADD_RTL_GOTO(body_seq, line, endlabel);

	    vals = node->nd_head;
	    if (!vals) {
		compile_bug(ERROR_ARGS "NODE_WHEN: must be NODE_ARRAY, but 0");
	    }
	    switch (nd_type(vals)) {
	    case NODE_ARRAY:
		while (vals) {
		    val = vals->nd_head;
		    temp_vars_num = start_vars_num;
		    when_result = anywhere_result;
		    CHECK(COMPILE(ret, "when2", val, &when_result, &temp_vars_num));
		    ADD_RTL_INSNL(ret, nd_line(val), bt, l1, INT2LINT(ls - when_result));
		    vals = vals->nd_next;
		}
		break;
	    case NODE_SPLAT:
	    case NODE_ARGSCAT:
	    case NODE_ARGSPUSH:
		{
		    int temp_res = stack_result;
		    temp_vars_num = start_vars_num;
		    target_op = setup_result_var_number(iseq, &temp_res, &temp_vars_num);
		}
		add_value_load(iseq, ret, nd_line(vals), ls - target_op, Qnil);
		pattern_op = anywhere_result;
		CHECK(COMPILE(ret, "when2/cond splat", vals, &pattern_op, &temp_vars_num));
		{
		    int temp_res = stack_result;
		    int res = setup_result_var_number(iseq, &temp_res, &temp_vars_num);

		    ADD_INSN4(ret, nd_line(vals), check_match,
			      INT2LINT(ls - res), INT2LINT(ls - target_op), INT2LINT(ls - pattern_op),
			      INT2FIX(VM_CHECKMATCH_TYPE_WHEN | VM_CHECKMATCH_ARRAY));
		    ADD_RTL_INSNL(ret, nd_line(vals), bt, l1, INT2LINT(ls - res));
		}
		break;
	    default:
		UNKNOWN_NODE("NODE_WHEN", vals);
	    }
	    node = node->nd_next;
	}
	/* else */
	body_op = anywhere_result;
	temp_vars_num = start_vars_num;
	CHECK(COMPILE_(ret, "else", node, popped, &body_op, &temp_vars_num));
	if (!popped && case_result != body_op) { /* Make sure about the same place for result */
	    add_local_move(iseq, ret, nd_line(orig_node), ls - case_result, ls - body_op);
	}
	ADD_RTL_GOTO(ret, nd_line(orig_node), endlabel);

	ADD_SEQ(ret, body_seq);
	ADD_LABEL(ret, endlabel);

	break;
    }
    case NODE_OPT_N:
    case NODE_WHILE:
    case NODE_UNTIL:{
	LABEL *prev_start_label = ISEQ_COMPILE_DATA(iseq)->start_label;
	LABEL *prev_end_label = ISEQ_COMPILE_DATA(iseq)->end_label;
	LABEL *prev_redo_label = ISEQ_COMPILE_DATA(iseq)->redo_label;
	int prev_loopval_popped = ISEQ_COMPILE_DATA(iseq)->loopval_popped;
	int temp_vars_num = *curr_temp_vars_num;
	int body_res = anywhere_result;
	struct iseq_compile_data_ensure_node_stack enl;

	LABEL *next_label = ISEQ_COMPILE_DATA(iseq)->start_label = NEW_LABEL(line);	/* next  */
	LABEL *redo_label = ISEQ_COMPILE_DATA(iseq)->redo_label = NEW_LABEL(line);	/* redo  */
	LABEL *break_label = ISEQ_COMPILE_DATA(iseq)->end_label = NEW_LABEL(line);	/* break */
	LABEL *end_label = NEW_LABEL(line);

	LABEL *next_catch_label = NEW_LABEL(line);
	LABEL *tmp_label = NULL;

	next_label->sp = redo_label->sp = break_label->sp = end_label->sp = next_catch_label->sp = temp_vars_num;

	ISEQ_COMPILE_DATA(iseq)->loopval_popped = 0;
	push_ensure_entry(iseq, &enl, 0, 0);

	if (type == NODE_OPT_N || node->nd_state == 1) {
	  ADD_RTL_GOTO(ret, line, next_label);
	}
	else {
	    tmp_label = NEW_LABEL(line);
	    tmp_label->sp = temp_vars_num;
	    ADD_RTL_GOTO(ret, line, tmp_label);
	}
	ADD_LABEL(ret, next_catch_label);
	ADD_RTL_GOTO(ret, line, next_label);
	if (tmp_label) ADD_LABEL(ret, tmp_label);

	ADD_LABEL(ret, redo_label);
	CHECK(COMPILE(ret, "while body", node->nd_body, &body_res, &temp_vars_num));
	temp_vars_num = *curr_temp_vars_num;

	ADD_LABEL(ret, next_label);	/* next */

	if (type == NODE_WHILE) {
	    compile_branch_condition(iseq, ret, node->nd_cond,
				     redo_label, end_label, NULL, &temp_vars_num);
	}
	else if (type == NODE_UNTIL) {
	    /* until */
	    compile_branch_condition(iseq, ret, node->nd_cond,
				     end_label, redo_label, NULL, &temp_vars_num);
	} else {
	    int call_start = ls + *curr_temp_vars_num + 1;
	    int temp_res = stack_result;

	    setup_result_var_number(iseq, &temp_res, &temp_vars_num);
	    ADD_RTL_SEND_R(ret, line, idGets, INT2FIX(0), NULL, TRUE, call_start,
			   (VALUE)INT2FIX(VM_CALL_FCALL), NULL);
	    ADD_RTL_INSNL(ret, line, bt, redo_label, INT2LINT(ls - call_start));
	}

	ADD_LABEL(ret, end_label);

	if (node->nd_state == Qundef) {
	    compile_bug(ERROR_ARGS "unsupported: putundef");
	}

	if (!popped) {
	    add_value_load(iseq, ret, line,
			   ls - setup_result_var_number(iseq, result, curr_temp_vars_num), Qnil);
	}

	ADD_LABEL(ret, break_label);	/* break */

	ADD_CATCH_ENTRY(CATCH_TYPE_BREAK, redo_label, break_label,
			0, break_label);
	ADD_CATCH_ENTRY(CATCH_TYPE_NEXT, redo_label, break_label, 0,
			next_catch_label);
	ADD_CATCH_ENTRY(CATCH_TYPE_REDO, redo_label, break_label, 0,
			ISEQ_COMPILE_DATA(iseq)->redo_label);

	ISEQ_COMPILE_DATA(iseq)->start_label = prev_start_label;
	ISEQ_COMPILE_DATA(iseq)->end_label = prev_end_label;
	ISEQ_COMPILE_DATA(iseq)->redo_label = prev_redo_label;
	ISEQ_COMPILE_DATA(iseq)->loopval_popped = prev_loopval_popped;
	ISEQ_COMPILE_DATA(iseq)->ensure_node_stack = ISEQ_COMPILE_DATA(iseq)->ensure_node_stack->prev;
	break;
    }
    case NODE_FOR:
	if (node->nd_var) {
	    /* massign to var in "for"
	     * args.length == 1 && Array === (tmp = args[0]) ? tmp : args
	     */
	    NODE *var = node->nd_var;
	    LABEL *not_single = NEW_LABEL(nd_line(var));
	    LABEL *not_ary = NEW_LABEL(nd_line(var));
	    int res = setup_result_var_number(iseq, result, curr_temp_vars_num);
	    int temp_vars_num = *curr_temp_vars_num;
	    int var_op = anywhere_result;

	    CHECK(COMPILE(ret, "for var", var, &var_op, &temp_vars_num));
	    {
		int temp_op, temp_op2, temp_op3, temp_res = stack_result;
		temp_op = setup_result_var_number(iseq, &temp_res, &temp_vars_num);
		not_single->sp = temp_vars_num;
		temp_res = stack_result;
		temp_op2 = setup_result_var_number(iseq, &temp_res, &temp_vars_num);
		not_ary->sp = temp_vars_num;
		temp_res = stack_result;
		temp_op3 = setup_result_var_number(iseq, &temp_res, &temp_vars_num);

		if (res != var_op) {
		    add_local_move(iseq, ret, line, ls - res, ls - var_op);
		}
		ADD_INSN4(ret, line, length, BIN(cont_op1),
			  (VALUE) new_calldata(iseq, idLength, 0, ls - temp_op, 0, NULL, FALSE),
			  INT2LINT(ls - temp_op), INT2LINT(ls - res));
		ADD_INSN5(ret, line, eqi, BIN(cont_op2),
			  (VALUE) new_calldata(iseq, idEq, 1, ls - temp_op - 1, 0, NULL, FALSE),
			  INT2LINT(ls - temp_op), INT2LINT(ls - temp_op), INT2FIX(1));
		ADD_RTL_INSNL(ret, line, bf, not_single, INT2LINT(ls - temp_op));
		add_value_load(iseq, ret, line, ls - temp_op2, INT2FIX(0));
		ADD_INSN5(ret, line, ind, BIN(cont_op2),
			  (VALUE) new_calldata(iseq, idAREF, 1, ls - temp_op2 - 1, 0, NULL, FALSE),
			  INT2LINT(ls - temp_op), INT2LINT(ls - res), INT2LINT(ls - temp_op2));
		add_value_load(iseq, ret, line, ls - temp_op2, rb_cArray);
		add_local_move(iseq, ret, line, ls - temp_op3, ls - temp_op);
		ADD_RTL_SEND_R(ret, line, idEqq, INT2FIX(1), NULL, FALSE, temp_op2,
			       (VALUE)INT2FIX(VM_CALL_FCALL), NULL);
		ADD_RTL_INSNL(ret, line, bf, not_ary, INT2LINT(ls - temp_op2));
		add_local_move(iseq, ret, line, ls - res, ls - temp_op);
		ADD_LABEL(ret, not_ary);
	    }
	    ADD_LABEL(ret, not_single);
	    break;
	}
    case NODE_ITER:{
	const rb_iseq_t *prevblock = ISEQ_COMPILE_DATA(iseq)->current_block;
	LABEL *retry_label = NEW_LABEL(line);
	LABEL *retry_end_l = NEW_LABEL(line);

	ADD_LABEL(ret, retry_label);
	retry_label->sp = *curr_temp_vars_num;
	if (nd_type(node) == NODE_FOR) {
	    int caller_op = stack_result;

	    CHECK(COMPILE(ret, "iter caller (for)", node->nd_iter, &caller_op, curr_temp_vars_num));

	    ISEQ_COMPILE_DATA(iseq)->current_block = NEW_CHILD_ISEQ(node->nd_body, make_name_for_block(iseq),
								    ISEQ_TYPE_BLOCK, line);
	    ADD_RTL_SEND_R(ret, line, idEach, INT2FIX(0), ISEQ_COMPILE_DATA(iseq)->current_block,
			   FALSE, caller_op, (VALUE) INT2FIX(0), NULL);
	    if (result != NULL)
		*result = caller_op;
	}
	else {
	    ISEQ_COMPILE_DATA(iseq)->current_block = NEW_CHILD_ISEQ(node->nd_body, make_name_for_block(iseq),
								    ISEQ_TYPE_BLOCK, line);
	    CHECK(COMPILE(ret, "iter caller", node->nd_iter, result, curr_temp_vars_num));
	}
	ADD_LABEL(ret, retry_end_l);
	retry_end_l->sp = retry_label->sp;

	ISEQ_COMPILE_DATA(iseq)->current_block = prevblock;

	ADD_CATCH_ENTRY(CATCH_TYPE_BREAK, retry_label, retry_end_l, 0, retry_end_l);

	break;
    }
    case NODE_BREAK:{
	unsigned long level = 0;

	if (ISEQ_COMPILE_DATA(iseq)->redo_label != 0) {
	    /* while/until */
	    int temp_vars_num;
	    LABEL *splabel = NEW_LABEL(0);
	    ADD_LABEL(ret, splabel);
	    splabel->sp = *curr_temp_vars_num;
	    CHECK(COMPILE_(ret, "break val (while/until)", node->nd_stts,
			   ISEQ_COMPILE_DATA(iseq)->loopval_popped,
			   result, curr_temp_vars_num));
	    temp_vars_num = *curr_temp_vars_num;
	    add_ensure_iseq(ret, iseq, 0, NULL, &temp_vars_num);
	    ADD_RTL_GOTO(ret, line, ISEQ_COMPILE_DATA(iseq)->end_label);
	}
	else if (iseq->body->type == ISEQ_TYPE_BLOCK) {
	    int op_result, temp_vars_num;

	    if (iseq->body->parent_iseq != NULL)
		iseq->body->parent_iseq->body->break_next_redo_raise_p = TRUE;
	break_by_insn:
	    temp_vars_num = *curr_temp_vars_num;
	    op_result = anywhere_result;
	    /* escape from block */
	    CHECK(COMPILE(ret, "break val (block)", node->nd_stts, &op_result, &temp_vars_num));
	    ADD_INSN2(ret, line, raise_except, INT2LINT(ls - op_result),
		      INT2FIX(level | TAG_BREAK));
	}
	else if (iseq->body->type == ISEQ_TYPE_EVAL) {
	break_in_eval:
	    COMPILE_ERROR(ERROR_ARGS "Can't escape from eval with break");
	    goto ng;
	}
	else {
	    const rb_iseq_t *ip = iseq->body->parent_iseq;

	    while (ip) {
		if (!ISEQ_COMPILE_DATA(ip)) {
		    ip = 0;
		    break;
		}

		level++;
		if (ISEQ_COMPILE_DATA(ip)->redo_label != 0) {
		    level = VM_THROW_NO_ESCAPE_FLAG;
		    ip->body->break_next_redo_raise_p = TRUE;
		    goto break_by_insn;
		}
		else if (ip->body->type == ISEQ_TYPE_BLOCK) {
		    level <<= VM_THROW_LEVEL_SHIFT;
		    ip->body->break_next_redo_raise_p = TRUE;
		    goto break_by_insn;
		}
		else if (ip->body->type == ISEQ_TYPE_EVAL) {
		    goto break_in_eval;
		}

		ip = ip->body->parent_iseq;
	    }
	    COMPILE_ERROR(ERROR_ARGS "Invalid break");
	    goto ng;
	}
	break;
    }
    case NODE_NEXT:{
	unsigned long level = 0;
	int temp_vars_num = *curr_temp_vars_num;

	if (ISEQ_COMPILE_DATA(iseq)->redo_label != 0) {
	    LABEL *splabel = NEW_LABEL(0);
	    splabel->sp = *curr_temp_vars_num;
	    debugs("next in while loop\n");
	    ADD_LABEL(ret, splabel);
	    CHECK(COMPILE(ret, "next val/valid syntax?", node->nd_stts, NULL, &temp_vars_num));
	    temp_vars_num = *curr_temp_vars_num;
	    add_ensure_iseq(ret, iseq, 0, NULL, &temp_vars_num);
	    ADD_RTL_GOTO(ret, line, ISEQ_COMPILE_DATA(iseq)->start_label);
	}
	else if (ISEQ_COMPILE_DATA(iseq)->end_label) {
	    int next_res = anywhere_result;
	    LABEL *splabel = NEW_LABEL(0);
	    splabel->sp = *curr_temp_vars_num;
	    debugs("next in block\n");
	    ADD_LABEL(ret, splabel);
	    CHECK(COMPILE(ret, "next val", node->nd_stts, &next_res, &temp_vars_num));
	    add_ensure_iseq(ret, iseq, 0, NULL, &temp_vars_num);
	    if (next_res > ls)
		ADD_INSN2(ret, line, temp_ret,
			  INT2LINT(ls - next_res), INT2FIX(RUBY_EVENT_B_RETURN));
	    else
		ADD_INSN2(ret, line, loc_ret,
			  INT2LINT(ls - next_res), INT2FIX(RUBY_EVENT_B_RETURN));
	}
	else if (iseq->body->type == ISEQ_TYPE_EVAL) {
	next_in_eval:
	    COMPILE_ERROR(ERROR_ARGS "Can't escape from eval with next");
	    goto ng;
	}
	else {
	    const rb_iseq_t *ip = iseq;

	    while (ip) {
		if (!ISEQ_COMPILE_DATA(ip)) {
		    ip = 0;
		    break;
		}

		level = VM_THROW_NO_ESCAPE_FLAG;
		if (ISEQ_COMPILE_DATA(ip)->redo_label != 0) {
		    /* while loop */
		    break;
		}
		else if (ip->body->type == ISEQ_TYPE_BLOCK) {
		    break;
		}
		else if (ip->body->type == ISEQ_TYPE_EVAL) {
		    goto next_in_eval;
		}

		ip = ip->body->parent_iseq;
	    }
	    if (ip != 0) {
		int op_result = anywhere_result;

		CHECK(COMPILE(ret, "next val", node->nd_stts, &op_result, &temp_vars_num));
		ADD_INSN2(ret, line, raise_except,
			  INT2LINT(ls - op_result),
			  INT2FIX(level | TAG_NEXT));
		ip->body->break_next_redo_raise_p = TRUE;
	    }
	    else {
		COMPILE_ERROR(ERROR_ARGS "Invalid next");
		goto ng;
	    }
	}
	break;
    }
    case NODE_REDO:{
	int temp_vars_num = *curr_temp_vars_num;
	if (ISEQ_COMPILE_DATA(iseq)->redo_label) {
	    LABEL *splabel = NEW_LABEL(0);
	    splabel->sp = *curr_temp_vars_num;
	    debugs("redo in while");
	    ADD_LABEL(ret, splabel);
	    add_ensure_iseq(ret, iseq, 0, NULL, &temp_vars_num);
	    ADD_RTL_GOTO(ret, line, ISEQ_COMPILE_DATA(iseq)->redo_label);
	}
	else if (iseq->body->type == ISEQ_TYPE_EVAL) {
	redo_in_eval:
	    COMPILE_ERROR(ERROR_ARGS "Can't escape from eval with redo");
	    goto ng;
	}
	else if (ISEQ_COMPILE_DATA(iseq)->start_label) {
	    LABEL *splabel = NEW_LABEL(0);

	    splabel->sp = *curr_temp_vars_num;
	    debugs("redo in block");
	    ADD_LABEL(ret, splabel);
	    add_ensure_iseq(ret, iseq, 0, NULL, &temp_vars_num);
	    ADD_RTL_GOTO(ret, line, ISEQ_COMPILE_DATA(iseq)->start_label);
	}
	else {
	    const rb_iseq_t *ip = iseq;
	    const unsigned long level = VM_THROW_NO_ESCAPE_FLAG;

	    while (ip) {
		if (!ISEQ_COMPILE_DATA(ip)) {
		    ip = 0;
		    break;
		}

		if (ISEQ_COMPILE_DATA(ip)->redo_label != 0) {
		    break;
		}
		else if (ip->body->type == ISEQ_TYPE_BLOCK) {
		    break;
		}
		else if (ip->body->type == ISEQ_TYPE_EVAL) {
		    goto redo_in_eval;
		}

		ip = ip->body->parent_iseq;
	    }
	    if (ip != 0) {
		ADD_INSN2(ret, line, raise_except_val, Qnil, INT2FIX(level | TAG_REDO));
		ip->body->break_next_redo_raise_p = TRUE;
	    }
	    else {
		COMPILE_ERROR(ERROR_ARGS "Invalid redo");
		goto ng;
	    }
	}
	break;
    }
    case NODE_RETRY:{
	if (iseq->body->type == ISEQ_TYPE_RESCUE) {
	    ADD_INSN2(ret, 0, raise_except_val, Qnil, INT2FIX(TAG_RETRY));
	}
	else {
	    COMPILE_ERROR(ERROR_ARGS "Invalid retry");
	    goto ng;
	}
	break;
    }
    case NODE_BEGIN:{
	CHECK(COMPILE_(ret, "NODE_BEGIN", node->nd_body, popped, result, curr_temp_vars_num));
	break;
    }
    case NODE_RESCUE:{
	int line1, rescue_result, else_result, temp_vars_num;
	LABEL *lstart = NEW_LABEL(line);
	LABEL *lend = NEW_LABEL(line);
	LABEL *lcont = NEW_LABEL(line);
	const rb_iseq_t *rescue;
	int resbody_result = setup_result_var_number(iseq, result, curr_temp_vars_num);

	ISEQ_COMPILE_DATA(iseq)->resbody_result = resbody_result;
	rescue = NEW_CHILD_ISEQ(node->nd_resq,
				rb_str_concat(rb_str_new2("rescue in "), iseq->body->location.label),
				ISEQ_TYPE_RESCUE, line);

	if (result != NULL) {
	    /* Set up preferences from result  */
	    rescue_result = *result;
	    else_result = rescue_result;
	} else {
	    rescue_result = else_result = anywhere_result;
	}

	temp_vars_num = *curr_temp_vars_num;
	lstart->rescued = LABEL_RESCUE_BEG;
	lend->rescued = LABEL_RESCUE_END;
	ADD_LABEL(ret, lstart);
	lstart->sp = temp_vars_num;
	CHECK(COMPILE(ret, "rescue head", node->nd_head, &rescue_result, &temp_vars_num));
	line1 = ISEQ_COMPILE_DATA(iseq)->last_line;
	if (!popped && rescue_result != anywhere_result && rescue_result != resbody_result)
	    add_local_move(iseq, ret, line1, ls - resbody_result, ls - rescue_result);
	ADD_LABEL(ret, lend);
	lend->sp = *curr_temp_vars_num;
	if (node->nd_else) {
	    temp_vars_num = *curr_temp_vars_num;
	    else_result = anywhere_result;
	    CHECK(COMPILE(ret, "rescue else", node->nd_else, &else_result, &temp_vars_num));
	    if (!popped && else_result != resbody_result)
		add_local_move(iseq, ret, line1, ls - resbody_result, ls - else_result);
	}

	ADD_INSN(ret, line, nop);
	ADD_LABEL(ret, lcont);
	lcont->sp = *curr_temp_vars_num;

	/* register catch entry */
	ADD_CATCH_ENTRY(CATCH_TYPE_RESCUE, lstart, lend, rescue, lcont);
	ADD_CATCH_ENTRY(CATCH_TYPE_RETRY, lend, lcont, 0, lstart);
	break;
    }
    case NODE_RESBODY:{
	int pls, res, resbody_result, temp_vars_num;
	NODE *resq = node;
	int one_p = resq && resq->nd_head == NULL;
	NODE *narg;
	LABEL *label_miss, *label_hit;

	while (resq) {
	    label_miss = NEW_LABEL(line);
	    label_hit = NEW_LABEL(line);

	    narg = resq->nd_args;
	    if (narg) {
		switch (nd_type(narg)) {
		case NODE_ARRAY:
		    while (narg) {
			res = stack_result;
			temp_vars_num = *curr_temp_vars_num;
			CHECK(COMPILE(ret, "rescue arg", narg->nd_head, &res, &temp_vars_num));
			ADD_INSN4(ret, line, check_match, INT2LINT(ls - res), INT2LINT(LVAR_ERRINFO + VM_ENV_DATA_SIZE - 1), INT2LINT(ls - res),
				  INT2FIX(VM_CHECKMATCH_TYPE_RESCUE));
			label_hit->sp = temp_vars_num;
			ADD_RTL_INSNL(ret, line, bt, label_hit, INT2LINT(ls - res));
			narg = narg->nd_next;
		    }
		    break;
		case NODE_SPLAT:
		case NODE_ARGSCAT:
		case NODE_ARGSPUSH:
		    res = stack_result;
		    temp_vars_num = *curr_temp_vars_num;
		    CHECK(COMPILE(ret, "rescue/cond splat", narg, &res, &temp_vars_num));
		    ADD_INSN4(ret, line, check_match, INT2LINT(ls - res), INT2LINT(LVAR_ERRINFO + VM_ENV_DATA_SIZE - 1), INT2LINT(ls - res),
			      INT2FIX(VM_CHECKMATCH_TYPE_RESCUE | VM_CHECKMATCH_ARRAY));
		    label_hit->sp = temp_vars_num;
		    ADD_RTL_INSNL(ret, line, bt, label_hit, INT2LINT(ls - res));
		    break;
		default:
		    UNKNOWN_NODE("NODE_RESBODY", narg);
		}
	    }
	    else {
		int temp_res = stack_result;
		temp_vars_num = *curr_temp_vars_num;
		res = setup_result_var_number(iseq, &temp_res, &temp_vars_num);
		add_value_load(iseq, ret, line, ls - res, rb_eStandardError);
		ADD_INSN4(ret, line, check_match, INT2LINT(ls - res), INT2LINT(VM_ENV_DATA_SIZE), INT2LINT(ls - res),
			  INT2FIX(VM_CHECKMATCH_TYPE_RESCUE));
		label_hit->sp = temp_vars_num;
		ADD_RTL_INSNL(ret, line, bt, label_hit, INT2LINT(ls - res));
	    }
	    ADD_RTL_GOTO(ret, line, label_miss);
	    ADD_LABEL(ret, label_hit);
	    res = anywhere_result;
	    temp_vars_num = *curr_temp_vars_num;
	    CHECK(COMPILE(ret, "resbody body", resq->nd_body, &res, &temp_vars_num));
	    if (ISEQ_COMPILE_DATA(iseq)->option->tailcall_optimization) {
		ADD_INSN(ret, line, nop);
	    }
	    assert(iseq->body->parent_iseq != NULL);
	    pls = iseq->body->parent_iseq->body->local_table_size;
	    resbody_result = ISEQ_COMPILE_DATA(iseq->body->parent_iseq)->resbody_result;
	    if (res != anywhere_result)
		add_ret_to(iseq, ret, line, INT2LINT(pls - resbody_result), INT2LINT(ls - res));
	    ADD_LABEL(ret, label_miss);
	    label_miss->sp = *curr_temp_vars_num;
	    resq = resq->nd_head;
	}
	if (one_p && result != NULL && *result == *curr_temp_vars_num + ls + 1)
	    /* The result is on the stack reserve it.  */
	    increment_temps_var(iseq, curr_temp_vars_num, 1);
	break;
    }
    case NODE_ENSURE:{
	DECL_ANCHOR(ensr);
	const rb_iseq_t *ensure = NEW_CHILD_ISEQ(node->nd_ensr,
						 rb_str_concat(rb_str_new2("ensure in "), iseq->body->location.label),
						 ISEQ_TYPE_ENSURE, line);
	LABEL *lstart = NEW_LABEL(line);
	LABEL *lend = NEW_LABEL(line);
	LABEL *lcont = NEW_LABEL(line);
	LINK_ELEMENT *last;
	int last_leave = 0;
	struct ensure_range er;
	struct iseq_compile_data_ensure_node_stack enl;
	struct ensure_range *erange;
	int temp_vars_num = *curr_temp_vars_num + 1;

	INIT_ANCHOR(ensr);
	/* The result of ensure body is ignored  */
	CHECK(COMPILE_POPPED(ensr, "ensure ensr", node->nd_ensr, NULL, &temp_vars_num));
	last = ensr->last;
	last_leave = (last && IS_INSN(last)
		      && (IS_INSN_ID(last, val_ret)
			  || IS_INSN_ID(last, loc_ret)
			  || IS_INSN_ID(last, temp_ret)));
	if (!popped && last_leave)
	    popped = 1;

	er.begin = lstart;
	er.end = lend;
	er.next = 0;
	push_ensure_entry(iseq, &enl, &er, node->nd_ensr);

	ADD_LABEL(ret, lstart);
	lstart->sp = lcont->sp = lend->sp = *curr_temp_vars_num;
	CHECK(COMPILE_(ret, "ensure head", node->nd_head, popped, result, curr_temp_vars_num));
	ADD_LABEL(ret, lend);
	if (ensr->anchor.next == 0) {
	    ADD_INSN(ret, line, nop);
	}
	else {
	    ADD_SEQ(ret, ensr);
	}
	ADD_LABEL(ret, lcont);

	erange = ISEQ_COMPILE_DATA(iseq)->ensure_node_stack->erange;
	if (lstart->link.next != &lend->link) {
	    while (erange) {
		ADD_CATCH_ENTRY(CATCH_TYPE_ENSURE, erange->begin, erange->end,
				ensure, lcont);
		erange = erange->next;
	    }
	}

	ISEQ_COMPILE_DATA(iseq)->ensure_node_stack = enl.prev;
	break;
      }

      case NODE_AND:
      case NODE_OR:{
	LABEL *end_label = NEW_LABEL(line);
	int temp_vars_num = *curr_temp_vars_num;
	/* We can not use result because of cases r = expr || expr using r.  */
	int result1 = anywhere_result, result2 = anywhere_result;
	LINK_ELEMENT *last;
	int line1, line2;

	end_label->sp = temp_vars_num;
	CHECK(COMPILE(ret, "nd_1st", node->nd_1st, &result1, &temp_vars_num));
	last = ret->last;
	line1 = ISEQ_COMPILE_DATA(iseq)->last_line;
	if (type == NODE_AND) {
	    ADD_RTL_INSNL(ret, line, bf, end_label, INT2LINT(ls - result1));
	}
	else {
	    ADD_RTL_INSNL(ret, line, bt, end_label, INT2LINT(ls - result1));
	}
	temp_vars_num = *curr_temp_vars_num;
	CHECK(COMPILE_(ret, "nd_2nd", node->nd_2nd, popped, &result2, &temp_vars_num));
	line2 = ISEQ_COMPILE_DATA(iseq)->last_line;
	if (result != NULL)
	    joint_result(iseq, ret, line1, line2, result1, last, result2, result, curr_temp_vars_num);

	ADD_LABEL(ret, end_label);
	end_label->sp = *curr_temp_vars_num;
	break;
      }

      case NODE_MASGN:{/*???*/
	compile_massign(iseq, ret, node, popped, result, curr_temp_vars_num);
	break;
      }

      case NODE_LASGN:{
	ID id = node->nd_vid;
	int idx = get_local_var_idx(iseq, id);
	int lev = get_lvar_level(iseq);
	int val_result;

	debugs("lvar: %"PRIsVALUE" idx: %d\n", rb_id2str(id), idx);
	val_result = (lev == 0 ? idx : anywhere_result);
	CHECK(COMPILE(ret, "rvalue", node->nd_value, &val_result, curr_temp_vars_num));

	{
	    int idx_ls = iseq->body->local_iseq->body->local_table_size;
	    if (lev != 0) {
		ADD_INSN3(ret, line, var2uploc, INT2LINT(idx_ls - idx), INT2LINT(ls - val_result), INT2FIX(lev));
	    } else if (val_result != idx)
		add_local_move(iseq, ret, line, idx_ls - idx, ls - val_result);
	    update_result(result, iseq, ret, line, val_result, curr_temp_vars_num);
	}
	break;
      }
      case NODE_DASGN:
      case NODE_DASGN_CURR:{
	int idx, lv, idx_ls;
	rb_iseq_t *id_iseq;
	int val_result = anywhere_result;

	idx = get_dyna_var_idx(iseq, node->nd_vid, &lv, &idx_ls, &id_iseq);

	if (idx <= -VM_ENV_DATA_SIZE) {
	    compile_bug(ERROR_ARGS "NODE_DASGN(_CURR): unknown id (%"PRIsVALUE")",
			rb_id2str(node->nd_vid));
	}

	if (lv == 0)
	    val_result = idx;
	CHECK(COMPILE(ret, "dvalue", node->nd_value, &val_result, curr_temp_vars_num));
	debugi("dassn id", rb_id2str(node->nd_vid) ? node->nd_vid : '*');

	if (lv != 0) {
	    ADD_INSN3(ret, line, var2uploc, INT2LINT(idx_ls - idx),
		      INT2LINT(ls - val_result),  INT2FIX(lv));
	}
	else if (val_result != idx)
	    add_local_move(iseq, ret, line, ls - idx, ls - val_result);
	if (!popped)
	    update_result(result, iseq, ret, line, val_result, curr_temp_vars_num);
	break;
      }
      case NODE_GASGN:{
	int val_op = anywhere_result;

	CHECK(COMPILE(ret, "lvalue", node->nd_value, &val_op, curr_temp_vars_num));

	ADD_INSN2(ret, line, var2global, ((VALUE)node->nd_entry | 1), INT2LINT(ls - val_op));
	update_result(result, iseq, ret, line, val_op, curr_temp_vars_num);
	break;
      }
      case NODE_IASGN:
      case NODE_IASGN2:{
	int val_op = anywhere_result;
	INSN *last;

	CHECK(COMPILE(ret, "lvalue", node->nd_value, &val_op, curr_temp_vars_num));
	if (0 && result == NULL && (last = (INSN *) ret->last) != NULL && IS_INSN_ID(last, val2temp)
	    && last->operands[0] == (VALUE) INT2LINT(ls - val_op)) {
	  ADD_INSN3(ret, line, val2ivar,
		    ID2SYM(node->nd_vid), get_ivar_ic_value(iseq, node->nd_vid),
		    last->operands[1]);
	  REMOVE_ELEM(ret->last->prev);
	} else {
	  ADD_INSN3(ret, line, var2ivar,
		    ID2SYM(node->nd_vid), get_ivar_ic_value(iseq, node->nd_vid),
		    INT2LINT(ls - val_op));
	  update_result(result, iseq, ret, line, val_op, curr_temp_vars_num);
	}
	break;
      }
      case NODE_CDECL:{
	int val_result = anywhere_result;
	int obj_result = anywhere_result;
	int temp_vars_num;

	CHECK(COMPILE(ret, "lvalue", node->nd_value, &val_result, curr_temp_vars_num));
	temp_vars_num = *curr_temp_vars_num;

	if (node->nd_vid) {
	    ADD_INSN2(ret, line, specialobj2var,
		      INT2LINT(ls - setup_result_var_number(iseq, &obj_result, &temp_vars_num)),
		      INT2FIX(VM_SPECIAL_OBJECT_CONST_BASE));
	    ADD_INSN3(ret, line, var2const, ID2SYM(node->nd_vid),
		      INT2LINT(ls - val_result), INT2LINT(ls - obj_result));
	    if (result != NULL)
		*result = val_result;
	}
	else {
	    compile_cpath(ret, iseq, node->nd_else, &obj_result, &temp_vars_num);
	    ADD_INSN3(ret, line, var2const, ID2SYM(node->nd_else->nd_mid),
		      INT2LINT(ls - val_result), INT2LINT(ls - obj_result));
	    if (result != NULL)
		*result = val_result;
	}
	break;
      }
      case NODE_CVASGN:{
	int val_op = anywhere_result;

	CHECK(COMPILE(ret, "cvasgn val", node->nd_value, &val_op, curr_temp_vars_num));
	ADD_INSN2(ret, line, var2cvar, ID2SYM(node->nd_vid), INT2LINT(ls - val_op));
	if (result != NULL)
	    *result = val_op;
	break;
      }
      case NODE_OP_ASGN1: {
	DECL_ANCHOR(args);
	VALUE argc;
	unsigned int flag = 0;
	unsigned int asgnflag = 0;
	ID id = node->nd_mid;
	int boff = 0;
	int ary_result = anywhere_result, ind_result = anywhere_result;
	int el_result = anywhere_result, rhs_result = anywhere_result;
	int op_result = anywhere_result;
	int temp_vars_num = *curr_temp_vars_num;
	int call_start, self_p = FALSE;
	int opt_p = ((get_opt_id(id, 1) != BIN(nop) || id == 0 || id == 1)
		     && ! private_recv_p(node)
		     && nd_type(node->nd_args->nd_head) == NODE_ARRAY
		     && node->nd_args->nd_head->nd_alen == 1);

	/*
	 * a[x] (op)= y
	 *
	 * nil       # nil
	 * eval a    # nil a
	 * eval x    # nil a x
	 * dupn 2    # nil a x a x
	 * send :[]  # nil a x a[x]
	 * eval y    # nil a x a[x] y
	 * send op   # nil a x ret
	 * setn 3    # ret a x ret
	 * send []=  # ret ?
	 * pop       # ret
	 *
	 * RTL_INSNS:
	 * a = eval a
	 * x = eval x
	 * el = aref a, x
	 * y = eval y
	 * v = op el, y
	 * aset a, x, v
	 *
	 * or:
	 * set_{}_value      # nil
	 * push eval a       # nil a
	 * push eval x       # nil a x
	 * copy a x          # nil a x a x
	 * send :[]          # nil a x a[x]
	 * push eval y       # nil a x a[x] y
	 * send op           # nil a x ret
	 * loc2temp          # ret a x ret
	 * send []=          # ret ?
	 */

	/*
	 * nd_recv[nd_args->nd_head] (nd_mid)= nd_args->nd_body;
	 * NODE_OP_ASGN nd_recv
	 *              nd_args->nd_head
	 *              nd_args->nd_body
	 *              nd_mid
	 */

	if (opt_p && result != NULL && *result != anywhere_result && *result != stack_result)
	    op_result = *result;
	else {
	    int temp_res = stack_result;
	    /* Reserve the result place.  */
	    op_result = setup_result_var_number(iseq, &temp_res, curr_temp_vars_num);
	    temp_vars_num = *curr_temp_vars_num;
	}
	if (private_recv_p(node)) {
	    int temp_res = stack_result;
	    assert(! opt_p);
	    asgnflag = VM_CALL_FCALL;
	    self_p = TRUE;
	    setup_result_var_number(iseq, &temp_res, &temp_vars_num); /* reserve slot for self_p */
	    if (USE_SELF_LD) {
		ADD_INSN1(ret, line, self2var, INT2LINT(ls - temp_res));
		self_p = FALSE;
	    }
	} else {
	    int temp_res = stack_result;
	    asgnflag = 0;

	    COMPILE(ret, "NODE_OP_ASGN1 recv", node->nd_recv,
		    (opt_p ? &ary_result : &temp_res), &temp_vars_num);
	}
	call_start = ls + temp_vars_num;

	switch (nd_type(node->nd_args->nd_head)) {
	  case NODE_ZARRAY:
	    argc = INT2FIX(0);
	    break;
	  case NODE_BLOCK_PASS:
	    boff = 1;
	    assert(! opt_p);
	  default:
	    {
		INIT_ANCHOR(args);
		if (!opt_p)
		    ind_result = stack_result;
		argc = setup_args(iseq, args, node->nd_args->nd_head, FALSE,
				  &flag, NULL, &ind_result, &temp_vars_num);
		assert(! opt_p || FIX2INT(argc) == 1);
		ADD_SEQ(ret, args);
	    }
	}
	if (opt_p) {
	    setup_result_var_number(iseq, &el_result, &temp_vars_num);
	    ADD_INSN5(ret, line, ind, BIN(cont_op2),
		      (VALUE) new_calldata(iseq, idAREF, FIX2INT(argc), -1 - temp_vars_num, flag, NULL, FALSE),
		      INT2LINT(ls - el_result),
		      INT2LINT(ls - ary_result), INT2LINT(ls - ind_result));
	} else {
	    int temp_res = stack_result;
	    int aref_start = call_start + FIX2INT(argc) + 1 + boff;

	    if (FIX2INT(argc) + (self_p ? 0 : 1) + boff == 1)
	      ADD_INSN2(ret, line, temp2temp, INT2LINT(ls - aref_start - (self_p ? 1 : 0)), INT2LINT(ls - call_start - (self_p ? 1 : 0)));
	    else
	      ADD_INSN3(ret, line, var2var, INT2LINT(ls - aref_start - (self_p ? 1 : 0)), INT2LINT(ls - call_start - (self_p ? 1 : 0)),
			FIXNUM_INC(argc, (self_p ? 0 : 1) + boff));
	    ADD_RTL_SEND_R(ret, line, idAREF, argc, NULL, self_p,
			   call_start + FIX2INT(argc) + 1 + boff, INT2FIX(flag), NULL);
	    el_result = aref_start;
	    setup_result_var_number(iseq, &temp_res, &temp_vars_num); /* reserve result */
	}

	flag |= asgnflag;
	assert(! opt_p || ! flag);

	if (id == 0 || id == 1) {
	    /* 0: or, 1: and
	       a[x] ||= y

	       unless/if a[x]
	       a[x]= y
	       else
	       nil
	       end


	       RTL_INSNS:
	       a = eval a
	       x = eval x
	       el = aref a, x

	       bt/bf el, label
	       eval y
	       aset a, x, y
	       jmp lfin
	       label:
	       res = el
	       lfin:
	    */
	    LABEL *label = NEW_LABEL(line);
	    LABEL *lfin = NEW_LABEL(line);
	    int check_p;

	    if (id == 0) {
		ADD_RTL_INSNL(ret, line, bt, label, INT2LINT(ls - el_result));
	    } else {
		ADD_RTL_INSNL(ret, line, bf, label, INT2LINT(ls - el_result));
	    }

	    assert(op_result != stack_result);
	    check_p = (result != NULL && op_result != anywhere_result
		       && *result == op_result);
	    CHECK(COMPILE(ret, "NODE_OP_ASGN1 args->body: ", node->nd_args->nd_body,
			  &op_result, &temp_vars_num));

	    add_local_move(iseq, ret, line, ls - call_start - 2, ls - op_result);
	    finish_op1_assign(iseq, line, ret, opt_p, check_p, result, op_result, ary_result, ind_result,
			      &temp_vars_num, flag, boff, call_start, argc, self_p);
	    ADD_RTL_GOTO(ret, line, lfin);
	    ADD_LABEL(ret, label);
	    label->sp = lfin->sp = temp_vars_num;
	    add_local_move(iseq, ret, line, ls - op_result, ls - el_result);
	    if (result != NULL)
		*result = op_result;
	    ADD_LABEL(ret, lfin);
	}
	else {
	    int temp_res = stack_result;
	    CHECK(COMPILE(ret, "NODE_OP_ASGN1 args->body: ", node->nd_args->nd_body,
			  (opt_p ? &rhs_result : &temp_res), &temp_vars_num));
	    {
		int op_call_start = call_start + FIX2INT(argc) + 1 + boff;
		int check_p = (result != NULL && op_result != anywhere_result
			       && *result == op_result);

		assert(op_result != stack_result);
		temp_vars_num = el_result - ls - 1;
		gen_op2(iseq, line, ret, opt_p, id, INT2FIX(1), FALSE, op_call_start,
			ISEQ_COMPILE_DATA(iseq)->current_block, 0,  NULL,
			&op_result, &temp_vars_num, el_result, rhs_result);
		finish_op1_assign(iseq, line, ret, opt_p, check_p, result, op_result, ary_result, ind_result,
				  &temp_vars_num, flag, boff, call_start, argc, self_p);
	    }
	}

	break;
      }
      case NODE_OP_ASGN2:{
	ID atype = node->nd_next->nd_mid;
	ID vid = node->nd_next->nd_vid, aid = rb_id_attrset(vid);
	VALUE asgnflag;
	LABEL *lfin = NEW_LABEL(line);
	LABEL *lcfin = NEW_LABEL(line);
	LABEL *lskip = 0;
	int obj_result = anywhere_result;
	int fld_result = anywhere_result, rhs_result = anywhere_result;
	int op_result = anywhere_result, before_op_result;
	int assgn_temp_vars_num, temp_vars_num = *curr_temp_vars_num;
	int call_start, self_p = FALSE;
	int opt_p = ((get_opt_id(atype, 1) != BIN(nop) || atype == 0 || atype == 1)
		     && ! private_recv_p(node));

	/*
	  class C; attr_accessor :c; end
	  r = C.new
	  r.a &&= v # asgn2

	  eval r    # r
	  dup       # r r
	  eval r.a  # r o

	  # or
	  dup       # r o o
	  if lcfin  # r o
	  pop       # r
	  eval v    # r v
	  swap      # v r
	  topn 1    # v r v
	  send a=   # v ?
	  jump lfin # v ?

	  lcfin:      # r o
	  swap      # o r

	  lfin:       # o ?
	  pop       # o

	  # and
	  dup       # r o o
	  unless lcfin
	  pop       # r
	  eval v    # r v
	  swap      # v r
	  topn 1    # v r v
	  send a=   # v ?
	  jump lfin # v ?

	  # others
	  eval v    # r o v
	  send ??   # r w
	  send a=   # w

	*/

	if (opt_p && result != NULL && *result != anywhere_result && *result != stack_result)
	    op_result = *result;
	else {
	    int temp_res = stack_result;
	    /* Reserve the result stack slot */
	    op_result = setup_result_var_number(iseq, &temp_res, curr_temp_vars_num);
	    temp_vars_num = *curr_temp_vars_num;
	}

	if (private_recv_p(node)) {
	    int temp_res = stack_result;
	    assert(!opt_p);
	    asgnflag = VM_CALL_FCALL;
	    self_p = TRUE;
	    setup_result_var_number(iseq, &temp_res, &temp_vars_num); /* reserve slot for self_p */
	    obj_result = ls + temp_vars_num;
	    if (USE_SELF_LD) {
		ADD_INSN1(ret, line, self2var, INT2LINT(ls - temp_res));
		self_p = FALSE;
	    }
	    call_start = ls + temp_vars_num;
	} else {
	    asgnflag = 0;
	    if (!opt_p)
	      obj_result = stack_result;
	    COMPILE(ret, "NODE_OP_ASGN2#recv", node->nd_recv,
		    &obj_result, &temp_vars_num);
	    if (node->nd_next->nd_aid) {
		lskip = NEW_LABEL(line);
		lskip->sp = temp_vars_num;
		ADD_RTL_INSNL(ret, line, bnil, lskip, INT2LINT(ls - obj_result));
	    }
	    call_start = ls + temp_vars_num + (obj_result > ls ? 0 : 1);
	}

	if (!self_p && obj_result != call_start) {
	    add_local_move(iseq, ret, line, ls - call_start, ls - obj_result);
	    increment_temps_var(iseq, &temp_vars_num, 1); /* reserve stack */
	}
	assgn_temp_vars_num = temp_vars_num;
	if (! self_p) {
	    add_local_move(iseq, ret, line, ls - call_start - 1, ls - call_start);
	    increment_temps_var(iseq, &temp_vars_num, 1); /* reserve stack for copy.  */
	}
	fld_result = call_start + (self_p ? 0 : 1);
	ADD_RTL_SEND_R(ret, line, vid,
		       INT2FIX(0), NULL, self_p, fld_result,
		       (VALUE) INT2FIX(0), NULL);

	if (atype == 0 || atype == 1) {	/* 0: OR or 1: AND */
	    int val_result;

	    if (atype == 0) {
		ADD_RTL_INSNL(ret, line, bt, lcfin, INT2LINT(ls - fld_result));
	    } else {
		ADD_RTL_INSNL(ret, line, bf, lcfin, INT2LINT(ls - fld_result));
	    }
	    val_result = before_op_result = op_result;
	    CHECK(COMPILE(ret, "NODE_OP_ASGN2 val", node->nd_value, &val_result, &temp_vars_num));
	    add_local_move(iseq, ret, line, ls - op_result, ls - val_result);

	    if (val_result != before_op_result)
		add_local_move(iseq, ret, line, ls - before_op_result, ls - op_result);
	    if (val_result != call_start + 1)
		add_local_move(iseq, ret, line, ls - call_start - 1, ls - op_result);
	    ADD_RTL_SEND_R(ret, line, aid, INT2FIX(1),
			   NULL, self_p, call_start, INT2FIX(asgnflag), NULL);

	    lfin->sp = lcfin->sp = call_start - ls;
	    ADD_RTL_GOTO(ret, line, lfin);
	    ADD_LABEL(ret, lcfin);

	    /* We need remove temporary on the stack. */
	    add_local_move(iseq, ret, line, ls - op_result, ls - fld_result);
	    if (result != NULL)
		*result = op_result;
	    ADD_LABEL(ret, lfin);
	    if (lskip) {
		ADD_LABEL(ret, lskip);
	    }
	}
	else {
	    int op2_result = op_result;

	    if (!opt_p)
		rhs_result = stack_result;
	    CHECK(COMPILE(ret, "NODE_OP_ASGN2 val", node->nd_value,
			  &rhs_result, &temp_vars_num));
	    assert(op_result != stack_result);
	    gen_op2(iseq, line, ret, opt_p, atype, INT2FIX(1), FALSE, fld_result,
		    ISEQ_COMPILE_DATA(iseq)->current_block, 0,  NULL,
		    &op2_result, &assgn_temp_vars_num, fld_result, rhs_result);
	    if (op_result != op2_result)
		add_local_move(iseq, ret, line, ls - op_result, ls - op2_result);
	    if (result != NULL) {
		*result = op_result;
	    }
	    if (op_result != call_start + 1)
		add_local_move(iseq, ret, line, ls - call_start - 1, ls - op_result);
	    ADD_RTL_SEND_R(ret, line, aid,
			   INT2FIX(1), NULL, self_p, call_start,
			   (VALUE) INT2FIX(asgnflag), NULL);
	    if (lskip) {
		ADD_LABEL(ret, lskip);
	    }
	}
	break;
      }
      case NODE_OP_CDECL: {
	LABEL *lfin = 0;
	LABEL *lassign = 0;
	ID mid;
	int obj_op, cref_op, val_op;
	/* Result place */
	int res = setup_result_var_number(iseq, result, curr_temp_vars_num);
	int temp_vars_num = *curr_temp_vars_num;

	switch (nd_type(node->nd_head)) {
	  case NODE_COLON3:
	    {
		int temp_res = stack_result;
		cref_op = setup_result_var_number(iseq, &temp_res, &temp_vars_num);
		add_value_load(iseq, ret, line, ls - cref_op, rb_cObject);
	    }
	    break;
	  case NODE_COLON2:
	    cref_op = anywhere_result;
	    CHECK(COMPILE(ret, "NODE_OP_CDECL/colon2#nd_head", node->nd_head->nd_head, &cref_op, &temp_vars_num));
	    break;
	  default:
	    COMPILE_ERROR(ERROR_ARGS "%s: invalid node in NODE_OP_CDECL",
			  ruby_node_name(nd_type(node->nd_head)));
	    goto ng;
	}
	mid = node->nd_head->nd_mid;
	/* cref */
	if (node->nd_aid == 0) {
	    lassign = NEW_LABEL(line);
	    lassign->sp = temp_vars_num;
	    {
		int temp_res = stack_result;
		int saved_temp_vars_num = temp_vars_num;
		int def_res = setup_result_var_number(iseq, &temp_res, &temp_vars_num);

		/* We can reuse def_res slot after this.  */
		temp_vars_num = saved_temp_vars_num;
		ADD_INSN5(ret, line, defined_p, INT2LINT(ls - def_res), INT2LINT(ls - cref_op),
			  INT2FIX(DEFINED_CONST), ID2SYM(mid), Qfalse);
		ADD_RTL_INSNL(ret, line, bf, lassign, INT2LINT(ls - def_res));
	    }
	}

	if (node->nd_aid == 0 || node->nd_aid == 1)
	    obj_op = res;
	else
	    {
		int temp_res = stack_result;
		obj_op = setup_result_var_number(iseq, &temp_res, &temp_vars_num);
	    }

	ADD_INSN3(ret, line, const2var, ID2SYM(mid),
		  INT2LINT(ls - obj_op), INT2LINT(ls - cref_op));

	if (node->nd_aid == 0 || node->nd_aid == 1) {
	    lfin = NEW_LABEL(line);
	    lfin->sp = temp_vars_num;
	    if (node->nd_aid == 0)
		ADD_RTL_INSNL(ret, line, bt, lfin, INT2LINT(ls - obj_op));
	    else
		ADD_RTL_INSNL(ret, line, bf, lfin, INT2LINT(ls - obj_op));

	    if (lassign) ADD_LABEL(ret, lassign);

	    val_op = anywhere_result;
	    CHECK(COMPILE(ret, "NODE_OP_CDECL#nd_value", node->nd_value, &val_op, &temp_vars_num));

	    if (val_op != res) {
		add_local_move(iseq, ret, line, ls - res, ls - val_op);
	    }
	    ADD_INSN3(ret, line, var2const, ID2SYM(mid),
		      INT2LINT(ls - res), INT2LINT(ls - cref_op));
	    ADD_LABEL(ret, lfin);			    /* cref [value] */
	}
	else {
	    val_op = stack_result;

	    /* should be result */
	    CHECK( COMPILE(ret, "NODE_OP_CDECL#nd_value", node->nd_value, &val_op, &temp_vars_num));

	    /* obj_op and result should be on stack */
	    ADD_RTL_SEND_R(ret, line, node->nd_aid, INT2FIX(1), NULL, FALSE,
			   obj_op, (VALUE) INT2FIX(VM_CALL_FCALL), NULL);
	    add_local_move(iseq, ret, line, ls - res, ls - obj_op);
	    ADD_INSN3(ret, line, var2const, ID2SYM(mid),
		      INT2LINT(ls - res), INT2LINT(ls - cref_op));
	}
	break;
      }
      case NODE_OP_ASGN_AND:
      case NODE_OP_ASGN_OR:{
	LABEL *lfin = NEW_LABEL(line);
	LABEL *lassign;
	int temp_vars_num;
	int head_result, val_result;
	LINK_ELEMENT *last;

	if (nd_type(node) == NODE_OP_ASGN_OR) {
	    LABEL *lfinish[2];
	    int head_op = anywhere_result;

	    lfinish[0] = lfin;
	    lfinish[1] = 0;
	    temp_vars_num = *curr_temp_vars_num;
	    defined_expr(iseq, ret, node->nd_head, lfinish, Qfalse, &head_op, &temp_vars_num);
	    lassign = lfinish[1];
	    if (!lassign) {
		lassign = NEW_LABEL(line);
		lassign->sp = *curr_temp_vars_num;
	    }
	    ADD_RTL_INSNL(ret, line, bf, lassign, INT2LINT(ls - head_op));
	}
	else {
	    lassign = NEW_LABEL(line);
	    lassign->sp = *curr_temp_vars_num;
	}

	head_result = val_result = anywhere_result;

	temp_vars_num = *curr_temp_vars_num;
	CHECK(COMPILE(ret, "NODE_OP_ASGN_AND/OR#nd_head", node->nd_head,
		      &head_result, &temp_vars_num));
	last = ret->last;

	if (nd_type(node) == NODE_OP_ASGN_AND) {
	    ADD_RTL_INSNL(ret, line, bf, lfin, INT2LINT(ls - head_result));
	}
	else {
	    ADD_RTL_INSNL(ret, line, bt, lfin,  INT2LINT(ls - head_result));
	}

	ADD_LABEL(ret, lassign);
	temp_vars_num = *curr_temp_vars_num;
	CHECK(COMPILE(ret, "NODE_OP_ASGN_AND/OR#nd_value", node->nd_value,
		      &val_result, &temp_vars_num));

	if (result != NULL)
	    joint_result(iseq, ret, nd_line(node->nd_head), nd_line(node->nd_value),
			 head_result, last, val_result, result, curr_temp_vars_num);

	ADD_LABEL(ret, lfin);
	lfin->sp = *curr_temp_vars_num;

	break;
      }
      case NODE_CALL: {
	enum ruby_vminsn_type new_id;

	if (node->nd_recv && ! private_recv_p(node) && node->nd_args == NULL
	    && ISEQ_COMPILE_DATA(iseq)->current_block == NULL
	    && (new_id = get_opt_id(node->nd_mid, 0)) != BIN(nop)) {
	    int temp_vars_num = *curr_temp_vars_num;
	    int recv_result = anywhere_result;
	    int res = setup_result_var_number(iseq, result, curr_temp_vars_num);

	    COMPILE(ret, "recv", node->nd_recv, &recv_result, &temp_vars_num);
	    ADD_ELEM(ret, (LINK_ELEMENT *)
		     new_insn_body(iseq, line, new_id, 4, BIN(cont_op1),
				   (VALUE) new_calldata(iseq, node->nd_mid, 0, -1 - *curr_temp_vars_num, 0, NULL, FALSE),
				   INT2LINT(ls - res), INT2LINT(ls - recv_result)));
	    break;
	}
	/* optimization shortcut
	 *   "literal".freeze -> opt_str_freeze("literal")
	 */
	if (node->nd_recv && nd_type(node->nd_recv) == NODE_STR &&
	    node->nd_mid == idFreeze && node->nd_args == NULL &&
	    ISEQ_COMPILE_DATA(iseq)->current_block == NULL &&
	    ISEQ_COMPILE_DATA(iseq)->option->specialized_instruction) {
	    VALUE str = rb_fstring(node->nd_recv->nd_lit);

	    iseq_add_mark_object(iseq, str);
	    ADD_INSN2(ret, line, str_freeze_call,
		      INT2LINT(ls - setup_result_var_number(iseq, result, curr_temp_vars_num)),
		      str);
	    break;
	}
	/* optimization shortcut
	 *   obj["literal"] -> opt_aref_with(obj, "literal")
	 */
	if (node->nd_mid == idAREF && !private_recv_p(node) && node->nd_args &&
	    nd_type(node->nd_args) == NODE_ARRAY && node->nd_args->nd_alen == 1 &&
	    nd_type(node->nd_args->nd_head) == NODE_STR &&
	    ISEQ_COMPILE_DATA(iseq)->current_block == NULL &&
	    ISEQ_COMPILE_DATA(iseq)->option->specialized_instruction) {
	    VALUE str = rb_fstring(node->nd_args->nd_head->nd_lit);
	    int temp_vars_num = *curr_temp_vars_num;
	    int recv_result = anywhere_result;
	    int res = setup_result_var_number(iseq, result, curr_temp_vars_num);

	    node->nd_args->nd_head->nd_lit = str;
	    CHECK(COMPILE(ret, "recv", node->nd_recv, &recv_result, &temp_vars_num));
	    ADD_INSN5(ret, line, inds, BIN(cont_op2),
		      (VALUE) new_calldata(iseq, node->nd_mid, 1, -1 - *curr_temp_vars_num, 0, NULL, FALSE),
		      INT2LINT(ls - res), INT2LINT(ls - recv_result), str);
	    break;
	}
      }
      case NODE_QCALL:
      case NODE_FCALL:
      case NODE_VCALL:{		/* VCALL: variable or call */
	/*
	  call:  obj.method(...)
	  fcall: func(...)
	  vcall: func
	*/
	int recv_result = anywhere_result, op_result = anywhere_result;
	int temp_vars_num = *curr_temp_vars_num;
	DECL_ANCHOR(recv);
	DECL_ANCHOR(args);
	LABEL *lskip = 0;
	ID mid = node->nd_mid;
	VALUE argc;
	unsigned int flag = 0;
	struct rb_call_info_kw_arg *keywords = NULL;
	const rb_iseq_t *parent_block = ISEQ_COMPILE_DATA(iseq)->current_block;
	int self_p = FALSE;
	int call_start = ls + temp_vars_num + 1;
	enum ruby_vminsn_type new_id = get_opt_id(mid, 1);
	int simple_p = (type == NODE_CALL
			/* No block */
			&& parent_block == NULL);
	int opt_p = (simple_p && new_id != BIN(nop) && node->nd_args
		     && nd_type(node->nd_args) == NODE_ARRAY
		     && node->nd_args->nd_alen == 1
		     && ! private_recv_p(node)
		     /* Has no kw args:  */
		     && nd_type(node->nd_args->nd_head) != NODE_HASH
		     && !has_keyword_args_p(node->nd_args));
	int recv_p = opt_p || (simple_p && node->nd_args == NULL);

	ISEQ_COMPILE_DATA(iseq)->current_block = NULL;
	INIT_ANCHOR(recv);
	INIT_ANCHOR(args);
	/* receiver */
	if (type == NODE_CALL || type == NODE_QCALL) {
	    if (!recv_p)
	      recv_result = stack_result;
	    CHECK(COMPILE(recv, "recv", node->nd_recv, &recv_result, &temp_vars_num));
	    if (type == NODE_QCALL) {
		lskip = NEW_LABEL(line);
		ADD_RTL_INSNL(recv, line, bnil, lskip, INT2LINT(ls - recv_result));
	    }
	}
	else if (type == NODE_FCALL || type == NODE_VCALL) {
	    int temp_res = stack_result;
	    assert(!opt_p);
	    self_p = TRUE;
	    /* Reserve slot for reciever */
	    setup_result_var_number(iseq, &temp_res, &temp_vars_num);
	    if (USE_SELF_LD) {
		ADD_INSN1(ret, line, self2var, INT2LINT(ls - temp_res));
		self_p = FALSE;
		recv_result = temp_res;
	    }
	}

	/* args */
	if (nd_type(node) != NODE_VCALL) {
	    int temp_res = stack_result;
	    argc = setup_args(iseq, args, node->nd_args, FALSE,
			      &flag, &keywords,
			      (opt_p ? &op_result : &temp_res), &temp_vars_num);
	}
	else {
	    argc = INT2FIX(0);
	}

	ADD_SEQ(ret, recv);
	ADD_SEQ(ret, args);

	debugp_param("call args argc", argc);
	debugp_param("call method", ID2SYM(mid));

	switch (nd_type(node)) {
	  case NODE_VCALL:
	    flag |= VM_CALL_VCALL;
	    /* VCALL is funcall, so fall through */
	  case NODE_FCALL:
	    flag |= VM_CALL_FCALL;
	}

	assert(! opt_p || ! flag);

	gen_op2(iseq, line, ret, opt_p, mid, argc, self_p,
		call_start, parent_block, flag,  keywords, result,
		curr_temp_vars_num, recv_result, op_result);

	if (lskip) {
	    ADD_LABEL(ret, lskip);
	    lskip->sp = *curr_temp_vars_num;
	}

	break;
    }
      case NODE_SUPER:
      case NODE_ZSUPER:{
	DECL_ANCHOR(args);
	int argc;
	unsigned int flag = 0;
	struct rb_call_info_kw_arg *keywords = NULL;
	const rb_iseq_t *parent_block = ISEQ_COMPILE_DATA(iseq)->current_block;
	int temp_vars_num = *curr_temp_vars_num;
	int super_start = ls + *curr_temp_vars_num + 1;

	get_temp_stack_slot(iseq, &temp_vars_num);  /* reserve place for reciever */
	INIT_ANCHOR(args);
	ISEQ_COMPILE_DATA(iseq)->current_block = NULL;
	if (nd_type(node) == NODE_SUPER) {
	    int temp_res = stack_result;
	    VALUE vargc = setup_args(iseq, args, node->nd_args, FALSE, &flag,
				     &keywords, &temp_res, &temp_vars_num);
	    argc = FIX2INT(vargc);
	}
	else {
	    /* NODE_ZSUPER */
	    int i;
	    const rb_iseq_t *liseq = iseq->body->local_iseq;
	    int lvar_level = get_lvar_level(iseq);
	    int rest_start, post_param_start;

	    argc = liseq->body->param.lead_num;

	    /* normal arguments */
	    for (i = 0; i < liseq->body->param.lead_num; i++) {
		int idx = liseq->body->local_table_size - i + VM_ENV_DATA_SIZE - 1;
		int temp_res = stack_result;
		if (lvar_level == 0)
		    add_local_move(iseq, args, line,
				   ls - setup_result_var_number(iseq, &temp_res, &temp_vars_num),
				   idx);
		else {
		    ADD_INSN3(args, line, uploc2temp,
			      INT2LINT(ls - setup_result_var_number(iseq, &temp_res, &temp_vars_num)),
			      INT2LINT(idx), INT2FIX(lvar_level));
		}
	    }

	    if (liseq->body->param.flags.has_opt) {
		/* optional arguments */
		int j;
		for (j = 0; j < liseq->body->param.opt_num; j++) {
		    int idx = liseq->body->local_table_size - (i + j) + VM_ENV_DATA_SIZE - 1;
		    int temp_res = stack_result;
		    int res = ls - setup_result_var_number(iseq, &temp_res, &temp_vars_num);
		    if (lvar_level == 0)
			add_local_move(iseq, args, line, res, idx);
		    else {
			ADD_INSN3(args, line, uploc2temp,
				  INT2LINT(res),
				  INT2LINT(idx), INT2FIX(lvar_level));
		    }
		}
		i += j;
		argc = i;
	    }
	    rest_start = ls + temp_vars_num + 1;
	    if (liseq->body->param.flags.has_rest) {
		/* rest argument */
		int idx = liseq->body->local_table_size - liseq->body->param.rest_start + VM_ENV_DATA_SIZE - 1;
		int temp_res = stack_result;

		setup_result_var_number(iseq, &temp_res, &temp_vars_num);
		if (lvar_level == 0)
		    add_local_move(iseq, args, line, ls - temp_res, idx);
		else {
		    ADD_INSN3(args, line, uploc2temp,
			      INT2LINT(ls - temp_res),
			      INT2LINT(idx), INT2FIX(lvar_level));
		}

		argc = liseq->body->param.rest_start + 1;
		flag |= VM_CALL_ARGS_SPLAT;
	    }
	    post_param_start = ls + temp_vars_num + 1;
	    if (liseq->body->param.flags.has_post) {
		/* post arguments */
		int post_len = liseq->body->param.post_num;
		int post_start = liseq->body->param.post_start;
		int saved_temp_vars_num = temp_vars_num;

		if (liseq->body->param.flags.has_rest) {
		    int j;
		    for (j=0; j<post_len; j++) {
			int idx = liseq->body->local_table_size - (post_start + j) + VM_ENV_DATA_SIZE - 1;
			int temp_res = stack_result;
			if (lvar_level == 0)
			    add_local_move(iseq, args, line,
					   ls - setup_result_var_number(iseq, &temp_res, &temp_vars_num),
					   idx);
			else {
			    ADD_INSN3(args, line, uploc2temp,
				      INT2LINT(ls - setup_result_var_number(iseq, &temp_res, &temp_vars_num)),
				      INT2LINT(idx), INT2FIX(lvar_level));
			}
		    }
		    {
			int temp_res = stack_result;
			int start;

			temp_vars_num = saved_temp_vars_num;
			start = setup_result_var_number(iseq, &temp_res, &temp_vars_num);

			assert(start == post_param_start && post_param_start == rest_start + 1);
			ADD_INSN3(args, line, make_array,
				  INT2LINT(ls - start),
				  INT2LINT(ls - post_param_start),
				  INT2FIX(j));
			ADD_INSN3(args, line, concat_array,
				  INT2LINT(ls - rest_start),
				  INT2LINT(ls - rest_start),
				  INT2LINT(ls - post_param_start));
		    }
		    /* argc is settled at above */
		    temp_vars_num = post_param_start - ls;
		}
		else {
		    int j;
		    for (j=0; j<post_len; j++) {
			int idx = liseq->body->local_table_size - (post_start + j) + VM_ENV_DATA_SIZE - 1;
			int temp_res = stack_result;
			int post_arg_res = setup_result_var_number(iseq, &temp_res, &temp_vars_num);
			if (lvar_level == 0)
			    add_local_move(iseq, args, line, ls - post_arg_res, idx);
			else {
			    ADD_INSN3(args, line, uploc2temp,
				      INT2LINT(ls - post_arg_res),
				      INT2LINT(idx), INT2FIX(lvar_level));
			}
		    }
		    argc = post_len + post_start;
		}
	    }


	    if (liseq->body->param.flags.has_kw) { /* TODO: support keywords */
		int temp_res = stack_result;
		int kw_start = ls - setup_result_var_number(iseq, &temp_res, &temp_vars_num);
		int dup_start;
		argc++;

		ADD_INSN2(args, line, specialobj2var, INT2LINT(kw_start),
			  INT2FIX(VM_SPECIAL_OBJECT_VMCORE));
		temp_res = stack_result;
		dup_start = ls - setup_result_var_number(iseq, &temp_res, &temp_vars_num);
		if (liseq->body->param.flags.has_kwrest) {

		    if (lvar_level == 0)
			add_local_move(iseq, args, line, dup_start,
				       liseq->body->local_table_size - liseq->body->param.keyword->rest_start + VM_ENV_DATA_SIZE - 1);
		    else {
			ADD_INSN3(args, line, uploc2temp,
				  INT2LINT(dup_start),
				  INT2LINT(liseq->body->local_table_size - liseq->body->param.keyword->rest_start + VM_ENV_DATA_SIZE - 1),
				  INT2FIX(lvar_level));
		    }
		    ADD_RTL_SEND_R(args, line, rb_intern("dup"), INT2FIX(0), NULL, FALSE,
				   ls - dup_start, (VALUE) INT2FIX(0), NULL);
		}
		else {
		    ADD_INSN3(args, line, make_hash,
			      INT2LINT(dup_start), INT2LINT(dup_start), /* it does not matter for empty hash  */
			      INT2FIX(0));
		}
		for (i = 0; i < liseq->body->param.keyword->num; ++i) {
		    ID id = liseq->body->param.keyword->table[i];
		    int idx = liseq->body->local_iseq->body->local_table_size - get_local_var_idx(liseq, id);
		    int temp_res = stack_result;
		    add_value_load(iseq, args, line,
				   ls - setup_result_var_number(iseq, &temp_res, &temp_vars_num),
				   ID2SYM(id));
		    temp_res = stack_result;
		    if (lvar_level == 0)
			add_local_move(iseq, args, line,
				       ls - setup_result_var_number(iseq, &temp_res, &temp_vars_num),
				       idx);
		    else {
			ADD_INSN3(args, line, uploc2temp,
				  INT2LINT(ls - setup_result_var_number(iseq, &temp_res, &temp_vars_num)),
				  INT2LINT(idx),
				  INT2FIX(lvar_level));
		    }
		}
		ADD_RTL_SEND_R(args, line, id_core_hash_merge_ptr, INT2FIX(i * 2 + 1), NULL, FALSE,
			       ls - kw_start, (VALUE) INT2FIX(0), NULL);

		temp_vars_num = kw_start - ls;

		if (liseq->body->param.flags.has_rest) {
		    ADD_INSN3(args, line, make_array, INT2LINT(kw_start), INT2LINT(kw_start),
			      INT2FIX(1));
		    assert(ls - rest_start < 0 && ls - rest_start == kw_start + 1);
		    ADD_INSN3(args, line, concat_array, INT2LINT(ls - rest_start),
			      INT2LINT(ls - rest_start), INT2LINT(kw_start));
		    temp_vars_num = rest_start - ls;
		    --argc;
		}
	    } else if (liseq->body->param.flags.has_kwrest) {
		int temp_res = stack_result;
		int dup_start = ls - setup_result_var_number(iseq, &temp_res, &temp_vars_num);

		if (lvar_level == 0)
		    add_local_move(iseq, args, line, dup_start,
				   liseq->body->local_table_size - liseq->body->param.keyword->rest_start + VM_ENV_DATA_SIZE - 1);
		else {
		    ADD_INSN3(args, line, uploc2temp, INT2LINT(dup_start),
			      INT2LINT(liseq->body->local_table_size - liseq->body->param.keyword->rest_start + VM_ENV_DATA_SIZE - 1),
			      INT2FIX(lvar_level));
		}
		ADD_RTL_SEND_R(args, line, rb_intern("dup"), INT2FIX(0),
			       NULL, FALSE, ls - dup_start, (VALUE) INT2FIX(0), NULL);

		if (liseq->body->param.flags.has_rest) {
		    ADD_INSN3(args, line, make_array, INT2LINT(dup_start), INT2LINT(dup_start),
			      INT2FIX(1));
		    assert(dup_start < 0 && dup_start + 1 == ls - rest_start);
		    ADD_INSN3(args, line, concat_array, INT2LINT(rest_start),
			      INT2LINT(rest_start), INT2LINT(dup_start));
		    temp_vars_num = rest_start - ls;
		} else {
		    argc++;
		}
	    }
	}

	ADD_SEQ(ret, args);
	ADD_INSN4(ret, line, call_super,
		  new_calldata(iseq, 0, argc, ls - super_start, flag | VM_CALL_SUPER | VM_CALL_FCALL, keywords, parent_block != NULL),
		  INT2LINT(ls - super_start),
		  parent_block, nd_type(node) == NODE_ZSUPER ? Qfalse : Qtrue);
	if (result != NULL)
	    *result = super_start;
	/* Reserve slot for result  */
	get_temp_stack_slot(iseq, curr_temp_vars_num);
	break;
      }
      case NODE_ARRAY:{
	compile_array_(iseq, ret, node, COMPILE_ARRAY_TYPE_ARRAY, NULL, popped, result, curr_temp_vars_num);
	break;
      }
      case NODE_ZARRAY:{
	ADD_INSN3(ret, line, make_array,
		  INT2LINT(ls - setup_result_var_number(iseq, result, curr_temp_vars_num)),
		  INT2LINT(0), INT2FIX(0));
	break;
      }
      case NODE_VALUES:{
	NODE *n = node;
	int temp_vars_num = *curr_temp_vars_num;
	int call_start = ls + temp_vars_num + 1;

	while (n) {
	    int temp_res = stack_result;
	    CHECK(COMPILE(ret, "values item", n->nd_head, &temp_res, &temp_vars_num));
	    n = n->nd_next;
	}
	ADD_INSN3(ret, line, make_array,
		  INT2LINT(ls - setup_result_var_number(iseq, result, curr_temp_vars_num)),
		  INT2LINT(ls - call_start), INT2FIX(node->nd_alen));
	break;
      }
      case NODE_HASH:{
	DECL_ANCHOR(list);
	int type = node->nd_head ? nd_type(node->nd_head) : NODE_ZARRAY;

	INIT_ANCHOR(list);
	switch (type) {
	  case NODE_ARRAY:
	    compile_array(iseq, list, node->nd_head, COMPILE_ARRAY_TYPE_HASH, result, curr_temp_vars_num);
	    ADD_SEQ(ret, list);
	    break;

	  case NODE_ZARRAY: {
	    ADD_INSN3(ret, line, make_hash,
		      INT2LINT(ls - setup_result_var_number(iseq, result, curr_temp_vars_num)),
		      INT2LINT(0), INT2FIX(0));
	    break;
	  }
	  default:
	    compile_bug(ERROR_ARGS_AT(node->nd_head) "can't make hash with this node: %s",
			ruby_node_name(type));
	}

	break;
    }
      case NODE_RETURN:{
	rb_iseq_t *is = iseq;

	if (is) {
	    enum iseq_type type = is->body->type;
	    const rb_iseq_t *parent_iseq = is->body->parent_iseq;
	    enum iseq_type parent_type = parent_iseq ? parent_iseq->body->type : type;

	    if (type == ISEQ_TYPE_TOP || type == ISEQ_TYPE_MAIN ||
		((type == ISEQ_TYPE_RESCUE || type == ISEQ_TYPE_ENSURE) &&
		 (parent_type == ISEQ_TYPE_TOP || parent_type == ISEQ_TYPE_MAIN))) {
		ADD_INSN2(ret, line, val_ret, Qnil, INT2FIX(RUBY_EVENT_RETURN));
	    }
	    else {
		LABEL *splabel = 0;
		int ret_result = (result == NULL ? anywhere_result : *result);
		int temp_vars_num = *curr_temp_vars_num;

		if (type == ISEQ_TYPE_METHOD) {
		    splabel = NEW_LABEL(0);
		    ADD_LABEL(ret, splabel);
		    splabel->sp = temp_vars_num;
		}

		CHECK(COMPILE(ret, "return nd_stts (return val)", node->nd_stts, &ret_result, &temp_vars_num));

		if (type == ISEQ_TYPE_METHOD) {
		    add_ensure_iseq(ret, iseq, 1, NULL, &temp_vars_num);
		    if (ret_result > ls)
			ADD_INSN2(ret, line, temp_ret, INT2LINT(ls - ret_result), INT2FIX(RUBY_EVENT_RETURN));
		    else
			ADD_INSN2(ret, line, loc_ret, INT2LINT(ls - ret_result), INT2FIX(RUBY_EVENT_RETURN));
		} else {
		    ADD_INSN2(ret, line, raise_except, INT2LINT(ls - ret_result),
			      INT2FIX(TAG_RETURN));
		    if (result != NULL)
			*result = ret_result;
		}
	    }
	}
	break;
      }
      case NODE_YIELD:{
	DECL_ANCHOR(args);
	VALUE argc;
	unsigned int flag = 0;
	struct rb_call_info_kw_arg *keywords = NULL;
	int call_start = ls + *curr_temp_vars_num + 1;
	int temp_vars_num = *curr_temp_vars_num;

	INIT_ANCHOR(args);
	if (iseq->body->type == ISEQ_TYPE_TOP) {
	    COMPILE_ERROR(ERROR_ARGS "Invalid yield");
	    goto ng;
	}

	if (node->nd_head) {
	    int temp_res = stack_result;
	    argc = setup_args(iseq, args, node->nd_head, FALSE, &flag,
			      &keywords, &temp_res, &temp_vars_num);
	}
	else {
	    argc = INT2FIX(0);
	}

	ADD_SEQ(ret, args);
	ADD_INSN2(ret, line, call_block,
		  new_calldata(iseq, 0, FIX2INT(argc), ls - call_start, flag, keywords, FALSE),
		  INT2LINT(ls - call_start));
	if (result != NULL)
	    *result = call_start;
	/* Reserve slot for result  */
	get_temp_stack_slot(iseq, curr_temp_vars_num);
	break;
      }
      case NODE_LVAR:{
	if (!popped) {
	    ID id = node->nd_vid;
	    int idx = get_local_var_idx(iseq, id);

	    debugs("id: %"PRIsVALUE" idx: %d\n", rb_id2str(id), ls - idx);
	    if (get_lvar_level(iseq) == 0) {
		if (result != NULL && *result == stack_result) {
		    add_local_move(iseq, ret, line, ls - setup_result_var_number(iseq, result, curr_temp_vars_num),
				   ls - idx);
		}
		else if (result != NULL)
		    *result = idx;
	    } else {
		int idx_ls = iseq->body->local_iseq->body->local_table_size;

		ADD_INSN3(ret, line, uploc2var,
			  INT2LINT(ls - setup_result_var_number(iseq, result, curr_temp_vars_num)),
			  INT2LINT(idx_ls - idx), INT2FIX(get_lvar_level(iseq)));
	    }

	}
	break;
      }
      case NODE_DVAR:{
	int lv, idx, idx_ls;
	rb_iseq_t *id_iseq;

	debugi("nd_vid", node->nd_vid);
	if (!popped) {
	    idx = get_dyna_var_idx(iseq, node->nd_vid, &lv, &idx_ls, &id_iseq);
	    if (idx <= -VM_ENV_DATA_SIZE) {
		compile_bug(ERROR_ARGS "unknown dvar (%"PRIsVALUE")",
			    rb_id2str(node->nd_vid));
	    }
	    if (lv == 0)
		{
		    if (result != NULL && *result == stack_result) {
			add_local_move(iseq, ret, line,
				       ls - setup_result_var_number(iseq, result, curr_temp_vars_num),
				       ls - idx);
		    }
		    else if (result != NULL)
			*result = idx;
		}
	    else {
		ADD_INSN3(ret, line, uploc2var,
			  INT2LINT(ls - setup_result_var_number(iseq, result, curr_temp_vars_num)),
			  INT2LINT(idx_ls - idx), INT2FIX(lv));
	    }
	}
	break;
      }
      case NODE_GVAR:{
	ADD_INSN2(ret, line, global2var,
		  INT2LINT(ls - setup_result_var_number(iseq, result, curr_temp_vars_num)),
		  ((VALUE)node->nd_entry | 1));
	break;
      }
      case NODE_IVAR:{
	debugi("nd_vid", node->nd_vid);
	ADD_INSN3(ret, line, ivar2var,
		  INT2LINT(ls - setup_result_var_number(iseq, result, curr_temp_vars_num)),
		  ID2SYM(node->nd_vid), get_ivar_ic_value(iseq, node->nd_vid));
	break;
      }
      case NODE_CONST:{

	debugi("nd_vid", node->nd_vid);

	if (ISEQ_COMPILE_DATA(iseq)->option->inline_const_cache) {
	    int ic_index = iseq->body->is_size++;
	    int res = setup_result_var_number(iseq, result, curr_temp_vars_num);

	    ADD_INSN4(ret, line, const_cached_val_ld,
		      INT2LINT(ls - res),
		      Qnil, ID2SYM(node->nd_vid), INT2FIX(ic_index));
	}
	else {
	    int res = setup_result_var_number(iseq, result, curr_temp_vars_num);

	    ADD_INSN3(ret, line, const_ld_val, ID2SYM(node->nd_vid),
		      INT2LINT(ls - res), Qnil);
	}

	break;
      }
      case NODE_CVAR:{
	ADD_INSN2(ret, line, cvar2var,
		  INT2LINT(ls - setup_result_var_number(iseq, result, curr_temp_vars_num)),
		  ID2SYM(node->nd_vid));
	break;
      }
      case NODE_NTH_REF:{
	if (!node->nd_nth) {
	    add_value_load(iseq, ret, line,
			   ls - setup_result_var_number(iseq, result, curr_temp_vars_num),
			   Qnil);
	} else {
	    ADD_INSN3(ret, line, special2var,
		      INT2LINT(ls - setup_result_var_number(iseq, result, curr_temp_vars_num)),
		      INT2FIX(1) /* '~'  */,
		      INT2FIX(node->nd_nth << 1));
	}
	break;
      }
      case NODE_BACK_REF:{
	ADD_INSN3(ret, line, special2var,
		  INT2LINT(ls - setup_result_var_number(iseq, result, curr_temp_vars_num)),
		  INT2FIX(1) /* '~' */, INT2FIX(0x01 | (node->nd_nth << 1)));
	break;
      }
      case NODE_MATCH:
      case NODE_MATCH2:
      case NODE_MATCH3:{
	int regex_op = anywhere_result, str_op = anywhere_result;
	int top = - *curr_temp_vars_num - 1;
	int res = setup_result_var_number(iseq, result, curr_temp_vars_num);
	int temp_vars_num = *curr_temp_vars_num;
	int capture_temp_vars_num = temp_vars_num;

	DECL_ANCHOR(recv);
	DECL_ANCHOR(val);

	INIT_ANCHOR(recv);
	INIT_ANCHOR(val);
	switch (nd_type(node)) {
	  case NODE_MATCH:
	    {
		int temp_res = stack_result;

		str_op = setup_result_var_number(iseq, &temp_res, &temp_vars_num);
		temp_res = stack_result;
		regex_op = setup_result_var_number(iseq, &temp_res, &temp_vars_num);
		add_value_load(iseq, recv, line, ls - str_op, node->nd_lit);
		ADD_INSN3(val, line, special2var, INT2LINT(ls - regex_op),
			  INT2FIX(0), INT2FIX(0));
	    }
	    break;
	  case NODE_MATCH2:
	    CHECK(COMPILE(recv, "receiver", node->nd_recv, &regex_op, &temp_vars_num));
	    CHECK(COMPILE(val, "value", node->nd_value, &str_op, &temp_vars_num));
	    break;
	  case NODE_MATCH3:
	    CHECK(COMPILE(recv, "receiver", node->nd_value, &str_op, &temp_vars_num));
	    CHECK(COMPILE(val, "value", node->nd_recv, &regex_op, &temp_vars_num));
	    break;
	}

	if (val->last == val->anchor.next
	    && (INSN_OF(recv->last) == BIN(val2loc)
		|| INSN_OF(recv->last) == BIN(val2temp))
	    && nd_type(node) == NODE_MATCH2) {
	    ADD_SEQ(ret, val);
	    ADD_INSN3(ret, line, regexp_match1,
		      INT2LINT(ls - res), OPERAND_AT(recv->last, 1), INT2LINT(ls - str_op));
	}
	else {
	    ADD_SEQ(ret, recv);
	    ADD_SEQ(ret, val);
	    ADD_INSN5(ret, line, regexp_match2, BIN(cont_op2),
		      new_calldata(iseq, idEqTilde, 1, top, 0, NULL, FALSE),
		      INT2LINT(ls - res), INT2LINT(ls - str_op), INT2LINT(ls - regex_op));
	}

	if (node->nd_args) {
	    compile_named_capture_assign(iseq, ret, node->nd_args, &capture_temp_vars_num);
	}

	break;
      }
      case NODE_LIT:{
	debugp_param("lit", node->nd_lit);
	if (!popped) {
	    add_value_load(iseq, ret, line,
			   (int) iseq->body->local_table_size - setup_result_var_number(iseq, result, curr_temp_vars_num),
			   node->nd_lit);
	}
	break;
      }
      case NODE_STR:{

	debugp_param("nd_lit", node->nd_lit);
	if (!popped) {
	    node->nd_lit = rb_fstring(node->nd_lit);
	    if (!ISEQ_COMPILE_DATA(iseq)->option->frozen_string_literal) {
		ADD_INSN2(ret, line, str2var,
			  INT2LINT(ls - setup_result_var_number(iseq, result, curr_temp_vars_num)),
			  node->nd_lit);
	    }
	    else {
		if (ISEQ_COMPILE_DATA(iseq)->option->debug_frozen_string_literal || RTEST(ruby_debug)) {
		    VALUE debug_info = rb_ary_new_from_args(2, iseq->body->location.path, INT2FIX(line));
		    VALUE str = rb_str_dup(node->nd_lit);
		    rb_ivar_set(str, id_debug_created_info, rb_obj_freeze(debug_info));
		    add_value_load(iseq, ret, line,
				   ls - setup_result_var_number(iseq, result, curr_temp_vars_num),
				   rb_obj_freeze(str));
		    iseq_add_mark_object_compile_time(iseq, str);
		}
		else {
		    add_value_load(iseq, ret, line,
				   ls - setup_result_var_number(iseq, result, curr_temp_vars_num),
				   node->nd_lit);
		}
	    }
	}
	break;
      }
      case NODE_DSTR:{

	  compile_dstr(iseq, ret, node, result, curr_temp_vars_num);
	  if (ISEQ_COMPILE_DATA(iseq)->option->frozen_string_literal && result != NULL) {
	      VALUE debug_info = Qnil;
	      if (ISEQ_COMPILE_DATA(iseq)->option->debug_frozen_string_literal || RTEST(ruby_debug)) {
		  debug_info = rb_ary_new_from_args(2, iseq->body->location.path, INT2FIX(line));
		  iseq_add_mark_object_compile_time(iseq, rb_obj_freeze(debug_info));
	      }
	      ADD_INSN2(ret, line, freeze_string, INT2LINT(ls - *result), debug_info);
	  }
	break;
      }
      case NODE_XSTR:{
	node->nd_lit = rb_fstring(node->nd_lit);
	{
	    int temp_res = stack_result;
	    int call_start = setup_result_var_number(iseq, &temp_res, curr_temp_vars_num);
	    int temp_vars_num = *curr_temp_vars_num;

	    ADD_RTL_CALL_RECEIVER(ret, line, INT2LINT(ls - call_start));
	    temp_res = stack_result;
	    add_value_load(iseq, ret, line,
			    ls - setup_result_var_number(iseq, &temp_res, &temp_vars_num),
			    node->nd_lit);
	    ADD_RTL_SEND_R(ret, line, idBackquote, INT2FIX(1), NULL, FALSE, call_start,
			   (VALUE) INT2FIX(VM_CALL_FCALL), NULL);
	    if (result != NULL)
		*result = call_start;
	}
	break;
      }
      case NODE_DXSTR:{
	int temp_res = stack_result;
	int call_start = setup_result_var_number(iseq, &temp_res, curr_temp_vars_num);
	int temp_vars_num = *curr_temp_vars_num;

	ADD_RTL_CALL_RECEIVER(ret, line, INT2LINT(ls - call_start));
	temp_res = stack_result;
	compile_dstr(iseq, ret, node, &temp_res, &temp_vars_num);
	ADD_RTL_SEND_R(ret, line, idBackquote, INT2FIX(1), NULL, FALSE, call_start,
		       (VALUE) INT2FIX(VM_CALL_FCALL), NULL);
	if (result != NULL)
	    *result = call_start;
	break;
      }
      case NODE_EVSTR:{
	int temp_vars_num = *curr_temp_vars_num;
	int val_op = anywhere_result;

	CHECK(COMPILE(ret, "nd_body", node->nd_body, &val_op, &temp_vars_num));

	ADD_INSN2(ret, line, to_string,
		  INT2LINT(ls - setup_result_var_number(iseq, result, curr_temp_vars_num)),
		  INT2LINT(ls - val_op));
	break;
      }
      case NODE_DREGX:{
	compile_dregx(iseq, ret, node, result, curr_temp_vars_num);
	break;
      }
      case NODE_DREGX_ONCE:{
	int ic_index = iseq->body->is_size++;
	NODE *dregx_node = NEW_NODE(NODE_DREGX, node->u1.value, node->u2.value, node->u3.value);
	NODE *block_node = NEW_NODE(NODE_SCOPE, 0, dregx_node, 0);
	const rb_iseq_t * block_iseq = NEW_CHILD_ISEQ(block_node, make_name_for_block(iseq), ISEQ_TYPE_BLOCK, line);

	ADD_INSN3(ret, line, run_once,
		  INT2LINT(ls - setup_result_var_number(iseq, result, curr_temp_vars_num)),
		  block_iseq, INT2FIX(ic_index));
	break;
      }
      case NODE_ARGSCAT:{
	int op1_result = anywhere_result, op2_result = anywhere_result;
	int temp_vars_num = *curr_temp_vars_num;

	CHECK(COMPILE(ret, "argscat head", node->nd_head, &op1_result, &temp_vars_num));
	CHECK(COMPILE(ret, "argscat body", node->nd_body, &op2_result, &temp_vars_num));
	ADD_INSN3(ret, line, concat_array,
		  INT2LINT(ls - setup_result_var_number(iseq, result, curr_temp_vars_num)),
		  INT2LINT(ls - op1_result), INT2LINT(ls - op2_result));
	break;
      }
      case NODE_ARGSPUSH:{
	int op1_result = anywhere_result, op2_result = anywhere_result;
	int temp_vars_num = *curr_temp_vars_num;
	int top, res;

	CHECK(COMPILE(ret, "arsgpush head", node->nd_head, &op1_result, &temp_vars_num));
	CHECK(COMPILE(ret, "argspush body", node->nd_body, &op2_result, &temp_vars_num));
	top = -temp_vars_num - 1;
	ADD_INSN3(ret, line, make_array,
		  INT2LINT(top), INT2LINT(ls - op2_result), INT2FIX(1));
	res = setup_result_var_number(iseq, result, curr_temp_vars_num);
	ADD_INSN3(ret, line, concat_array,
		  INT2LINT(ls - res), INT2LINT(ls - op1_result), INT2LINT(top));
	break;
      }
      case NODE_SPLAT:{
	int op_result = anywhere_result;
	int temp_vars_num = *curr_temp_vars_num;

	CHECK(COMPILE(ret, "splat", node->nd_head, &op_result, &temp_vars_num));
	ADD_INSN3(ret, line, splat_array,
		  INT2LINT(ls - setup_result_var_number(iseq, result, curr_temp_vars_num)),
		  INT2LINT(ls - op_result), Qtrue);
	break;
      }
      case NODE_DEFN:{

	const rb_iseq_t *method_iseq = NEW_ISEQ(node->nd_defn,
						rb_id2str(node->nd_mid),
						ISEQ_TYPE_METHOD, line);

	debugp_param("defn/iseq", rb_iseqw_new(method_iseq));

	{
	    int temp_vars_num = *curr_temp_vars_num;
	    int call_start = ls + temp_vars_num + 1;
	    int temp_res = stack_result;

	    ADD_INSN2(ret, line, specialobj2var,
		      INT2LINT(ls - setup_result_var_number(iseq, &temp_res, &temp_vars_num)),
		      INT2FIX(VM_SPECIAL_OBJECT_VMCORE));
	    temp_res = stack_result;
	    add_value_load(iseq, ret, line,
			   ls - setup_result_var_number(iseq, &temp_res, &temp_vars_num),
			   ID2SYM(node->nd_mid));
	    temp_res = stack_result;
	    ADD_INSN2(ret, line, iseq2var,
		      INT2LINT(ls - setup_result_var_number(iseq, &temp_res, &temp_vars_num)),
		      method_iseq);
	    ADD_RTL_SEND_R(ret, line, id_core_define_method, INT2FIX(2), NULL, FALSE,
			   call_start, (VALUE)INT2FIX(0), NULL);
	    if (result != NULL)
		*result = call_start;
	}

	break;
      }
      case NODE_DEFS:{
	const rb_iseq_t * singleton_method = NEW_ISEQ(node->nd_defn,
						      rb_id2str(node->nd_mid),
						      ISEQ_TYPE_METHOD, line);

	debugp_param("defs/iseq", rb_iseqw_new(singleton_method));
	{
	    int call_start = ls + *curr_temp_vars_num + 1;
	    int temp_vars_num = *curr_temp_vars_num;
	    int temp_res = stack_result;

	    ADD_INSN2(ret, line, specialobj2var,
		      INT2LINT(ls - setup_result_var_number(iseq, &temp_res, &temp_vars_num)),
		      INT2FIX(VM_SPECIAL_OBJECT_VMCORE));
	    temp_res = stack_result;
	    CHECK(COMPILE(ret, "defs: recv", node->nd_recv, &temp_res, &temp_vars_num));
	    temp_res = stack_result;
	    add_value_load(iseq, ret, line,
			   ls - setup_result_var_number(iseq, &temp_res, &temp_vars_num),
			   ID2SYM(node->nd_mid));
	    temp_res = stack_result;
	    ADD_INSN2(ret, line, iseq2var,
		      INT2LINT(ls - setup_result_var_number(iseq, &temp_res, &temp_vars_num)),
		      singleton_method);
	    ADD_RTL_SEND_R(ret, line, id_core_define_singleton_method, INT2FIX(3), NULL,
			   FALSE, call_start, (VALUE) INT2FIX(0), NULL);
	    if (result != NULL)
		*result = call_start;
	    /* Reserve slot for result  */
	    get_temp_stack_slot(iseq, curr_temp_vars_num);
	}

	break;
      }
      case NODE_ALIAS:{
	int temp_res = stack_result;
	int call_start = setup_result_var_number(iseq, &temp_res, curr_temp_vars_num);
	int temp_vars_num = *curr_temp_vars_num;

	ADD_INSN2(ret, line, specialobj2var, INT2LINT(ls - call_start),
		  INT2FIX(VM_SPECIAL_OBJECT_VMCORE));
	temp_res = stack_result;
	ADD_INSN2(ret, line, specialobj2var,
		  INT2LINT(ls - setup_result_var_number(iseq, &temp_res, &temp_vars_num)),
		  INT2FIX(VM_SPECIAL_OBJECT_CBASE));
	temp_res = stack_result;
	CHECK(COMPILE(ret, "alias arg1", node->u1.node, &temp_res, &temp_vars_num));
	temp_res = stack_result;
	CHECK(COMPILE(ret, "alias arg2", node->u2.node, &temp_res, &temp_vars_num));
	ADD_RTL_SEND_R(ret, line, id_core_set_method_alias, INT2FIX(3), NULL,
		       FALSE, call_start, (VALUE) INT2FIX(0), NULL);
	if (result != NULL)
	    *result = call_start;
	break;
      }
      case NODE_VALIAS:{
	int temp_res = stack_result;
	int call_start = setup_result_var_number(iseq, &temp_res, curr_temp_vars_num);
	int temp_vars_num = *curr_temp_vars_num;

	ADD_INSN2(ret, line, specialobj2var, INT2LINT(ls - call_start),
		  INT2FIX(VM_SPECIAL_OBJECT_VMCORE));
	temp_res = stack_result;
	add_value_load(iseq, ret, line,
		       ls - setup_result_var_number(iseq, &temp_res, &temp_vars_num),
		       ID2SYM(node->u1.id));
	temp_res = stack_result;
	add_value_load(iseq, ret, line,
		       ls - setup_result_var_number(iseq, &temp_res, &temp_vars_num),
		       ID2SYM(node->u2.id));
	ADD_RTL_SEND_R(ret, line, id_core_set_variable_alias, INT2FIX(2), NULL,
		       FALSE, call_start, (VALUE) INT2FIX(0), NULL);
	if (result != NULL)
	    *result = call_start;
	break;
      }
      case NODE_UNDEF:{
        int temp_res = stack_result;
	int call_start = setup_result_var_number(iseq, &temp_res, curr_temp_vars_num);
	int temp_vars_num = *curr_temp_vars_num;

	ADD_INSN2(ret, line, specialobj2var, INT2LINT(ls - call_start),
		  INT2FIX(VM_SPECIAL_OBJECT_VMCORE));
	temp_res = stack_result;
	ADD_INSN2(ret, line, specialobj2var,
		  INT2LINT(ls - setup_result_var_number(iseq, &temp_res, &temp_vars_num)),
		  INT2FIX(VM_SPECIAL_OBJECT_CBASE));
	temp_res = stack_result;
	CHECK(COMPILE(ret, "undef arg", node->u2.node, &temp_res, &temp_vars_num));
	ADD_RTL_SEND_R(ret, line, id_core_undef_method, INT2FIX(2), NULL,
		       FALSE, call_start, (VALUE) INT2FIX(0), NULL);
	if (result != NULL)
	    *result = call_start;
	break;
      }
      case NODE_CLASS:{
	const rb_iseq_t *class_iseq = NEW_CHILD_ISEQ(node->nd_body,
						     rb_sprintf("<class:%"PRIsVALUE">", rb_id2str(node->nd_cpath->nd_mid)),
						     ISEQ_TYPE_CLASS, line);
	int top = -*curr_temp_vars_num - 1;
	int cbase_op = anywhere_result, super_op = stack_result;
	int temp_vars_num = *curr_temp_vars_num;
	VALUE noscope = compile_cpath(ret, iseq, node->nd_cpath, &cbase_op, &temp_vars_num);
	int flags = VM_DEFINECLASS_TYPE_CLASS;

	if (!noscope) flags |= VM_DEFINECLASS_FLAG_SCOPED;
	if (node->nd_super) flags |= VM_DEFINECLASS_FLAG_HAS_SUPERCLASS;
	CHECK(COMPILE(ret, "super", node->nd_super, &super_op, &temp_vars_num));
	ADD_INSN6(ret, line, define_class, ID2SYM(node->nd_cpath->nd_mid), class_iseq, INT2FIX(flags),
		  INT2LINT(ls - cbase_op), INT2LINT(ls - super_op), INT2LINT(top));
	if (result != NULL) {
	    /* Reserve stack slot for the result */
	    setup_result_var_number(iseq, NULL, curr_temp_vars_num);
	    *result = ls - top;
	}
	break;
      }
      case NODE_MODULE:{
        const rb_iseq_t *module_iseq = NEW_CHILD_ISEQ(node->nd_body,
						      rb_sprintf("<module:%"PRIsVALUE">", rb_id2str(node->nd_cpath->nd_mid)),
						      ISEQ_TYPE_CLASS, line);
	int cbase_op = anywhere_result, top = -*curr_temp_vars_num - 1, temp_vars_num = *curr_temp_vars_num;
	VALUE noscope = compile_cpath(ret, iseq, node->nd_cpath, &cbase_op, &temp_vars_num);
	int flags = VM_DEFINECLASS_TYPE_MODULE;

	if (!noscope) flags |= VM_DEFINECLASS_FLAG_SCOPED;
	{
	    int temp_res = stack_result;
	    int super_op = setup_result_var_number(iseq, &temp_res, &temp_vars_num);

	    add_value_load(iseq, ret, line, ls - super_op, Qnil); /* dummy */
	    ADD_INSN6(ret, line, define_class, ID2SYM(node->nd_cpath->nd_mid), module_iseq, INT2FIX(flags),
		      INT2LINT(ls - cbase_op), INT2LINT(ls - super_op), INT2LINT(top));
	    if (result != NULL) {
		/* Reserve stack slot for the result */
		setup_result_var_number(iseq, NULL, curr_temp_vars_num);
		*result = ls - top;
	    }
	}
	break;
      }
      case NODE_SCLASS:{
	ID singletonclass;
	const rb_iseq_t *singleton_class = NEW_ISEQ(node->nd_body, rb_str_new2("singleton class"),
						    ISEQ_TYPE_CLASS, line);
	int top = -*curr_temp_vars_num - 1;
	int cbase_op = anywhere_result, temp_vars_num = *curr_temp_vars_num;

	CHECK(COMPILE(ret, "sclass#recv", node->nd_recv, &cbase_op, &temp_vars_num));
	CONST_ID(singletonclass, "singletonclass");
	{
	    int temp_res = stack_result;
	    int super_op = setup_result_var_number(iseq, &temp_res, &temp_vars_num);

	    add_value_load(iseq, ret, line, ls - super_op, Qnil);
	    ADD_INSN6(ret, line, define_class, ID2SYM(singletonclass), singleton_class,
		      INT2FIX(VM_DEFINECLASS_TYPE_SINGLETON_CLASS),
		      INT2LINT(ls - cbase_op), INT2LINT(ls - super_op), INT2LINT(top));
	    if (result != NULL) {
		/* Reserve stack slot for the result */
		setup_result_var_number(iseq, NULL, curr_temp_vars_num);
		*result = ls - top;
	    }
	}
	break;
      }
      case NODE_COLON2:{
	if (rb_is_const_id(node->nd_mid)) {
	    /* constant */
	    LABEL *lend = NEW_LABEL(line);
	    int ic_index = iseq->body->is_size++;
	    int pref_flag = FALSE;

	    DECL_ANCHOR(pref);
	    DECL_ANCHOR(body);

	    INIT_ANCHOR(pref);
	    INIT_ANCHOR(body);
	    compile_colon2(iseq, node, pref, body, result, curr_temp_vars_num, &pref_flag);
	    if (!pref_flag) {
		if (ISEQ_COMPILE_DATA(iseq)->option->inline_const_cache && result != NULL) {
		    ADD_INSN3(ret, line, get_inline_cache, lend, INT2LINT(ls - *result),
			      INT2FIX(ic_index));
		}

		ADD_SEQ(ret, body);

		if (ISEQ_COMPILE_DATA(iseq)->option->inline_const_cache) {
		    if (result != NULL)
			ADD_INSN2(ret, line, set_inline_cache, INT2LINT(ls - *result),
				  INT2FIX(ic_index));
		    ADD_LABEL(ret, lend);
		    lend->sp = *curr_temp_vars_num;
		}
	    }
	    else {
		ADD_SEQ(ret, pref);
		ADD_SEQ(ret, body);
	    }
	} else {
	    int call_start, temp_vars_num = *curr_temp_vars_num;
	    int temp_res = stack_result;

	    setup_result_var_number(iseq, &temp_res, &temp_vars_num); /* reserve slot for self_p */
	    call_start = ls + temp_vars_num;
	    if (USE_SELF_LD)
		ADD_INSN1(ret, line, self2var, INT2LINT(ls - call_start));
	    CHECK(COMPILE(ret, "colon2#nd_head", node->nd_head, result, &temp_vars_num));
	    ADD_RTL_SEND_R(ret, line, node->nd_mid, INT2FIX(VM_CALL_FCALL), NULL, !USE_SELF_LD, call_start,
			   (VALUE) INT2FIX(VM_CALL_FCALL), NULL);
	    if (result != NULL) {
		*result = call_start;
		/* Reserve slot for result  */
		get_temp_stack_slot(iseq, curr_temp_vars_num);
	    }
	}
	break;
      }
      case NODE_COLON3:{
	int ic_index = iseq->body->is_size++;

	debugi("colon3#nd_mid", node->nd_mid);

	if (ISEQ_COMPILE_DATA(iseq)->option->inline_const_cache) {
	    int res = setup_result_var_number(iseq, result, curr_temp_vars_num);

	    ADD_INSN4(ret, line, const_cached_val_ld,
		      INT2LINT(ls - res),
		      rb_cObject, ID2SYM(node->nd_mid), INT2FIX(ic_index));
	}
	else {
	    int res = setup_result_var_number(iseq, result, curr_temp_vars_num);

	    ADD_INSN3(ret, line, const_ld_val, ID2SYM(node->nd_mid),
		      INT2LINT(ls - res), rb_cObject);
	}
	break;
      }
      case NODE_DOT2:
      case NODE_DOT3:{
	int excl = type == NODE_DOT3;
	VALUE flag = INT2FIX(excl);
	NODE *b = node->nd_beg;
	NODE *e = node->nd_end;
	int temp_vars_num = *curr_temp_vars_num;
	int low = anywhere_result, high = anywhere_result;

	if (number_literal_p(b) && number_literal_p(e)) {
	    VALUE val = rb_range_new(b->nd_lit, e->nd_lit, excl);
	    iseq_add_mark_object_compile_time(iseq, val);
	    add_value_load
		(iseq, ret, line,
		 INT2LINT(ls - setup_result_var_number(iseq, result,
						       curr_temp_vars_num)),
		 val);
	    break;
	}

	CHECK(COMPILE(ret, "min", (NODE *) node->nd_beg, &low, &temp_vars_num));
	CHECK(COMPILE(ret, "max", (NODE *) node->nd_end, &high, &temp_vars_num));
	ADD_INSN4(ret, line, make_range,
		  INT2LINT(ls - setup_result_var_number(iseq, result, curr_temp_vars_num)),
		  INT2LINT(ls - low), INT2LINT(ls - high), flag);
	break;
      }
      case NODE_FLIP2:
      case NODE_FLIP3:{
	LABEL *lend = NEW_LABEL(line);
	LABEL *ltrue = NEW_LABEL(line);
	LABEL *lfalse = NEW_LABEL(line);
	int res, temp_vars_num;

	if (result == NULL || *result == stack_result || *result == anywhere_result)
	    {
		int st_res = stack_result;
		res = setup_result_var_number(iseq, &st_res, curr_temp_vars_num);
	    }
	else
	    res = *result;
	temp_vars_num = *curr_temp_vars_num;

	compile_branch_condition(iseq, ret, node, ltrue, lfalse, &res, &temp_vars_num);
	ADD_RTL_GOTO(ret, line, lend);
	ADD_LABEL(ret, ltrue);
	ltrue->sp = temp_vars_num;
	add_value_load(iseq, ret, line, ls - res, Qtrue);
	ADD_RTL_GOTO(ret, line, lend);
	ADD_LABEL(ret, lfalse);
	lfalse->sp = temp_vars_num;
	add_value_load(iseq, ret, line, ls - res, Qfalse);
	ADD_LABEL(ret, lend);
	lend->sp = temp_vars_num;

	if (result != NULL)
	    *result = res;

	break;
      }
      case NODE_SELF:{
	ADD_INSN1(ret, line, self2var,
		  INT2LINT(ls - setup_result_var_number(iseq, result, curr_temp_vars_num)));
	break;
      }
      case NODE_NIL:{
	add_value_load(iseq, ret, line,
		       ls - setup_result_var_number(iseq, result, curr_temp_vars_num),
		       Qnil);
	break;
      }
      case NODE_TRUE:{
	add_value_load(iseq, ret, line,
		       ls - setup_result_var_number(iseq, result, curr_temp_vars_num),
		       Qtrue);
	break;
      }
      case NODE_FALSE:{
	add_value_load(iseq, ret, line,
		       ls - setup_result_var_number(iseq, result, curr_temp_vars_num),
		       Qfalse);

	break;
      }
      case NODE_ERRINFO:{
	if (iseq->body->type == ISEQ_TYPE_RESCUE) {
	    add_local_move(iseq, ret, line,
			   ls - setup_result_var_number(iseq, result, curr_temp_vars_num),
			   VM_ENV_DATA_SIZE);
	}
	else {
	    const rb_iseq_t *ip = iseq;
	    int level = 0;
	    while (ip) {
		if (ip->body->type == ISEQ_TYPE_RESCUE) {
		    break;
		}
		ip = ip->body->parent_iseq;
		level++;
	    }
	    if (ip) {
		if (level == 0)
		    add_local_move(iseq, ret, line,
				   ls - setup_result_var_number(iseq, result, curr_temp_vars_num),
				   VM_ENV_DATA_SIZE);
		else {
		    ADD_INSN3(ret, line, uploc2var,
			      INT2LINT(ls - setup_result_var_number(iseq, result, curr_temp_vars_num)),
			      INT2LINT(2), INT2FIX(level));
		}
	    }
	    else {
		add_value_load(iseq, ret, line,
			       ls - setup_result_var_number(iseq, result, curr_temp_vars_num),
			       Qnil);
	    }
	}
	break;
      }
      case NODE_DEFINED:{
	if (!node->nd_head) {
	    VALUE str = rb_iseq_defined_string(DEFINED_NIL);
	    add_value_load(iseq, ret, nd_line(node),
			   ls - setup_result_var_number(iseq, result, curr_temp_vars_num),
			   str);
	}
	else {
   	    LINK_ELEMENT *last;
	    int empty_result, head_result, temp_vars_num, line1;
	    LABEL *lfinish[2];
	    lfinish[0] = NEW_LABEL(line);
	    lfinish[1] = 0;

	    temp_vars_num = *curr_temp_vars_num;
	    empty_result = setup_result_var_number(iseq, result, &temp_vars_num);
	    add_value_load(iseq, ret, line, ls - empty_result, Qnil);
	    line1 = ISEQ_COMPILE_DATA(iseq)->last_line;
	    last = ret->last;
	    head_result = anywhere_result;
	    temp_vars_num = *curr_temp_vars_num;
	    defined_expr(iseq, ret, node->nd_head, lfinish, Qtrue,
			 result == NULL ? NULL : &head_result, &temp_vars_num);
	    if (result != NULL)
		joint_result(iseq, ret, line1, nd_line(node->nd_head),
			     empty_result, last, head_result, result, curr_temp_vars_num);
	    if (lfinish[1]) {
		ADD_LABEL(ret, lfinish[1]);
	    }
	    ADD_LABEL(ret, lfinish[0]);
	    lfinish[0]->sp = *curr_temp_vars_num;
	}
	break;
      }
      case NODE_POSTEXE:{
	/* compiled to:
	 *   ONCE{ rb_mRubyVMFrozenCore::core#set_postexe{ ... } }
	 */
	int is_index = iseq->body->is_size++;
	const rb_iseq_t *once_iseq = NEW_CHILD_ISEQ((NODE *)IFUNC_NEW(build_postexe_iseq, node->nd_body, 0),
						    make_name_for_block(iseq), ISEQ_TYPE_BLOCK, line);

	assert(*curr_temp_vars_num == 0);
	ADD_INSN3(ret, line, run_once,
		  INT2LINT(ls - setup_result_var_number(iseq, result, curr_temp_vars_num)),
		  once_iseq, INT2FIX(is_index));
	break;
      }
      case NODE_KW_ARG:
	{
	    LABEL *end_label = NEW_LABEL(nd_line(node));
	    NODE *default_value = node->nd_body->nd_value;

	    if (default_value == (NODE *)-1) {
		/* required argument. do nothing */
		compile_bug(ERROR_ARGS "unreachable");
	    }
	    else if (nd_type(default_value) == NODE_LIT ||
		     nd_type(default_value) == NODE_NIL ||
		     nd_type(default_value) == NODE_TRUE ||
		     nd_type(default_value) == NODE_FALSE) {
		compile_bug(ERROR_ARGS "unreachable");
	    }
	    else {
		/* if keywordcheck(_kw_bits, nth_keyword)
		 *   kw = default_value
		 * end
		 */
		int kw_bits_idx = iseq->body->local_table_size - iseq->body->param.keyword->bits_start;
		int keyword_idx = iseq->body->param.keyword->num;

		ADD_INSN3(ret, line, bkw, end_label, INT2LINT(kw_bits_idx + VM_ENV_DATA_SIZE - 1), INT2FIX(keyword_idx));
		CHECK(COMPILE_POPPED(ret, "keyword default argument", node->nd_body, result, curr_temp_vars_num));
		ADD_LABEL(ret, end_label);
		end_label->sp = *curr_temp_vars_num;
	    }

	    break;
	}
      case NODE_DSYM:{
	  int temp_vars_num = *curr_temp_vars_num;
	  int call_start = ls + temp_vars_num + 1;
	  int temp_res = popped ? anywhere_result : stack_result;

	  compile_dstr(iseq, ret, node, &temp_res, &temp_vars_num);
	  if (!popped && result != NULL) {
	      ADD_RTL_SEND_R(ret, line, idIntern, INT2FIX(0), NULL, FALSE, call_start, (VALUE)INT2FIX(0), NULL);
	      *result = call_start;
	      increment_temps_var(iseq, curr_temp_vars_num, 1); /* Reserve slot for result */
	  }
	  break;
      }
      case NODE_ATTRASGN:{
	unsigned int flag = 0;
	ID mid = node->nd_mid;
	LABEL *lskip = 0;
	VALUE argc;
	int recv_op = anywhere_result, call_start = 0, temp_vars_num = *curr_temp_vars_num;
	int val_result = anywhere_result;

	if (mid == idASET && !private_recv_p(node) && node->nd_args
	    && nd_type(node->nd_args) == NODE_ARRAY && node->nd_args->nd_alen == 2
	    && ISEQ_COMPILE_DATA(iseq)->current_block == NULL) {
	    int arr_result = anywhere_result, val_result = anywhere_result;

	    if (result != NULL)
		val_result = *result;

	    if (nd_type(node->nd_args->nd_head) == NODE_STR &&
		ISEQ_COMPILE_DATA(iseq)->option->specialized_instruction) {
		    VALUE str = rb_fstring(node->nd_args->nd_head->nd_lit);

		    node->nd_args->nd_head->nd_lit = str;
		    iseq_add_mark_object(iseq, str);
		    CHECK(COMPILE(ret, "recv", node->nd_recv, &arr_result, &temp_vars_num));
		    CHECK(COMPILE(ret, "value", node->nd_args->nd_next->nd_head, &val_result, &temp_vars_num));
		    ADD_INSN4(ret, line, indsets,
			      new_calldata(iseq, idASET, 2, -1 - temp_vars_num, 0, NULL, FALSE),
			      INT2LINT(ls - arr_result), str,  INT2LINT(ls - val_result));
	    } else {
		int ind_result = anywhere_result;

		COMPILE(ret, "recv", node->nd_recv, &arr_result, &temp_vars_num);
		COMPILE(ret, "index", node->nd_args->nd_head, &ind_result, &temp_vars_num);
		COMPILE(ret, "value", node->nd_args->nd_next->nd_head, &val_result, &temp_vars_num);

		ADD_INSN4(ret, line, indset,
			  new_calldata(iseq, idASET, 2, -1 - temp_vars_num, 0, NULL, FALSE),
			  INT2LINT(ls - arr_result), INT2LINT(ls - ind_result),  INT2LINT(ls - val_result));
	    }
	    if (result != NULL) {
		assert(val_result != anywhere_result && val_result != stack_result);
		if (*result == anywhere_result)
		    *result = val_result;
		else if (*result == stack_result) {
		    setup_result_var_number(iseq, result, curr_temp_vars_num);
		    if (val_result != *result)
			add_local_move(iseq, ret, line, ls - *result, ls - val_result);
		}
		else if (val_result != *result)
		    add_local_move(iseq, ret, line, ls - *result, ls - val_result);
	    }
	    break;
	}

	{
	    int temp_res = stack_result;
	    val_result = setup_result_var_number(iseq, result, curr_temp_vars_num); /* reserve slot for result */
	    temp_vars_num = *curr_temp_vars_num;
	    call_start = ls + temp_vars_num + 1;
	    if (private_recv_p(node)) {
		flag |= VM_CALL_FCALL;
		setup_result_var_number(iseq, &temp_res, &temp_vars_num); /* reserve slot for self_p */
		if (USE_SELF_LD) {
		    ADD_INSN1(ret, line, self2var, INT2LINT(ls - temp_res));
		    recv_op = temp_res;
		}
	    } else {
		COMPILE(ret, "recv", node->nd_recv, &recv_op, &temp_vars_num);
		if (call_start != recv_op) {
		    setup_result_var_number(iseq, &temp_res, &temp_vars_num); /* reserve slot for recv */
		}
	    }
	    temp_res = stack_result;
	    argc = setup_args(iseq, ret, node->nd_args, FALSE, &flag,
			      NULL, &temp_res, &temp_vars_num);
	}

	debugp_param("argc", argc);
	debugp_param("nd_mid", ID2SYM(mid));

	if (!rb_is_attrset_id(mid)) {
	    /* safe nav attr */
	    mid = rb_id_attrset(mid);
	    lskip = NEW_LABEL(line);
	    ADD_RTL_INSNL(ret, line, bnil, lskip, INT2LINT(ls - recv_op));
	}

	{
	    if (flag & VM_CALL_ARGS_SPLAT) {
		/* put on stack last non-block arg and -1 */
		add_local_move(iseq, ret, line, ls - (call_start + FIX2INT(argc) + 2),
			       ls - (call_start + FIX2INT(argc)));
		add_value_load(iseq, ret, line,
			       ls - (call_start + FIX2INT(argc) + 3),
			       INT2FIX(-1));
		increment_temps_var(iseq, &temp_vars_num, 2); /* reserve 2 stack slots.  */
		if (1)
		    /* ??? Optimize memory traffic.  */
		  ADD_INSN5(ret, line, ind, BIN(cont_op2),
			    (VALUE) new_calldata(iseq, idAREF, 1, -1 - temp_vars_num, 0, NULL, FALSE),
			    INT2LINT(ls - val_result),
			    INT2LINT(ls - (call_start + FIX2INT(argc) + 2)),
			    INT2LINT(ls - (call_start + FIX2INT(argc) + 3)));
		else {
		    ADD_RTL_SEND_R(ret, line, idAREF, INT2FIX(1), NULL, FALSE,
				   call_start + FIX2INT(argc) + 1, INT2FIX(0), NULL);
		    add_local_move(iseq, ret, line, ls - val_result, ls - (call_start + FIX2INT(argc) + 1));
		}
	    }
	    else {
		if (! popped)
		    add_local_move(iseq, ret, line, ls - val_result, ls - (call_start + FIX2INT(argc)));
	    }
	    ADD_RTL_SEND_RECV(ret, line, mid, argc, NULL, recv_op,
			      call_start, INT2FIX(flag), NULL);

	    if (lskip) {
		ADD_LABEL(ret, lskip);
		lskip->sp = *curr_temp_vars_num;
	    }
	}

	break;
      }
      case NODE_PRELUDE:{
	const rb_compile_option_t *orig_opt = ISEQ_COMPILE_DATA(iseq)->option;
	int init_temp_vars_num = *curr_temp_vars_num;
	if (node->nd_orig) {
	    rb_compile_option_t new_opt = *orig_opt;
	    rb_iseq_make_compile_option(&new_opt, node->nd_orig);
	    ISEQ_COMPILE_DATA(iseq)->option = &new_opt;
	}
	CHECK(COMPILE_POPPED(ret, "prelude", node->nd_head, NULL, curr_temp_vars_num));
	*curr_temp_vars_num = init_temp_vars_num;
	CHECK(COMPILE_(ret, "body", node->nd_body, popped, result, curr_temp_vars_num));
	ISEQ_COMPILE_DATA(iseq)->option = orig_opt;
	break;
      }
      case NODE_LAMBDA:{
	/* compile same as lambda{...} */
	const rb_iseq_t *block = NEW_CHILD_ISEQ(node->nd_body, make_name_for_block(iseq), ISEQ_TYPE_BLOCK, line);
	int temp_res = stack_result;
	int call_start = setup_result_var_number(iseq, &temp_res, curr_temp_vars_num);


	ADD_RTL_VMCORE_SEND(ret, line, idLambda, block, call_start);
	if (result != NULL)
	    *result = call_start;
	break;
      }
      default:
	rb_bug("iseq_compile_each: unknown node: %s", ruby_node_name(type));
	return COMPILE_NG;
    }

    /* check & remove redundant trace(line) */
    if (saved_last_element &&
	ret->last == saved_last_element &&
	((INSN *)saved_last_element)->insn_id == BIN(trace)) {
	POP_ELEMENT(ret);
    }

    debug_node_end();
    return COMPILE_OK;
}

/***************************/
/* instruction information */
/***************************/

static int
insn_data_length(INSN *iobj)
{
    return insn_len(iobj->insn_id);
}

static VALUE
opobj_inspect(VALUE obj)
{
    struct RBasic *r = (struct RBasic *) obj;
    if (!SPECIAL_CONST_P(r)  && r->klass == 0) {
	switch (BUILTIN_TYPE(r)) {
	  case T_STRING:
	    obj = rb_str_new_cstr(RSTRING_PTR(obj));
	    break;
	  case T_ARRAY:
	    obj = rb_ary_dup(obj);
	    break;
	}
    }
    return rb_inspect(obj);
}



static VALUE
insn_data_to_s_detail(INSN *iobj)
{
    VALUE str = rb_sprintf("%-20s ", insn_name(iobj->insn_id));

    if (iobj->operands) {
	const char *types = insn_op_types(iobj->insn_id);
	int j;

	for (j = 0; types[j]; j++) {
	    char type = types[j];

	    switch (type) {
	      case TS_OFFSET:	/* label(destination position) */
		{
		    LABEL *lobj = (LABEL *)OPERAND_AT(iobj, j);
		    rb_str_catf(str, "<L%03d>", lobj->label_no);
		    break;
		}
		break;
	      case TS_ISEQ:	/* iseq */
		{
		    rb_iseq_t *iseq = (rb_iseq_t *)OPERAND_AT(iobj, j);
		    VALUE val = Qnil;
		    if (0 && iseq) { /* TODO: invalidate now */
			val = (VALUE)iseq;
		    }
		    rb_str_concat(str, opobj_inspect(val));
		}
		break;
	      case TS_INSN:
		{
		    enum ruby_vminsn_type i = (enum ruby_vminsn_type ) OPERAND_AT(iobj, j);
		    rb_str_catf(str, "%s", insn_name_info[i]);
		    break;
		}
		break;
	      case TS_LINDEX:
	      case TS_SINDEX:
	      case TS_RINDEX:
		{
		    long l = (long) OPERAND_AT(iobj, j);
		    rb_str_catf(str, "%3ld", l);
		    break;
		}
		break;
	      case TS_NUM:	/* ulong */
	      case TS_VALUE:	/* VALUE */
		{
		    VALUE v = OPERAND_AT(iobj, j);
		    rb_str_concat(str, opobj_inspect(v));
		    break;
		}
	      case TS_ID:	/* ID */
		rb_str_concat(str, opobj_inspect(OPERAND_AT(iobj, j)));
		break;
	      case TS_GENTRY:
		{
		    struct rb_global_entry *entry = (struct rb_global_entry *)
		      (OPERAND_AT(iobj, j) & (~1));
		    rb_str_append(str, rb_id2str(entry->id));
		    break;
		}
	      case TS_IC:	/* inline cache */
		rb_str_catf(str, "<ic:%d>", FIX2INT(OPERAND_AT(iobj, j)));
		break;
	      case TS_CALLINFO: /* call info */
		{
		    /* TS_CALLINFO is not be used for RTL insns.  */
		    break;
		}
	      case TS_CALLCACHE: /* call cache */
		{
		    /* TS_CALLINFO is not be used for RTL insns.  */
		    break;
		}
	      case TS_CALLDATA: /* call data */
		{
		    struct rb_call_data *cd = (struct rb_call_data *)OPERAND_AT(iobj, j);
		    rb_str_cat2(str, "<calldata:");
		    if (cd->call_info.mid) rb_str_catf(str, "%"PRIsVALUE, rb_id2str(cd->call_info.mid));
		    rb_str_catf(str, ", %d>", cd->call_info.orig_argc);
		    break;
		}
	      case TS_CDHASH:	/* case/when condition cache */
		rb_str_cat2(str, "<ch>");
		break;
	      case TS_FUNCPTR:
		{
		    rb_insn_func_t func = (rb_insn_func_t)OPERAND_AT(iobj, j);
#ifdef HAVE_DLADDR
		    Dl_info info;
		    if (dladdr(func, &info) && info.dli_sname) {
			rb_str_cat2(str, info.dli_sname);
			break;
		    }
#endif
		    rb_str_catf(str, "<%p>", func);
		}
		break;
	      default:{
		rb_raise(rb_eSyntaxError, "unknown operand type: %c", type);
	      }
	    }
	    if (types[j + 1]) {
		rb_str_cat2(str, ", ");
	    }
	}
    }
    return str;
}

static void
dump_disasm_list(struct iseq_link_element *link)
{
    int pos = 0;
    INSN *iobj;
    LABEL *lobj;
    VALUE str;

    printf("-- raw disasm--------\n");

    while (link) {
	switch (link->type) {
	  case ISEQ_ELEMENT_INSN:
	    {
		iobj = (INSN *)link;
		str = insn_data_to_s_detail(iobj);
		printf("%04d %-65s(%4u)\n", pos, StringValueCStr(str), iobj->line_no);
		pos += insn_data_length(iobj);
		break;
	    }
	  case ISEQ_ELEMENT_LABEL:
	    {
		lobj = (LABEL *)link;
		printf("<L%03d>\n", lobj->label_no);
		break;
	    }
	  case ISEQ_ELEMENT_NONE:
	    {
		printf("[none]\n");
		break;
	    }
	  default:
	    /* ignore */
	    rb_raise(rb_eSyntaxError, "dump_disasm_list error: %ld\n", FIX2LONG(link->type));
	}
	link = link->next;
    }
    printf("---------------------\n");
    fflush(stdout);
}

const char *
rb_insns_name(int i)
{
    return insn_name_info[i];
}

VALUE
rb_insns_name_array(void)
{
    VALUE ary = rb_ary_new();
    int i;
    for (i = 0; i < numberof(insn_name_info); i++) {
	rb_ary_push(ary, rb_fstring(rb_str_new2(insn_name_info[i])));
    }
    return rb_obj_freeze(ary);
}

static LABEL *
register_label(rb_iseq_t *iseq, struct st_table *labels_table, VALUE obj)
{
    LABEL *label = 0;
    st_data_t tmp;
    obj = rb_convert_type(obj, T_SYMBOL, "Symbol", "to_sym");

    if (st_lookup(labels_table, obj, &tmp) == 0) {
	label = NEW_LABEL(0);
	st_insert(labels_table, obj, (st_data_t)label);
    }
    else {
	label = (LABEL *)tmp;
    }
    LABEL_REF(label);
    return label;
}

static VALUE
get_exception_sym2type(VALUE sym)
{
#undef rb_intern
#define rb_intern(str) rb_intern_const(str)
    static VALUE symRescue, symEnsure, symRetry;
    static VALUE symBreak, symRedo, symNext;

    if (symRescue == 0) {
	symRescue = ID2SYM(rb_intern("rescue"));
	symEnsure = ID2SYM(rb_intern("ensure"));
	symRetry  = ID2SYM(rb_intern("retry"));
	symBreak  = ID2SYM(rb_intern("break"));
	symRedo   = ID2SYM(rb_intern("redo"));
	symNext   = ID2SYM(rb_intern("next"));
    }

    if (sym == symRescue) return CATCH_TYPE_RESCUE;
    if (sym == symEnsure) return CATCH_TYPE_ENSURE;
    if (sym == symRetry)  return CATCH_TYPE_RETRY;
    if (sym == symBreak)  return CATCH_TYPE_BREAK;
    if (sym == symRedo)   return CATCH_TYPE_REDO;
    if (sym == symNext)   return CATCH_TYPE_NEXT;
    rb_raise(rb_eSyntaxError, "invalid exception symbol: %+"PRIsVALUE, sym);
    return 0;
}

static int
iseq_build_from_ary_exception(rb_iseq_t *iseq, struct st_table *labels_table,
		     VALUE exception)
{
    int i;

    for (i=0; i<RARRAY_LEN(exception); i++) {
	const rb_iseq_t *eiseq;
	VALUE v, type;
	const VALUE *ptr;
	LABEL *lstart, *lend, *lcont;
	unsigned int sp;

	v = rb_convert_type(RARRAY_AREF(exception, i), T_ARRAY,
					 "Array", "to_ary");
	if (RARRAY_LEN(v) != 6) {
	    rb_raise(rb_eSyntaxError, "wrong exception entry");
	}
	ptr  = RARRAY_CONST_PTR(v);
	type = get_exception_sym2type(ptr[0]);
	if (ptr[1] == Qnil) {
	    eiseq = NULL;
	}
	else {
	    eiseq = rb_iseqw_to_iseq(rb_iseq_load(ptr[1], (VALUE)iseq, Qnil));
	}

	lstart = register_label(iseq, labels_table, ptr[2]);
	lend   = register_label(iseq, labels_table, ptr[3]);
	lcont  = register_label(iseq, labels_table, ptr[4]);
	sp     = NUM2UINT(ptr[5]);

	(void)sp;

	ADD_CATCH_ENTRY(type, lstart, lend, eiseq, lcont);

	RB_GC_GUARD(v);
    }
    return COMPILE_OK;
}

static struct st_table *
insn_make_insn_table(void)
{
    struct st_table *table;
    int i;
    table = st_init_numtable();

    for (i=0; i<VM_INSTRUCTION_SIZE; i++) {
	st_insert(table, ID2SYM(rb_intern(insn_name(i))), i);
    }

    return table;
}

static const rb_iseq_t *
iseq_build_load_iseq(const rb_iseq_t *iseq, VALUE op)
{
    VALUE iseqw;
    const rb_iseq_t *loaded_iseq;

    if (RB_TYPE_P(op, T_ARRAY)) {
	iseqw = rb_iseq_load(op, (VALUE)iseq, Qnil);
    }
    else if (CLASS_OF(op) == rb_cISeq) {
	iseqw = op;
    }
    else {
	rb_raise(rb_eSyntaxError, "ISEQ is required");
    }

    loaded_iseq = rb_iseqw_to_iseq(iseqw);
    iseq_add_mark_object(iseq, (VALUE)loaded_iseq);
    return loaded_iseq;
}

static VALUE
iseq_build_calldata_from_hash(rb_iseq_t *iseq, VALUE op)
{
    ID mid = 0;
    int orig_argc = 0;
    int call_start = -1;
    unsigned int flag = 0;
    struct rb_call_info_kw_arg *kw_arg = 0;

    if (!NIL_P(op)) {
	VALUE vmid = rb_hash_aref(op, ID2SYM(rb_intern("mid")));
	VALUE vflag = rb_hash_aref(op, ID2SYM(rb_intern("flag")));
	VALUE vorig_argc = rb_hash_aref(op, ID2SYM(rb_intern("orig_argc")));
	VALUE vcall_start = rb_hash_aref(op, ID2SYM(rb_intern("call_start")));
	VALUE vkw_arg = rb_hash_aref(op, ID2SYM(rb_intern("kw_arg")));

	if (!NIL_P(vmid)) mid = SYM2ID(vmid);
	if (!NIL_P(vflag)) flag = NUM2UINT(vflag);
	if (!NIL_P(vorig_argc)) orig_argc = FIX2INT(vorig_argc);
	if (!NIL_P(vcall_start)) call_start = FIX2INT(vcall_start);

	if (!NIL_P(vkw_arg)) {
	    int i;
	    int len = RARRAY_LENINT(vkw_arg);
	    size_t n = rb_call_info_kw_arg_bytes(len);

	    kw_arg = xmalloc(n);
	    kw_arg->keyword_len = len;
	    for (i = 0; i < len; i++) {
		VALUE kw = RARRAY_AREF(vkw_arg, i);
		SYM2ID(kw);	/* make immortal */
		kw_arg->keywords[i] = kw;
	    }
	}
    }

    return (VALUE)new_calldata(iseq, mid, orig_argc, call_start, flag, kw_arg, (flag & VM_CALL_ARGS_SIMPLE) == 0);
}

static int
iseq_build_from_ary_body(rb_iseq_t *iseq, LINK_ANCHOR *const anchor,
			 VALUE body, VALUE labels_wrapper)
{
    /* TODO: body should be frozen */
    const VALUE *ptr = RARRAY_CONST_PTR(body);
    long i, len = RARRAY_LEN(body);
    struct st_table *labels_table = DATA_PTR(labels_wrapper);
    int j;
    int line_no = 0;
    int ret = COMPILE_OK;

    /*
     * index -> LABEL *label
     */
    static struct st_table *insn_table;

    if (insn_table == 0) {
	insn_table = insn_make_insn_table();
    }

    for (i=0; i<len; i++) {
	VALUE obj = ptr[i];

	if (SYMBOL_P(obj)) {
	    LABEL *label = register_label(iseq, labels_table, obj);
	    ADD_LABEL(anchor, label);
	}
	else if (FIXNUM_P(obj)) {
	    line_no = NUM2INT(obj);
	}
	else if (RB_TYPE_P(obj, T_ARRAY)) {
	    VALUE *argv = 0;
	    int argc = RARRAY_LENINT(obj) - 1;
	    st_data_t insn_id;
	    VALUE insn;

	    insn = (argc < 0) ? Qnil : RARRAY_AREF(obj, 0);
	    if (st_lookup(insn_table, (st_data_t)insn, &insn_id) == 0) {
		/* TODO: exception */
		COMPILE_ERROR(iseq, line_no,
			      "unknown instruction: %+"PRIsVALUE, insn);
		ret = COMPILE_NG;
		break;
	    }

	    if (argc != insn_len((VALUE)insn_id)-1) {
		COMPILE_ERROR(iseq, line_no,
			      "operand size mismatch");
		ret = COMPILE_NG;
		break;
	    }

	    if (argc > 0) {
		argv = compile_data_alloc(iseq, sizeof(VALUE) * argc);
		for (j=0; j<argc; j++) {
		    VALUE op = rb_ary_entry(obj, j+1);
		    switch (insn_op_type((VALUE)insn_id, j)) {
		      case TS_OFFSET: {
			LABEL *label = register_label(iseq, labels_table, op);
			argv[j] = (VALUE)label;
			break;
		      }
		      case TS_LINDEX:
		      case TS_SINDEX:
		      case TS_RINDEX:
			argv[j] = INT2LINT(NUM2INT(op));
			break;
		      case TS_INSN:
		      case TS_NUM:
			(void)NUM2INT(op);
			argv[j] = op;
			break;
		      case TS_VALUE:
			argv[j] = op;
			iseq_add_mark_object(iseq, op);
			break;
		      case TS_ISEQ:
			{
			    if (op != Qnil) {
				argv[j] = (VALUE)iseq_build_load_iseq(iseq, op);
			    }
			    else {
				argv[j] = 0;
			    }
			}
			break;
		      case TS_GENTRY:
			op = rb_convert_type(op, T_SYMBOL, "Symbol", "to_sym");
			argv[j] = (VALUE)rb_global_entry(SYM2ID(op));
			break;
		      case TS_IC:
			argv[j] = op;
			if (NUM2UINT(op) >= iseq->body->is_size) {
			    iseq->body->is_size = NUM2INT(op) + 1;
			}
			break;
		      case TS_CALLINFO:
			/* TS_CALLINFO is not be used for RTL insns.  */
			break;
		      case TS_CALLCACHE:
			/* TS_CALLCACHE is not be used for RTL insns.  */
			break;
		      case TS_CALLDATA:
			argv[j] = iseq_build_calldata_from_hash(iseq, op);
			break;
		      case TS_ID:
			argv[j] = rb_convert_type(op, T_SYMBOL,
						  "Symbol", "to_sym");
			break;
		      case TS_CDHASH:
			{
			    int i;
			    VALUE map = rb_hash_new();

			    rb_hash_tbl_raw(map)->type = &cdhash_type;
			    op = rb_convert_type(op, T_ARRAY, "Array", "to_ary");
			    for (i=0; i<RARRAY_LEN(op); i+=2) {
				VALUE key = RARRAY_AREF(op, i);
				VALUE sym = RARRAY_AREF(op, i+1);
				LABEL *label =
				  register_label(iseq, labels_table, sym);
				rb_hash_aset(map, key, (VALUE)label | 1);
			    }
			    RB_GC_GUARD(op);
			    argv[j] = map;
			    rb_iseq_add_mark_object(iseq, map);
			}
			break;
		      case TS_FUNCPTR:
			{
#if SIZEOF_VALUE <= SIZEOF_LONG
			    long funcptr = NUM2LONG(op);
#else
			    LONG_LONG funcptr = NUM2LL(op);
#endif
			    argv[j] = (VALUE)funcptr;
			}
			break;
		      default:
			rb_raise(rb_eSyntaxError, "unknown operand: %c", insn_op_type((VALUE)insn_id, j));
		    }
		}
	    }
	    ADD_ELEM(anchor,
		     (LINK_ELEMENT*)new_insn_core(iseq, line_no,
						  (enum ruby_vminsn_type)insn_id, argc, argv));
	}
	else {
	    rb_raise(rb_eTypeError, "unexpected object for instruction");
	}
    }
    DATA_PTR(labels_wrapper) = 0;
    validate_labels(iseq, labels_table);
    if (!ret) return ret;
    return iseq_setup(iseq, anchor);
}

#define CHECK_ARRAY(v)   rb_convert_type((v), T_ARRAY, "Array", "to_ary")
#define CHECK_SYMBOL(v)  rb_convert_type((v), T_SYMBOL, "Symbol", "to_sym")

static int
int_param(int *dst, VALUE param, VALUE sym)
{
    VALUE val = rb_hash_aref(param, sym);
    switch (TYPE(val)) {
      case T_NIL:
	return FALSE;
      case T_FIXNUM:
	*dst = FIX2INT(val);
	return TRUE;
      default:
	rb_raise(rb_eTypeError, "invalid %+"PRIsVALUE" Fixnum: %+"PRIsVALUE,
		 sym, val);
    }
    return FALSE;
}

static const struct rb_iseq_param_keyword *
iseq_build_kw(rb_iseq_t *iseq, VALUE params, VALUE keywords)
{
    int i, j;
    int len = RARRAY_LENINT(keywords);
    int default_len;
    VALUE key, sym, default_val;
    VALUE *dvs;
    ID *ids;
    struct rb_iseq_param_keyword *keyword = ZALLOC(struct rb_iseq_param_keyword);

    iseq->body->param.flags.has_kw = TRUE;

    keyword->num = len;
#define SYM(s) ID2SYM(rb_intern(#s))
    (void)int_param(&keyword->bits_start, params, SYM(kwbits));
    i = keyword->bits_start - keyword->num;
    ids = (VALUE *)&iseq->body->local_table[i];
#undef SYM

    /* required args */
    for (i = 0; i < len; i++) {
	VALUE val = RARRAY_AREF(keywords, i);

	if (!SYMBOL_P(val)) {
	    goto default_values;
	}
	ids[i] = SYM2ID(val);
	keyword->required_num++;
    }

  default_values: /* note: we intentionally preserve `i' from previous loop */
    default_len = len - i;
    if (default_len == 0) {
	return keyword;
    }

    dvs = ALLOC_N(VALUE, default_len);

    for (j = 0; i < len; i++, j++) {
	key = RARRAY_AREF(keywords, i);
	CHECK_ARRAY(key);

	switch (RARRAY_LEN(key)) {
	  case 1:
	    sym = RARRAY_AREF(key, 0);
	    default_val = Qundef;
	    break;
	  case 2:
	    sym = RARRAY_AREF(key, 0);
	    default_val = RARRAY_AREF(key, 1);
	    break;
	  default:
	    rb_raise(rb_eTypeError, "keyword default has unsupported len %+"PRIsVALUE, key);
	}
	ids[i] = SYM2ID(sym);
	dvs[j] = default_val;
    }

    keyword->table = ids;
    keyword->default_values = dvs;

    return keyword;
}

void
rb_iseq_build_from_ary(rb_iseq_t *iseq, VALUE misc, VALUE locals, VALUE params,
		       VALUE exception, VALUE body)
{
#define SYM(s) ID2SYM(rb_intern(#s))
    int i, len;
    ID *tbl;
    struct st_table *labels_table = st_init_numtable();
    VALUE labels_wrapper = Data_Wrap_Struct(0, 0, st_free_table, labels_table);
    VALUE arg_opt_labels = rb_hash_aref(params, SYM(opt));
    VALUE keywords = rb_hash_aref(params, SYM(keyword));
    VALUE sym_arg_rest = ID2SYM(rb_intern("#arg_rest"));
    DECL_ANCHOR(anchor);
    INIT_ANCHOR(anchor);

    len = RARRAY_LENINT(locals);
    iseq->body->local_table_size = len;
    iseq->body->local_table = tbl = len > 0 ? (ID *)ALLOC_N(ID, iseq->body->local_table_size) : NULL;

    for (i = 0; i < len; i++) {
	VALUE lv = RARRAY_AREF(locals, i);

	if (sym_arg_rest == lv) {
	    tbl[i] = 0;
	}
	else {
	    tbl[i] = FIXNUM_P(lv) ? (ID)FIX2LONG(lv) : SYM2ID(CHECK_SYMBOL(lv));
	}
    }

    /*
     * we currently ignore misc params,
     * local_size, stack_size and param.size are all calculated
     */

#define INT_PARAM(F) int_param(&iseq->body->param.F, params, SYM(F))
    if (INT_PARAM(lead_num)) {
	iseq->body->param.flags.has_lead = TRUE;
    }
    if (INT_PARAM(post_num)) iseq->body->param.flags.has_post = TRUE;
    if (INT_PARAM(post_start)) iseq->body->param.flags.has_post = TRUE;
    if (INT_PARAM(rest_start)) iseq->body->param.flags.has_rest = TRUE;
    if (INT_PARAM(block_start)) iseq->body->param.flags.has_block = TRUE;
#undef INT_PARAM

    switch (TYPE(arg_opt_labels)) {
      case T_ARRAY:
	len = RARRAY_LENINT(arg_opt_labels);
	iseq->body->param.flags.has_opt = !!(len - 1 >= 0);

	if (iseq->body->param.flags.has_opt) {
	    VALUE *opt_table = ALLOC_N(VALUE, len);

	    for (i = 0; i < len; i++) {
		VALUE ent = RARRAY_AREF(arg_opt_labels, i);
		LABEL *label = register_label(iseq, labels_table, ent);
		opt_table[i] = (VALUE)label;
	    }

	    iseq->body->param.opt_num = len - 1;
	    iseq->body->param.opt_table = opt_table;
	}
      case T_NIL:
	break;
      default:
	rb_raise(rb_eTypeError, ":opt param is not an array: %+"PRIsVALUE,
		 arg_opt_labels);
    }

    switch (TYPE(keywords)) {
      case T_ARRAY:
	iseq->body->param.keyword = iseq_build_kw(iseq, params, keywords);
      case T_NIL:
	break;
      default:
	rb_raise(rb_eTypeError, ":keywords param is not an array: %+"PRIsVALUE,
		 keywords);
    }

    if (Qtrue == rb_hash_aref(params, SYM(ambiguous_param0))) {
	iseq->body->param.flags.ambiguous_param0 = TRUE;
    }

    if (int_param(&i, params, SYM(kwrest))) {
	struct rb_iseq_param_keyword *keyword = (struct rb_iseq_param_keyword *)iseq->body->param.keyword;
	if (keyword == NULL) {
	    iseq->body->param.keyword = keyword = ZALLOC(struct rb_iseq_param_keyword);
	}
	keyword->rest_start = i;
	iseq->body->param.flags.has_kwrest = TRUE;
    }
#undef SYM
    iseq_calc_param_size(iseq);

    /* exception */
    iseq_build_from_ary_exception(iseq, labels_table, exception);

    /* body */
    iseq_build_from_ary_body(iseq, anchor, body, labels_wrapper);
}

/* for parser */

int
rb_dvar_defined(ID id, const struct rb_block *base_block)
{
    const rb_iseq_t *iseq;

    if (base_block && (iseq = vm_block_iseq(base_block)) != NULL) {
	while (iseq->body->type == ISEQ_TYPE_BLOCK ||
	       iseq->body->type == ISEQ_TYPE_RESCUE ||
	       iseq->body->type == ISEQ_TYPE_ENSURE ||
	       iseq->body->type == ISEQ_TYPE_EVAL ||
	       iseq->body->type == ISEQ_TYPE_MAIN
	       ) {
	    unsigned int i;

	    for (i = 0; i < iseq->body->local_table_size; i++) {
		if (iseq->body->local_table[i] == id) {
		    return 1;
		}
	    }
	    iseq = iseq->body->parent_iseq;
	}
    }
    return 0;
}

int
rb_local_defined(ID id, const struct rb_block *base_block)
{
    const rb_iseq_t *iseq;

    if (base_block && (iseq = vm_block_iseq(base_block)) != NULL) {
	unsigned int i;
	iseq = iseq->body->local_iseq;

	for (i=0; i<iseq->body->local_table_size; i++) {
	    if (iseq->body->local_table[i] == id) {
		return 1;
	    }
	}
    }
    return 0;
}

static int
caller_location(VALUE *path, VALUE *absolute_path)
{
    const rb_thread_t *const th = GET_THREAD();
    const rb_control_frame_t *const cfp =
	rb_vm_get_ruby_level_next_cfp(th, th->cfp);

    if (cfp) {
	int line = rb_vm_get_sourceline(cfp);
	*path = cfp->iseq->body->location.path;
	*absolute_path = cfp->iseq->body->location.absolute_path;
	return line;
    }
    else {
	*path = rb_fstring_cstr("<compiled>");
	*absolute_path = *path;
	return 1;
    }
}

typedef struct {
    VALUE arg;
    rb_insn_func_t func;
    int line;
} accessor_args;

static const rb_iseq_t *
method_for_self(VALUE name, VALUE arg, rb_insn_func_t func,
		VALUE (*build)(rb_iseq_t *, LINK_ANCHOR *const, VALUE))
{
    VALUE path, absolute_path;
    accessor_args acc;

    acc.arg = arg;
    acc.func = func;
    acc.line = caller_location(&path, &absolute_path);
    return rb_iseq_new_with_opt((NODE *)IFUNC_NEW(build, (VALUE)&acc, 0),
				rb_sym2str(name), path, absolute_path,
				INT2FIX(acc.line), 0, ISEQ_TYPE_METHOD, 0);
}

static VALUE
for_self_aref(rb_iseq_t *iseq, LINK_ANCHOR *const ret, VALUE a)
{
    const accessor_args *const args = (void *)a;
    const int line = args->line;

    iseq_set_local_table(iseq, 0);
    iseq->body->param.lead_num = 0;
    iseq->body->param.size = 0;

    /* reserving stack slot for param */
    if (1 > (int) iseq->body->local_iseq->body->temp_vars_num)
	iseq->body->local_iseq->body->temp_vars_num = 1;
    iseq->body->call_c_func_p = TRUE;

    add_value_load(iseq, ret, line, -1, args->arg);
    ADD_INSN2(ret, line, call_c_func, (VALUE)args->func, INT2LINT(1));

    return Qnil;
}

static VALUE
for_self_aset(rb_iseq_t *iseq, LINK_ANCHOR *const ret, VALUE a)
{
    const accessor_args *const args = (void *)a;
    const int line = args->line;
    static const ID vars[] = {1, idUScore};

    iseq_set_local_table(iseq, vars);
    iseq->body->param.lead_num = 1;
    iseq->body->param.size = 1;

    /* reserving stack slots for params */
    if (2 > (int) iseq->body->local_iseq->body->temp_vars_num)
	iseq->body->local_iseq->body->temp_vars_num = 2;
    iseq->body->call_c_func_p = TRUE;

    add_local_move(iseq, ret, line, -1, numberof(vars) + VM_ENV_DATA_SIZE - 2);
    add_value_load(iseq, ret, line, -2, args->arg);
    ADD_INSN2(ret, line, call_c_func, (VALUE)args->func, INT2LINT(2));
    return Qnil;
}

/*
 * func (index) -> (value)
 */
const rb_iseq_t *
rb_method_for_self_aref(VALUE name, VALUE arg, rb_insn_func_t func)
{
    return method_for_self(name, arg, func, for_self_aref);
}

/*
 * func (index, value) -> (index, value)
 */
const rb_iseq_t *
rb_method_for_self_aset(VALUE name, VALUE arg, rb_insn_func_t func)
{
    return method_for_self(name, arg, func, for_self_aset);
}

/* ISeq binary format */

typedef unsigned int ibf_offset_t;
#define IBF_OFFSET(ptr) ((ibf_offset_t)(VALUE)(ptr))

struct ibf_header {
    char magic[4]; /* YARB */
    unsigned int major_version;
    unsigned int minor_version;
    unsigned int size;
    unsigned int extra_size;

    unsigned int iseq_list_size;
    unsigned int id_list_size;
    unsigned int object_list_size;

    ibf_offset_t iseq_list_offset;
    ibf_offset_t id_list_offset;
    ibf_offset_t object_list_offset;
};

struct ibf_id_entry {
    enum {
	ibf_id_enc_ascii,
	ibf_id_enc_utf8,
	ibf_id_enc_other
    } enc : 2;
    char body[1];
};

struct ibf_dump {
    VALUE str;
    VALUE iseq_list;      /* [iseq0 offset, ...] */
    VALUE obj_list;       /* [objs] */
    st_table *iseq_table; /* iseq -> iseq number */
    st_table *id_table;   /* id -> id number */
};

rb_iseq_t * iseq_alloc(void);

struct ibf_load {
    const char *buff;
    const struct ibf_header *header;
    ID *id_list;     /* [id0, ...] */
    VALUE iseq_list; /* [iseq0, ...] */
    VALUE obj_list;  /* [obj0, ...] */
    VALUE loader_obj;
    VALUE str;
    rb_iseq_t *iseq;
};

static ibf_offset_t
ibf_dump_pos(struct ibf_dump *dump)
{
    return (unsigned int)rb_str_strlen(dump->str);
}

static ibf_offset_t
ibf_dump_write(struct ibf_dump *dump, const void *buff, unsigned long size)
{
    ibf_offset_t pos = ibf_dump_pos(dump);
    rb_str_cat(dump->str, (const char *)buff, size);
    /* TODO: overflow check */
    return pos;
}

static void
ibf_dump_overwrite(struct ibf_dump *dump, void *buff, unsigned int size, long offset)
{
    VALUE str = dump->str;
    char *ptr = RSTRING_PTR(str);
    if ((unsigned long)(size + offset) > (unsigned long)RSTRING_LEN(str))
	rb_bug("ibf_dump_overwrite: overflow");
    memcpy(ptr + offset, buff, size);
}

static void *
ibf_load_alloc(const struct ibf_load *load, ibf_offset_t offset, int size)
{
    void *buff = ruby_xmalloc(size);
    memcpy(buff, load->buff + offset, size);
    return buff;
}

#define IBF_W(b, type, n) (type *)(VALUE)ibf_dump_write(dump, (b), sizeof(type) * (n))
#define IBF_WV(variable)   ibf_dump_write(dump, &(variable), sizeof(variable))
#define IBF_WP(b, type, n) ibf_dump_write(dump, (b), sizeof(type) * (n))
#define IBF_R(val, type, n) (type *)ibf_load_alloc(load, IBF_OFFSET(val), sizeof(type) * (n))

static int
ibf_table_lookup(struct st_table *table, st_data_t key)
{
    st_data_t val;

    if (st_lookup(table, key, &val)) {
	return (int)val;
    }
    else {
	return -1;
    }
}

static int
ibf_table_index(struct st_table *table, st_data_t key)
{
    int index = ibf_table_lookup(table, key);

    if (index < 0) { /* not found */
	index = (int)table->num_entries;
	st_insert(table, key, (st_data_t)index);
    }

    return index;
}

/* dump/load generic */

static VALUE ibf_load_object(const struct ibf_load *load, VALUE object_index);
static rb_iseq_t *ibf_load_iseq(const struct ibf_load *load, const rb_iseq_t *index_iseq);

static VALUE
ibf_dump_object(struct ibf_dump *dump, VALUE obj)
{
    long index = RARRAY_LEN(dump->obj_list);
    long i;
    for (i=0; i<index; i++) {
	if (RARRAY_AREF(dump->obj_list, i) == obj) return (VALUE)i; /* dedup */
    }
    rb_ary_push(dump->obj_list, obj);
    return (VALUE)index;
}

static VALUE
ibf_dump_id(struct ibf_dump *dump, ID id)
{
    return (VALUE)ibf_table_index(dump->id_table, (st_data_t)id);
}

static ID
ibf_load_id(const struct ibf_load *load, const ID id_index)
{
    ID id;

    if (id_index == 0) {
	id = 0;
    }
    else {
	id = load->id_list[(long)id_index];

	if (id == 0) {
	    long *indices = (long *)(load->buff + load->header->id_list_offset);
	    VALUE str = ibf_load_object(load, indices[id_index]);
	    id = NIL_P(str) ? 0 : rb_intern_str(str); /* str == nil -> internal junk id */
	    load->id_list[(long)id_index] = id;
	}
    }

    return id;
}

/* dump/load: code */

static VALUE
ibf_dump_calldata(struct ibf_dump *dump, const struct rb_call_data *cd)
{
    return (cd->call_info.flag & VM_CALL_KWARG) ? Qtrue : Qfalse;
}

static ibf_offset_t ibf_dump_iseq_each(struct ibf_dump *dump, const rb_iseq_t *iseq);

static rb_iseq_t *
ibf_dump_iseq(struct ibf_dump *dump, const rb_iseq_t *iseq)
{
    if (iseq == NULL) {
	return (rb_iseq_t *)-1;
    }
    else {
	int iseq_index = ibf_table_lookup(dump->iseq_table, (st_data_t)iseq);
	if (iseq_index < 0) {
	    iseq_index = ibf_table_index(dump->iseq_table, (st_data_t)iseq);
	    rb_ary_store(dump->iseq_list, iseq_index, LONG2NUM(ibf_dump_iseq_each(dump, rb_iseq_check(iseq))));
	}
	return (rb_iseq_t *)(VALUE)iseq_index;
    }
}

static VALUE
ibf_dump_gentry(struct ibf_dump *dump, const struct rb_global_entry *entry)
{
    return (VALUE)ibf_dump_id(dump, entry->id);
}

static VALUE
ibf_load_gentry(const struct ibf_load *load, const struct rb_global_entry *entry)
{
    ID gid = ibf_load_id(load, (ID)(VALUE)entry);
    return (VALUE)rb_global_entry(gid);
}

static VALUE *
ibf_dump_code(struct ibf_dump *dump, const rb_iseq_t *iseq)
{
    const int iseq_size = iseq->body->iseq_size;
    int code_index;
    VALUE *code;
    const VALUE *orig_code = rb_iseq_original_iseq(iseq);

    code = ALLOCA_N(VALUE, iseq_size);

    for (code_index=0; code_index<iseq_size;) {
	const VALUE insn = orig_code[code_index];
	const char *types = insn_op_types(insn);
	int op_index;

	code[code_index++] = (VALUE)insn;

	for (op_index=0; types[op_index]; op_index++, code_index++) {
	    VALUE op = orig_code[code_index];
	    switch (types[op_index]) {
	      case TS_CDHASH:
	      case TS_VALUE:
		code[code_index] = ibf_dump_object(dump, op);
		break;
	      case TS_ISEQ:
		code[code_index] = (VALUE)ibf_dump_iseq(dump, (const rb_iseq_t *)op);
		break;
	      case TS_IC:
		{
		    unsigned int i;
		    for (i=0; i<iseq->body->is_size; i++) {
			if (op == (VALUE)&iseq->body->is_entries[i]) {
			    break;
			}
		    }
		    code[code_index] = i;
		}
		break;
	      case TS_CALLINFO:
		/* TS_CALLINFO is not be used for RTL insns.  */
		break;
	      case TS_CALLCACHE:
		/* TS_CALLINFO is not be used for RTL insns.  */
		break;
	      case TS_ID:
		code[code_index] = ibf_dump_id(dump, (ID)op);
		break;
	      case TS_CALLDATA:
		code[code_index] = ibf_dump_calldata(dump, (const struct rb_call_data *)op);
		break;
	      case TS_GENTRY:
		code[code_index] = ibf_dump_gentry(dump, (const struct rb_global_entry *)op);
		break;
	      case TS_FUNCPTR:
		rb_raise(rb_eRuntimeError, "TS_FUNCPTR is not supported");
		break;
	      default:
		code[code_index] = op;
		break;
	    }
	}
	assert(insn_len(insn) == op_index+1);
    }

    return IBF_W(code, VALUE, iseq_size);
}

static VALUE *
ibf_load_code(const struct ibf_load *load, const rb_iseq_t *iseq, const struct rb_iseq_constant_body *body)
{
    const int iseq_size = body->iseq_size;
    int code_index;
    VALUE *code = IBF_R(body->iseq_encoded, VALUE, iseq_size);

    struct rb_call_data *cd_entries = iseq->body->cd_entries;
    struct rb_call_data_with_kwarg *cd_kw_entries = (struct rb_call_data_with_kwarg *)&iseq->body->cd_entries[iseq->body->cd_size];
    union iseq_inline_storage_entry *is_entries = iseq->body->is_entries;

    for (code_index=0; code_index<iseq_size;) {
	const VALUE insn = code[code_index++];
	const char *types = insn_op_types(insn);
	int op_index;

	for (op_index=0; types[op_index]; op_index++, code_index++) {
	    VALUE op = code[code_index];

	    switch (types[op_index]) {
	      case TS_CDHASH:
	      case TS_VALUE:
		code[code_index] = ibf_load_object(load, op);
		break;
	      case TS_ISEQ:
		code[code_index] = (VALUE)ibf_load_iseq(load, (const rb_iseq_t *)op);
		break;
	      case TS_IC:
		code[code_index] = (VALUE)&is_entries[(int)op];
		break;
	      case TS_CALLINFO:
		/* We don't use CALLINFO for RTL insns.  */
		break;
	      case TS_CALLCACHE:
		/* We don't use CALLCACHE for RTL insns.  */
		break;
	      case TS_CALLDATA:
		code[code_index] = op ? (VALUE)cd_kw_entries++ : (VALUE)cd_entries++; /* op is Qtrue (kw) or Qfalse (!kw) */
		break;
	      case TS_ID:
		code[code_index] = ibf_load_id(load, (ID)op);
		break;
	      case TS_GENTRY:
		code[code_index] = ibf_load_gentry(load, (const struct rb_global_entry *)op);
		break;
	      case TS_FUNCPTR:
		rb_raise(rb_eRuntimeError, "TS_FUNCPTR is not supported");
		break;
	      default:
		/* code[code_index] = op; */
		break;
	    }
	}
	assert(insn_len(insn) == op_index+1);
    };


    return code;
}

static VALUE *
ibf_dump_param_opt_table(struct ibf_dump *dump, const rb_iseq_t *iseq)
{
    int opt_num = iseq->body->param.opt_num;

    if (opt_num > 0) {
	return IBF_W(iseq->body->param.opt_table, VALUE, opt_num + 1);
    }
    else {
	return NULL;
    }
}

static VALUE *
ibf_load_param_opt_table(const struct ibf_load *load, const struct rb_iseq_constant_body *body)
{
    int opt_num = body->param.opt_num;

    if (opt_num > 0) {
	ibf_offset_t offset = IBF_OFFSET(body->param.opt_table);
	VALUE *table = ALLOC_N(VALUE, opt_num+1);
	MEMCPY(table, load->buff + offset, VALUE, opt_num+1);
	return table;
    }
    else {
	return NULL;
    }
}

static struct rb_iseq_param_keyword *
ibf_dump_param_keyword(struct ibf_dump *dump, const rb_iseq_t *iseq)
{
    const struct rb_iseq_param_keyword *kw = iseq->body->param.keyword;

    if (kw) {
	struct rb_iseq_param_keyword dump_kw = *kw;
	int dv_num = kw->num - kw->required_num;
	ID *ids = kw->num > 0 ? ALLOCA_N(ID, kw->num) : NULL;
	VALUE *dvs = dv_num > 0 ? ALLOCA_N(VALUE, dv_num) : NULL;
	int i;

	for (i=0; i<kw->num; i++) ids[i] = (ID)ibf_dump_id(dump, kw->table[i]);
	for (i=0; i<dv_num; i++) dvs[i] = (VALUE)ibf_dump_object(dump, kw->default_values[i]);

	dump_kw.table = IBF_W(ids, ID, kw->num);
	dump_kw.default_values = IBF_W(dvs, VALUE, dv_num);
	return IBF_W(&dump_kw, struct rb_iseq_param_keyword, 1);
    }
    else {
	return NULL;
    }
}

static const struct rb_iseq_param_keyword *
ibf_load_param_keyword(const struct ibf_load *load, const struct rb_iseq_constant_body *body)
{
    if (body->param.keyword) {
	struct rb_iseq_param_keyword *kw = IBF_R(body->param.keyword, struct rb_iseq_param_keyword, 1);
	ID *ids = IBF_R(kw->table, ID, kw->num);
	int dv_num = kw->num - kw->required_num;
	VALUE *dvs = IBF_R(kw->default_values, VALUE, dv_num);
	int i;

	for (i=0; i<kw->num; i++) {
	    ids[i] = ibf_load_id(load, ids[i]);
	}
	for (i=0; i<dv_num; i++) {
	    dvs[i] = ibf_load_object(load, dvs[i]);
	}

	kw->table = ids;
	kw->default_values = dvs;
	return kw;
    }
    else {
	return NULL;
    }
}

static struct iseq_line_info_entry *
ibf_dump_line_info_table(struct ibf_dump *dump, const rb_iseq_t *iseq)
{
    return IBF_W(iseq->body->line_info_table, struct iseq_line_info_entry, iseq->body->line_info_size);
}

static struct iseq_line_info_entry *
ibf_load_line_info_table(const struct ibf_load *load, const struct rb_iseq_constant_body *body)
{
    return IBF_R(body->line_info_table, struct iseq_line_info_entry, body->line_info_size);
}

static ID *
ibf_dump_local_table(struct ibf_dump *dump, const rb_iseq_t *iseq)
{
    const int size = iseq->body->local_table_size - 1;
    ID *table = ALLOCA_N(ID, size);
    int i;

    for (i=0; i<size; i++) {
	table[i] = ibf_dump_id(dump, iseq->body->local_table[i]);
    }

    return IBF_W(table, ID, size);
}

static ID *
ibf_load_local_table(const struct ibf_load *load, const struct rb_iseq_constant_body *body)
{
    const int size = body->local_table_size - 1;

    if (size > 0) {
	ID *table = IBF_R(body->local_table, ID, size);
	int i;

	for (i=0; i<size; i++) {
	    table[i] = ibf_load_id(load, table[i]);
	}
	return table;
    }
    else {
	return NULL;
    }
}

static struct iseq_catch_table *
ibf_dump_catch_table(struct ibf_dump *dump, const rb_iseq_t *iseq)
{
    const struct iseq_catch_table *table = iseq->body->catch_table;

    if (table) {
	int byte_size = iseq_catch_table_bytes(iseq->body->catch_table->size);
	struct iseq_catch_table *dump_table = (struct iseq_catch_table *)ALLOCA_N(char, byte_size);
	unsigned int i;
	dump_table->size = table->size;
	for (i=0; i<table->size; i++) {
	    dump_table->entries[i] = table->entries[i];
	    dump_table->entries[i].iseq = ibf_dump_iseq(dump, table->entries[i].iseq);
	}
	return (struct iseq_catch_table *)(VALUE)ibf_dump_write(dump, dump_table, byte_size);
    }
    else {
	return NULL;
    }
}

static struct iseq_catch_table *
ibf_load_catch_table(const struct ibf_load *load, const struct rb_iseq_constant_body *body)
{
    if (body->catch_table) {
	struct iseq_catch_table *table;
	unsigned int i;
	unsigned int size;
	size = *(unsigned int *)(load->buff + IBF_OFFSET(body->catch_table));
	table = ibf_load_alloc(load, IBF_OFFSET(body->catch_table), iseq_catch_table_bytes(size));
	for (i=0; i<size; i++) {
	    table->entries[i].iseq = ibf_load_iseq(load, table->entries[i].iseq);
	}
	return table;
    }
    else {
	return NULL;
    }
}

static struct rb_call_data *
ibf_dump_cd_entries(struct ibf_dump *dump, const rb_iseq_t *iseq)
{
    const unsigned int cd_size = iseq->body->cd_size;
    const unsigned int cd_kw_size = iseq->body->cd_kw_size;
    const struct rb_call_data *cd_entries = iseq->body->cd_entries;
    struct rb_call_data *dump_cd_entries;
    struct rb_call_data_with_kwarg *dump_cd_kw_entries;
    int byte_size = cd_size * sizeof(struct rb_call_data) +
                    cd_kw_size * sizeof(struct rb_call_data_with_kwarg);
    unsigned int i;

    dump_cd_entries = (struct rb_call_data *)ALLOCA_N(char, byte_size);
    dump_cd_kw_entries = (struct rb_call_data_with_kwarg *)&dump_cd_entries[cd_size];
    memcpy(dump_cd_entries, cd_entries, byte_size);

    for (i=0; i<cd_size; i++) { /* conver ID for each cd */
	dump_cd_entries[i].call_info.mid = ibf_dump_id(dump, dump_cd_entries[i].call_info.mid);
    }
    for (i=0; i<cd_kw_size; i++) {
	const struct rb_call_info_kw_arg *kw_arg = dump_cd_kw_entries[i].kw_arg;
	int j;
	VALUE *keywords = ALLOCA_N(VALUE, kw_arg->keyword_len);
	for (j=0; j<kw_arg->keyword_len; j++) {
	    keywords[j] = (VALUE)ibf_dump_object(dump, kw_arg->keywords[j]); /* kw_arg->keywords[n] is Symbol */
	}
	dump_cd_kw_entries[i].kw_arg = (struct rb_call_info_kw_arg *)(VALUE)ibf_dump_write(dump, &kw_arg->keyword_len, sizeof(int));
	ibf_dump_write(dump, keywords, sizeof(VALUE) * kw_arg->keyword_len);

	dump_cd_kw_entries[i].call_info.mid = ibf_dump_id(dump, dump_cd_kw_entries[i].call_info.mid);
    }
    return (struct rb_call_data *)(VALUE)ibf_dump_write(dump, dump_cd_entries, byte_size);
}

static struct rb_call_data *
ibf_load_cd_entries(const struct ibf_load *load, const struct rb_iseq_constant_body *body)
{
    unsigned int i;
    const unsigned int cd_size = body->cd_size;
    const unsigned int cd_kw_size = body->cd_kw_size;
    struct rb_call_data *cd_entries = ibf_load_alloc(load, IBF_OFFSET(body->cd_entries),
						     sizeof(struct rb_call_data) * body->cd_size +
						     sizeof(struct rb_call_data_with_kwarg) * body->cd_kw_size);
    struct rb_call_data_with_kwarg *cd_kw_entries = (struct rb_call_data_with_kwarg *)&cd_entries[cd_size];

    for (i=0; i<cd_size; i++) {
	cd_entries[i].call_info.mid = ibf_load_id(load, cd_entries[i].call_info.mid);
    }
    for (i=0; i<cd_kw_size; i++) {
	int j;
	ibf_offset_t kw_arg_offset = IBF_OFFSET(cd_kw_entries[i].kw_arg);
	const int keyword_len = *(int *)(load->buff + kw_arg_offset);
	const VALUE *keywords = (VALUE *)(load->buff + kw_arg_offset + sizeof(int));
	struct rb_call_info_kw_arg *kw_arg = ruby_xmalloc(sizeof(struct rb_call_info_kw_arg) + sizeof(VALUE) * (keyword_len - 1));
	kw_arg->keyword_len = keyword_len;
	for (j=0; j<kw_arg->keyword_len; j++) {
	    kw_arg->keywords[j] = (VALUE)ibf_load_object(load, keywords[j]);
	}
	cd_kw_entries[i].kw_arg = kw_arg;
	cd_kw_entries[i].call_info.mid = ibf_load_id(load, cd_kw_entries[i].call_info.mid);
    }

    return cd_entries;
}

static ibf_offset_t
ibf_dump_iseq_each(struct ibf_dump *dump, const rb_iseq_t *iseq)
{
    struct rb_iseq_constant_body dump_body;
    dump_body = *iseq->body;

    dump_body.location.path = ibf_dump_object(dump, dump_body.location.path);
    dump_body.location.absolute_path = ibf_dump_object(dump, dump_body.location.absolute_path);
    dump_body.location.base_label = ibf_dump_object(dump, dump_body.location.base_label);
    dump_body.location.label = ibf_dump_object(dump, dump_body.location.label);

    dump_body.iseq_encoded =    ibf_dump_code(dump, iseq);
    dump_body.param.opt_table = ibf_dump_param_opt_table(dump, iseq);
    dump_body.param.keyword =   ibf_dump_param_keyword(dump, iseq);
    dump_body.line_info_table = ibf_dump_line_info_table(dump, iseq);
    dump_body.local_table =     ibf_dump_local_table(dump, iseq);
    dump_body.catch_table =     ibf_dump_catch_table(dump, iseq);
    dump_body.parent_iseq =     ibf_dump_iseq(dump, iseq->body->parent_iseq);
    dump_body.local_iseq =      ibf_dump_iseq(dump, iseq->body->local_iseq);
    dump_body.is_entries =      NULL;
    dump_body.cd_entries =      ibf_dump_cd_entries(dump, iseq);
    dump_body.mark_ary =        ISEQ_FLIP_CNT(iseq);

    return ibf_dump_write(dump, &dump_body, sizeof(dump_body));
}

static VALUE
ibf_load_location_str(const struct ibf_load *load, VALUE str_index)
{
    VALUE str = ibf_load_object(load, str_index);
    if (str != Qnil) {
	str = rb_fstring(str);
    }
    return str;
}

static void
ibf_load_iseq_each(const struct ibf_load *load, rb_iseq_t *iseq, ibf_offset_t offset)
{
    struct rb_iseq_constant_body *load_body = iseq->body = ZALLOC(struct rb_iseq_constant_body);
    const struct rb_iseq_constant_body *body = (struct rb_iseq_constant_body *)(load->buff + offset);

    /* memcpy(load_body, load->buff + offset, sizeof(*load_body)); */
    load_body->type = body->type;
    load_body->stack_max = body->stack_max;
    load_body->iseq_size = body->iseq_size;
    load_body->param = body->param;
    load_body->local_table_size = body->local_table_size;
    load_body->is_size = body->is_size;
    load_body->cd_size = body->cd_size;
    load_body->cd_kw_size = body->cd_kw_size;
    load_body->line_info_size = body->line_info_size;

    RB_OBJ_WRITE(iseq, &load_body->mark_ary, iseq_mark_ary_create((int)body->mark_ary));

    RB_OBJ_WRITE(iseq, &load_body->location.path,          ibf_load_location_str(load, body->location.path));
    RB_OBJ_WRITE(iseq, &load_body->location.absolute_path, ibf_load_location_str(load, body->location.absolute_path));
    RB_OBJ_WRITE(iseq, &load_body->location.base_label,    ibf_load_location_str(load, body->location.base_label));
    RB_OBJ_WRITE(iseq, &load_body->location.label,         ibf_load_location_str(load, body->location.label));
    load_body->location.first_lineno = body->location.first_lineno;

    load_body->is_entries      = ZALLOC_N(union iseq_inline_storage_entry, body->is_size);
    load_body->cd_entries      = ibf_load_cd_entries(load, body);
    load_body->param.opt_table = ibf_load_param_opt_table(load, body);
    load_body->param.keyword   = ibf_load_param_keyword(load, body);
    load_body->line_info_table = ibf_load_line_info_table(load, body);
    load_body->local_table     = ibf_load_local_table(load, body);
    load_body->catch_table     = ibf_load_catch_table(load, body);
    load_body->parent_iseq     = ibf_load_iseq(load, body->parent_iseq);
    load_body->local_iseq      = ibf_load_iseq(load, body->local_iseq);

    load_body->iseq_encoded    = ibf_load_code(load, iseq, body);

    rb_iseq_translate_threaded_code(iseq);
}


static void
ibf_dump_iseq_list(struct ibf_dump *dump, struct ibf_header *header)
{
    const long size = RARRAY_LEN(dump->iseq_list);
    ibf_offset_t *list = ALLOCA_N(ibf_offset_t, size);
    long i;

    for (i=0; i<size; i++) {
	list[i] = (ibf_offset_t)NUM2LONG(rb_ary_entry(dump->iseq_list, i));
    }

    header->iseq_list_offset = ibf_dump_write(dump, list, sizeof(ibf_offset_t) * size);
    header->iseq_list_size = (unsigned int)size;
}

struct ibf_dump_id_list_i_arg {
    struct ibf_dump *dump;
    long *list;
    int current_i;
};

static int
ibf_dump_id_list_i(st_data_t key, st_data_t val, st_data_t ptr)
{
    struct ibf_dump_id_list_i_arg *arg = (struct ibf_dump_id_list_i_arg *)ptr;
    int i = (int)val;
    ID id = (ID)key;
    assert(arg->current_i == i);
    arg->current_i++;

    if (rb_id2name(id)) {
	arg->list[i] = (long)ibf_dump_object(arg->dump, rb_id2str(id));
    }
    else {
	arg->list[i] = 0;
    }

    return ST_CONTINUE;
}

static void
ibf_dump_id_list(struct ibf_dump *dump, struct ibf_header *header)
{
    const long size = dump->id_table->num_entries;
    struct ibf_dump_id_list_i_arg arg;
    arg.list = ALLOCA_N(long, size);
    arg.dump = dump;
    arg.current_i = 0;

    st_foreach(dump->id_table, ibf_dump_id_list_i, (st_data_t)&arg);

    header->id_list_offset = ibf_dump_write(dump, arg.list, sizeof(long) * size);
    header->id_list_size = (unsigned int)size;
}

#define IBF_OBJECT_INTERNAL FL_PROMOTED0

/*
 * Binary format
 * - ibf_object_header
 * - ibf_object_xxx (xxx is type)
 */

struct ibf_object_header {
    unsigned int type: 5;
    unsigned int special_const: 1;
    unsigned int frozen: 1;
    unsigned int internal: 1;
};

enum ibf_object_class_index {
    IBF_OBJECT_CLASS_OBJECT,
    IBF_OBJECT_CLASS_ARRAY,
    IBF_OBJECT_CLASS_STANDARD_ERROR
};

struct ibf_object_string {
    long encindex;
    long len;
    char ptr[1];
};

struct ibf_object_regexp {
    long srcstr;
    char option;
};

struct ibf_object_array {
    long len;
    long ary[1];
};

struct ibf_object_hash {
    long len;
    long keyval[1];
};

struct ibf_object_struct_range {
    long class_index;
    long len;
    long beg;
    long end;
    int excl;
};

struct ibf_object_bignum {
    ssize_t slen;
    BDIGIT digits[1];
};

enum ibf_object_data_type {
    IBF_OBJECT_DATA_ENCODING
};

struct ibf_object_complex_rational {
    long a, b;
};

struct ibf_object_symbol {
    long str;
};

#define IBF_OBJHEADER(offset)     (struct ibf_object_header *)(load->buff + (offset))
#define IBF_OBJBODY(type, offset) (type *)(load->buff + sizeof(struct ibf_object_header) + (offset))

static void
ibf_dump_object_unsupported(struct ibf_dump *dump, VALUE obj)
{
    rb_obj_info_dump(obj);
    rb_bug("ibf_dump_object_unsupported: unsupported");
}

static VALUE
ibf_load_object_unsupported(const struct ibf_load *load, const struct ibf_object_header *header, ibf_offset_t offset)
{
    rb_bug("unsupported");
    return Qnil;
}

static void
ibf_dump_object_class(struct ibf_dump *dump, VALUE obj)
{
    enum ibf_object_class_index cindex;
    if (obj == rb_cObject) {
	cindex = IBF_OBJECT_CLASS_OBJECT;
    }
    else if (obj == rb_cArray) {
	cindex = IBF_OBJECT_CLASS_ARRAY;
    }
    else if (obj == rb_eStandardError) {
	cindex = IBF_OBJECT_CLASS_STANDARD_ERROR;
    }
    else {
	rb_obj_info_dump(obj);
	rb_p(obj);
	rb_bug("unsupported class");
    }
    ibf_dump_write(dump, &cindex, sizeof(cindex));
}

static VALUE
ibf_load_object_class(const struct ibf_load *load, const struct ibf_object_header *header, ibf_offset_t offset)
{
    enum ibf_object_class_index *cindexp = IBF_OBJBODY(enum ibf_object_class_index, offset);
    enum ibf_object_class_index cindex = *cindexp;

    switch (cindex) {
      case IBF_OBJECT_CLASS_OBJECT:
	return rb_cObject;
      case IBF_OBJECT_CLASS_ARRAY:
	return rb_cArray;
      case IBF_OBJECT_CLASS_STANDARD_ERROR:
	return rb_eStandardError;
    }

    rb_bug("ibf_load_object_class: unknown class (%d)", (int)cindex);
}


static void
ibf_dump_object_float(struct ibf_dump *dump, VALUE obj)
{
    double dbl = RFLOAT_VALUE(obj);
    ibf_dump_write(dump, &dbl, sizeof(dbl));
}

static VALUE
ibf_load_object_float(const struct ibf_load *load, const struct ibf_object_header *header, ibf_offset_t offset)
{
    double *dblp = IBF_OBJBODY(double, offset);
    return DBL2NUM(*dblp);
}

static void
ibf_dump_object_string(struct ibf_dump *dump, VALUE obj)
{
    long encindex = (long)rb_enc_get_index(obj);
    long len = RSTRING_LEN(obj);
    const char *ptr = RSTRING_PTR(obj);

    if (encindex > RUBY_ENCINDEX_BUILTIN_MAX) {
	rb_encoding *enc = rb_enc_from_index((int)encindex);
	const char *enc_name = rb_enc_name(enc);
	encindex = RUBY_ENCINDEX_BUILTIN_MAX + ibf_dump_object(dump, rb_str_new2(enc_name));
    }

    IBF_WV(encindex);
    IBF_WV(len);
    IBF_WP(ptr, char, len);
}

static VALUE
ibf_load_object_string(const struct ibf_load *load, const struct ibf_object_header *header, ibf_offset_t offset)
{
    const struct ibf_object_string *string = IBF_OBJBODY(struct ibf_object_string, offset);
    VALUE str = rb_str_new(string->ptr, string->len);
    int encindex = (int)string->encindex;

    if (encindex > RUBY_ENCINDEX_BUILTIN_MAX) {
	VALUE enc_name_str = ibf_load_object(load, encindex - RUBY_ENCINDEX_BUILTIN_MAX);
	encindex = rb_enc_find_index(RSTRING_PTR(enc_name_str));
    }
    rb_enc_associate_index(str, encindex);

    if (header->internal) rb_obj_hide(str);
    if (header->frozen)   str = rb_fstring(str);

    return str;
}

static void
ibf_dump_object_regexp(struct ibf_dump *dump, VALUE obj)
{
    struct ibf_object_regexp regexp;
    regexp.srcstr = RREGEXP_SRC(obj);
    regexp.option = (char)rb_reg_options(obj);
    regexp.srcstr = (long)ibf_dump_object(dump, regexp.srcstr);
    IBF_WV(regexp);
}

static VALUE
ibf_load_object_regexp(const struct ibf_load *load, const struct ibf_object_header *header, ibf_offset_t offset)
{
    const struct ibf_object_regexp *regexp = IBF_OBJBODY(struct ibf_object_regexp, offset);
    VALUE srcstr = ibf_load_object(load, regexp->srcstr);
    VALUE reg = rb_reg_compile(srcstr, (int)regexp->option, NULL, 0);

    if (header->internal) rb_obj_hide(reg);
    if (header->frozen)   rb_obj_freeze(reg);

    return reg;
}

static void
ibf_dump_object_array(struct ibf_dump *dump, VALUE obj)
{
    long i, len = (int)RARRAY_LEN(obj);
    IBF_WV(len);
    for (i=0; i<len; i++) {
	long index = (long)ibf_dump_object(dump, RARRAY_AREF(obj, i));
	IBF_WV(index);
    }
}

static VALUE
ibf_load_object_array(const struct ibf_load *load, const struct ibf_object_header *header, ibf_offset_t offset)
{
    const struct ibf_object_array *array = IBF_OBJBODY(struct ibf_object_array, offset);
    VALUE ary = rb_ary_new_capa(array->len);
    int i;

    for (i=0; i<array->len; i++) {
	rb_ary_push(ary, ibf_load_object(load, array->ary[i]));
    }

    if (header->internal) rb_obj_hide(ary);
    if (header->frozen)   rb_obj_freeze(ary);

    return ary;
}

static int
ibf_dump_object_hash_i(st_data_t key, st_data_t val, st_data_t ptr)
{
    struct ibf_dump *dump = (struct ibf_dump *)ptr;
    long key_index = (long)ibf_dump_object(dump, (VALUE)key);
    long val_index = (long)ibf_dump_object(dump, (VALUE)val);
    IBF_WV(key_index);
    IBF_WV(val_index);
    return ST_CONTINUE;
}

static void
ibf_dump_object_hash(struct ibf_dump *dump, VALUE obj)
{
    long len = RHASH_SIZE(obj);
    IBF_WV(len);
    if (len > 0) st_foreach(RHASH(obj)->ntbl, ibf_dump_object_hash_i, (st_data_t)dump);
}

static VALUE
ibf_load_object_hash(const struct ibf_load *load, const struct ibf_object_header *header, ibf_offset_t offset)
{
    const struct ibf_object_hash *hash = IBF_OBJBODY(struct ibf_object_hash, offset);
    VALUE obj = rb_hash_new();
    int i;

    for (i=0; i<hash->len; i++) {
	VALUE key = ibf_load_object(load, hash->keyval[i*2  ]);
	VALUE val = ibf_load_object(load, hash->keyval[i*2+1]);
	rb_hash_aset(obj, key, val);
    }
    rb_hash_rehash(obj);

    if (header->internal) rb_obj_hide(obj);
    if (header->frozen)   rb_obj_freeze(obj);

    return obj;
}

static void
ibf_dump_object_struct(struct ibf_dump *dump, VALUE obj)
{
    if (rb_obj_is_kind_of(obj, rb_cRange)) {
	struct ibf_object_struct_range range;
	VALUE beg, end;
	range.len = 3;
	range.class_index = 0;

	rb_range_values(obj, &beg, &end, &range.excl);
	range.beg = (long)ibf_dump_object(dump, beg);
	range.end = (long)ibf_dump_object(dump, end);

	IBF_WV(range);
    }
    else {
	rb_bug("ibf_dump_object_struct: unsupported class");
    }
}

static VALUE
ibf_load_object_struct(const struct ibf_load *load, const struct ibf_object_header *header, ibf_offset_t offset)
{
    const struct ibf_object_struct_range *range = IBF_OBJBODY(struct ibf_object_struct_range, offset);
    VALUE beg = ibf_load_object(load, range->beg);
    VALUE end = ibf_load_object(load, range->end);
    VALUE obj = rb_range_new(beg, end, range->excl);
    if (header->internal) rb_obj_hide(obj);
    if (header->frozen)   rb_obj_freeze(obj);
    return obj;
}

static void
ibf_dump_object_bignum(struct ibf_dump *dump, VALUE obj)
{
    ssize_t len = BIGNUM_LEN(obj);
    ssize_t slen = BIGNUM_SIGN(obj) > 0 ? len : len * -1;
    BDIGIT *d = BIGNUM_DIGITS(obj);

    IBF_WV(slen);
    IBF_WP(d, BDIGIT, len);
}

static VALUE
ibf_load_object_bignum(const struct ibf_load *load, const struct ibf_object_header *header, ibf_offset_t offset)
{
    const struct ibf_object_bignum *bignum = IBF_OBJBODY(struct ibf_object_bignum, offset);
    int sign = bignum->slen > 0;
    ssize_t len = sign > 0 ? bignum->slen : -1 * bignum->slen;
    VALUE obj = rb_integer_unpack(bignum->digits, len * 2, 2, 0,
				  INTEGER_PACK_LITTLE_ENDIAN | (sign == 0 ? INTEGER_PACK_NEGATIVE : 0));
    if (header->internal) rb_obj_hide(obj);
    if (header->frozen)   rb_obj_freeze(obj);
    return obj;
}

static void
ibf_dump_object_data(struct ibf_dump *dump, VALUE obj)
{
    if (rb_data_is_encoding(obj)) {
	rb_encoding *enc = rb_to_encoding(obj);
	const char *name = rb_enc_name(enc);
	enum ibf_object_data_type type = IBF_OBJECT_DATA_ENCODING;
	long len = strlen(name) + 1;
	IBF_WV(type);
	IBF_WV(len);
	IBF_WP(name, char, strlen(name) + 1);
    }
    else {
	ibf_dump_object_unsupported(dump, obj);
    }
}

static VALUE
ibf_load_object_data(const struct ibf_load *load, const struct ibf_object_header *header, ibf_offset_t offset)
{
    const enum ibf_object_data_type *typep = IBF_OBJBODY(enum ibf_object_data_type, offset);
    /* const long *lenp = IBF_OBJBODY(long, offset + sizeof(enum ibf_object_data_type)); */
    const char *data = IBF_OBJBODY(char, offset + sizeof(enum ibf_object_data_type) + sizeof(long));

    switch (*typep) {
      case IBF_OBJECT_DATA_ENCODING:
	{
	    VALUE encobj = rb_enc_from_encoding(rb_enc_find(data));
	    return encobj;
	}
    }

    return ibf_load_object_unsupported(load, header, offset);
}

static void
ibf_dump_object_complex_rational(struct ibf_dump *dump, VALUE obj)
{
    long real = (long)ibf_dump_object(dump, RCOMPLEX(obj)->real);
    long imag = (long)ibf_dump_object(dump, RCOMPLEX(obj)->imag);

    IBF_WV(real);
    IBF_WV(imag);
}

static VALUE
ibf_load_object_complex_rational(const struct ibf_load *load, const struct ibf_object_header *header, ibf_offset_t offset)
{
    const struct ibf_object_complex_rational *nums = IBF_OBJBODY(struct ibf_object_complex_rational, offset);
    VALUE a = ibf_load_object(load, nums->a);
    VALUE b = ibf_load_object(load, nums->b);
    VALUE obj = header->type == T_COMPLEX ?
      rb_complex_new(a, b) : rb_rational_new(a, b);

    if (header->internal) rb_obj_hide(obj);
    if (header->frozen)   rb_obj_freeze(obj);
    return obj;
}

static void
ibf_dump_object_symbol(struct ibf_dump *dump, VALUE obj)
{
    VALUE str = rb_sym2str(obj);
    long str_index = (long)ibf_dump_object(dump, str);
    IBF_WV(str_index);
}

static VALUE
ibf_load_object_symbol(const struct ibf_load *load, const struct ibf_object_header *header, ibf_offset_t offset)
{
    /* const struct ibf_object_header *header = IBF_OBJHEADER(offset); */
    const struct ibf_object_symbol *symbol = IBF_OBJBODY(struct ibf_object_symbol, offset);
    VALUE str = ibf_load_object(load, symbol->str);
    ID id = rb_intern_str(str);
    return ID2SYM(id);
}

typedef void (*ibf_dump_object_function)(struct ibf_dump *dump, VALUE obj);
static ibf_dump_object_function dump_object_functions[RUBY_T_MASK+1] = {
    ibf_dump_object_unsupported, /* T_NONE */
    ibf_dump_object_unsupported, /* T_OBJECT */
    ibf_dump_object_class,       /* T_CLASS */
    ibf_dump_object_unsupported, /* T_MODULE */
    ibf_dump_object_float,       /* T_FLOAT */
    ibf_dump_object_string,      /* T_STRING */
    ibf_dump_object_regexp,      /* T_REGEXP */
    ibf_dump_object_array,       /* T_ARRAY */
    ibf_dump_object_hash,        /* T_HASH */
    ibf_dump_object_struct,      /* T_STRUCT */
    ibf_dump_object_bignum,      /* T_BIGNUM */
    ibf_dump_object_unsupported, /* T_FILE */
    ibf_dump_object_data,        /* T_DATA */
    ibf_dump_object_unsupported, /* T_MATCH */
    ibf_dump_object_complex_rational, /* T_COMPLEX */
    ibf_dump_object_complex_rational, /* T_RATIONAL */
    ibf_dump_object_unsupported, /* 0x10 */
    ibf_dump_object_unsupported, /* 0x11 T_NIL */
    ibf_dump_object_unsupported, /* 0x12 T_TRUE */
    ibf_dump_object_unsupported, /* 0x13 T_FALSE */
    ibf_dump_object_symbol,      /* 0x14 T_SYMBOL */
    ibf_dump_object_unsupported, /* T_FIXNUM */
    ibf_dump_object_unsupported, /* T_UNDEF */
    ibf_dump_object_unsupported, /* 0x17 */
    ibf_dump_object_unsupported, /* 0x18 */
    ibf_dump_object_unsupported, /* 0x19 */
    ibf_dump_object_unsupported, /* T_IMEMO 0x1a */
    ibf_dump_object_unsupported, /* T_NODE 0x1b */
    ibf_dump_object_unsupported, /* T_ICLASS 0x1c */
    ibf_dump_object_unsupported, /* T_ZOMBIE 0x1d */
    ibf_dump_object_unsupported, /* 0x1e */
    ibf_dump_object_unsupported  /* 0x1f */
};

static ibf_offset_t
lbf_dump_object_object(struct ibf_dump *dump, VALUE obj)
{
    struct ibf_object_header obj_header;
    ibf_offset_t current_offset = ibf_dump_pos(dump);
    obj_header.type = TYPE(obj);

    if (SPECIAL_CONST_P(obj)) {
	if (RB_TYPE_P(obj, T_SYMBOL) ||
	    RB_TYPE_P(obj, T_FLOAT)) {
	    obj_header.internal = FALSE;
	    goto dump_object;
	}
	obj_header.special_const = TRUE;
	obj_header.frozen = TRUE;
	obj_header.internal = TRUE;
	IBF_WV(obj_header);
	IBF_WV(obj);
    }
    else {
	obj_header.internal = (RBASIC_CLASS(obj) == 0) ? TRUE : FALSE;
      dump_object:
	obj_header.special_const = FALSE;
	obj_header.frozen = FL_TEST(obj, FL_FREEZE) ? TRUE : FALSE;
	IBF_WV(obj_header);
	(*dump_object_functions[obj_header.type])(dump, obj);
    }

    return current_offset;
}

typedef VALUE (*ibf_load_object_function)(const struct ibf_load *load, const struct ibf_object_header *header, ibf_offset_t);
static ibf_load_object_function load_object_functions[RUBY_T_MASK+1] = {
    ibf_load_object_unsupported, /* T_NONE */
    ibf_load_object_unsupported, /* T_OBJECT */
    ibf_load_object_class,       /* T_CLASS */
    ibf_load_object_unsupported, /* T_MODULE */
    ibf_load_object_float,       /* T_FLOAT */
    ibf_load_object_string,      /* T_STRING */
    ibf_load_object_regexp,      /* T_REGEXP */
    ibf_load_object_array,       /* T_ARRAY */
    ibf_load_object_hash,        /* T_HASH */
    ibf_load_object_struct,      /* T_STRUCT */
    ibf_load_object_bignum,      /* T_BIGNUM */
    ibf_load_object_unsupported, /* T_FILE */
    ibf_load_object_data,        /* T_DATA */
    ibf_load_object_unsupported, /* T_MATCH */
    ibf_load_object_complex_rational, /* T_COMPLEX */
    ibf_load_object_complex_rational, /* T_RATIONAL */
    ibf_load_object_unsupported, /* 0x10 */
    ibf_load_object_unsupported, /* T_NIL */
    ibf_load_object_unsupported, /* T_TRUE */
    ibf_load_object_unsupported, /* T_FALSE */
    ibf_load_object_symbol,
    ibf_load_object_unsupported, /* T_FIXNUM */
    ibf_load_object_unsupported, /* T_UNDEF */
    ibf_load_object_unsupported, /* 0x17 */
    ibf_load_object_unsupported, /* 0x18 */
    ibf_load_object_unsupported, /* 0x19 */
    ibf_load_object_unsupported, /* T_IMEMO 0x1a */
    ibf_load_object_unsupported, /* T_NODE 0x1b */
    ibf_load_object_unsupported, /* T_ICLASS 0x1c */
    ibf_load_object_unsupported, /* T_ZOMBIE 0x1d */
    ibf_load_object_unsupported, /* 0x1e */
    ibf_load_object_unsupported  /* 0x1f */
};

static VALUE
ibf_load_object(const struct ibf_load *load, VALUE object_index)
{
    if (object_index == 0) {
	return Qnil;
    }
    else if (object_index >= load->header->object_list_size) {
	rb_raise(rb_eIndexError, "object index out of range: %"PRIdVALUE, object_index);
    }
    else {
	VALUE obj = rb_ary_entry(load->obj_list, (long)object_index);
	if (obj == Qnil) { /* TODO: avoid multiple Qnil load */
	    ibf_offset_t *offsets = (ibf_offset_t *)(load->header->object_list_offset + load->buff);
	    ibf_offset_t offset = offsets[object_index];
	    const struct ibf_object_header *header = IBF_OBJHEADER(offset);

	    if (header->special_const) {
		VALUE *vp = IBF_OBJBODY(VALUE, offset);
		obj = *vp;
	    }
	    else {
		obj = (*load_object_functions[header->type])(load, header, offset);
	    }

	    rb_ary_store(load->obj_list, (long)object_index, obj);
	}
	iseq_add_mark_object(load->iseq, obj);
	return obj;
    }
}

static void
ibf_dump_object_list(struct ibf_dump *dump, struct ibf_header *header)
{
    VALUE list = rb_ary_tmp_new(RARRAY_LEN(dump->obj_list));
    int i, size;

    for (i=0; i<RARRAY_LEN(dump->obj_list); i++) {
	VALUE obj = RARRAY_AREF(dump->obj_list, i);
	ibf_offset_t offset = lbf_dump_object_object(dump, obj);
	rb_ary_push(list, UINT2NUM(offset));
    }
    size = i;
    header->object_list_offset = ibf_dump_pos(dump);

    for (i=0; i<size; i++) {
	ibf_offset_t offset = NUM2UINT(RARRAY_AREF(list, i));
	IBF_WV(offset);
    }

    header->object_list_size = size;
}

static void
ibf_dump_mark(void *ptr)
{
    struct ibf_dump *dump = (struct ibf_dump *)ptr;
    rb_gc_mark(dump->str);
    rb_gc_mark(dump->iseq_list);
    rb_gc_mark(dump->obj_list);
}

static void
ibf_dump_free(void *ptr)
{
    struct ibf_dump *dump = (struct ibf_dump *)ptr;
    if (dump->iseq_table) {
	st_free_table(dump->iseq_table);
	dump->iseq_table = 0;
    }
    if (dump->id_table) {
	st_free_table(dump->id_table);
	dump->id_table = 0;
    }
    ruby_xfree(dump);
}

static size_t
ibf_dump_memsize(const void *ptr)
{
    struct ibf_dump *dump = (struct ibf_dump *)ptr;
    size_t size = sizeof(*dump);
    if (dump->iseq_table) size += st_memsize(dump->iseq_table);
    if (dump->id_table) size += st_memsize(dump->id_table);
    return size;
}

static const rb_data_type_t ibf_dump_type = {
    "ibf_dump",
    {ibf_dump_mark, ibf_dump_free, ibf_dump_memsize,},
    0, 0, RUBY_TYPED_WB_PROTECTED | RUBY_TYPED_FREE_IMMEDIATELY
};

static void
ibf_dump_setup(struct ibf_dump *dump, VALUE dumper_obj)
{
    RB_OBJ_WRITE(dumper_obj, &dump->str, rb_str_new(0, 0));
    RB_OBJ_WRITE(dumper_obj, &dump->iseq_list, rb_ary_tmp_new(0));
    RB_OBJ_WRITE(dumper_obj, &dump->obj_list, rb_ary_tmp_new(1));
    rb_ary_push(dump->obj_list, Qnil); /* 0th is nil */
    dump->iseq_table = st_init_numtable(); /* need free */
    dump->id_table = st_init_numtable();   /* need free */

    ibf_table_index(dump->id_table, 0); /* id_index:0 is 0 */
}

VALUE
iseq_ibf_dump(const rb_iseq_t *iseq, VALUE opt)
{
    struct ibf_dump *dump;
    struct ibf_header header = {{0}};
    VALUE dump_obj;
    VALUE str;

    if (iseq->body->parent_iseq != NULL ||
	iseq->body->local_iseq != iseq) {
	rb_raise(rb_eRuntimeError, "should be top of iseq");
    }
    if (RTEST(ISEQ_COVERAGE(iseq))) {
	rb_raise(rb_eRuntimeError, "should not compile with coverage");
    }

    dump_obj = TypedData_Make_Struct(0, struct ibf_dump, &ibf_dump_type, dump);
    ibf_dump_setup(dump, dump_obj);

    ibf_dump_write(dump, &header, sizeof(header));
    ibf_dump_write(dump, RUBY_PLATFORM, strlen(RUBY_PLATFORM) + 1);
    ibf_dump_iseq(dump, iseq);

    header.magic[0] = 'Y'; /* YARB */
    header.magic[1] = 'A';
    header.magic[2] = 'R';
    header.magic[3] = 'B';
    header.major_version = ISEQ_MAJOR_VERSION;
    header.minor_version = ISEQ_MINOR_VERSION;
    ibf_dump_iseq_list(dump, &header);
    ibf_dump_id_list(dump, &header);
    ibf_dump_object_list(dump, &header);
    header.size = ibf_dump_pos(dump);

    if (RTEST(opt)) {
	VALUE opt_str = opt;
	const char *ptr = StringValuePtr(opt_str);
	header.extra_size = RSTRING_LENINT(opt_str);
	ibf_dump_write(dump, ptr, header.extra_size);
    }
    else {
	header.extra_size = 0;
    }

    ibf_dump_overwrite(dump, &header, sizeof(header), 0);

    str = dump->str;
    ibf_dump_free(dump);
    DATA_PTR(dump_obj) = NULL;
    RB_GC_GUARD(dump_obj);
    return str;
}

static const ibf_offset_t *
ibf_iseq_list(const struct ibf_load *load)
{
    return (ibf_offset_t *)(load->buff + load->header->iseq_list_offset);
}

void
ibf_load_iseq_complete(rb_iseq_t *iseq)
{
    struct ibf_load *load = RTYPEDDATA_DATA(iseq->aux.loader.obj);
    rb_iseq_t *prev_src_iseq = load->iseq;
    load->iseq = iseq;
    ibf_load_iseq_each(load, iseq, ibf_iseq_list(load)[iseq->aux.loader.index]);
    ISEQ_COMPILE_DATA(iseq) = NULL;
    FL_UNSET(iseq, ISEQ_NOT_LOADED_YET);
    load->iseq = prev_src_iseq;
}

#if USE_LAZY_LOAD
const rb_iseq_t *
rb_iseq_complete(const rb_iseq_t *iseq)
{
    ibf_load_iseq_complete((rb_iseq_t *)iseq);
    return iseq;
}
#endif

static rb_iseq_t *
ibf_load_iseq(const struct ibf_load *load, const rb_iseq_t *index_iseq)
{
    int iseq_index = (int)(VALUE)index_iseq;

    if (iseq_index == -1) {
	return NULL;
    }
    else {
	VALUE iseqv = rb_ary_entry(load->iseq_list, iseq_index);

	if (iseqv != Qnil) {
	    return (rb_iseq_t *)iseqv;
	}
	else {
	    rb_iseq_t *iseq = iseq_imemo_alloc();
	    FL_SET(iseq, ISEQ_NOT_LOADED_YET);
	    iseq->aux.loader.obj = load->loader_obj;
	    iseq->aux.loader.index = iseq_index;
	    rb_ary_store(load->iseq_list, iseq_index, (VALUE)iseq);

#if !USE_LAZY_LOAD
	    ibf_load_iseq_complete(iseq);
#endif /* !USE_LAZY_LOAD */

	    if (load->iseq) {
		iseq_add_mark_object(load->iseq, (VALUE)iseq);
	    }
	    return iseq;
	}
    }
}

static void
ibf_load_setup(struct ibf_load *load, VALUE loader_obj, VALUE str)
{
    rb_check_safe_obj(str);

    if (RSTRING_LENINT(str) < (int)sizeof(struct ibf_header)) {
	rb_raise(rb_eRuntimeError, "broken binary format");
    }
    RB_OBJ_WRITE(loader_obj, &load->str, str);
    load->loader_obj = loader_obj;
    load->buff = StringValuePtr(str);
    load->header = (struct ibf_header *)load->buff;
    RB_OBJ_WRITE(loader_obj, &load->iseq_list, rb_ary_tmp_new(0));
    RB_OBJ_WRITE(loader_obj, &load->obj_list, rb_ary_tmp_new(0));
    load->id_list = ZALLOC_N(ID, load->header->id_list_size);
    load->iseq = NULL;

    if (RSTRING_LENINT(str) < (int)load->header->size) {
	rb_raise(rb_eRuntimeError, "broken binary format");
    }
    if (strncmp(load->header->magic, "YARB", 4) != 0) {
	rb_raise(rb_eRuntimeError, "unknown binary format");
    }
    if (load->header->major_version != ISEQ_MAJOR_VERSION ||
	load->header->minor_version != ISEQ_MINOR_VERSION) {
	rb_raise(rb_eRuntimeError, "unmatched version file (%u.%u for %u.%u)",
		 load->header->major_version, load->header->minor_version, ISEQ_MAJOR_VERSION, ISEQ_MINOR_VERSION);
    }
    if (strcmp(load->buff + sizeof(struct ibf_header), RUBY_PLATFORM) != 0) {
	rb_raise(rb_eRuntimeError, "unmatched platform");
    }
}

static void
ibf_loader_mark(void *ptr)
{
    if (ptr) {
	struct ibf_load *load = (struct ibf_load *)ptr;
	rb_gc_mark(load->str);
	rb_gc_mark(load->iseq_list);
	rb_gc_mark(load->obj_list);
    }
}

static void
ibf_loader_free(void *ptr)
{
    if (ptr) {
	struct ibf_load *load = (struct ibf_load *)ptr;
	ruby_xfree(load->id_list);
	ruby_xfree(load);
    }
}

static size_t
ibf_loader_memsize(const void *ptr)
{
    struct ibf_load *load = (struct ibf_load *)ptr;
    return sizeof(struct ibf_load) + load->header->id_list_size * sizeof(ID);
}

static const rb_data_type_t ibf_load_type = {
    "ibf_loader",
    {ibf_loader_mark, ibf_loader_free, ibf_loader_memsize,},
    0, 0, RUBY_TYPED_WB_PROTECTED | RUBY_TYPED_FREE_IMMEDIATELY
};

const rb_iseq_t *
iseq_ibf_load(VALUE str)
{
    struct ibf_load *load;
    const rb_iseq_t *iseq;
    VALUE loader_obj = TypedData_Make_Struct(0, struct ibf_load, &ibf_load_type, load);

    ibf_load_setup(load, loader_obj, str);
    iseq = ibf_load_iseq(load, 0);

    RB_GC_GUARD(loader_obj);
    return iseq;
}

VALUE
iseq_ibf_load_extra_data(VALUE str)
{
    struct ibf_load *load;
    VALUE loader_obj = TypedData_Make_Struct(0, struct ibf_load, &ibf_load_type, load);
    VALUE extra_str;

    ibf_load_setup(load, loader_obj, str);
    extra_str = rb_str_new(load->buff + load->header->size, load->header->extra_size);
    RB_GC_GUARD(loader_obj);
    return extra_str;
}
