#include <string.h>
#include "mir.h"

struct ptr_pair {
    const void *old;
    void *new;
};

typedef struct ptr_pair ptr_pair_t;

DEF_HTAB(ptr_pair_t);

static htab_hash_t ptr_pair_hash(ptr_pair_t pp, void *arg) {
    return (htab_hash_t)(uintptr_t)pp.old;
}

static int ptr_pair_eq_p(ptr_pair_t pp1, ptr_pair_t pp2, void *arg) {
    return pp1.old == pp2.old;
}

static void *find_new_ptr(HTAB(ptr_pair_t) *map, const void *old) {
    ptr_pair_t pp, tab_pp;

    assert(old != NULL);
    pp.old = old;
    if (HTAB_DO(ptr_pair_t, map, pp, HTAB_FIND, tab_pp))
        return tab_pp.new;
    return NULL;
}

static void insert_new_ptr(HTAB(ptr_pair_t) *map, const void *old, void *new) {
    int exist_p;
    ptr_pair_t pp, tab_pp;

    pp.old = old;
    pp.new = new;
    exist_p = HTAB_DO(ptr_pair_t, map, pp, HTAB_INSERT, tab_pp);
    assert(!exist_p);
}

static MIR_item_t get_new_item(HTAB(ptr_pair_t) *map, MIR_item_t old) {
    MIR_item_t new = find_new_ptr(map, old);
    assert(new != NULL);
    return new;
}

static void insert_new_item(HTAB(ptr_pair_t) *map, MIR_item_t old, MIR_item_t new) { insert_new_ptr(map, old, new); }

static MIR_insn_t get_label(MIR_context_t ctx, HTAB(ptr_pair_t) *map, MIR_insn_t old_label) {
    int find_p;
    ptr_pair_t pp, tab_pp;

    assert(old_label != NULL);
    pp.old = old_label;
    if (HTAB_DO(ptr_pair_t, map, pp, HTAB_FIND, tab_pp))
        return tab_pp.new;
    pp.new = MIR_new_label(ctx);
    HTAB_DO(ptr_pair_t, map, pp, HTAB_INSERT, tab_pp);
    return pp.new;
}

DEF_VARR(char);

DEF_VARR(MIR_op_t);

static int env_prefix_p(const char *name) {
    return strncmp(name, "sir_", 4) == 0 || strncmp(name, "vm_sir_", 7) == 0 || strncmp(name, "mirjit_", 7) == 0;
}

int main(int argc, const char *argv[]) {
    const char *name;
    MIR_op_t op;
    VARR(char) *txt;
    VARR(MIR_op_t) *ops;
    HTAB(ptr_pair_t) *map;
    MIR_insn_t insn, new_insn;
    MIR_module_t m, last_m, nm;
    DLIST(MIR_module_t) *module_list;
    MIR_context_t ctx = MIR_init ();
    FILE *out_env_file, *out_resolver_file;
    int ok_p;
    
    if (argc != 3) {
        fprintf(stderr, "Usage %s shrinked-mir-env-file mir-env-resolver-file < input-mir-envfile\n", argv[0]);
        exit(1);
    }
    if ((out_env_file = fopen(argv[1], "w")) == NULL) {
        fprintf(stderr, "can not create output mir file %s\n", argv[1]);
        exit(1);
    }
    if ((out_resolver_file = fopen(argv[2], "w")) == NULL) {
        fprintf(stderr, "can not create resolver file file %s\n", argv[2]);
        exit(1);
    }
    VARR_CREATE(char, txt, 8 * 1024 * 1024);
    for (int c = getchar(); c != EOF; c = getchar())
        VARR_PUSH(char, txt, c);
    VARR_PUSH(char, txt, 0);
    MIR_scan_string(ctx, VARR_ADDR(char, txt));
    VARR_DESTROY(char, txt);
    VARR_CREATE(MIR_op_t, ops, 16);
    HTAB_CREATE(ptr_pair_t, map, 1024, ptr_pair_hash, ptr_pair_eq_p, NULL);
    module_list = MIR_get_module_list(ctx);
    last_m = DLIST_TAIL(MIR_module_t, *module_list);
    for (m = DLIST_HEAD(MIR_module_t, *module_list);
         m != NULL;
         m = DLIST_NEXT(MIR_module_t, m)) {
        for (MIR_item_t item = DLIST_HEAD(MIR_item_t, m->items);
             item != NULL;
             item = DLIST_NEXT(MIR_item_t, item)) {
            if (item->item_type != MIR_func_item || !env_prefix_p(item->u.func->name))
                continue;
            insert_new_ptr(map, item->u.func->name, (void *)1);
            for (insn = DLIST_HEAD (MIR_insn_t, item->u.func->insns);
                 insn != NULL;
                 insn = DLIST_NEXT (MIR_insn_t, insn)) {
                for (int i = 0; i < insn->nops; i++) {
                    op = insn->ops[i];
                    if (op.mode != MIR_OP_REF) continue;
                    name = MIR_item_name (ctx, op.u.ref);
                    assert(name == _MIR_uniq_string(ctx, name));
                    if (find_new_ptr(map, name) == NULL)
                        insert_new_ptr(map, name, (void *)1);
                }
            }
        }
        nm = MIR_new_module(ctx, m->name);
        for (MIR_item_t item = DLIST_HEAD(MIR_item_t, m->items);
             item != NULL;
             item = DLIST_NEXT(MIR_item_t, item)) {
            MIR_item_t new_item;
            int insert_p = TRUE;

            name = MIR_item_name (ctx, item);
            if (name == NULL || find_new_ptr(map, name) == NULL) {
#if 0
                if (name != NULL && !item->export_p && strncmp(name, "S0_", 3) != 0 && strncmp(name, "attr_", 5) != 0)
                    fprintf(stderr, ">>>%s\n", name);
#endif
                continue;
            }
            switch (item->item_type) {
            case MIR_export_item:
                if (!env_prefix_p(item->u.export_id)) {
                    new_item = MIR_new_import(ctx, item->u.export_id);
                } else {
                    new_item = MIR_new_export(ctx, item->u.export_id);
                }
                break;
            case MIR_func_item:
                if (!env_prefix_p(item->u.func->name)) {
                    fprintf(out_resolver_file, "  {\"%s\", %s},\n", name, name);
                    new_item = MIR_new_import(ctx, item->u.func->name);
                    break;
                }
                new_item = (item->u.func->vararg_p ? MIR_new_vararg_func_arr : MIR_new_func_arr)
                    (ctx, item->u.func->name, item->u.func->nres, item->u.func->res_types,
                     item->u.func->nargs, VARR_ADDR(MIR_var_t, item->u.func->vars));
                for (int i = item->u.func->nargs; i < (int)VARR_LENGTH(MIR_var_t, item->u.func->vars); i++)
                    MIR_new_func_reg(ctx, new_item->u.func,
                                     VARR_ADDR(MIR_var_t, item->u.func->vars)[i].type,
                                     VARR_ADDR(MIR_var_t, item->u.func->vars)[i].name);
                insert_new_item(map, item, new_item);
                insert_p = FALSE;
                for (insn = DLIST_HEAD (MIR_insn_t, item->u.func->insns);
                     insn != NULL;
                     insn = DLIST_NEXT (MIR_insn_t, insn)) {
                    if (insn->code == MIR_LABEL) {
                        MIR_append_insn(ctx, new_item, get_label(ctx, map, insn));
                        continue;
                    }
                    VARR_TRUNC(MIR_op_t, ops, 0);
                    for (int i = 0; i < insn->nops; i++) {
                        op = insn->ops[i];
                        switch (op.mode) {
                        case MIR_OP_REG:
                            op = MIR_new_reg_op(ctx, MIR_reg(ctx,
                                                             MIR_reg_name(ctx, op.u.reg, item->u.func),
                                                             new_item->u.func));
                            break;
                        case MIR_OP_MEM:
                            if (op.u.mem.base != 0)
                                op.u.mem.base = MIR_reg(ctx,
                                                        MIR_reg_name(ctx, op.u.mem.base, item->u.func),
                                                        new_item->u.func);
                            if (op.u.mem.index != 0)
                                op.u.mem.index = MIR_reg(ctx,
                                                         MIR_reg_name(ctx, op.u.mem.index, item->u.func),
                                                         new_item->u.func);
                            break;
                        case MIR_OP_REF:
                            op.u.ref = get_new_item(map, op.u.ref);
                            break;
                        case MIR_OP_LABEL:
                            op.u.label = get_label(ctx, map, op.u.label);
                            break;
                        default:
                        }
                        VARR_PUSH(MIR_op_t, ops, op);
                    }
                    new_insn = MIR_new_insn_arr (ctx, insn->code, VARR_LENGTH(MIR_op_t, ops),
                                                 VARR_ADDR(MIR_op_t, ops));
                    MIR_append_insn(ctx, new_item, new_insn);
                }
                MIR_finish_func (ctx);
                break;
            case MIR_proto_item:
                new_item = (item->u.proto->vararg_p ? MIR_new_vararg_proto_arr : MIR_new_proto_arr)
                    (ctx, item->u.proto->name, item->u.proto->nres, item->u.proto->res_types,
                     VARR_LENGTH(MIR_var_t, item->u.proto->args), VARR_ADDR(MIR_var_t, item->u.proto->args));
                break;
            case MIR_import_item:
                new_item = MIR_new_import(ctx, item->u.import_id);
            case MIR_forward_item:
                new_item = MIR_new_import(ctx, item->u.forward_id);
                break;
            case MIR_data_item:
                if (item->u.data->name != NULL
                    && strcmp(item->u.data->name + strlen(item->u.data->name) - 8, "__func__") == 0) {
                    new_item = MIR_new_data (ctx, item->u.data->name, item->u.data->el_type, item->u.data->nel,
                                             item->u.data->u.els);
                    break;
                }
                fprintf(stderr, "%s: environment can not have data", item->u.data->name);
                exit(1);
            case MIR_ref_data_item:
                fprintf(stderr, "%s: environment can not have ref data\n", item->u.ref_data->name);
                exit(1);
            case MIR_expr_data_item:
                fprintf(stderr, "%s: environment can not have expr data\n", item->u.expr_data->name);
                exit(1);
            case MIR_bss_item:
                fprintf(stderr, "%s: environment can not have bss\n", item->u.bss->name);
                exit(1);
            default:
                assert(FALSE);
            }
            if (insert_p)
                insert_new_item(map, item, new_item);
        }
        MIR_finish_module(ctx);
        MIR_write_module(ctx, out_env_file, nm);
        if (m == last_m)
            break;
    }
    HTAB_DESTROY(ptr_pair_t, map);
    VARR_DESTROY(MIR_op_t, ops);
    MIR_finish(ctx);
    ok_p = ferror(out_resolver_file) == 0;
    ok_p = ferror(out_env_file) == 0 && ok_p;
    ok_p = fclose(out_resolver_file) == 0 && ok_p;
    ok_p = fclose(out_env_file) == 0 && ok_p;
    return ok_p ? 0 : 1;
}
