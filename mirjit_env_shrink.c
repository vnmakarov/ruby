#include "mir.h"
DEF_VARR(char);

int main(void) {
    MIR_context_t ctx = MIR_init ();
    VARR(char) *txt;

    VARR_CREATE(char, txt, 8 * 1024 * 1024);
    for (int c = getchar(); c != EOF; c = getchar())
        VARR_PUSH(char, txt, c);
    VARR_PUSH(char, txt, 0);
    MIR_scan_string(ctx, VARR_ADDR(char, txt));
    MIR_write (ctx, stdout);
    MIR_finish(ctx);
    VARR_DESTROY(char, txt);
    return 0;
}
