#include "all.h"
int traceindent;
Name tracename;

/* error.c S24b */
jmp_buf errorjmp;
jmp_buf testjmp;

static ErrorMode mode = NORMAL;
/* error.c S24c */
void set_error_mode(ErrorMode new_mode) {
  assert(new_mode == NORMAL || new_mode == TESTING);
  mode = new_mode;
}
/* error.c S25 */
Printbuf errorbuf;
void runerror(const char *fmt, ...) {
    extern int traceindent;

    va_list_box box;

    if (!errorbuf)
        errorbuf = printbuf();

    assert(fmt);
    va_start(box.ap, fmt);
    vbprint(errorbuf, fmt, &box);
    va_end(box.ap);

    switch (mode) {
    case NORMAL:
        fflush(stdout);
        char *msg = bufcopy(errorbuf);
        fprintf(stderr, "Run-time error: %s\n", msg);
        fflush(stderr);
        free(msg);
        bufreset(errorbuf);
    traceindent = 0;

        longjmp(errorjmp, 1);

    case TESTING:
        longjmp(testjmp, 1);

    default:
        assert(0);
    }
}
/* error.c S26a */
static ErrorFormat toplevel_error_format = WITH_LOCATIONS;

void synerror(Sourceloc src, const char *fmt, ...) {
    extern int traceindent;

    va_list_box box;

    switch (mode) {
    case NORMAL:
        assert(fmt);
        fflush(stdout);
        if (toplevel_error_format == WITHOUT_LOCATIONS
        && !strcmp(src->sourcename, "standard input"))
            fprint(stderr, "syntax error: ");
        else
            fprint(stderr, "syntax error in %s, line %d: ", src->sourcename, src
                                                                        ->line);
        Printbuf buf = printbuf();
        va_start(box.ap, fmt);
        vbprint(buf, fmt, &box);
        va_end(box.ap);

        fwritebuf(buf, stderr);
        freebuf(&buf);
        fprintf(stderr, "\n");
        fflush(stderr);
    traceindent = 0;

        longjmp(errorjmp, 1);

    default:
        assert(0);
    }
}
/* error.c S26b */
void set_toplevel_error_format(ErrorFormat new_format) {
  assert(new_format == WITH_LOCATIONS || new_format == WITHOUT_LOCATIONS);
  toplevel_error_format = new_format;
}
/* error.c S26c */
void checkargc(Exp e, int expected, int actual) {
    if (expected != actual)
        runerror("in %e, expected %d argument%s but found %d",
                 e, expected, expected == 1 ? "" : "s", actual);
}
/* error.c S27a */
Name duplicatename(Namelist xs) {
    if (xs != NULL) {
        Name n = xs->hd;
        for (Namelist tail = xs->tl; tail; tail = tail->tl)
            if (n == tail->hd)
                return n;
        return duplicatename(xs->tl);
    }
    return NULL;
}
