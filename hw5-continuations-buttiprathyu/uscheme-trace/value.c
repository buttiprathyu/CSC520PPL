#include "all.h"
int traceindent;
Name tracename;

/* value.c S172b */
bool istrue(Value v) {
    return v.alt != BOOLV || v.u.boolv;
}

Value truev, falsev;

void initvalue(void) {
    truev  = mkBoolv(true);
    falsev = mkBoolv(false);
}
/* value.c S173a */
Value unspecified (void) {
    switch ((rand()>>4) & 0x3) {
        case 0:  return truev;
        case 1:  return mkNum(rand());
        case 2:  return mkSym(strtoname("this value is unspecified"));
        case 3:  return mkPrimitive(-12, NULL);
        default: return mkNil();
    }
}
