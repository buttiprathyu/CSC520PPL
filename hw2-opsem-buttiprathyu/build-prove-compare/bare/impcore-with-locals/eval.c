#include "all.h"
/* eval.c 48c */
static Valuelist evallist(Explist es, Valenv globals, Funenv functions, Valenv
                                                       formals, Valenv locals);

/* eval.c 49a */
/* locals are included as a parameter to all calls to eval */
Value eval(Exp e, Valenv globals, Funenv functions, Valenv formals, Valenv
                                                                locals) {
    checkoverflow(1000000 * sizeof(char *));
                                        // see last section of Appendix A (OMIT)
    switch (e->alt) {
    case LITERAL:
        /* evaluate [[e->u.literal]] and return the result 49b */
        return e->u.literal;
    case VAR:
        /* evaluate [[e->u.var]] and return the result 50a */
        /* adding locals as first condition*/
        if (isvalbound(e->u.var, locals))
            return fetchval(e->u.var, locals);
        else if (isvalbound(e->u.var, formals))
            return fetchval(e->u.var, formals);
        else if (isvalbound(e->u.var, globals))
            return fetchval(e->u.var, globals);
        else
            runerror("unbound variable %n", e->u.var);
        assert(0);   // not reached, but the compiler can't know
        // return 0;
    case SET:
        /* evaluate [[e->u.set]] and return the result 50b */
        {
            Value v = eval(e->u.set.exp, globals, functions, formals, locals);
            if (isvalbound(e->u.set.name, locals))
                bindval(e->u.set.name, v, locals);
            else if (isvalbound(e->u.set.name, formals))
                bindval(e->u.set.name, v, formals);
            else if (isvalbound(e->u.set.name, globals))
                bindval(e->u.set.name, v, globals);
            else
               runerror("tried to set unbound variable %n in %e", e->u.set.name
                                                                          , e);
            return v;
        }
    case IFX:
        /* evaluate [[e->u.ifx]] and return the result 51a */
        if (eval(e->u.ifx.cond, globals, functions, formals, locals) != 0)
            return eval(e->u.ifx.truex, globals, functions, formals, locals);
        else
            return eval(e->u.ifx.falsex, globals, functions, formals, locals);
    case WHILEX:
        /* evaluate [[e->u.whilex]] and return the result 51b */
       while (eval(e->u.whilex.cond, globals, functions, formals, locals) != 0)
            eval(e->u.whilex.exp, globals, functions, formals, locals);
       return 0;
    case BEGIN:
        /* evaluate [[e->u.begin]] and return the result 52a */
        {
            Value lastval = 0;
            for (Explist es = e->u.begin; es; es = es->tl)
                lastval = eval(es->hd, globals, functions, formals, locals);
            return lastval;
        }
    case APPLY:
        /* evaluate [[e->u.apply]] and return the result 52b */
        {
            Fun f;

/* make [[f]] the function denoted by [[e->u.apply.name]], or call [[runerror]] 52c */
            if (!isfunbound(e->u.apply.name, functions))
                runerror("call to undefined function %n in %e",
                                            e->u.apply.name, e);
            f = fetchfun(e->u.apply.name, functions);
            switch (f.alt) {
            case USERDEF:
                /* apply [[f.u.userdef]] and return the result 53b */
                {
                    /* creating new local lists */
                    Namelist local_xs = f.u.userdef.locals;
                    Valuelist local_vs = NULL;
                    int i = 0;

                    Namelist  xs = f.u.userdef.formals;
                    Valuelist vs = evallist(e->u.apply.actuals, globals,
                                                  functions, formals, locals);
                    checkargc(e, lengthNL(xs), lengthVL(vs));


                    /* initialize local variables to zero */
                    while(i < lengthNL(local_xs)){
                        local_vs = mkVL(0, local_vs);
                        i = lengthVL(local_vs);
                    }

                    checkargc(e, lengthNL(local_xs), lengthVL(local_vs));
                    return eval(f.u.userdef.body, globals, functions, mkValenv(
                                       xs, vs), mkValenv(local_xs, local_vs));

                }
            case PRIMITIVE:
                /* apply [[f.u.primitive]] and return the result 54a */
                {
                    Valuelist vs = evallist(e->u.apply.actuals, globals,
                                                   functions, formals, locals);
                    if (f.u.primitive == strtoname("print"))

              /* apply \impcore\ primitive [[print]] to [[vs]] and return 54b */
                        {
                            checkargc(e, 1, lengthVL(vs));
                            Value v = nthVL(vs, 0);
                            print("%v", v);
                            return v;
                        }
                    else if (f.u.primitive == strtoname("println"))

          /* apply \impcore\ primitive [[println]] to [[vs]] and return S142d */
                        {
                            checkargc(e, 1, lengthVL(vs));
                            Value v = nthVL(vs, 0);
                            print("%v\n", v);
                            return v;
                        }
                    else if (f.u.primitive == strtoname("printu"))

           /* apply \impcore\ primitive [[printu]] to [[vs]] and return S143a */
                        {
                            checkargc(e, 1, lengthVL(vs));
                            Value v = nthVL(vs, 0);
                            print_utf8(v);
                            return v;
                        }
                    else

                       /* apply arithmetic primitive to [[vs]] and return 55a */
                        {
                            const char *s = nametostr(f.u.primitive);
                            Value v, w;

/* check that [[vs]] has exactly two values, and assign them to [[v]] and [[w]] 55c */
                            checkargc(e, 2, lengthVL(vs));
                            v = nthVL(vs, 0);
                            w = nthVL(vs, 1);

/* if operation [[s]] would overflow on [[v]] and [[w]], call [[runerror]] 55b */
                            checkarith(s[0], v, w, 32);
                            assert(strlen(s) == 1);

                            switch (s[0]) {
                            case '<':
                                return v < w;
                            case '>':
                                return v > w;
                            case '=':
                                return v == w;
                            case '+':
                                return v + w;
                            case '-':
                                return v - w;
                            case '*':
                                return v * w;
                            case '/':
                                if (w == 0)
                                    runerror("division by zero in %e", e);
                                return v / w;
                            default:
                                assert(0);
                            }
                        }
                }
            default:
                assert(0);
            }
        }
    }
    assert(0);
}
/* eval.c 53a */
static Valuelist evallist(Explist es, Valenv globals, Funenv functions, Valenv
                                                    formals, Valenv locals) {
    if (es == NULL) {
        return NULL;
    } else {
        Value v = eval(es->hd, globals, functions, formals, locals);
        return mkVL(v, evallist(es->tl, globals, functions, formals, locals));
    }
}
/* eval.c 56a */
void evaldef(Def d, Valenv globals, Funenv functions, Echo echo) {
    switch (d->alt) {
    case VAL:
        /* evaluate [[d->u.val]], mutating [[globals]] 56b */
        {
            Value v = eval(d->u.val.exp, globals, functions, mkValenv(NULL, NULL
                                                      ), mkValenv(NULL, NULL));
            bindval(d->u.val.name, v, globals);
            if (echo == ECHOES)
                print("%v\n", v);
        }
        return;
    case EXP:
        /* evaluate [[d->u.exp]] and possibly print the result 56c */
        {
            Value v = eval(d->u.exp, globals, functions, mkValenv(NULL, NULL),
                                                      mkValenv(NULL, NULL));
            bindval(strtoname("it"), v, globals);
            if (echo == ECHOES)
                print("%v\n", v);
        }
        return;
    case DEFINE:
        /* evaluate [[d->u.define]], mutating [[functions]] 57a */
        if(duplicatename(d->u.define.userfun.formals) != NULL)
           runerror(
           "Formal parameter %n appears twice in definition of function %n",
           duplicatename(d->u.define.userfun.formals), d->u.define.name);
        bindfun(d->u.define.name, mkUserdef(d->u.define.userfun), functions);
        if (echo == ECHOES)
            print("%n\n", d->u.define.name);
        return;
    }
    assert(0);
}
/* eval.c S132a */
void readevalprint(XDefstream xdefs, Valenv globals, Funenv functions, Echo echo
                                                                           ) {
    UnitTestlist pending_unit_tests = NULL;
                                            // to be run when xdefs is exhausted

    for (XDef d = getxdef(xdefs); d; d = getxdef(xdefs))
        switch (d->alt) {
        case TEST:
            pending_unit_tests = mkUL(d->u.test, pending_unit_tests);
            break;
        case USE:

/* evaluate [[d->u.use]], possibly mutating [[globals]] and [[functions]] S132c */
            {
                const char *filename = nametostr(d->u.use);
                FILE *fin = fopen(filename, "r");
                if (fin == NULL)
                    runerror("cannot open file \"%s\"", filename);
                readevalprint(filexdefs(filename, fin, NO_PROMPTS), globals,
                                                              functions, echo);
                fclose(fin);
            }
            break;
        case DEF:
            evaldef(d->u.def, globals, functions, echo);
            break;
        default:
            assert(0);
        }
    reset_overflow_check();     /* OMIT */

    process_tests(pending_unit_tests, globals, functions);
}
