#include "all.h"
/* parse.c S167a */
struct Usage usage_table[] = {
    { ADEF(VAL),           "(val x e)" },
    { ADEF(DEFINE),        "(define fun (formals) body)" },
    { ANXDEF(USE),         "(use filename)" },
    { ATEST(CHECK_EXPECT), "(check-expect exp-to-run exp-expected)" },
    { ATEST(CHECK_ASSERT), "(check-assert exp)" },
    { ATEST(CHECK_ERROR),  "(check-error exp)" },

    { SET,     "(set x e)" },
    { IFX,     "(if cond true false)" },
    { WHILEX,  "(while cond body)" },
    { BEGIN,   "(begin exp ... exp)" },
    { LAMBDAX, "(lambda (formals) body)" },

    { ALET(LET),     "(let ((var exp) ...) body)" },
    { ALET(LETSTAR), "(let* ((var exp) ...) body)" },
    { ALET(LETREC),  "(letrec ((var exp) ...) body)" },
    /* \uscheme\ [[usage_table]] entries added in exercises S169d */
    /* add expected usage for each new syntactic form */
    /* \uscheme\ [[usage_table]] entries added in exercises ((answers)) S457f */
    { SUGAR(CAND), "(&& exp ...)" },
    /* \uscheme\ [[usage_table]] entries added in exercises ((answers)) S458d */
    { SUGAR(COR), "(|| exp ...)" },
    /* \uscheme\ [[usage_table]] entries added in exercises ((answers)) S460c */
    { SUGAR(COND), "(cond (question-exp answer-exp) ...)" },
    /* \uscheme\ [[usage_table]] entries added in exercises ((answers)) S461e */
    { SUGAR(RECORD), "(record record-name (field-name ...))" },
    { -1, NULL }
};
/* parse.c S167c */
static ShiftFun quoteshifts[] = { sSexp,                 stop };
static ShiftFun setshifts[]   = { sName, sExp,           stop };
static ShiftFun ifshifts[]    = { sExp, sExp, sExp,      stop };
static ShiftFun whileshifts[] = { sExp, sExp,            stop };
static ShiftFun beginshifts[] = { sExps,                 stop };
static ShiftFun letshifts[]   = { sBindings, sExp,       stop };
static ShiftFun lambdashifts[]= { sNamelist, sExp,       stop };
static ShiftFun applyshifts[] = { sExp, sExps,           stop };
/* arrays of shift functions added to \uscheme\ in exercises S168e */
/* define arrays of shift functions as needed for [[exptable]] rows */
/* arrays of shift functions added to \uscheme\ in exercises ((answers)) S457d */
static ShiftFun conditionalshifts[] = { sExps, stop };
/* arrays of shift functions added to \uscheme\ in exercises ((answers)) S460a */
static ShiftFun condshifts[] = { sQas, stop };
/* arrays of shift functions added to \uscheme\ in exercises ((answers)) S461b */
ShiftFun recordshifts[] = { sName, setcontextname, sNamelist, stop };

struct ParserRow exptable[] = {
  { "set",    ANEXP(SET),     setshifts },
  { "if",     ANEXP(IFX),     ifshifts },
  { "while",  ANEXP(WHILEX),  whileshifts },
  { "begin",  ANEXP(BEGIN),   beginshifts },
  { "let",    ALET(LET),      letshifts },
  { "let*",   ALET(LETSTAR),  letshifts },
  { "letrec", ALET(LETREC),   letshifts },
  { "lambda", ANEXP(LAMBDAX), lambdashifts },
  { "quote",  ANEXP(LITERAL), quoteshifts }, 
  /* rows added to \uscheme's [[exptable]] in exercises S169a */
  /* add a row for each new syntactic form of Exp */
  /* rows added to \uscheme's [[exptable]] in exercises ((answers)) S457e */
  { "&&", SUGAR(CAND), conditionalshifts },
  /* rows added to \uscheme's [[exptable]] in exercises ((answers)) S458c */
  { "||", SUGAR(COR), conditionalshifts },
  /* rows added to \uscheme's [[exptable]] in exercises ((answers)) S460b */
  { "cond", SUGAR(COND), condshifts },
  { NULL,     ANEXP(APPLY),   applyshifts }  // must come last
};
/* parse.c S168a */
bool read_tick_as_quote = true;
/* parse.c S168b */
Exp reduce_to_exp(int code, struct Component *comps) {
    switch(code) {
    case ANEXP(SET):     return mkSet(comps[0].name, comps[1].exp);
    case ANEXP(IFX):     return mkIfx(comps[0].exp, comps[1].exp, comps[2].exp);
    case ANEXP(WHILEX):  return mkWhilex(comps[0].exp, comps[1].exp);
    case ANEXP(BEGIN):   return mkBegin(comps[0].exps);
    case ALET(LET):
    case ALET(LETSTAR):
    case ALET(LETREC):   return mkLetx(code+LET-ALET(LET), 
                                       comps[0].names, comps[0].exps, comps[1].
                                                                           exp);
    case ANEXP(LAMBDAX): return mkLambdax(mkLambda(comps[0].names, comps[1].exp)
                                                                              );
    case ANEXP(APPLY):   return mkApply(comps[0].exp, comps[1].exps);
    case ANEXP(LITERAL): return mkLiteral(comps[0].value);
    /* cases for \uscheme's [[reduce_to_exp]] added in exercises S169b */
    /* add a case for each new syntactic form of Exp */

/* cases for \uscheme's [[reduce_to_exp]] added in exercises ((answers)) S457b */
    // case ALET(LETSTAR):
    //   return desugarLetStar(comps[0].names, comps[0].exps, comps[1].exp);

/* cases for \uscheme's [[reduce_to_exp]] added in exercises ((answers)) S457c */
    case SUGAR(CAND): return desugarAnd(comps[0].exps);

/* cases for \uscheme's [[reduce_to_exp]] added in exercises ((answers)) S458e */
    case SUGAR(COR): return desugarOr(comps[0].exps);

/* cases for \uscheme's [[reduce_to_exp]] added in exercises ((answers)) S459c */
    case SUGAR(COND):
        return desugarCond(comps[0].qa_pairs.questions, comps[0].
                                                              qa_pairs.answers);
    }
    assert(0);
}
/* parse.c S168c */
XDef reduce_to_xdef(int code, struct Component *out) {
    switch(code) {
    case ADEF(VAL):    return mkDef(mkVal(out[0].name, out[1].exp));
    case ADEF(DEFINE): return mkDef(mkDefine(out[0].name,
                                             mkLambda(out[1].names, out[2].exp))
                                                                              );
    case ANXDEF(USE):  return mkUse(out[0].name);
    case ATEST(CHECK_EXPECT): 
                       return mkTest(mkCheckExpect(out[0].exp, out[1].exp));
    case ATEST(CHECK_ASSERT): 
                       return mkTest(mkCheckAssert(out[0].exp));
    case ATEST(CHECK_ERROR): 
                       return mkTest(mkCheckError(out[0].exp));
    case ADEF(EXP):    return mkDef(mkExp(out[0].exp));
    /* cases for \uscheme's [[reduce_to_xdef]] added in exercises S169c */
    /* add a case for each new syntactic form of definition */

/* cases for \uscheme's [[reduce_to_xdef]] added in exercises ((answers)) S461a */
    case SUGAR(RECORD):
        return mkDef(mkDefs(desugarRecord(out[0].name, out[1].names)));
    default:           assert(0);  // incorrectly configured parser
    }
}
/* parse.c S169e */
ParserResult sSexp(ParserState s) {
    if (s->input == NULL) {
        return INPUT_EXHAUSTED;
    } else {
        Par p = s->input->hd;
        halfshift(s);
        s->components[s->nparsed++].value = parsesx(p, s->context.source);
        return PARSED;
    }
}
/* parse.c S169f */
ParserResult sBindings(ParserState s) {
    if (s->input == NULL) {
        return INPUT_EXHAUSTED;
    } else {
        Par p = s->input->hd;
        switch (p->alt) {
        case ATOM:
            usage_error(code_of_name(s->context.name), BAD_INPUT, &s->context);
            return BAD_INPUT; // not reached
        case LIST:
            halfshift(s);
            s->components[s->nparsed++] = parseletbindings(&s->context, p->
                                                                        u.list);
            return PARSED;
        }
        assert(0);
    }
}
/* parse.c S170b */
Value parsesx(Par p, Sourceloc source) {
    switch (p->alt) {
    case ATOM:
        /* return [[p->u.atom]] interpreted as an S-expression S170c */
        {
            Name n        = p->u.atom;
            const char *s = nametostr(n);

            char *t;                        // first nondigit in s
            long l = strtol(s, &t, 10);     // value of digits in s, if any
            if (*t == '\0' && *s != '\0')   // s is all digits
                return mkNum(l);
            else if (strcmp(s, "#t") == 0)
                return truev;
            else if (strcmp(s, "#f") == 0)
                return falsev;
            else if (strcmp(s, ".") == 0)
                synerror(source,
                    "this interpreter cannot handle . in quoted S-expressions");
            else
                return mkSym(n);
        }
    case LIST:
        /* return [[p->u.list]] interpreted as an S-expression S170d */
        if (p->u.list == NULL)
            return mkNil();
        else
            return cons(parsesx(p->u.list->hd, source),
                        parsesx(mkList(p->u.list->tl), source));
    }
    assert(0);
}
/* parse.c S171 */
struct Component parseletbindings(ParsingContext context, Parlist input) {
    if (input == NULL) {
        struct Component output = { .names = NULL, .exps = NULL };
        return output;
    } else if (input->hd->alt == ATOM) {
        synerror(context->source,
                 "in %p, expected (... (x e) ...) in bindings, but found %p",
                 context->par, input->hd);
        assert(0);  // not reached
    } else {
        /* state and row are set up to parse one binding */
        struct ParserState s = mkParserState(input->hd, context->source);
        s.context = *context;
        static ShiftFun bindingshifts[] = { sName, sExp, stop };
        struct ParserRow row = { .code   = code_of_name(context->name)
                               , .shifts = bindingshifts
                               };
        rowparse(&row, &s);

        /* now parse the remaining bindings, then add the first at the front */
        struct Component output = parseletbindings(context, input->tl);
        output.names = mkNL(s.components[0].name, output.names);
        output.exps  = mkEL(s.components[1].exp,  output.exps);
        return output;
    }
}
/* parse.c S172a */
Exp exp_of_atom (Sourceloc loc, Name n) {
    if (n == strtoname("#t"))
        return mkLiteral(truev);
    else if (n == strtoname("#f"))
        return mkLiteral(falsev);

    const char *s = nametostr(n);
    char *t;                      // first nondigit in s, if any
    long l = strtol(s, &t, 10);   // number represented by s, if any
    if (*t != '\0' || *s == '\0') // not a nonempty sequence of digits
        return mkVar(n);
    else if (((l == LONG_MAX || l == LONG_MIN) && errno == ERANGE) ||
             l > (long)INT32_MAX || l < (long)INT32_MIN)
    {
        synerror(loc, "arithmetic overflow in integer literal %s", s);
        return mkVar(n); // unreachable
    } else {  // the number is the whole atom, and not too big
        return mkLiteral(mkNum(l));
    }
}
/* parse.c S182b */
void check_exp_duplicates(Sourceloc source, Exp e) {
    switch (e->alt) {
    case LAMBDAX:
        if (duplicatename(e->u.lambdax.formals) != NULL)
            synerror(source, "formal parameter %n appears twice in lambda",
                     duplicatename(e->u.lambdax.formals));
        return;
    case LETX:
        if (e->u.letx.let != LETSTAR && duplicatename(e->u.letx.xs) != NULL)
            synerror(source, "bound name %n appears twice in %s",
                     duplicatename(e->u.letx.xs),
                     e->u.letx.let == LET ? "let" : "letrec");
        return;
    default:
        return;
    }
}

void check_def_duplicates(Sourceloc source, Def d) {
    if (d->alt == DEFINE && duplicatename(d->u.define.lambda.formals) != NULL)
        synerror(source,
                 "formal parameter %n appears twice in define",
                 duplicatename(d->u.define.lambda.formals));
}
/* parse.c S183b */
Name namecat(Name n1, Name n2) {
    const char *s1 = nametostr(n1);
    const char *s2 = nametostr(n2);
    char *buf = malloc(strlen(s1) + strlen(s2) + 1);
    assert(buf);
    sprintf(buf, "%s%s", s1, s2);
    Name answer = strtoname(buf);
    free(buf);
    return answer;
}
/* parse.c 173 */
Exp desugarLetStar(Namelist xs, Explist es, Exp body) {
    if (xs == NULL || es == NULL) {
        assert(xs == NULL && es == NULL);
        return body;
    } else {
        return desugarLet(mkNL(xs->hd, NULL), mkEL(es->hd, NULL),
                          desugarLetStar(xs->tl, es->tl, body));
    }
}
/* parse.c ((answers)) S457a */
Exp desugarLet(Namelist xs, Explist es, Exp body) {
    return mkApply(mkLambdax(mkLambda(xs, body)), es);
}
/* parse.c ((answers)) S458a */
Exp desugarAnd(Explist args) {
    if (args == NULL)
        return mkLiteral(truev);
    else if (args->tl == NULL)
        return args->hd;
    else
        return mkIfx(args->hd, desugarAnd(args->tl), mkLiteral(falsev));
}
/* parse.c ((answers)) S458g */
bool memberNL(Name x, Namelist xs) {
    if (xs == NULL)
        return false;
    else if (x == xs->hd)
        return true;
    else
        return memberNL(x, xs->tl);
}
/* parse.c ((answers)) S459a */
Exp desugarOr(Explist args) {
    if (args == NULL)
        return mkLiteral(falsev);
    else if (args->tl == NULL)
        return args->hd;
    else {
        Exp e1 = args->hd;
        Exp e2 = desugarOr(args->tl);
        char xstring[64];   // name for let-bound temporary
        Namelist free = freevars(e2, NULL, NULL);
        for (int suffix = 1; ; suffix++) {
             size_t n = snprintf(xstring, sizeof(xstring), "x%d", suffix);
             assert(n < sizeof(xstring));
             Name x = strtoname(xstring);
             if (memberNL(x, free))
                 ; // try again
             else {
                 Exp ifx  = mkIfx(mkVar(x), mkVar(x), e2);
                 return mkLetx(LET, mkNL(x, NULL), mkEL(e1, NULL), ifx);
             }
        }
    }
}
/* parse.c ((answers)) S459e */
static Exp errorexp(Exp arg) {
    return mkApply(mkVar(strtoname("error")), mkEL(arg, NULL));
}
/* parse.c ((answers)) S459f */
Exp desugarCond(Explist questions, Explist answers) {
    if (questions && answers)
        return mkIfx(questions->hd, answers->hd,
                     desugarCond(questions->tl, answers->tl));
    else {
        Value message = mkSym(strtoname("cond:-all-question-results-were-false")
                                                                              );
        return errorexp(mkLiteral(message));
    }
}
/* parse.c ((answers)) S460e */
ParserResult sQas(ParserState s) {
    struct qa_pairs qas;
    writeqapairs(&s->context, &qas, s->input);
    assert(s->nparsed < MAXCOMPS);
    s->input = NULL;
    s->components[s->nparsed++].qa_pairs = qas;
    return PARSED;
}
/* parse.c ((answers)) S460g */
static ShiftFun qashifts[] = { sExp, sExp, stop };
void writeqapairs(ParsingContext context, struct qa_pairs *pairs, Parlist input)
                                                                               {
    if (input == NULL) {
        pairs->questions = NULL;
        pairs->answers   = NULL;
    } else if (input->hd->alt == ATOM) {
        synerror(context->source,

   "in %p, expected (... (exp exp) ...) in question/answer pairs, but found %p",
                 context->par, input->hd);
    } else {
        /* state and row are set up to parse one QA pair */
        struct ParserState s = mkParserState(input->hd, context->source);
        s.context = *context;
        struct ParserRow row = { .code   = code_of_name(context->name)
                               , .shifts = qashifts
                               };

        /* we parse left to right but add components right to left */
        rowparse(&row, &s);
        writeqapairs(context, pairs, input->tl);
        pairs->questions = mkEL(s.components[0].exp, pairs->questions);
        pairs->answers   = mkEL(s.components[1].exp, pairs->answers);
    }
}
/* parse.c ((answers)) S461g */
static Def recordConstructor(Name recname, Namelist fieldnames);
static Def recordPredicate(Name recname, Namelist fieldnames);
static Deflist recordAccessors(Name recname, int preceding, Namelist fieldnames)
                                                                               ;

/* parse.c ((answers)) S461h */
static Exp consexp(Exp car, Exp cdr) {
    return mkApply(mkLiteral(mkPrimitive(CONS, binary)), mkEL(car, mkEL(cdr,
                                                                        NULL)));
}
static Exp carexp(Exp e);
static Exp cdrexp(Exp e) {
    return mkApply(mkLiteral(mkPrimitive(CDR, unary)), mkEL(e, NULL));
}
static Exp nullp(Exp e) {
    return mkApply(mkLiteral(mkPrimitive(NULLP, unary)), mkEL(e, NULL));
}
static Exp pairp(Exp e) {
    return mkApply(mkLiteral(mkPrimitive(PAIRP, unary)), mkEL(e, NULL));
}
static Exp and_also(Exp p, Exp q) {
    return mkIfx(p, q, mkLiteral(mkBoolv(false)));
}
/* parse.c ((answers)) S461i */
static Exp cdrs(int n, Exp e) {
    if (n == 0)
        return e;
    else
        return cdrexp(cdrs(n-1, e));
}
/* parse.c ((answers)) S462a */
static Exp varlist(Namelist names) {
    if (names == NULL)
        return mkLiteral(mkNil());
    else
        return consexp(mkVar(names->hd), varlist(names->tl));
}
/* parse.c ((answers)) S462b */
static Namelist prefix_the(Namelist names);

static Def recordConstructor(Name recname, Namelist fieldnames) {
    Name conname = namecat(strtoname("make-"), recname);
    Namelist argnames = prefix_the(fieldnames); 
    Exp body = consexp(mkLiteral(mkSym(conname)), varlist(argnames));
    return mkDefine(conname, mkLambda(argnames, body));
}
/* parse.c ((answers)) S462c */
static Namelist prefix_the(Namelist names) {
    if (names == NULL)
        return NULL;
    else
        return mkNL(namecat(strtoname("the-"), names->hd), prefix_the(names->tl)
                                                                              );
}
/* parse.c ((answers)) S462d */
static Exp goodRecordCdr(Exp looking_at, Namelist fieldnames) {
    if (fieldnames == NULL)
        return nullp(looking_at);
    else
        return and_also(pairp(looking_at),
                        goodRecordCdr(cdrexp(looking_at), fieldnames->tl));
}

static Def recordPredicate(Name recname, Namelist fieldnames) {
    if (recname == strtoname("pair"))
        runerror(
          "Records may not be called 'pair'; the 'pair?' variable is captured");
    Name predname = namecat(recname, strtoname("?"));
    Value tag = mkSym(namecat(strtoname("make-"), recname));
    Name r = strtoname("r");
    Exp good_car = mkApply(mkVar(strtoname("=")),
                           mkEL(carexp(mkVar(r)),
                           mkEL(mkLiteral(tag), NULL)));
    Exp body = and_also(pairp(mkVar(r)),
                        and_also(good_car, goodRecordCdr(cdrexp(mkVar(r)),
                                                                  fieldnames)));
    return mkDefine(predname, mkLambda(mkNL(r, NULL), body));
}
/* parse.c ((answers)) S463a */
static Deflist recordAccessors(Name recname, int preceding, Namelist fieldnames)
                                                                               {
    if (fieldnames == NULL)
        return NULL;
    else {
       Name field = fieldnames->hd;
       Name predname = namecat(recname, strtoname("?"));
       Name accessor = namecat(recname, namecat(strtoname("-"), field));
       Name r = strtoname("r");
       Exp fieldexp = carexp(cdrs(preceding+1, mkVar(r)));
       Exp body = mkIfx(mkApply(mkVar(predname), mkEL(mkVar(r), NULL)),
                        fieldexp,
                        errorexp(consexp(mkLiteral(mkSym(strtoname(
                                                           "value-passed-to"))),
                                 consexp(mkLiteral(mkSym(accessor)),
                                 consexp(mkLiteral(mkSym(strtoname("is-not-a")))
                                                                               ,
                                 consexp(mkLiteral(mkSym(recname)),
                                 mkLiteral(mkNil())))))));
       Def d = mkDefine(accessor, mkLambda(mkNL(r, NULL), body));
       return mkDL(d, recordAccessors(recname, preceding+1, fieldnames->tl));
    }
}
/* parse.c ((answers)) S463c */
/* functions for desugaring [[record]] definitions 213a */
Deflist desugarRecord(Name recname, Namelist fieldnames) {
    return mkDL(recordConstructor(recname, fieldnames),
           mkDL(recordPredicate(recname, fieldnames),
                recordAccessors(recname, 0, fieldnames)));
}
/* functions for desugaring [[record]] definitions 213b */
static Exp carexp(Exp e) {
    return mkApply(mkLiteral(mkPrimitive(CAR, unary)), mkEL(e, NULL));
}
