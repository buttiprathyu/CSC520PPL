(* HW8 constraint solver key *)
(* pg 498, problem 19, Ramsey book,
 * Using the ideas in Section 7.5.3, implement a function solve which takes 
 * as argument a constraint of type con and returns an idempotent substitution 
 * of type subst. (If a substitution is created using only the value idsubst 
 * and the functions |--> and compose, then it is guaranteed to be idempotent.) 
 * The resulting substitution should solve the constraint, obeying the law
 *      solves (solve C, C).
 * If the constraint has no solution, your function should call 
 * unsatisfiableEquality from chunk 486d, which raises the TypeError exception.
 *)
exception TypeError of string

(* set definition from S4.1.3 pg S79 *)
type 'a set = 'a list
val emptyset = []
fun member x =
  List.exists (fn y => y = x)
fun insert (x, ys) =
  if member x ys then ys else x::ys
fun union (xs, ys) = foldl insert ys xs
fun inter (xs, ys) =
  List.filter (fn x => member x ys) xs
fun diff  (xs, ys) =
  List.filter (fn x => not (member x ys)) xs


(* names and environments, Section 5.1, pg 344 *)
type name = string

(* environment, Section 5.1, pg 345 *)
type 'a env = (name * 'a) list
val emptyEnv = []

(* lookup and check of existing bindings *)
exception NotFound of name
fun find (name, []) = raise NotFound name
  | find (name, (n, v)::tail) = if name = n then v else find (name, tail)

(* environment composition *)
infix 6 <+>
fun pairs <+> pairs' = pairs' @ pairs

(* adding new bindings *)
exception BindListLength
fun bind (name, v, rho) = (name, v) :: rho
fun bindList (n::vars, v::vals, rho) = bindList (vars, vals, bind (n, v, rho))
  | bindList ([], [], rho) = rho
  | bindList _ = raise BindListLength

(* type constructors, Section 7.4.2 in Ramsey pg 456 *)
type tycon = name
fun eqTycon (mu, mu') = mu = mu'
fun tyconString mu = mu

(* type data structure, Section 7.4.1 in Ramsey pg 455 *)
type tyvar  = name
datatype ty = TYVAR of tyvar         (* type variable alpha *)
            | TYCON of tycon         (* type constructor mu *)
            | CONAPP of ty * ty list (* type-level application *)

datatype type_scheme = FORALL of tyvar list * ty

(* Hindley-Milner types, Ramsey pg 459 *)
val inttype         = TYCON "int"
val booltype        = TYCON "bool"
val symtype         = TYCON "sym"
val alpha           = TYVAR "a"
val beta            = TYVAR "b"
val unittype        = TYCON "unit"
fun listtype ty     = CONAPP (TYCON "list", [ty])
fun pairtype (x, y) = CONAPP (TYCON "pair", [x, y])

fun funtype (args, result) =
  CONAPP (TYCON "function", [CONAPP (TYCON "arguments", args), result])

fun asFuntype (CONAPP (TYCON "function", [CONAPP (TYCON "arguments", args), 
                                          result])) = SOME (args, result)
  | asFuntype _ = NONE

(* substitution type for thetas, pg 457 in Ramsey *)
type subst = ty env
fun varsubst theta =
  (fn a => find (a, theta) handle NotFound _ => TYVAR a)

(* substitution function from Ramsey, pg 458 *)
fun tysubst theta =
  let fun subst (TYVAR a) = varsubst theta a
        | subst (TYCON c) = TYCON c
        | subst (CONAPP (tau, taus)) = CONAPP (subst tau, map subst taus)
in subst
end

(* dom and compose for theta, pg 458 in Ramsey *)
fun dom theta = map (fn (a, _) => a) theta
fun compose (theta2, theta1) =
  let val domain  = union (dom theta2, dom theta1)
      val replace = tysubst theta2 o varsubst theta1
  in  map (fn a => (a, replace a)) domain
  end

exception BugInTypeInference of string

fun instantiate (FORALL (formals, tau), actuals) =
  tysubst (bindList (formals, actuals, emptyEnv)) tau
  handle BindListLength => raise BugInTypeInference
                                      "number of types in instantiation"

(* identity substitution environment, theta, pg 458 in Ramsey *)
val idsubst = emptyEnv

(* type var substitution operator, pg 459 in Ramsey *)
infix 7 |-->
fun a |--> (TYVAR a') = if a = a' then idsubst else 
                                 bind (a, TYVAR a', emptyEnv)
      | a |--> tau    = bind (a, tau, emptyEnv)

(* comparing types, Section 7.4.4, pg. 459 *)
fun eqType (TYVAR a, TYVAR a') = a = a'
  | eqType (TYCON c, TYCON c') = eqTycon (c, c')
  | eqType (CONAPP (tau, taus), CONAPP (tau', taus')) =
      eqType (tau, tau') andalso eqTypes (taus, taus')
  | eqType _ = false
and eqTypes (taus, taus') = ListPair.allEq eqType (taus, taus')

(* constraint data structure, from 7.6.3 pg 485 *)
datatype con = ~ of ty * ty
             | /\ of con * con
             | TRIVIAL
infix 4 ~
infix 3 /\

(* supplement pg S80 *)
val reverse = rev

(* free type variables, Ramsey pg 481 *)
fun freetyvars t =
  let fun f (TYVAR v,          ftvs) = insert (v, ftvs)
        | f (TYCON _,          ftvs) = ftvs
        | f (CONAPP (ty, tys), ftvs) = foldl f (f (ty, ftvs)) tys
    in  reverse (f (t, emptyset))
    end

(* intString from S77 *)
fun intString n =
  String.map (fn #"~" => #"-" | c => c) (Int.toString n)

(* spaceSep from Ramsey S77 *)
fun separate (zero, sep) =
  (* list with separator *)
  let fun s []     = zero
        | s [x] = x
        | s (h::t) = h ^ sep ^ s t
  in  s
  end
val spaceSep = separate ("", " ")   (* list separated by spaces *)
val commaSep = separate ("", ", ")  (* list separated by commas *)

(* typeString from S251 *)
fun typeString (TYCON c) = c 
  | typeString (TYVAR a) = a
  (*| typeString (FUNTY (args, result)) =
      "(" ^ spaceSep (map typeString args) ^ " -> " ^ typeString result ^ ")"
*)
  | typeString (CONAPP (tau, [])) = "(" ^ typeString tau ^ ")"
  | typeString (CONAPP (tau, tys)) =
      "(" ^ typeString tau ^ " " ^ spaceSep (map typeString tys) ^ ")"
(*  | typeString (FORALL (tyvars, tau)) =
      "(forall (" ^ spaceSep tyvars ^ ") " ^ typeString tau ^ ")"
*)

(* canonicalize, Ramsey pg 483 *)
fun canonicalize (FORALL (bound, ty)) =
  let fun canonicalTyvarName n =
        if n < 26 then "'" ^ str (chr (ord #"a" + n))
        else "'v" ^ intString (n - 25)
      val free = diff (freetyvars ty, bound)
      fun unusedIndex n =
        if member (canonicalTyvarName n) free then unusedIndex (n+1) else n
      fun newBoundVars (index, [])                = []
        | newBoundVars (index, oldvar :: oldvars) =
            let val n = unusedIndex index
            in  canonicalTyvarName n :: newBoundVars (n+1, oldvars)
            end
      val newBound = newBoundVars (0, bound)
in FORALL (newBound, tysubst (bindList 
              (bound, map TYVAR newBound, emptyEnv)) ty)
end

fun generalize (tau, tyvars) =
  canonicalize (FORALL (diff (freetyvars tau, tyvars), tau))

(* constraint substitution, Ramsey pg 486 *)
fun consubst theta =
  let fun subst (tau1 ~ tau2) = tysubst theta tau1 ~ tysubst theta tau2
        | subst (c1 /\ c2)    = subst c1 /\ subst c2
        | subst TRIVIAL       = TRIVIAL
  in  subst
end

(* Useful functions from HW8 writeup *)
fun eqsubst (theta1, theta2) =
  let val domain  = union (dom theta2, dom theta1)
      fun eqOn a = (varsubst theta1 a = varsubst theta2 a)
  in  List.all eqOn domain
  end

fun freetyvarsConstraint (t ~  t') = union (freetyvars t, freetyvars t')
  | freetyvarsConstraint (c /\ c') = union (freetyvarsConstraint c,
                                             freetyvarsConstraint c')
  | freetyvarsConstraint TRIVIAL    = emptyset

fun conjoinConstraints []      = TRIVIAL
  | conjoinConstraints [c]     = c
  | conjoinConstraints (c::cs) = c /\ conjoinConstraints cs

fun unsatisfiableEquality (t1, t2) = 
    let val t1_arrow_t2 = funtype([t1], t2)
       val FORALL (_, canonical) = 
                canonicalize (FORALL (freetyvars t1_arrow_t2, t1_arrow_t2))
    in case asFuntype canonical
         of SOME ([t1'], t2') =>
               raise TypeError ("cannot make" ^ typeString t1' ^ "equal to" 
                               ^ typeString t2')
          | _ => let exception ThisCan'tHappen in raise ThisCan'tHappen end
    end

(* solve *)
(* exception LeftAsExercise of string *)
fun solve (TRIVIAL) = idsubst
  | solve (c1 /\ c2) = 
        let 
            val theta1 = solve c1
            val theta2 = solve (consubst theta1 c2)
        in compose (theta2, theta1)
        end

  | solve (TYCON t1 ~ TYCON t2) =
        if (t1 = t2)
            then idsubst 
            else unsatisfiableEquality(TYCON t1, TYCON t2)
  | solve (TYVAR v ~ tau) = bind (v, tau, idsubst)
  | solve (tau ~ TYVAR v) = bind (v, tau, idsubst)
  | solve (CONAPP (n1,ts1) ~ CONAPP (n2,ts2)) = 
        let
            (* given two lists of types, combines into *)
            (* a conjunction list of simple equalities *)
            fun c_list [] [] = TRIVIAL
              | c_list (x::xs) (y::ys) = (x ~ y /\ (c_list xs ys))
              | c_list _  _  = raise BugInTypeInference 
                                  "invalid use of conjunction list"
        in solve (n1 ~ n2 /\ c_list ts1 ts2)
        end
  | solve (t1 ~ t2)  = unsatisfiableEquality(t1, t2)


(* solves definition from pg 487 *)
fun isSolved TRIVIAL = true
  | isSolved (tau ~ tau') = eqType (tau, tau')
  | isSolved (c /\ c') = isSolved c andalso isSolved c'
fun solves (theta, c) = isSolved (consubst theta c)

(* exception TypeError of string *)
fun hasSolution     c = (solve c; true) handle TypeError _ => false
fun hasGoodSolution c = solves (solve c, c) handle TypeError _ => false
val hasNoSolution : con -> bool = not o hasSolution
fun solutionEquivalentTo (c, theta) = eqsubst (solve c, theta)

(* test cases from HW8 writeup *)
val () = Unit.checkAssert "int ~ bool cannot be solved"
         (fn () => hasNoSolution (inttype ~ booltype))

val () = Unit.checkAssert "bool ~ bool can be solved"
         (fn () => hasSolution (booltype ~ booltype))

val () = Unit.checkAssert "bool ~ bool is solved by the identity substitution"
         (fn () => solutionEquivalentTo (booltype ~ booltype, idsubst))

val () = Unit.checkAssert "bool ~ 'a is solved by 'a |--> bool"
         (fn () => solutionEquivalentTo (booltype ~ TYVAR "'a",
                                         "'a" |--> booltype))

(* from Prof provided recitiation solution *)
val () = 
      let val c1 = alpha ~ TYVAR "c"
          val c2 = beta ~ TYVAR "c"
          val testname = "1C: good solution for "
          in Unit.checkAssert testname
          (fn () => hasGoodSolution(c1 /\ c2))
      end

(* from HW8 writeup, two more constraints to add tests for *)
(* TYVAR "a" ~ TYVAR "b" /\ TYVAR "b" ~ TYCON "bool" *)
val () = Unit.checkAssert 
         "a ~ b and b ~ bool can be solved"
         (fn () => hasSolution (alpha ~ beta /\ beta ~ booltype))

val () = Unit.checkAssert 
         "'a ~ b and b ~ bool is solved by a, b |--> bool"
         (fn () => solutionEquivalentTo (alpha ~ beta /\
                                         beta ~ booltype,
                                         compose("a" |--> booltype, 
                                         "b" |--> booltype)
                                         ))

(* CONAPP (TYCON "list", [TYVAR "a"]) ~ TYCON "int" *)
val () = Unit.checkAssert 
         "CONAPP (list a) ~ int cannot be solved"
         (fn () => hasNoSolution (listtype alpha ~ inttype))

(* TYVAR "a" ~ TYCON "sym" /\ TYVAR "b" ~ TYCON "bool" *)
val () = Unit.checkAssert 
         "'a ~ sym and 'b ~ bool can be solved"
         (fn () => hasSolution (alpha ~ symtype /\ 
                                beta ~ booltype))
val () = Unit.checkAssert 
         "'a ~ sym and 'b ~ bool is solved by 'a |--> sym o 'b |--> bool"
         (fn () => solutionEquivalentTo (alpha ~ symtype /\
                                         beta ~ booltype,
                                         compose("a" |--> symtype, 
                                         "b" |--> booltype)
                                         ))

(* CONAPP (TYCON "pair", [TYVAR "a", TYVAR "b"]) ~ TYCON "int" *)
val () = Unit.checkAssert 
         "pair (a, b) ~ int cannot be solved"
         (fn () => hasNoSolution (pairtype(alpha, beta) ~ inttype))

(* TYCON "sym" ~ TYCON "int" *)
val () = Unit.checkAssert 
         "sym ~ int cannot be solved"
         (fn () => hasNoSolution (symtype ~ inttype))

fun isIdempotent pairs =
    let fun distinct a' (a, tau) = a <> a' 
                                   andalso not (member a' (freetyvars tau))
        fun good (prev', (a, tau)::next) =
              List.all (distinct a) prev' andalso List.all (distinct a) next
              andalso good ((a, tau)::prev', next)
          | good (_, []) = true
    in  good ([], pairs)
    end

val solve =
    fn c => let val theta = solve c
            in  if isIdempotent theta then theta
                else raise BugInTypeInference "non-idempotent substitution"
            end

val () = Unit.report()
val () = Unit.reportWhenFailures ()

