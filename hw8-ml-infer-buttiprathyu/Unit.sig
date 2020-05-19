val checkExpectWith : (''a -> string) -> string -> (unit -> ''a) -> ''a -> unit
  (* Calling `checkExpectWith show name test result` has these effects:
       Expression `test()` is evaluated, and the result is compared
       with `result`.  If they are equal, the test passes; otherwise it fails.
       If the test fails, `name` is used to identify the failing test.
   *)

val intString : int -> string
val stringString : string -> string
val boolString : bool -> string

val listString : ('a -> string) -> ('a list -> string)
val optionString : ('a -> string) -> ('a option -> string)
val pairString : ('a -> string) -> ('b -> string) -> ('a * 'b -> string)
val showNothing : 'a -> string   (* just says "a value" *)


val checkAssert : string -> (unit -> bool) -> unit
  (* Calling `checkAssert name test` has these effects:
       Expression `test()` is evaluated, and if the result is `true`,
       the test passes; otherwise it fails.  If the test fails, `name`
       is used to identify the failing test.
   *)

val checkExnWith : ('a -> string) -> string -> (unit -> 'a) -> unit
  (* Calling `checkExn show name test` has these effects:
       Expression `test()` is evaluated, and if it raises an exception,
       the test passes.  If `test()` returns a value, the test fails.
       If the test fails, `name` is used to identify the failing test.
   *)

val checkExnSatisfiesWith : ('a -> string) -> string -> (unit -> 'a) -> (string * (exn -> bool)) -> unit
  (* Calling `checkExnSatisfiesWith show name test (ename, predicate)` 
     has these effects:
       Expression `test()` is evaluated, and if it raises an exception `e`,
       the test passes, provided `predicate e` is true.  If `test()` raises
       a non-satisfying exception, or if `test()` returns a value, the test fails,
       complaining that exception `ename` was expected.
       If the test fails, `name` is used to identify the failing test.

       Example:
           checkExnSatisfiesWith
              Int.toString
              "sum of big ints overflows"
              (fn () => valOf Int.maxInt + valOf Int.maxInt)
              ("Overflow", fn Overflow => true | _ => false)
   *)


val report : unit -> unit
  (* If any tests were run, print, on standard output, a report of what passed. *)

val reportWhenFailures : unit -> unit
  (* If any tests failed, print, on standard output,
     a report of what did and did not pass. *)
