(* Solutions to the Core ML assignment, HW6 *)
(* Prathyusha Butti *)
(* It took 8 hours *)
(* Submission : 27th March, 2020 *)
(*load "Unit"*)

(*** Problem A - mynull ***)
(* Takes a list and returns true if empty else false *)
fun mynull []       = true
  | mynull (_::_)   = false

val () =
    Unit.checkExpectWith Bool.toString "mynull [] should be true"
    (fn () => mynull [])
    true

val () =
    Unit.checkExpectWith Bool.toString "mynull [] should be false"
    (fn () => mynull [1,2,3])
    false

(*** Problem B - firstVowel ***)
(* Takes a char list xs and returns true if 1st char is a vowel else false *)
fun firstVowel []  = false
  | firstVowel (x::_) =
        case x of #"a" => true
                | #"e" => true
                | #"i" => true
                | #"o" => true
                | #"u" => true
                | _  => false

val () =
    Unit.checkExpectWith Bool.toString "firstVowel on empty list"
    (fn () => firstVowel [])
    false

val () =
    Unit.checkExpectWith Bool.toString "firstVowel on vowels"
    (fn () => firstVowel [#"u", #"r"])
    true

val () =
    Unit.checkExpectWith Bool.toString "firstVowel on consonants"
    (fn () => firstVowel [#"g",#"e",#"r"])
    false

(*** Problem C - reverse ***)
(* Takes a list xs and returns the reverse of the list *)
fun reverse xs = foldl op :: [] xs

val () =
    Unit.checkExpectWith
         (Unit.listString Unit.intString) "reverse on empty list"
    (fn () => reverse [])
    []

val () =
    Unit.checkExpectWith
         (Unit.listString Unit.intString) "reverse on integer list"
    (fn () => reverse [1,2,3])
    [3,2,1]

val () =
    Unit.checkExpectWith
        (Unit.listString Unit.stringString) "reverse on string list"
    (fn () => reverse ["g","l","c"])
    ["c","l","g"]

(*** Problem C - minlist ***)
(* Takes a non-empty integer list and returns the smallest element *)
fun minlist [] = raise Match
  | minlist (x::xs) = foldl Int.min x xs

val () =
    Unit.checkExpectWith
         (Int.toString) "minlist on integer list"
    (fn () => minlist [45,3,44])
    3

val () =
    Unit.checkExnWith
         (Int.toString) "minlist on empty list"
    (fn () => minlist [])

(*** Problem D - zip ***)
(* Takes a pair of lists of equal length *)
(* returns the equivalent list of pairs *)
exception Mismatch
fun zip ([],[]) = []
  | zip (xs,[]) = raise Mismatch
  | zip ([],ys) = raise Mismatch
  | zip (x::xs,y::ys) = (x,y) :: zip (xs,ys)

val () =
    Unit.checkExpectWith
         (Unit.listString
         (Unit.pairString Int.toString Int.toString))
         "zip on empty list"
    (fn () => zip ([],[]))
    []

val () =
    Unit.checkExpectWith
         (Unit.listString
         (Unit.pairString Int.toString Int.toString))
         "zip on equal integer list"
    (fn () => zip ([1,2,3],[4,5,6]))
    [(1,4),(2,5),(3,6)]

val () =
    Unit.checkExnWith
         (Unit.listString
         (Unit.pairString Int.toString Int.toString))
         "zip on unequal list"
    (fn () => zip ([1],[1,2]))

val () =
    Unit.checkExnWith
         (Unit.listString
         (Unit.pairString Int.toString Int.toString))
         "zip with ys as empty list"
    (fn () => zip ([1],[]))

val () =
    Unit.checkExnWith
         (Unit.listString
         (Unit.pairString Int.toString Int.toString))
         "zip with xs as empty list"
    (fn () => zip ([],[1]))

val () =
    Unit.checkExpectWith
         (Unit.listString
         (Unit.pairString String.toString String.toString))
         "zip on equal string list"
    (fn () => zip (["a","b"],["c","d"]))
    [("a","c"),("b","d")]

(*** Problem E - pairfoldrEq ***)
(* Takes a function f, accumulator start, pair of lists of equal length *)
(* returns the pair-wise foldr of two lists *)
fun pairfoldrEq f start ([],[]) = start
  | pairfoldrEq f start (x::xs, y::ys) = f (x, y, pairfoldrEq f start (xs, ys))
  | pairfoldrEq _ _ _ = raise Mismatch

val () =
    Unit.checkExpectWith
         Int.toString
         "pairFoldrEq on sum of two lists"
    (fn () => pairfoldrEq (fn(x,y,z)=>x+y+z) 0 ([1,2,3],[1,2,3]))
    12

val () =
    Unit.checkExpectWith
         Int.toString
         "pairFoldrEq on sum of two empty lists"
    (fn () => pairfoldrEq (fn(x,y,z)=>x+y+z) 0 ([],[]))
    0

val () =
    Unit.checkExnWith
         Int.toString
         "pairFoldrEq on sum of two unequal lists"
    (fn () => pairfoldrEq (fn(x,y,z)=>x+y+z) 0 ([],[1]))


(*** Problem E - ziptoo ***)
(* Takes a pair of lists of equal length *)
(* returns the equivalent list of pairs using pairfoldrEq *)
fun ziptoo ([],[]) = []
  | ziptoo (xs,[]) = raise Mismatch
  | ziptoo ([],ys) = raise Mismatch
  | ziptoo (x::xs,y::ys) = pairfoldrEq(fn (x,y,z)=> (x,y) :: z) [] (x::xs,y::ys)

val () =
    Unit.checkExpectWith
         (Unit.listString
         (Unit.pairString Int.toString Int.toString))
         "ziptoo on empty list"
    (fn () => ziptoo ([],[]))
    []

val () =
    Unit.checkExpectWith
         (Unit.listString
         (Unit.pairString Int.toString Int.toString))
         "ziptoo on equal integer list"
    (fn () => ziptoo ([1,2,3],[4,5,6]))
    [(1,4),(2,5),(3,6)]

val () =
    Unit.checkExnWith
         (Unit.listString
         (Unit.pairString Int.toString Int.toString))
         "ziptoo on unequal list"
    (fn () => ziptoo ([1],[1,2]))

val () =
    Unit.checkExnWith
         (Unit.listString
         (Unit.pairString Int.toString Int.toString))
         "ziptoo with ys as empty list"
    (fn () => ziptoo ([1],[]))

val () =
    Unit.checkExnWith
         (Unit.listString
         (Unit.pairString Int.toString Int.toString))
         "ziptoo with xs as empty list"
    (fn () => ziptoo ([],[1]))

val () =
    Unit.checkExpectWith
         (Unit.listString
         (Unit.pairString String.toString String.toString))
         "ziptoo on equal string list"
    (fn () => ziptoo (["a","b"],["c","d"]))
    [("a","c"),("b","d")]

(*** Problem F - concat ***)
(* Takes a list of lists of 'a and produces a single list of 'a *)
(* containing all the elements in the correct order *)
fun concat [] = []
  | concat (x::xs) = x @ (concat xs)

val () =
    Unit.checkExpectWith
         (Unit.listString Unit.intString) "concat on empty list"
    (fn () => concat [])
    []

val () =
    Unit.checkExpectWith
         (Unit.listString Unit.intString) "concat on integer list"
    (fn () => concat [[1],[2,3]])
    [1,2,3]

val () = Unit.report()
val () = Unit.reportWhenFailures ()  (* put me at the _end_ *)