(* Solutions to the Core ML assignment, HW7 *)
(* Prathyusha Butti *)
(* It took 5 hours *) 
(* Submission : 8th April, 2020 *)
(*load "Unit"*)

(*** Problem N - Search trees ***)

datatype 'a tree = NODE of 'a tree * 'a * 'a tree 
                 | LEAF

fun insert cmp =
let fun ins (x, LEAF) = NODE (LEAF, x, LEAF)
      | ins (x, NODE (left, y, right)) = 
            (case cmp (x, y)
                 of LESS    => NODE (ins (x, left), y, right)
                 | GREATER => NODE (left, y, ins (x, right))
                 | EQUAL   => NODE (left, x, right))
    in  ins
    end

datatype 'a set = SET of ('a * 'a -> order) * 'a tree

fun nullset cmp = SET (cmp, LEAF)

(*** function specifyOrder ***)
(* Takes as input two elements and returns the order *)
fun specifyOrder (a, b) =
      if (a < b) then LESS
      else if (a > b) then GREATER
      else EQUAL

(*** function addelt : 'a * 'a set -> 'a set ***)
(* Takes a element and set as arguments and *)
(* returns the element added to the set *)
 fun addelt (a, SET(cmp, LEAF)) = SET(cmp, insert cmp (a, LEAF))
   | addelt (a, SET(cmp, t)) = SET(cmp, insert cmp (a, t))    

(*** function treeFoldr : ('a * 'b -> 'b) -> 'b -> 'a tree -> 'b ***)
(* This function folds a function over every element of a tree *)
(* , rightmost element first *)
   
fun treeFoldr f init t =
    let fun inOrder LEAF = []
          | inOrder (NODE(left, y, right)) 
                      = (inOrder left) @ [y] @ (inOrder right)
          in foldr f init (inOrder t)
          end

val emptyTree = LEAF
val treeElm1 = insert specifyOrder(1, emptyTree)
val treeElm2 = insert specifyOrder(2, treeElm1)
val treeElm3 = insert specifyOrder(3, treeElm2)
val treeElm4 = insert specifyOrder(4, treeElm3)


val () = 
     Unit.checkExpectWith (Unit.listString Int.toString)
     "to check for empty tree"
     (fn () => treeFoldr (op ::) [] emptyTree) []

val () = 
     Unit.checkExpectWith (Unit.listString Int.toString)
     "to check elements inserted into the tree"
     (fn () => treeFoldr (op ::) [] treeElm4) [1,2,3,4]

val () = 
     Unit.checkExpectWith (Int.toString)
     "sum of elements in a tree"
     (fn () => treeFoldr (op +) 0 treeElm4) 10

val () = 
     Unit.checkExpectWith (Int.toString)
     "difference of elements in a tree"
     (fn () => treeFoldr (op -) 0 treeElm4) ~2

(*** function setFold : ('a * 'b -> 'b) -> 'b -> 'a set -> 'b ***)
(* This function visits every element of the set exactly once *)
(* in unspecified order *)
fun setFold f init s = 
      (case s of SET(_, LEAF) => init
               | SET(cmp, t) => treeFoldr f init t)

val emptySet = SET(specifyOrder, LEAF)
val setElm1 = addelt(1, emptySet)
val setElm2 = addelt(2, setElm1)
val setElm3 = addelt(3, setElm2)
val setElm4 = addelt(4, setElm3)
val setElm5 = addelt(5, setElm4)

val () = 
     Unit.checkExpectWith (Unit.listString Int.toString)
     "to check elements inserted into the empty set"
     (fn () => setFold (op ::) [] emptySet) []

val () = 
     Unit.checkExpectWith (Unit.listString Int.toString)
     "to check elements inserted into the set"
     (fn () => setFold (op ::) [] setElm5) [1,2,3,4,5]

val () = 
     Unit.checkExpectWith (Int.toString)
     "to check sum of elements in set"
     (fn () => setFold (op +) 0 setElm5) 15

val () = 
     Unit.checkExpectWith (Int.toString)
     "to check difference of elements in the set"
     (fn () => setFold (op -) 0 setElm5) 3

val () = Unit.report()
val () = Unit.reportWhenFailures ()  (* put me at the _end_ *)