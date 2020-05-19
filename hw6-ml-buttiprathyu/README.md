# HW6-ml-start

val it = ((1, 2), (3, 4, 5)) : (int * int) * (int * int * int)
- [[1,2],[3,4,5]];
> val it = [[1, 2], [3, 4, 5]] : int list list
- [(2,2,[1])];
> val it = [(2, 2, [1])] : (int * int * int list) list
- [[],[[]],[]];
> val 'a it = [[], [[]], []] : 'a list list list
- fun  fone(x:int) = [x,x,x];
> val fone = fn : int -> int list
- fun ftwo(x) = (x,x,x);
> val 'a ftwo = fn : 'a -> 'a * 'a * 'a
- fun fthree(x,y) = [x^"b",y];
> val fthree = fn : string * string -> string list
- fun four(x,y,z) = (x+(size y),z);
> val 'a four = fn : int * string * 'a -> int * 'a
> hd(explode "south");
 > val it = #"s" : char
 - hd(tl(explode "north"));
 > val it = #"o" : char
 - hd(rev (explode "east"));
 > val it = #"t" : char
 - hd(tl(rev(explode "west")));
 > val it = #"s" : char
 - hd(rev(explode "west"));
 > val it = #"t" : char
 - (tl(rev(explode "west")));
 > val it = [#"s", #"e", #"w"] : char list
val first = hd o explode;
> val first = fn : string -> char
- val second = hd o tl o explode;
> val second = fn : string -> char
- first("first");
> val it = #"f" : char
- second("first");
> val it = #"i" : char
- val fourth = hd o tl o tl o tl o explode;
> val fourth = fn : string -> char
- fourth("first");
> val it = #"s" : char
- val third = hd o tl o tl o explode;
> val third = fn : string -> char
- third("first");
> val it = #"r" : char
val (x::y::zs,w) = ([1,2,3],("cp",105));
> val x = 1 : int
  val y = 2 : int
  val zs = [3] : int list
  val w = ("cp", 105) : string * int
- val (x::y::zs,w) = (("cp",105),[1,2,3]);
! Toplevel input:
! val (x::y::zs,w) = (("cp",105),[1,2,3]);
!                     ^^^^^^^^^^
! Type clash: expression of type
!   'a * 'b
! cannot have type
!   'c list
- val (x::y::zs,w)=([("cp",105)],(1,2,3))
;
! Uncaught exception:
! Bind
- val (x::y::zs,w) = ([("cp",105)],(1,2,3));
! Uncaught exception:
! Bind
- val (x::y::zs,w) = (["comp","105"], true);
> val x = "comp" : string
  val y = "105" : string
  val zs = [] : string list
  val w = true : bool
- val (x::y::zs,w) = ([true,false],2.718);
> val x = true : bool
  val y = false : bool
  val zs = [] : bool list
  val w = 2.718 : real