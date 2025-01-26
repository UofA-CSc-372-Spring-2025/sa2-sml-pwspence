(* Solutions to SA2 assignment, Intro to ML *)

(* Name: Phyllis Spence                     *)
(* Time spent on HW6: 0.5 hr
*)

(* Collaborators and references: chatGPT
*)

(* indicate planning to use the Unit testing module *)
use "Unit.sml";

(**** Problem A ****)

fun mynull []       = true
  | mynull (_::_)   = false

val () =
    Unit.checkExpectWith Bool.toString "mynull [] should be true"
    (fn () => mynull [])
    true

val () =
    Unit.checkExpectWith Bool.toString "mynull [] should be true"
    (fn () => mynull [1,2,3])
    false


(**** Problem B ****)
fun firstVowel [] = false
  | firstVowel (#"a"::_) = true
  | firstVowel (#"e"::_) = true
  | firstVowel (#"i"::_) = true
  | firstVowel (#"o"::_) = true
  | firstVowel (#"u"::_) = true
  | firstVowel (_::_) = false

val () =
    Unit.checkExpectWith Bool.toString "firstVowel 'ack' should be true"
    (fn () => firstVowel [#"a",#"c",#"k"])
    true

val () =
    Unit.checkExpectWith Bool.toString "firstVowel 'efj' should be true"
    (fn () => firstVowel [#"e",#"f",#"j"])
    true

val () =
    Unit.checkExpectWith Bool.toString "firstVowel 'ick' should be true"
    (fn () => firstVowel [#"i",#"c",#"k"])
    true

val () =
    Unit.checkExpectWith Bool.toString "firstVowel 'ock' should be true"
    (fn () => firstVowel [#"o",#"c",#"k"])
    true

val () =
    Unit.checkExpectWith Bool.toString "firstVowel 'uck' should be true"
    (fn () => firstVowel [#"u",#"c",#"k"])
    true

val () =
    Unit.checkExpectWith Bool.toString "firstVowel 'bae' should be false"
    (fn () => firstVowel [#"b",#"a",#"e"])
    false

val () =
    Unit.checkExpectWith Bool.toString "firstVowel 'kae' should be false"
    (fn () => firstVowel [#"k",#"a",#"e"])
    false

(**** Problem C ****)
fun reverse xs = foldl List.:: [] xs

val () =
  Unit.checkExpectWith (Unit.listString Int.toString) 
  "reverse [1,2] should be [2,1]"
  (fn () => reverse [1,2])
  [2,1]

val () =
  Unit.checkExpectWith (Unit.listString Int.toString) 
  "reverse [1,2,3,4] should be [4,3,2,1]"
  (fn () => reverse [1,2,3,4])
  [4,3,2,1]

(**** Problem D ****)
val max_int = Option.valOf Int.maxInt;
fun minlist [] = raise Fail "Empty list!"
  | minlist (xs:int list):int = foldl (fn (x,a) => Int.min(x,a)) max_int xs;

val () =
  Unit.checkExnWith Int.toString
  "minlist [] should raise an exception"
  (fn () => minlist [])

val () =
  Unit.checkExpectWith Int.toString
  "minlist [1,2,3,4,0] should be 0"
  (fn () => minlist [1,2,3,4,0])
  0

val () =
  Unit.checkExpectWith Int.toString
  "minlist [1,2,3,4,~5] should be ~5"
  (fn () => minlist [1,2,3,4,~5])
 ~5

(**** Problem E ****)
exception Mismatch

fun zip ([], []) = []
  | zip (x::xs, y::ys) = (x, y) :: zip (xs, ys)
  | zip (_, _) = raise Mismatch

val () =
  Unit.checkExnWith Unit.concatPairs
  "zip ([1,2,3], [4,5]) should raise an exception"
  (fn () => zip ([1,2,3], [4,5]))

val () =
  Unit.checkExnWith Unit.concatPairs
  "zip ([1,2], [4,5,6]) should raise an exception"
  (fn () => zip ([1,2], [4,5,6]))

val () =
  Unit.checkExpectWith Unit.concatPairs
  "zip ([1,2,3], [4,5,6]) should be [(1,4),(2,5),(3,6)]"
  (fn () => zip ([1,2,3], [4,5,6]))
 [(1,4),(2,5),(3,6)]

(**** Problem F ****)

fun concat [] = []
  | concat (x::xs) = x @ concat (xs)

val () =
  Unit.checkExpectWith (Unit.listString Int.toString)
  "concat [[1], [2, 3, 4], [], [5, 6]] should be [1, 2, 3, 4, 5, 6]"
  (fn () => concat [[1], [2, 3, 4], [], [5, 6]])
  [1, 2, 3, 4, 5, 6]

(**** Problem G ****)

fun isDigit #"0" = true
  | isDigit #"1" = true
  | isDigit #"2" = true
  | isDigit #"3" = true
  | isDigit #"4" = true
  | isDigit #"5" = true
  | isDigit #"6" = true
  | isDigit #"7" = true
  | isDigit #"8" = true
  | isDigit #"9" = true
  | isDigit _ = false;

val () =
  Unit.checkExpectWith Bool.toString
  "isDigit #'5' should be true"
  (fn () => isDigit #"5")
  true

val () =
  Unit.checkExpectWith Bool.toString
  "isDigit #'a' should be false"
  (fn () => isDigit #"a")
  false

(**** Problem H ****)
fun isAlpha c =
  let
     val code = Char.ord(c)
  in
     (code >= 65 andalso code <= 90) orelse
     (code >= 97 andalso code <= 122)
  end;

val () =
  Unit.checkExpectWith Bool.toString
  "isAlpha #'5' should be false"
  (fn () => isAlpha #"5")
  false

val () =
  Unit.checkExpectWith Bool.toString
  "isAlpha #'a' should be true"
  (fn () => isAlpha #"a")
  true

(**** Problem I ****)
fun svgCircle (cx, cy, r, fill) =
  "<circle cx=\"" ^ Int.toString(cx) ^
  "\" cy=\"" ^ Int.toString(cy) ^
  "\" r=\"" ^ Int.toString(r) ^
  "\" fill=\"" ^ fill ^ "\" />"

val () =
  Unit.checkExpectWith (fn x => x)
  "svgCircle (200, 300, 100, \"red\") should return <circle cx=\"200\" cy=\"300\" r=\"100\" fill=\"red\" />"
  (fn () => svgCircle (200, 300, 100, "red"))
  "<circle cx=\"200\" cy=\"300\" r=\"100\" fill=\"red\" />";

(**** Problem J ****)
fun partition _ [] = ([], [])
  | partition pred (x::xs) =
      let
        val (list1, list2) = partition pred xs
      in
        if pred x then
          (x::list1, list2)
        else
          (list1, x::list2)
      end;

val () =
  Unit.checkExpectWith (fn (l1, l2) => "(" ^ Unit.listString Int.toString l1 ^ ", " ^ Unit.listString Int.toString l2 ^ ")")
  "partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5] should return ([2, 4], [1, 3, 5])"
  (fn () => partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5])
  ([2, 4], [1, 3, 5])

val () =
  Unit.checkExpectWith (fn (l1, l2) => "(" ^ Unit.listString Char.toString l1 ^ ", " ^ Unit.listString Char.toString l2 ^ ")")
  "partition Char.isAlpha [#\"a\", #\"1\", #\"b\", #\"2\", #\"c\"] should return ([#\"a\", #\"b\", #\"c\"], [#\"1\", #\"2\"])"
  (fn () => partition Char.isAlpha [#"a", #"1", #"b", #"2", #"c"])
  ([#"a", #"b", #"c"], [#"1", #"2"])

val () =
  Unit.checkExpectWith (fn (l1, l2) => "(" ^ Unit.listString Int.toString l1 ^ ", " ^ Unit.listString Int.toString l2 ^ ")")
  "partition (fn x => x > 0) [~3, ~2, 0, 1, 2] should return ([1, 2], [~3, ~2, 0])"
  (fn () => partition (fn x => x > 0) [~3, ~2, 0, 1, 2])
  ([1, 2], [~3, ~2, 0])

(* Unit testing reporting *)

val () = Unit.report()
val () = Unit.reportWhenFailures ()  (* put me at the _end_ *)
