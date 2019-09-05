use "../../src/4/2.sml";

(* tests *)

fun cmp (x, y) = x < y

fun listToStream nil = $NIL
  | listToStream (e :: l) = $(CONS(e, listToStream l))

fun streamToList ($NIL) = nil
  | streamToList ($(CONS(x, s))) = x :: (streamToList s)

val t = streamToList o streamInsertionSort o listToStream

val l = [93,5,1,3,56,7]

val l' = t l

val ll = [1,2,3,4,5]

val ll' = t ll

val lll' = t [5,4,3,2,1]

val llll = [1,1,1]

val llll' = t llll

val lllll = []

val lllll' = t lllll

