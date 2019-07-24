use "../../src/4/2.sml";

(* tests *)

fun cmp (x, y) = x < y

fun listToStream nil = $NIL
  | listToStream (e :: l) = $(CONS(e, listToStream l))

fun streamToList ($NIL) = nil
  | streamToList ($(CONS(x, s))) = x :: (streamToList s)

(* fun t k = streamToList o (fn l => streamInsertionSort (k, l)) o listToStream  *)

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


(* val l1 = t 1 l0 *)
(* val l2 = t 2 l0 *)
(* val l3 = t 3 l0 *)
(* val l4 = t 3 l0 *)

(* val ll = [1,2,3,4,5] *)

(* val ll1 = t 1 ll *)
(* val ll2 = t 2 ll *)
(* val ll3 = t 3 ll *)
(* val ll4 = t 4 ll *)
(* val ll5 = t 5 ll *)

(* val lll = [1,2,3,4,5] *)

(* val lll1 = t 1 lll *)
(* val lll2 = t 2 lll *)
(* val lll3 = t 3 lll *)
(* val lll4 = t 4 lll *)
(* val lll5 = t 5 lll *)


(* val llll = [1,1,1] *)

(* val llll1 = t 1 llll *)
(* val llll1 = t 1 llll *)

(* (* val f = streamToList o listToStream *) *)

(* (* val l0 = t 4 [93,5,1,3,56,7] *) *)

