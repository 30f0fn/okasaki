(* Implement insertion sort on streams.  Show that extracting the first $k$ elements of `sort xs` takes only $O(n*k)$ time, where $n$ is the length of $xs$, rather than $O(n^2)$ time, as might be expected of insertion sort. *)

use "../../src/4/Stream.sml";
open Stream

fun cmp (x, y) = x < y 

(* 
intuitive idea
get head of inputlist
impatientInsert this 0-deeply into acc
get head of tail of inputlist
impatientInsert this 1-deeply into acc
...
get entry k of inputList
impatientInsert it k-deeply into inputList
get entry k+1 of inputList
impatientInsert it k-deeply into inputList

depth of impatientInsert is min(k, len(acc))

*)
                      


(* fun streamInsertionSort (k, s : int Stream) = *)
(*     let  *)
(*         exception emptyAccumulator; *)
(*         fun impatientInsert (0, x, acc) = $(CONS(x, acc)) *)
(*           (* | impatientInsert (d, _, $NIL) = raise emptyAccumulator *) *)
(*           | impatientInsert (d, x, s as ($(CONS(y, s')))) = *)
(*             if cmp (x, y) then $(CONS(x, s)) *)
(*             else $(CONS (y, impatientInsert (d - 1, x, s'))) *)
(*         fun accumSort (_, acc, $NIL) = acc *)
(*           | accumSort (d, acc, s as $(CONS(x, s'))) = *)
(*             accumSort (d + 1, *)
(*                      impatientInsert (Int.min(d, k), x, acc), *)
(*                      s') *)
(*     in *)
(*         accumSort (0, $NIL, s) *)
(*     end *)

(* fun lazy streamInsertionSort (cmp, 0, s : int Stream) = $NIL *)
(*   | streamInsertionSort (cmp, n, $NIL) = $NIL *)
(*   | streamInsertionSort (cmp, n, s as $(CONS(x, t))) = *)
(*     let *)
(*         fun getMin ($NIL : int Stream, optInt) = optInt *)
(*           | getMin ($(CONS(x, s)), NONE) = getMin (s, SOME x) *)
(*           | getMin ($(CONS(x, s)), SOME y) = if cmp (x, y) *)
(*                                              then getMin (s, SOME x) *)
(*                                              else getMin (s, SOME y) *)
(*         val SOME min = getMin (s, NONE) *)
(*         fun shiftMin (s, t) =  *)
(*     in *)
(*         $(CONS(min, streamInsertionSort (cmp, n-1, t))) *)
(*     end *)


fun streamInsertionSort ($NIL) = $NIL
  | streamInsertionSort ($(CONS(x,xs))) =
    let
        fun lazy insert (x, $NIL) = $(CONS(x, $NIL))
          | insert (x, s as $(CONS (y, s'))) =
            if x < y then $(CONS(x, s)) else $(CONS(y, insert(x, s')))
    in
        insert (x, streamInsertionSort(xs))
    end

