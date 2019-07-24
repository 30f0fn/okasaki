(* Implement insertion sort on streams.  Show that extracting the first $k$ elements of `sort xs` takes only $O(n*k)$ time, where $n$ is the length of $xs$, rather than $O(n^2)$ time, as might be expected of insertion sort. *)

use "../../src/4/Stream.sml";
open Stream

fun streamInsertionSort ($NIL) = $NIL
  | streamInsertionSort ($(CONS(x,xs))) =
    let
        fun lazy insert (x, $NIL) = $(CONS(x, $NIL))
          | insert (x, s as $(CONS (y, s'))) =
            if x < y then $(CONS(x, s)) else $(CONS(y, insert(x, s')))
    in
        insert (x, streamInsertionSort(xs))
    end

