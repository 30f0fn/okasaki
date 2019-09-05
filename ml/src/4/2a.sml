(* Implement insertion sort on streams.  Show that extracting the first $k$ elements of `sort xs` takes only $O(n*k)$ time, where $n$ is the length of $xs$, rather than $O(n^2)$ time, as might be expected of insertion sort. *)

use "../../src/4/Stream.sml";
open Stream

fun streamInsertionSort s =
    let
        fun lazy insert (ls, $NIL) = ls
          | lazy insert ($NIL, $(CONS (y, ls'))) = $(CONS (y, $NIL))
          | lazy insert (lt' as $(CONS (y, lt)), $(CONS (x, ls))) =
            let
                val inserted = if x < y
                               then $(CONS (x, lt'))
                               else $(CONS (y, streamInsertionSort $(CONS (x, lt))))
            in
                insert (inserted, ls)
            end
    in
        insert ($NIL, s)
    end

