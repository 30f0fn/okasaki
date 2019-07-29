(* Implement insertion sort on streams.  Show that extracting the first $k$ elements of `sort xs` takes only $O(n*k)$ time, where $n$ is the length of $xs$, rather than $O(n^2)$ time, as might be expected of insertion sort. *)

use "../../src/4/Stream.sml";
open Stream

fun streamInsertionSort ($ NIL) = $NIL
  | streamInsertionSort ($ (CONS (x, xs))) =
    let
        fun lazy insert ($ NIL) = $ (CONS (x, $ NIL))
          | insert (s as $ (CONS (y, s'))) =
            if x < y then $ (CONS (x, s)) else $ (CONS (y, insert s'))
    in
        insert (streamInsertionSort xs)
    end



(* 
streamInsertionSort ($ (CONS (3, $ (CONS (2, $ (CONS (1, $NIL)))))))
insert (3, streamInsertionSort ($ (CONS (2, $ (CONS (1, $NIL))))))
insert (3, insert (2, streamInsertionSort ($ (CONS (1, $NIL)))))
insert (3, insert (2, insert (1, streamInsertionSort ($ NIL))))
insert (3, insert (2, insert (1, $NIL)))
insert (3, insert (2, $ (CONS (1, $NIL))))
insert (3, $ (CONS (1, insert (2, $NIL))))
$ (CONS (1, insert (3, insert (2, $NIL))))
*)
