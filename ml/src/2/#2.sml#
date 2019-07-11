(* Rewrite member to take no more than d+1 comparisons by recording candidate elements that might be equal to the query element, and checking for equality only when you hit the bottom of the tree *)

use "./ORDERED.sml";
use "./SET.sml";

functor UnbalancedSet (Element : ORDERED) : SET =
    struct
        type Elem = Element.T
        datatype Tree = E | T of Tree * Elem * Tree
        type Set = Tree

        val empty = E

        fun insert (x, E) = T (E, x, E)
          | insert (x, t as T(a, y, b)) =
            if Element.lt(x, y) then T(insert (x, a), y, b)
            else if Element.lt(y, x) then T(a, y, insert (x, b))
            else t

        fun member (x, t) =
            let
                fun helper (x, E, SOME y) = Element.eq(x, y)
                  | helper (x, E, NONE) = false
                  | helper (x, (t as T(a, y, b)), poss) =
                    if Element.lt(x, y)
                    then helper (x, a, poss)
                    else helper (x, b, SOME y)
            in
                helper (x, t, NONE)
            end
    end

structure DupInts : ORDERED = struct
        type T = int
        fun eq (x,y) = x = y
        fun lt (x,y) = x < y
        fun leq (x,y) = x <= y
    end

