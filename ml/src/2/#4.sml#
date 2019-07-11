(* Rewrite member to take no more than d+1 comparisons by recording candidate elements that might be equal to the query element, and checking for equality only when you hit the bottom of the tree *)

use "ORDERED.sml";
use "SET.sml";


functor UnbalancedSet (Element : ORDERED) : SET =
    struct
        type Elem = Element.T
        datatype Tree = E | T of Tree * Elem * Tree
        type Set = Tree

        val empty = E

        fun insert (x, t) = 
            let
                exception AlreadyExists
                fun aux (E, NONE) = T (E, x, E)
                  | aux (E, SOME y) =
                    if Element.eq (x, y)
                    then raise AlreadyExists
                    else T (E, x, E)
                  | aux (t as T(a, y, b), poss) =
                    if Element.lt(x, y)
                    then T(aux (a, poss), y, b)
                    else T(a, y, aux (b, SOME y))
            in
                aux (t, NONE)
            end
            handle AlreadyExists => t;

        fun member (x, t) =
            let
                fun aux (E, SOME y) = Element.eq(x, y)
                  | aux (E, NONE) = false
                  | aux ((t as T(a, y, b)), poss) =
                    if Element.lt(x, y)
                    then aux (a, poss)
                    else aux (b, SOME y)
            in
                aux (t, NONE)
            end
    end

structure DupInts : ORDERED = struct
        type T = int
        fun eq (x,y) = x = y
        fun lt (x,y) = x < y
        fun leq (x,y) = x <= y
    end

