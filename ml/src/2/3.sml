(* Inserting an existing element into a binary search tree copies the entire search path even though the copied nodes are indistinguishable from the originals. Rewrite insert using exceptions to avoid this copying. Establish only one handler per insertion rather than one handler per iteration. *)

use "./ORDERED.sml";
use "./SET.sml";

functor UnbalancedSet (Element : ORDERED) : SET =
    struct
        type Elem = Element.T
        datatype Tree = E | T of Tree * Elem * Tree
        type Set = Tree

        val empty = E

        fun insert (x, t) = 
            let
                exception AlreadyExists
                fun aux E = T (E, x, E)
                  | aux (t as T(a, y, b)) =
                    if Element.lt(x, y) then T(aux a, y, b)
                    else if Element.lt(y, x) then T(a, y, aux b)
                    else raise AlreadyExists
            in
                aux t
            end
            handle AlreadyExists => t            

        fun member (x, t) =
            let
                fun aux (x, E, SOME y) = Element.eq(x, y)
                  | aux (x, E, NONE) = false
                  | aux (x, (t as T(a, y, b)), poss) =
                    if Element.lt(x, y)
                    then aux (x, a, poss)
                    else aux (x, b, SOME y)
            in
                aux (x, t, NONE)
            end
    end

