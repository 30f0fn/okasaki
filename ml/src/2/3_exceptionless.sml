(* Inserting an existing element into a binary search tree copies the entire search path even though the copied nodes are indistinguishable from the originals. Rewrite insert to avoid this copying, but do not use exceptions. *)

use "./ORDERED.sml";
use "./SET.sml";

functor UnbalancedSet (Element : ORDERED) : SET =
    struct
        type Elem = Element.T
        datatype Tree = E | T of Tree * Elem * Tree
        type Set = Tree

        val empty = E

        fun optT (SOME t1, x, SOME t2) = SOME (T (t1, x, t2))
          | optT (NONE, _, _) = NONE
          | optT (_, _, NONE) = NONE
            

        fun insert (x, t) = 
            let
                fun aux E = SOME (T (E, x, E))
                  | aux (t as T(a, y, b)) =
                    if Element.lt(x, y) then optT (aux a, y, SOME b)
                    else if Element.lt(y, x) then optT (SOME a, y, aux b)
                    else NONE
            in
                case aux t of
                    SOME r => r
                  | NONE => t
            end

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

