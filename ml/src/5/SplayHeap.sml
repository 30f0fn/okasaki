use "../../src/2/ORDERED.sml";
use "../../src/3/HEAP.sml";


functor SplayHeap (Element : ORDERED) : HEAP =
    struct
        structure Elem = Element
        datatype Heap = E | T of Heap * Elem.T * Heap

        val empty = E
        fun isEmpty E = true 
          | isEmpty  _ = false
                             
        fun partition (p, E) = (E, E)
          | partition (p, t as T (a, x, b)) =
            if Elem.leq (x, p)
            then
                case b of
                    E => (t, E)
                  | T (b1, y, b2) =>
                    if Elem.leq (y, p)
                    then
                        let
                            val (smaller, bigger) = partition (p, b2)
                        in
                            (T (T (a, x, b1), y, smaller), bigger)
                        end
                    else
                        let
                            val (smaller, bigger) = partition (p, b1)
                        in
                            (T (a, x, smaller), T (bigger, y, b2))
                        end
            else
                case a of
                    E => (E, t)
                  | T (a1, y, a2) =>
                    if Elem.lt (p, y)
                    then
                        let
                            val (smaller, bigger) = partition (p, a1)
                        in
                            (smaller, T (bigger, y, T (a2, x, b)))
                        end
                    else
                        let
                            val (smaller, bigger) = partition (p, a2)
                        in
                            (T (a1, y, smaller), T (bigger, x, b))
                        end

        fun insert (x, t) =
            let
                val (smaller, bigger) = partition (x, t)
            in
                T (smaller, x, bigger)
            end

        fun merge (E, t) = t
          | merge (T (a, x, b), t) =
            let
                val (smaller, bigger) = partition (x, t)
            in
                T (merge (a, smaller), x, merge (b, smaller))
            end
                            
        fun findMin (T (E, x, _)) = x
          | findMin (T (a, _, _)) = findMin a

        fun deleteMin (T (E, x, b)) = b
          | deleteMin (T (T (E, x, b), y, c)) = T (b, y, c)
          | deleteMin (T (T (a, x, b), y, c)) = T (deleteMin a, x, T (b, y, c)) 

    end
    




(* fun bigger (p, E) = E *)
(*   | bigger (p, T (a, x, b)) = *)
(*     if Elem.le (x, p) then bigger (p, b) *)
(*     else *)
(*         case a of *)
(*             E => T (a, x, b) *)
(*           | T (a1, y, a2) => if Elem.le (y, p) *)
(*                              then T (bigger (p, a2), x, b) *)
(*                              else T (bigger (p, a1), y, T (a2, x, b)) *)

(* fun smaller (p, E) = E *)
(*   | smaller (p, T (a, x, b)) = *)
(*     if Elem.gt (x, p) then smaller (p, a) *)
(*     else *)
(*         case b of *)
(*             E => T (a, x, b) *)
(*           | T (b1, x, b2) => if Elem.gt (y, p) *)
(*                              then T (a, x, smaller (p, b1)) *)
(*                              else T (T (a, x, b1), y, smaller (p, b2)) *)
                    

