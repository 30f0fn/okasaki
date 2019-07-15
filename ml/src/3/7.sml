(* Exercise 3.6 Improve the running time of findMin from $O(\log n)$ to $O(1)$ by storing the minimum element separately from the rest of the heap. Implement this as a functor, so don't assume access to heap internals.  *)

use "../../src/2/ORDERED.sml";
use "../../src/3/HEAP.sml";


functor ExplicitMin (H : HEAP) : HEAP =
    struct
        structure Elem = H.Elem
        datatype Heap = E | NE of Elem.T * H.Heap

        val empty = E

        fun isEmpty E = true
          | isEmpty _ = false

        fun insert (e, E) = NE (e, H.insert (e, H.empty)) 
          | insert (e, (NE (min, h))) =
            let
                val newMin = if Elem.lt (e, min) then e else min
            in
                NE (newMin, H.insert (e, h))
            end

        fun merge (E, h) = h
          | merge (h, E) = h
          | merge  (NE (e1, h1), NE (e2, h2)) =
            let
                val minElem = if Elem.leq (e1, e2) then e1 else e2
            in
                NE (minElem, H.merge(h1, h2))
            end

        fun findMin E = raise Empty
          | findMin (NE (e, _)) = e
                
        fun deleteMin E = raise Empty
          | deleteMin (NE (e, h)) =
            let
                val newHeap = H.deleteMin h
            in 
                NE (H.findMin newHeap, newHeap)
            end
    end
