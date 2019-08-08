use "../../src/2/ORDERED.sml";
use "../../src/3/HEAP.sml";

functor PairingHeap (Element : ORDERED) : HEAP = 
    struct
        structure Elem = Element
        datatype Heap = E | T of Elem.T * Heap list
                                               
        val empty = E
        fun isEmpty E = true
          | isEmpty _ = false

        fun findMin (T (a, hs)) = a

        fun merge (E, h) = h
          | merge (h, E) = h
          | merge (h1 as T (a1, hs1),
                   h2 as T (a2, hs2)) =
            if Elem.leq (a1, a2)
            then T (a1, h2 :: hs1)
            else T (a2, h1 :: hs2)

        fun insert (a, h) = merge (T (a, []), h)

        fun mergePairs [] = E
          | mergePairs [h] = h
          | mergePairs (h1 :: h2 :: hs) =
            merge (merge (h1, h2), mergePairs hs)

        fun deleteMin (T (x, hs)) = mergePairs hs
    end
