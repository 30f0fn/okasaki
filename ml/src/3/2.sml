(* Exercise 2.2 reimplement `insert` directly, without merge *)

use "../../src/3/HEAP.sml";
use "../2/ORDERED.sml";


exception EmptyHeapException 

functor LeftistHeap (Element : ORDERED) : HEAP =
    struct
        structure Elem = Element
        datatype Heap = E | T of int * Elem.T * Heap * Heap
        val empty = E
        fun isEmpty E = true 
          | isEmpty _ = false
        fun rank E = 0
          | rank (T (r, _, _, _)) = r
        fun makeT(x, a, b) = if rank a >= rank b
                             then T (rank b + 1, x, a, b)
                             else T (rank a + 1, x, b, a)
        fun merge (h, E) = h
          | merge (E, h) = h
          | merge (h1 as T(_, x1, a1, b1), h2 as T(_, x2, a2, b2)) =
            if Elem.leq (x1, x2) then makeT(x1, a1, merge(b1, h2))
            else makeT(x2, a2, merge(b2, h1))
        fun singleton x = T (1, x, E, E)
        fun findMin E = raise EmptyHeapException
          | findMin (T(_, x, _, _)) = x
        fun deleteMin E = raise EmptyHeapException
          | deleteMin (T(_, _, a, b)) = merge(a, b)
        (* fun insert (x, H) = merge (singleton x, H) *)
        fun insert (x, E) = singleton x
          | insert (x, T(r, y, a, b)) = if Elem.leq (y, x)
                                        then T(r, y, insert (x, a), b)
                                        else T(r, x, insert (y, a), b)
    end



