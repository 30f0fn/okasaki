(* Exercise 3.4  Weight-biased leftist heaps are an alternative to leftist heaps that replace the leftist property with the *weight-biased leftist property*: the size of any left child is at least as large as the size of its right sibling. *)

(* (b) Implement weight-biased leftist heaps. *)
(* (c) Currently, merge operates in two passes: a top-down pass consisting of calls to `merge`, and a bottom-up pass consisting of calls to the helper function `makeT`.  Modify `merge` for weight-biased leftist heaps to operate in a single, top-down pass. *)

use "../2/ORDERED.sml";
use "../3/HEAP.sml";

exception EmptyHeapException 

functor LeftistHeap (Element : ORDERED) : HEAP =
    struct
        structure Elem = Element
        datatype Heap = E | T of int * Elem.T * Heap * Heap
        val empty = E
        fun isEmpty E = true 
          | isEmpty _ = false

        fun size E = 0
          | size (T (s, _, _, _)) = s

        fun merge (h, E) = h
          | merge (E, h) = h
          | merge (h1 as T(w1, x1, a1, b1),
                   h2 as T(w2, x2, a2, b2)) =
            let
                val newSize = w1 + w2 + 1
            in
                case (Element.leq (x1, x2),
                      size b1 + size h2 > size a1,
                      size b2 + size h1 > size a2)
                 of
                    (true, true, _) => T(newSize, x1, merge(b1, h2), a1)
                  | (true, false, _) => T(newSize, x1, a1, merge(b1, h2))
                  | (false, _, true) => T(newSize, x2, merge(b2, h1), a2)
                  | (false,  _, false) => T(newSize, x2, a2, merge(b2, h1))
            end
                
        fun singleton x = T (1, x, E, E)
        fun findMin E = raise EmptyHeapException
          | findMin (T(_, x, _, _)) = x
        fun deleteMin E = raise EmptyHeapException
          | deleteMin (T(_, _, a, b)) = merge(a, b)
        fun insert (x, H) = merge (singleton x, H)
    end
