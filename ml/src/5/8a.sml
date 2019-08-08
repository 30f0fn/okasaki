(*
3849
convert multiway trees to binary trees.  convert each multiway node m into a binary node whose left child represents the leftmost child of m, and whose right child represents the sibling to the immediate right of m.  If either would-be children are missing, the corresponding child field is empty.  (So the right child of the root is always empty.)  This gives half-ordered binary trees where no element exceeds any element of its left subtree.

8a) Write a function toBinary which convert pairing heaps from existing representation into 
datatype BinTree = E' | T' of Elem.T * BinTree * BinTree
8b) Reimplement pairing heaps using this new implementation

for 8a and 8b, make a public BinTree datatype
set this as the return type of toBinary function, and as the value of the Heap field of BinaryPairingHeap

*)


use "../../src/2/ORDERED.sml";
use "../../src/5/HEAP_WITH_TOBINARY.sml";
use "../../src/5/BinTree.sml";

functor PairingHeap (Element : ORDERED) : HEAP_WITH_TOBINARY = 
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

        fun toBinary E = E'
          | toBinary t =
            let
                fun ofKids nil = E' 
                  | ofKids (T (a, grandkids) :: rest) =
                    T' (ofKids grandkids, a, ofKids rest)
            in 
                ofKids [t]
            end
    end
