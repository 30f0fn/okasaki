(*
3849
convert multiway trees to binary trees.  convert each multiway node m into a binary node whose left child represents the leftmost child of m, and whose right child represents the sibling to the immediate right of m.  If either would-be children are missing, the corresponding child field is empty.  (So the right child of the root is always empty.)  This gives half-ordered binary trees where no element exceeds any of its left descendants.

8a) Write a function toBinary which convert pairing heaps from existing representation into 
datatype BinTree = E' | T' of Elem.T * BinTree * BinTree
8b) Reimplement pairing heaps using this new implementation

for 8a and 8b, make a public BinTree datatype
set this as the return type of toBinary function, and as the value of the Heap field of BinaryPairingHeap

*)


use "../../src/2/ORDERED.sml";
use "../../src/3/HEAP.sml";
use "../../src/5/BinTree.sml";

functor BinaryPairingHeap (Element : ORDERED) : HEAP = 
    struct
        structure Elem = Element
        datatype Heap = E' | T' of Heap * Elem.T * Heap

        val empty = E'
        fun isEmpty E' = true
          | isEmpty _ = false

        exception Empty
                      
        fun findMin E' = raise Empty
          | findMin (T' (l, a, r)) = a

        fun merge (E', h) = h
          | merge (h, E') = h
          | merge (h1 as T' (l1, a1, r1),
                   h2 as T' (l2, a2, r2)) =
            if Elem.leq (a1, a2)
            then T' (merge (l1, r1), a1, h2)
            else T' (h1, a2, merge (l2, r2))

        fun insert (a, h) = merge (T' (E', a, E'), h)

        fun deleteMin (T' (l, _, r)) = merge(l, r)

    end

