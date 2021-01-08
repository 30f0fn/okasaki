(*
3849
convert multiway trees to binary trees.  
convert each multiway node m into a binary node whose left child represents the leftmost child of m, 
and whose right child represents the sibling to the immediate right of m.  
If either would-be children are missing, the corresponding child field is empty.  
(So the right child of the root is always empty.)  
This gives half-ordered binary trees where no element exceeds any element of its left subtree.

8a) Write a function toBinary which convert pairing heaps from existing representation into 
datatype BinTree = E' | T' of Elem.T * BinTree * BinTree
8b) Reimplement pairing heaps using this new implementation

for 8a and 8b, make a public BinTree datatype
set this as the return type of toBinary function, and as the value of the Heap field of BinaryPairingHeap

*)


use "../../src/2/ORDERED.sml";
use "../../src/5/HEAP_WITH_TOBINARY.sml";
use "../../src/5/BinTree.sml";
use "../../src/5/PairingHeap.sml";

functor PairingHeapWithToBinary (Element : ORDERED) = 
    struct
        structure PrngHp = PairingHeap
        open PrngHp

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
