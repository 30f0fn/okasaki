use "../../src/2/ORDERED.sml";
use "../../src/5/BinTree.sml";

signature HEAP_WITH_TOBINARY =
  sig
      structure Elem : ORDERED

      type Heap

      val empty : Heap
      val isEmpty : Heap -> bool

      val insert : Elem.T * Heap -> Heap
      val merge : Heap * Heap -> Heap
      val findMin : Heap -> Elem.T
      val deleteMin : Heap -> Heap

      val toBinary : Heap -> (Elem.T BinTree)
  end
