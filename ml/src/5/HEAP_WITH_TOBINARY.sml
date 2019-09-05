use "../../src/2/ORDERED.sml";
use "../../src/3/HEAP.sml";
use "../../src/5/BinTree.sml";

signature HEAP_WITH_TOBINARY =
  sig
      include HEAP
      val toBinary : Heap -> (Elem.T BinTree)
  end
