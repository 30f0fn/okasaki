use "../2/ORDERED.sml";
use "../3/HEAP.sml";

signature SORTINGHEAP =
  sig
      include HEAP
      val sort : Elem.T list -> Elem.T list
  end
