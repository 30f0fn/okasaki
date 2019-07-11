signature LISTIFY =
    sig
        structure Heap : HEAP
        val doIt : Heap.Heap -> Heap.Elem.T list
    end
