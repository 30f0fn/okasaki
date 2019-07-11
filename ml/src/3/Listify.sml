functor Listify (heap : HEAP) : LISTIFY =
    struct
        structure Heap = heap
        structure Elem = HeapStruct.Elem
        fun doIt h =
            let
                fun aux (h, l) = if Heap.isEmpty h
                                 then l
                                 else
                                     aux (Heap.deleteMin h, (Heap.findMin h) :: l)
            in
                aux (h, nil)
            end
    end
    
