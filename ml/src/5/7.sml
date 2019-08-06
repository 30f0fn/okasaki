use "../../src/2/ORDERED.sml";
use "../../src/5/SORTINGHEAP.sml";


functor SortingSplayHeap (Element : ORDERED) : SORTINGHEAP =
    struct
        structure Elem = Element
        datatype Heap = E | T of Heap * Elem.T * Heap

        val empty = E
        fun isEmpty E = true 
          | isEmpty  _ = false
                             
        fun partition (p, E) = (E, E)
          | partition (p, t as T (a, x, b)) =
            if Elem.leq (x, p)
            then
                case b of
                    E => (t, E)
                  | T (b1, y, b2) =>
                    if Elem.leq (y, p)
                    then
                        let
                            val (smaller, bigger) = partition (p, b2)
                        in
                            (T (T (a, x, b1), y, smaller), bigger)
                        end
                    else
                        let
                            val (smaller, bigger) = partition (p, b1)
                        in
                            (T (a, x, smaller), T (bigger, y, b2))
                        end
            else
                case a of
                    E => (E, t)
                  | T (a1, y, a2) =>
                    if Elem.lt (p, y)
                    then
                        let
                            val (smaller, bigger) = partition (p, a1)
                        in
                            (smaller, T (bigger, y, T (a2, x, b)))
                        end
                    else
                        let
                            val (smaller, bigger) = partition (p, a2)
                        in
                            (T (a1, y, smaller), T (bigger, x, b))
                        end

        fun insert (x, t) =
            let
                val (smaller, bigger) = partition (x, t)
            in
                T (smaller, x, bigger)
            end

        fun merge (E, t) = t
          | merge (T (a, x, b), t) =
            let
                val (smaller, bigger) = partition (x, t)
            in
                T (merge (a, smaller), x, merge (b, smaller))
            end
                            
        fun findMin (T (E, x, _)) = x
          | findMin (T (a, _, _)) = findMin a

        fun deleteMin (T (E, x, b)) = b
          | deleteMin (T (T (E, x, b), y, c)) = T (b, y, c)
          | deleteMin (T (T (a, x, b), y, c)) = T (deleteMin a, x, T (b, y, c)) 

        fun fromList (nil : Element.T list) = E
          | fromList (h :: t) = insert (h, (fromList t))

        fun inOrderTraverse E = nil
          | inOrderTraverse (T (E, x, b)) = x :: inOrderTraverse(b)
          | inOrderTraverse (T (a, x, b)) = (inOrderTraverse a) @
                                            (inOrderTraverse (T (E, x, b)))

        val sort = inOrderTraverse o fromList
    end


