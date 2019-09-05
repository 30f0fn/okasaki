use "../../src/2/ORDERED.sml";
use "../../src/5/SORTINGHEAP.sml";
use "../../src/5/SplayHeap.sml";

functor SortingSplayHeap (Element : ORDERED) : SORTINGHEAP =
    struct
        structure SplHp = SplayHeap(Element)
        open SplHp

        fun fromList (nil : Element.T list) = E
          | fromList (h :: t) = insert (h, (fromList t))

        fun inOrderTraverse E = nil
          | inOrderTraverse (T (E, x, b)) = x :: inOrderTraverse(b)
          | inOrderTraverse (T (a, x, b)) = (inOrderTraverse a) @
                                            (inOrderTraverse (T (E, x, b)))

        val sort = inOrderTraverse o fromList
    end


