Control.Print.printDepth := 1024;
(* <!-- 8923 --> *)

(* Implement a function `fromList` of type `Elem.t list -> Heap` that produces a leftist heap from an unordered list of elements by first converting each element into a singleton heap and then merging the heaps until only one heap remains.  Instead of merging the heaps in one right-to-left or left-to-right pass using `foldr` or `foldl`, merge the heaps in $\lceil \log n \rceil$ passes, where each pass merges adjacent pairs of heaps.  Show that fromList takes only $O(n)$ time. *)

use "../3/HEAP.sml";
use "../3/LeftistHeap.sml";

(* signature TO_HEAP = *)
(*     sig *)
(*         structure Elem : ORDERED *)
(*         type Heap *)
(*         val doIt : Elem.T -> Heap *)
(*     end *)
        
(* functor ToSingleton (Element : ORDERED) : TO_HEAP = *)
(*     struct *)
(*         structure Elem = Element *)
(*         structure HeapStruct = LeftistHeap(Elem) *)
(*         type Heap = HeapStruct.Heap *)
(*         fun doIt e = HeapStruct.insert (e, HeapStruct.empty) *)
(*     end *)



signature HEAPLIST_TO_HEAP =
    sig
        structure Heap : HEAP
        val doIt : Heap.Elem.T list -> Heap.Heap
    end



fun logReduceToSingleton f a nil = []
  | logReduceToSingleton f a (h :: nil) = [f (a, h)]
  | logReduceToSingleton f a (h1 :: h2 :: l) =
    let
        fun lrfa l = logReduceToSingleton f a l
    in
        lrfa (f (h1, h2) :: lrfa l)
    end



functor fromList (heap : HEAP) : HEAPLIST_TO_HEAP =
    struct
        structure Heap = heap
        fun toSingleton e = Heap.insert (e, Heap.empty)
        fun singletons l = map toSingleton l
        fun doIt l = hd (logReduceToSingleton Heap.merge Heap.empty (singletons l))
    end


































(* open LeftistHeap; *)

(* https://stackoverflow.com/a/19709150/3536879 *)

(* signature ITERABLE = *)
(*     sig *)
(*         type elem *)
(*         type collection *)
(*         val next : collection -> (elem * collection) option *)
(*         val nil : collection *)
(*     end *)

(* signature FOLDL = *)
(*     sig *)
(*         type elem *)
(*         type collection *)
(*         val foldl : ('b * elem -> 'b) -> 'b -> collection -> 'b *)
(*     end *)

(* signature MERGE = *)
(*     sig *)
(*         structure elem : ORDERED *)
(*         val merge : elem.T list -> HEAP *)
(*     end *)

(* functor StandardFoldl (iterable : ITERABLE) : FOLDL = *)
(*     struct *)
(*         type elem = iterable.elem *)
(*         type collection = iterable.collection *)
(*         fun foldl f seed coll = *)
(*             case iterable.next coll of *)
(*                 NONE => seed *)
(*               | SOME (e, coll') => foldl f (f (seed, e)) coll' *)
(*     end *)

(* fun logFoldlToSingleton f a nil = [] *)
(*   | logFoldlToSingleton f a (h :: nil) = [f (a, h)] *)
(*   | logFoldlToSingleton f a (h1 :: h2 :: l) = *)
(*     let *)
(*         fun lf l = logFoldlToSingleton f a l *)
(*     in *)
(*         lf (f (h1, h2) :: lf l) *)
(*     end *)


(* functor LogFoldl (element : ORDERED) : FOLDL = *)
(*     struct *)
(*         structure elem = element *)
(*         fun logFoldlToSingleton f a nil = nil *)
(*           | logFoldlToSingleton f a (h :: nil) = [f (a, h)] *)
(*           | logFoldlToSingleton f a (h1 :: h2 :: l) = *)
(*             let *)
(*                 fun lf l = logFoldlToSingleton f a l *)
(*             in *)
(*                 lf (f (h1, h2) :: lf l) *)
(*             end *)
(*         fun foldl f a l = *)
(*             let *)
(*                 val h :: _ = logFoldlToSingleton f a l *)
(*             in *)
(*                 h *)
(*             end *)
(*     end *)

(* (* functor LogFoldl (iterable : ITERABLE) : FOLDL = *) *)
(* (*     struct *) *)
(* (*         type elem = iterable.elem *) *)
(* (*         type collection = iterable.collection *) *)
(* (*         fun logFoldlToSingleton f a nil = [] *) *)
(* (*           | logFoldlToSingleton f a (h :: nil) = [f (a, h)] *) *)
(* (*           | logFoldlToSingleton f a (h1 :: h2 :: l) = *) *)
(* (*             let *) *)
(* (*                 fun lf l = logFoldlToSingleton f a l *) *)
(* (*             in *) *)
(* (*                 lf (f (h1, h2) :: lf l) *) *)
(* (*             end *) *)
(* (*         fun foldl f a l = *) *)
(* (*             let *) *)
(* (*                 val h :: _ = logFoldlToSingleton f a l *) *)
(* (*             in *) *)
(* (*                 h *) *)
(* (*             end *) *)
(* (*     end *) *)


(* structure DupInts : ORDERED = *)
(*     struct *)
(*         type T = int *)
(*         fun eq (x,y) = x = y *)
(*         fun lt (x,y) = x < y *)
(*         fun leq (x,y) = x <= y *)
(*     end *)


(* structure LeftistIntHeap = LeftistHeap(DupInts) *)

(* fun singleton x = LeftistIntHeap.insert (x, LeftistIntHeap.empty) *)



(* (* fun mergeSingletonsOf h1 h2 = merge ((singleton h1), (singleton h2)) *) *)

(* (* fun fromList l = logReduce mergeSingletonsOf empty l *) *)


