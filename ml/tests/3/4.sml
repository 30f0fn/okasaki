use "../../src/3/4.sml";

(* tests *)

fun throwsEmpty f h =
    let
        val _ = f h
    in
        false
    end
    handle EmptyHeapException => true

structure DupInts : ORDERED =
    struct
        type T = int
        fun eq (x,y) = x = y
        fun lt (x,y) = x < y
        fun leq (x,y) = x <= y
    end

structure IntHeap = LeftistHeap (DupInts)

open IntHeap

val emptyHeap = empty
val h_1 = insert (1, emptyHeap)
val h_1_2 = insert (2, h_1)
val h_1_2_3 = insert (3, h_1_2)
val h_1_3 = insert (3, h_1)
val h_1_3_2 = insert (2, h_1_3)
val h_2 = insert (2, emptyHeap)
val h_2_1 = insert (1, h_2)
val h_2_1_3 = insert (3, h_2_1)
val h_2_3 = insert (3, h_2)
val h_2_3_1 = insert (1, h_2_3)
val h_3 = insert (3, emptyHeap)
val h_3_1 = insert (1, h_3)
val h_3_1_2 = insert (2, h_3_1)
val h_3_2 = insert (2, h_3)
val h_3_2_1 = insert (1, h_3_2)
val h_3 = insert (3, emptyHeap)
val h_3_4 = insert (4, h_3)
val h_3_4_5 = insert (5, h_3_4)
val h_3_5 = insert (5, h_3)
val h_3_5_4 = insert (4, h_3_5)
val h_4 = insert (4, emptyHeap)
val h_4_3 = insert (3, h_4)
val h_4_3_5 = insert (5, h_4_3)
val h_4_5 = insert (5, h_4)
val h_4_5_3 = insert (3, h_4_5)
val h_5 = insert (5, emptyHeap)
val h_5_3 = insert (3, h_5)
val h_5_3_4 = insert (4, h_5_3)
val h_5_4 = insert (4, h_5)
val h_5_4_3 = insert (3, h_5_4)

val heaps123 = [
    (* emptyHeap, *)
    h_1,
    h_1_2,
    h_1_2_3,
    h_1_3,
    h_1_3_2,
    h_2,
    h_2_1,
    h_2_1_3,
    h_2_3,
    h_2_3_1,
    h_3,
    h_3_1,
    h_3_1_2,
    h_3_2,
    h_3_2_1
]

val heaps345 = [
    h_3,
    h_3_4,
    h_3_4_5,
    h_3_5,
    h_3_5_4,
    h_4,
    h_4_3,
    h_4_3_5,
    h_4_5,
    h_4_5_3,
    h_5,
    h_5_3,
    h_5_3_4,
    h_5_4,
    h_5_4_3
]


val empty_is_empty = isEmpty emptyHeap
val findMin_of_empty_throws_empty = throwsEmpty findMin emptyHeap
val findMin_of_h_1 = findMin h_1 = 1
val findMin_of_h_1_2 = findMin h_1_2 = 1
val findMin_of_h_1_2_3 = findMin h_1_2_3 = 1
val findMin_of_h_1_3 = findMin h_1_3 = 1
val findMin_of_h_1_3_2 = findMin h_1_3_2 = 1
val findMin_of_h_2 = findMin h_2 = 2
val findMin_of_h_2_1 = findMin h_2_1 = 1
val findMin_of_h_2_1_3 = findMin h_2_1_3 = 1
val findMin_of_h_2_3 = findMin h_2_3 = 2
val findMin_of_h_2_3_1 = findMin h_2_3_1 = 1
val findMin_of_h_3 = findMin h_3 = 3
val findMin_of_h_3_1 = findMin h_3_1 = 1
val findMin_of_h_3_1_2 = findMin h_3_1_2 = 1
val findMin_of_h_3_2 = findMin h_3_2 = 2
val findMin_of_h_3_2_1 = findMin h_3_2_1 = 1

val merge_1__4 = merge (h_1, h_4)
val merge_1_2__4 = merge (h_1_2, h_4)
val merge_1__5_3 = merge (h_1, h_5_3)
                         
(* verify that findMin merge h1 h2 returns the minimum of findMin h1 and findMin h2, for everything in a big cartesian product *)

fun test_merge_one_against_many (h1, nil) = true
  | test_merge_one_against_many (h1, h2 :: t) =
    let
        val m = merge(h1, h2)
        val min1 = findMin h1
        val min2 = findMin h2
    in
        if throwsEmpty findMin h1 andalso throwsEmpty findMin h2
        then throwsEmpty findMin m
        else
            findMin m = Int.min (findMin h1, findMin h2)
            andalso test_merge_one_against_many (h1, t)
    end

fun test_merge (nil, l2) = true
  | test_merge (h1 :: t1, l2) =
    test_merge_one_against_many (h1, l2) andalso test_merge (t1, l2)


val test_merge_123_345 = test_merge (heaps123, heaps345)

(* test delMin with the resultant heapSort *)

fun isSorted nil = true
  | isSorted (_ :: nil) = true
  | isSorted (h1 :: h2 :: l) = h1 <= h2 andalso isSorted (h2 :: l)

val intList = [9, 1, ~3, 7, 0, 3, ~6, 1, 7, 0, 100, 44, ~651]

fun heapify elements = foldl (fn (k, h) => insert (k, h))
                             emptyHeap
                             elements

val bigHeap = heapify intList

fun listify h =
    let
        fun aux (h, l) = if isEmpty h
                         then l
                         else
                             aux (deleteMin h, (findMin h) :: l)
    in
        aux (h, nil)
    end

fun reverse l =
    let
        fun aux (nil, l) = l
          | aux (h :: t, l) = aux (t, h :: l)
    in
        aux (l, nil)
    end

fun heapSort l = reverse (listify (heapify l))

val should_be_sorted = heapSort intList

val test_deleteMin = isSorted should_be_sorted
