(* Rewrite member to take no more than d+1 comparisons by recording candidate elements that might be equal to the query element, and checking for equality only when you hit the bottom of the tree *)

signature ORDERED =
sig
    type T
    val eq : T * T -> bool
    val lt : T * T -> bool
    val leq : T * T -> bool
end

signature SET =
sig
    type Elem
    type Set
    val empty : Set
    val insert : Elem * Set -> Set
    val member : Elem * Set -> bool
end

functor UnbalancedSet (Element : ORDERED) : SET =
struct
type Elem = Element.T
datatype Tree = E | T of Tree * Elem * Tree
type Set = Tree

val empty = E

fun insert (x, t) = 
    let
        exception AlreadyExists
        fun aux (E, NONE) = T (E, x, E)
          | aux (E, SOME y) =
            if Element.eq (x, y)
            then raise AlreadyExists
            else T (E, x, E)
          | aux (t as T(a, y, b), poss) =
            if Element.lt(x, y)
            then T(aux (a, poss), y, b)
            else T(a, y, aux (b, SOME y))
    in
        aux (t, NONE)
    end
    handle AlreadyExists => t;

fun member (x, t) =
    let
        fun aux (E, SOME y) = Element.eq(x, y)
          | aux (E, NONE) = false
          | aux ((t as T(a, y, b)), poss) =
            if Element.lt(x, y)
            then aux (a, poss)
            else aux (b, SOME y)
    in
        aux (t, NONE)
    end
end

structure DupInts : ORDERED = struct
type T = int
fun eq (x,y) = x = y
fun lt (x,y) = x < y
fun leq (x,y) = x <= y
end

(* TESTS *)

structure UnbalancedIntSet = UnbalancedSet (DupInts)

open UnbalancedIntSet

val emptySet = UnbalancedIntSet.empty
val set_1 = UnbalancedIntSet.insert(1, emptySet)
val set_1_2 = UnbalancedIntSet.insert(2, set_1)
val set_1_2_3 = UnbalancedIntSet.insert(3, set_1_2)
val set_1_1 = UnbalancedIntSet.insert(1, set_1)
val set_1_2_2 = UnbalancedIntSet.insert(2, set_1_2)
val set_1_2_1 = UnbalancedIntSet.insert(1, set_1_2)

val emptySet_contains_0 = member (0, emptySet)
val set_1_contains_0 = member (0, set_1)
val set_1_contains_1 = member (1, set_1)
val set_1_contains_2 = member (2, set_1)
val set_1_2_contains_0 = member (0, set_1_2)
val set_1_2_contains_1 = member (1, set_1_2)
val set_1_2_contains_2 = member (2, set_1_2)
val set_1_2_contains_3 = member (3, set_1_2)
val set_1_2_3_contains_0 = member (0, set_1_2_3)
val set_1_2_3_contains_1 = member (1, set_1_2_3)
val set_1_2_3_contains_2 = member (2, set_1_2_3)
val set_1_2_3_contains_3 = member (3, set_1_2_3)
val set_1_2_3_contains_4 = member (0, set_1_2_3)


val test_emptySet_contains_0 = member (0, emptySet) = true
val test_set_1_contains_0 = member (0, set_1) = false
val test_set_1_contains_1 = member (1, set_1) = true
val test_set_1_contains_2 = member (2, set_1) = false
val test_set_1_2_contains_0 = member (0, set_1_2) = false
val test_set_1_2_contains_1 = member (1, set_1_2) = true
val test_set_1_2_contains_2 = member (2, set_1_2) = true
val test_set_1_2_contains_3 = member (3, set_1_2) = false
val test_set_2_1_contains_0 = member (0, set_2_1) = false
val test_set_2_1_contains_1 = member (1, set_2_1) = true
val test_set_2_1_contains_2 = member (2, set_2_1) = true
val test_set_2_1_contains_3 = member (3, set_2_1) = false
val test_set_1_2_3_contains_0 = member (0, set_1_2_3) = false
val test_set_1_2_3_contains_1 = member (1, set_1_2_3) = true
val test_set_1_2_3_contains_2 = member (2, set_1_2_3) = true
val test_set_1_2_3_contains_3 = member (3, set_1_2_3) = true
val test_set_1_2_3_contains_4 = member (0, set_1_2_3) = false
val test_set_2_1_3_contains_0 = member (0, set_2_1_3) = false
val test_set_2_1_3_contains_1 = member (1, set_2_1_3) = true
val test_set_2_1_3_contains_2 = member (2, set_2_1_3) = true
val test_set_2_1_3_contains_3 = member (3, set_2_1_3) = true
val test_set_2_1_3_contains_4 = member (0, set_2_1_3) = false
val test_set_3_2_1_contains_0 = member (0, set_3_2_1) = false
val test_set_3_2_1_contains_1 = member (1, set_3_2_1) = true
val test_set_3_2_1_contains_2 = member (2, set_3_2_1) = true
val test_set_3_2_1_contains_3 = member (3, set_3_2_1) = true
val test_set_3_2_1_contains_4 = member (0, set_3_2_1) = false

