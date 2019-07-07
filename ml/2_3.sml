(* Inserting an existing element into a binary search tree copies the entire search path even though the copied nodes are indistinguishable from the originals. Rewrite insert using exceptions to avoid this copying. Establish only one handler per insertion rather than one handler per iteration. *)

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
        fun aux E = T (E, x, E)
          | aux (t as T(a, y, b)) =
            if Element.lt(x, y) then T(aux a, y, b)
            else if Element.lt(y, x) then T(a, y, aux b)
            else raise AlreadyExists
    in
        aux t
    end
    handle AlreadyExists => t

fun member (x, t) =
    let
        fun aux (x, E, SOME y) = Element.eq(x, y)
          | aux (x, E, NONE) = false
          | aux (x, (t as T(a, y, b)), poss) =
            if Element.lt(x, y)
            then aux (x, a, poss)
            else aux (x, b, SOME y)
    in
        aux (x, t, NONE)
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


