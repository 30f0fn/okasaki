(* Rewrite member to take no more than d+1 comparisons by recording candidate elements that might be equal to the query element, and checking for equality only when you hit the bottom of the tree *)

signature ORDERED =
sig
    type T
    val eq : T * T -> bool
    val lt : T * T -> bool
    val leq : T * T -> bool
end

signature FINITEMAP =
sig
    type Key
    type 'a Map
    val empty : 'a Map
    val bind : Key * 'a * 'a Map -> 'a Map
    val lookup : Key * 'a Map -> 'a (* raise NotFound if key is not found *)
end

exception NotFound;

functor UnbalancedFiniteMap (K : ORDERED) : FINITEMAP =
struct
type Key = K.T
datatype 'a MapTree = E | FM of 'a MapTree * (Key * 'a) * 'a MapTree
type 'a Map = 'a MapTree
val empty = E
fun bind (k, v, E) = FM (E, (k, v), E)
  | bind (k, v, mt as FM(a, (k1, v1), b)) =
    if K.lt(k, k1) then FM(bind(k, v, a), (k1, v1), b)
    else if K.lt(k1, k) then FM(a, (k1, v1), bind(k, v, b))
    else mt
fun lookup (k, E) = raise NotFound
  | lookup (k, mt as FM (a, (k1, v1), b)) =
    if K.lt (k, k1) then lookup (k, a)
    else if K.lt (k1, k) then lookup (k, b)
    else v1
end

(* TESTS *)

structure DupInts : ORDERED = struct
type T = int
fun eq (x,y) = x = y
fun lt (x,y) = x < y
fun leq (x,y) = x <= y
end

structure UnbalancedIntMap = UnbalancedFiniteMap (DupInts)

open UnbalancedIntMap

val emptyMap = empty
val map_1 = bind (1, "a", emptyMap)
val map_1_2 = bind(2, "b", map_1)
val map_1_2_3 = bind(3, "c", map_1_2)
val map_2 = bind (2, "b", emptyMap)
val map_2_1 = bind(1, "a", map_2)
val map_2_1_3 = bind(3, "c", map_2_1)
val map_3 = bind(3, "c", emptyMap)
val map_3_2 = bind(2, "b", map_3)
val map_3_2_1 = bind(1, "a", map_3_2)

fun omits (key, map) =
    let
        val _ = lookup (key, map)
    in
        false
    end
    handle NotFound => true

val test_emptyMap_omits_0 = omits (0, emptyMap)
val test_map_1_omits_0 = omits (0, map_1)
val test_map_1inds_1 = lookup (1, map_1) = "a"
val test_map_1_omits_2 = omits (2, map_1)
val test_map_1_2_omits_0 = omits (0, map_1_2)
val test_map_1_2_contains_1 = lookup (1, map_1_2) = "a"
val test_map_1_2_contains_2 = lookup (2, map_1_2) = "b"
val test_map_1_2_omits_3 = omits (3, map_1_2)
val test_map_2_1_omits_0 = omits (0, map_2_1)
val test_map_2_1_contains_1 = lookup (1, map_2_1) = "a"
val test_map_2_1_contains_2 = lookup (2, map_2_1) = "b"
val test_map_2_1_omits_3 = omits (3, map_2_1)
val test_map_1_2_3_omits_0 = omits (0, map_1_2_3)
val test_map_1_2_3_contains_1 = lookup (1, map_1_2_3) = "a"
val test_map_1_2_3_contains_2 = lookup (2, map_1_2_3) = "b"
val test_map_1_2_3_contains_3 = lookup (3, map_1_2_3) = "c"
val test_map_1_2_3_omits_4 = omits (0, map_1_2_3)
val test_map_2_1_3_omits_0 = omits (0, map_2_1_3)
val test_map_2_1_3_contains_1 = lookup (1, map_2_1_3) = "a"
val test_map_2_1_3_contains_2 = lookup (2, map_2_1_3) = "b"
val test_map_2_1_3_contains_3 = lookup (3, map_2_1_3) = "c"
val test_map_2_1_3_omits_4 = omits (0, map_2_1_3)
val test_map_3_2_1_omits_0 = omits (0, map_3_2_1)
val test_map_3_2_1_contains_1 = lookup (1, map_3_2_1) = "a"
val test_map_3_2_1_contains_2 = lookup (2, map_3_2_1) = "b"
val test_map_3_2_1_contains_3 = lookup (3, map_3_2_1) = "c"
val test_map_3_2_1_omits_4 = omits (0, map_3_2_1)

