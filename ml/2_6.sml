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
datatype 'a MapTree = E | MT of 'a MapTree * (Key * 'a) * 'a MapTree
type 'a Map = 'a MapTree
val empty = E
fun bind (k, v, E) = MT (E, (k, v), E)
  | bind (k, v, mt as MT(a, (k1, v1), b)) =
    if K.lt(k, k1) then MT(bind(k, v, a), (k1, v1), b)
    else if K.lt(k1, k) then MT(a, (k1, v1), bind(k, v, b))
    else mt
fun lookup (k, E) = raise NotFound
  | lookup (k, mt as MT (a, (k1, v1), b)) =
    if K.lt (k, k1) then lookup (k, a)
    else if K.lt (k1, k) then lookup (k, b)
    else v1
end

(* TESTS *)

(* structure UnbalancedIntSet = UnbalancedSet (DupInts) *)

(* open UnbalancedIntSet *)

(* val emptySet = UnbalancedIntSet.empty *)
(* val set_1 = UnbalancedIntSet.insert(1, emptySet) *)
(* val set_1_2 = UnbalancedIntSet.insert(2, set_1) *)
(* val set_1_2_3 = UnbalancedIntSet.insert(3, set_1_2) *)
(* val set_2_1 = UnbalancedIntSet.insert(2, set_1) *)
(* val set_2_1_3 = UnbalancedIntSet.insert(3, set_1_2) *)
(* val set_3_2_1 = UnbalancedIntSet.insert(3, set_1_2) *)

(* val list_1_2_contains_1 = inList 1 [1,2] *)

(* val test_emptySet_contains_0 = member (0, emptySet) = true *)
(* val test_set_1_contains_0 = member (0, set_1) = false *)
(* val test_set_1_contains_1 = member (1, set_1) = true *)
(* val test_set_1_contains_2 = member (2, set_1) = false *)
(* val test_set_1_2_contains_0 = member (0, set_1_2) = false *)
(* val test_set_1_2_contains_1 = member (1, set_1_2) = true *)
(* val test_set_1_2_contains_2 = member (2, set_1_2) = true *)
(* val test_set_1_2_contains_3 = member (3, set_1_2) = false *)
(* val test_set_2_1_contains_0 = member (0, set_2_1) = false *)
(* val test_set_2_1_contains_1 = member (1, set_2_1) = true *)
(* val test_set_2_1_contains_2 = member (2, set_2_1) = true *)
(* val test_set_2_1_contains_3 = member (3, set_2_1) = false *)
(* val test_set_1_2_3_contains_0 = member (0, set_1_2_3) = false *)
(* val test_set_1_2_3_contains_1 = member (1, set_1_2_3) = true *)
(* val test_set_1_2_3_contains_2 = member (2, set_1_2_3) = true *)
(* val test_set_1_2_3_contains_3 = member (3, set_1_2_3) = true *)
(* val test_set_1_2_3_contains_4 = member (0, set_1_2_3) = false *)
(* val test_set_2_1_3_contains_0 = member (0, set_2_1_3) = false *)
(* val test_set_2_1_3_contains_1 = member (1, set_2_1_3) = true *)
(* val test_set_2_1_3_contains_2 = member (2, set_2_1_3) = true *)
(* val test_set_2_1_3_contains_3 = member (3, set_2_1_3) = true *)
(* val test_set_2_1_3_contains_4 = member (0, set_2_1_3) = false *)
(* val test_set_3_2_1_contains_0 = member (0, set_3_2_1) = false *)
(* val test_set_3_2_1_contains_1 = member (1, set_3_2_1) = true *)
(* val test_set_3_2_1_contains_2 = member (2, set_3_2_1) = true *)
(* val test_set_3_2_1_contains_3 = member (3, set_3_2_1) = true *)
(* val test_set_3_2_1_contains_4 = member (0, set_3_2_1) = false *)

