use "../../src/2/6.sml";

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


