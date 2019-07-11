use "../../src/2/4.sml";


structure UnbalancedIntSet = UnbalancedSet (DupInts)

open UnbalancedIntSet

val emptySet = empty
val set_1 = insert (1, emptySet)
val set_1_2 = insert (2, set_1)
val set_1_2_3 = insert (3, set_1_2)
val set_1_3 = insert (3, set_1)
val set_1_3_2 = insert (2, set_1_3)
val set_2 = insert (2, emptySet)
val set_2_1 = insert (1, set_2)
val set_2_1_3 = insert (3, set_2_1)
val set_2_3 = insert (3, set_2)
val set_2_3_1 = insert (1, set_2_3)
val set_3 = insert (3, emptySet)
val set_3_1 = insert (1, set_3)
val set_3_1_2 = insert (2, set_3_1)
val set_3_2 = insert (2, set_3)
val set_3_2_1 = insert (1, set_3_2)

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

