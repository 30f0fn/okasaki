Control.Print.printDepth := 100;

use "../../src/3/10b.sml";
use "../../tests/2/DupInts.sml";

structure RedBlackIntSet = RedBlackSet (DupInts)

open RedBlackIntSet

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


val emptySet = empty
val set_0 = insert (0, emptySet)
val set_0_2 = insert (2, set_0)
val set_0_2_4 = insert (4, set_0_2)
val set_0_2_4_6 = insert (6, set_0_2_4)
val set_0_2_4_6_1 = insert (1, set_0_2_4_6)
val set_0_2_4_6_1_5 = insert (5, set_0_2_4_6_1)
val set_0_2_4_6_1_5_3 = insert (3, set_0_2_4_6_1)

                                                    
