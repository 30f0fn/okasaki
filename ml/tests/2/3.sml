use "../../src/2/3.sml";

structure DupInts : ORDERED = struct
        type T = int
        fun eq (x,y) = x = y
        fun lt (x,y) = x < y
        fun leq (x,y) = x <= y
    end

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


