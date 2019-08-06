use "../../src/5/7.sml";
use "../../tests/3/2.sml";

structure sortingHeap = SortingSplayHeap (DupInts)

open sortingHeap

val list0123 = [0, 1, 2, 3]
val list0123sorted = sort list0123

val list3210 = [3, 2, 1, 0]
val list3210sorted = sort list3210

val nilsorted = sort []

val list3120 = [3, 1, 2, 0]
val list3120sorted = sort list3120

