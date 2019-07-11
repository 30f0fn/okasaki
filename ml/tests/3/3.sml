Control.Print.printDepth := 1024;
(* <!-- 8923 --> *)

use "../../src/3/3.sml";
use "../../src/3/LISTIFY.sml";
use "../../src/3/Listify.sml";
use "../../tests/2/DupInts.sml";
use "../../tests/3/2.sml";

val log_add_4_5_6 = logReduceToSingleton (fn (a, b) => a + b) 0 [4, 5, 6]

structure HeapStruct = LeftistHeap(DupInts)

fun s e = HeapStruct.insert(e, HeapStruct.empty)

val singletonHeaps = map s [1, 8, 3, 5]

val log_merge_manual = hd (logReduceToSingleton HeapStruct.merge HeapStruct.empty singletonHeaps)

structure HeapStruct = LeftistHeap(DupInts)
structure mergeListStruct = fromList(HeapStruct)
val mergeList = mergeListStruct.doIt

val l = [1, 3, 9, 0, ~5, 3, 7, ~3]

val l_merged = mergeList l


structure listifyStructure = Listify(HeapStruct)
val listify = listifyStructure.doIt

val l_merged_listified = listify(l_merged)
