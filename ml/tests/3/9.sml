use "../../src/3/9.sml";

structure RedBlackFromList = RedBlackSet (DupInts)
val c = RedBlackFromList.fromList [];
val d = RedBlackFromList.fromList [1];
val e = RedBlackFromList.fromList [1, 2];
val f = RedBlackFromList.fromList [1, 2, 3];
val g = RedBlackFromList.fromList [1, 2, 3, 4];
val h = RedBlackFromList.fromList [1, 2, 3, 4, 5];
val i = RedBlackFromList.fromList [1, 2, 3, 4, 5, 6];
val j = RedBlackFromList.fromList [1, 2, 3, 4, 5, 6, 7];
val k = RedBlackFromList.fromList [1, 2, 3, 4, 5, 6, 7, 8];
