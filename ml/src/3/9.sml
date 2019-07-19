(* (* Write a function `fromOrdList` of type `Elem list -> Tree` that converts a sorted list with no duplicates into a red-black tree.  The function should run in $O(n)$ time. *) *)

(* The below approach cheats by converting list to an array. *)

(* The basic idea is to build a tree which is very nearly balanced: if it has depth d > 0, then it contains a complete binary tree of depth d - 1.  To satisfy the red-black condition that all branches have the same number of black nodes, we color red exactly those (leaf) nodes at depth d.  Clearly no red node has a red child, because only leaves are red. *)

(* To construct the tree, begin by converting the list to an array.  Now divide the array in half, except reserving the middle element.  We now construct a new tree whose root value is that middle element, and whose children are the trees recursively constructed from the two halves. *)

(* To enforce the red-black distribution noted above, we begin by calculating the depth of the maximal binary tree the desired tree contains, namely as the floor of the base two log of the array length.  Then proceed from the top down, maintaining the current depth as a parameter, and coloring nodes red just when the current depth exceeds the depth of that maximal binary tree.x *)

(* The number of steps of the recursion is proportional to the length of the array, because each step on a non-null subarray consumes an array element, and a step with null subarray is introduced only by a step on a length-2 subarray.  Furthermore, each step takes constant time. *)

(* It might be possible avoid the initial conversion of the list to an array, by iterating through the list and maintaining the constructed trees in a stack.  However, the naive approach to this would work only for lists of length 2^n - 1, and to fix this it would be necessary to invoke some "non-local" information to determine when a singleton tree should take a null tree as its sibling. *)


Control.Print.printDepth := 100;
use "../../tests/2/DupInts.sml";
use "../../src/3/SETFROMLIST.sml";
use "../../src/2/ORDERED.sml";

functor RedBlackSet (Element : ORDERED) : SETFROMLIST =
    struct

        type Elem = Element.T

        datatype Color = Red | Black
        datatype Tree = E | T of Color * Tree * Elem * Tree

        type Set = Tree
        val empty = E
 
        fun member (x, E) = false
          | member (x, T (_, a, y, b)) =
            if Element.lt (x, y) then member (x, a)
            else if Element.lt (y, x) then member (x, b)
            else true

        fun balance (Black, T (Red, T (Red, a, x, b), y, c), z, d)
            = T (Red, T (Black, a, x, b), y, T (Black, c, z, d))
          | balance (Black, T (Red, a, x, T (Red, b, y, c)), z, d)
            = T (Red, T (Black, a, x, b), y, T (Black, c, z, d))
          | balance (Black, a,  x, T (Red, T (Red, b, y, c), z, d))
            = T (Red, T (Black, a, x, b), y, T (Black, c, z, d))
          | balance (Black, a, x, T (Red, b, y, T (Red, c, z, d)))
            = T (Red, T (Black, a, x, b), y, T (Black, c, z, d))
          | balance body = T body

       fun fromList nil = E
         | fromList l = 
           let
               val ar = Array.fromList l
               val len = Array.length ar
               val redDepth = floor ((Math.ln (real len)) / (Math.ln 2.0))
               fun aux (ar, i, 0, _) = E
                 | aux (ar, i, 1, depth) =
                   let
                       val color = if depth >= redDepth then Red else Black
                   in
                       T (color, E, Array.sub(ar, i), E)
                   end
                 | aux (ar, i, len, depth) =
                   let
                       val leftLen = (len - 1) div 2
                       val rightLen = len - leftLen - 1
                   in
                       T (Black,
                          aux (ar, i, leftLen, depth + 1),
                          Array.sub(ar, i + leftLen),
                          aux (ar, i + leftLen + 1, rightLen, depth + 1))
                   end
           in
               aux (ar, 0, Array.length(ar), 0)
           end
                 
        fun insert (x, s) =
            let
                fun ins E = T (Red, E, x, E)
                  | ins (s as T (color, a, y, b)) =
                    if Element.lt (x, y) then balance (color, ins a, y, b)
                    else if Element.lt (y, x) then balance (color, a, y, ins b)
                    else s
                val T (_, a, y, b) = ins s
            in
                T (Black, a, y, b)
            end
    end
  


