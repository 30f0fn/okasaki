(* Write a function `fromOrdList` of type `Elem list -> Tree` that converts a sorted list with no duplicates into a red-black tree.  The function should run in $O(n)$ time. *)

(* Below approach cheats by converting list to an array. *)
(* basic idea - build a tree of depth d+1, which contains complete binary tree of depth d, and color a node black iff it has depth <= d.  To build the tree, divide the array in half (skipping the middle entry), recursively build trees from the halves, then build a new tree with middle entry as value of the root. *)


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
               val redDepth = floor ((Math.ln (real len + 1.0)) / (Math.ln 2.0))
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
  


