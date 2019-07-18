(* The `balance` function currently performs several unnecessary tests.  For example, when the `ins` function recurses on the left child, there is no need for `balance` to test for red-red violations involving the right child.  (a) Split `balance` into two functions, `lbalance` and `rbalance`, that test for violations involving the left child and right child respectively.  Replace the calls to `balance` in `ins` with calls to either `lbalance` or `rbalance`.  (b) Extending the same logic one step further, one of the remaining tests on the grandchildren is also unnecessary.  Rewrite `ins` so that it never tests the color of nodes not on the search path. *)

use "../../src/2/SET.sml";
use "../../src/2/ORDERED.sml";


functor RedBlackSet (Element : ORDERED) : SET =
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

        fun lbalance  (Black, T (Red, T (Red, a, x, b), y, c), z, d)
            = T (Red, T (Black, a, x, b), y, T (Black, c, z, d))
          | lbalance (Black, T (Red, a, x, T (Red, b, y, c)), z, d)
            = T (Red, T (Black, a, x, b), y, T (Black, c, z, d))
          | lbalance body = T body

        fun rbalance (Black, a,  x, T (Red, T (Red, b, y, c), z, d))
            = T (Red, T (Black, a, x, b), y, T (Black, c, z, d))
          | rbalance (Black, a, x, T (Red, b, y, T (Red, c, z, d)))
            = T (Red, T (Black, a, x, b), y, T (Black, c, z, d))
          | rbalance body = T body

        fun insert (x, s) =
            let
                fun ins E = T (Red, E, x, E)
                  | ins (s as T (color, a, y, b)) =
                    if Element.lt (x, y) then lbalance (color, ins a, y, b)
                    else if Element.lt (y, x) then rbalance (color, a, y, ins b)
                    else s
                val T (_, a, y, b) = ins s
            in
                T (Black, a, y, b)
            end
    end
  

