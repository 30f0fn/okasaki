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

        fun isEmpty E = true
          | isEmpty _ = false

        fun member (x, E) = false
          | member (x, T (_, a, y, b)) =
            if Element.lt (x, y) then member (x, a)
            else if Element.lt (y, x) then member (x, b)
            else true

        fun llbalance (Black, T (Red, T (Red, a, x, b), y, c), z, d)
            = T (Red, T (Black, a, x, b), y, T (Black, c, z, d))
          | llbalance body = T body

        fun lrbalance (Black, T (Red, a, x, T (Red, b, y, c)), z, d)
            = T (Red, T (Black, a, x, b), y, T (Black, c, z, d))
          | lrbalance body = T body

        fun rlbalance (Black, a,  x, T (Red, T (Red, b, y, c), z, d))
            = T (Red, T (Black, a, x, b), y, T (Black, c, z, d))
          | rlbalance body = T body

        fun rrbalance (Black, a, x, T (Red, b, y, T (Red, c, z, d)))
            = T (Red, T (Black, a, x, b), y, T (Black, c, z, d))
          | rrbalance body = T body

        datatype cmpValue = Lt of Color * Tree * Element.T * Tree
                          | Gt of Color * Tree * Element.T * Tree
                          | Eq
                          | No

        fun cmp (x, E) = No
          | cmp (x, T (c, a, y, b)) = if Element.lt (x, y)
                                      then Lt (c, a, y, b)
                                      else (if Element.lt (y, x)
                                            then Gt (c, a, y, b)
                                            else Eq)

        fun insert (x, s) = 
            let 
                fun ins E = T (Red, E, x, E)
                  | ins (s as T (clrS, a, y, b)) = 
                    case (cmp (x, s), cmp (x, a), cmp (x, b)) of
                        (No, _, _) => T (Red, E, x, E)
                      | (Eq, _, _) => s
                      | (Lt _, No, _) => T (clrS, T (Red, E, x, E), y, b)
                      | (Lt _, Eq, _) => s
                      | (Lt _, Lt (clrA, aa, ya, ab), _) =>
                        llbalance (clrS, T (clrA, ins aa, ya, ab), y, b)
                      | (Lt _, Gt (clrA, aa, ya, ab), _) =>
                        lrbalance (clrS, T (clrA, aa, ya, ins ab), y, b)
                      | (Gt _, _, No) => T (clrS, a, y, T (Red, E, x, E))
                      | (Gt _, _, Eq) => s
                      | (Gt _, _, Lt (clrB, ba, by, bb))
                        => rlbalance (clrS, a, y, T(clrB, ins ba, by, bb))
                      | (Gt _, _, Gt (clrB, ba, by, bb))
                        => rrbalance (clrS, a, y, T(clrB, ba, by, ins bb))
                val T (_, a, y, b) = ins s
            in
                T (Black, a, y, b)
            end
    end


