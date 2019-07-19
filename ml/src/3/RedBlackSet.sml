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

        fun balance (Black, T (Red, T (Red, a, x, b), y, c), z, d)
            = T (Red, T (Black, a, x, b), y, T (Black, c, z, d))
          | balance (Black, T (Red, a, x, T (Red, b, y, c)), z, d)
            = T (Red, T (Black, a, x, b), y, T (Black, c, z, d))
          | balance (Black, a,  x, T (Red, T (Red, b, y, c), z, d))
            = T (Red, T (Black, a, x, b), y, T (Black, c, z, d))
          | balance (Black, a, x, T (Red, b, y, T (Red, c, z, d)))
            = T (Red, T (Black, a, x, b), y, T (Black, c, z, d))
          | balance body = T body


        (* fun balance ((Black, T (Red, T (Red, a, x, b), y, c), z, d) *)
        (*   | balance (Black, T (Red, a, x, T (Red, b, y, c)), z, d) *)
        (*   | balance (Black, a,  x, T (Red, T (Red, b, y, c), d)) *)
        (*   | balance (Black, a, T (Red, b, y, T (Red, c, z, d)))) *)
        (*     = T (Red, T (Black, a, x, b), y, T (c, z, d)) *)
        (*   | balance body = T body *)

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
  
