(* Rewrite member to take no more than d+1 comparisons by recording candidate elements that might be equal to the query element, and checking for equality only when you hit the bottom of the tree *)

use "ORDERED.sml";
use "ORDERED.sml";
use "FINITEMAP.sml";

exception NotFound;

functor UnbalancedFiniteMap (K : ORDERED) : FINITEMAP =
    struct
        type Key = K.T
        datatype 'a MapTree = E | FM of 'a MapTree * (Key * 'a) * 'a MapTree
        type 'a Map = 'a MapTree
        val empty = E
        fun bind (k, v, E) = FM (E, (k, v), E)
          | bind (k, v, mt as FM(a, (k1, v1), b)) =
            if K.lt(k, k1) then FM(bind(k, v, a), (k1, v1), b)
            else if K.lt(k1, k) then FM(a, (k1, v1), bind(k, v, b))
            else mt
        fun lookup (k, E) = raise NotFound
          | lookup (k, mt as FM (a, (k1, v1), b)) =
            if K.lt (k, k1) then lookup (k, a)
            else if K.lt (k1, k) then lookup (k, b)
            else v1
    end

