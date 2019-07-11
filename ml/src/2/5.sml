(* Sharing [...] can be useful within a single object, not just between objects.  For example, if the two subtrees of a given node are identical, then they can be represented by the same tree.  (a) Using this idea, write a function `complete` of type Elem * int -> Tree where `complete x d` creates a complete binary tree of depth d with x stored in every node. (b) Extend this function to create balanced trees of arbitrary size.  These trees will not always be complete binary trees, but should be as balanced as possible: for any given node, the two subtrees should differ in size by at most one. *)

datatype 'a Tree =
         Empty | Node of 'a Tree * 'a * 'a Tree

fun complete x 0 = Empty 
  | complete x n =
    let
        val child = complete x (n - 1)
    in
        Node (child, x, child)
    end

val c3 = complete "a" 3

fun completer x 0 = Empty 
  | completer x n =
    let
        val left = completer x ((n - 1) div 2)
        val right = if n mod 2 = 1
                    then left
                    else completer x ((n + 1) div 2)
    in
        Node (left, x, right)
    end

fun size Empty = 0
  | size (Node (t, _, u)) = size t + 1 + size u

fun check Empty = true
  | check (Node (t, _, u)) =
    check t andalso ((check u) andalso (size t - size u <= 1))


