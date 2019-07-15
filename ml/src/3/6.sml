(* Most of the rank annotations in the initial implementation of binomial heaps are redundant because we know that the children of a node of rank r have ranks r - 1, ..., 0.  Thus, we can remove the rank annotations from each node and instead pair each tree at the top-level with its rank.  Do this.  *)

use "../../src/2/ORDERED.sml";
use "../../src/3/HEAP.sml";



functor BinomialHeap (Element : ORDERED) : HEAP =
    struct
        structure Elem = Element

        (* datatype Tree = Node of int * Elem.T * Tree list *)
        (* type Heap = Tree list *)

        datatype Tree = Node of Elem.T * Tree list
        type Heap = (int * Tree) list

        val empty = []
        fun isEmpty ts = null ts

        fun root (Node (x, _)) = x

        (* combine two trees, maintaining heap order *)
        fun link (t1 as Node (x1, c1),
                  t2 as Node (x2, c2)) =
            if Elem.leq (x1, x2)
            then Node (x1, t2 :: c1)
            else Node (x2, t1 :: c2)

        (* inserting a tree into a heap *)
        fun insTree ((r, t), []) = [(r, t)]
          | insTree ((r, t), ts as (r', t') :: ts') =
            if r < r' 
            then (r, t) :: ts
            else insTree ((r + 1, link (t, t')), ts')
        fun insert (x, ts) = insTree ((0, Node (x, [])), ts)
                                     
        (* Each heap can be regarded as an integer whose kth column 
           in binary notation contains a 1 iff the heap contains a tree
           of rank k.  The integer corresponding to the merge of binary
           heaps h1, h2 is precisely the sum of the integers corresponding
           to h1 and h2. *)

        (* merge two heaps *)
        fun merge (rts1, []) = rts1
          | merge ([], rts2) = rts2
          | merge (rts1 as (r1, t1) :: rts1',
                   rts2 as (r2, t2) :: rts2') =
            if r1 < r2
            then (r1, t1) :: merge (rts1', rts2)
            else (if r2 < r1
                  then (r2, t2) :: merge (rts1, rts2')
                  else insTree ((r1 + 1, link (t1, t2)),
                                merge (rts1', rts2')))

        exception EmptyHeap

        fun removeMinTree [] = raise EmptyHeap
          | removeMinTree [rt] = (rt, [])
          | removeMinTree ((r, t) :: rts) =
            let
                val ((r', t'), rts') = removeMinTree rts
            in
                if Elem.leq (root t, root t')
                then ((r, t), rts)
                else ((r', t'), (r, t) :: rts')
            end

        fun findMin ts =
            let
                val ((_, t), _) = removeMinTree ts
            in
                root t
            end

        (* remove the minimum tree t from H, then return to H the children of t
           in this reimplementation, the reversal of the children-list of a 
           binomial tree becomes a heap once the trees of the list are 
           decorated with rank info 
         *)

        fun rankChildren (_, []) = []
          | rankChildren (r, t :: ts) = (r, t) :: rankChildren (r-1, ts)

        fun deleteMin h =
            let
                val ((r, t), h') = removeMinTree h
                val Node (_, children) = t
                val rankedChildren = rankChildren (r - 1, children)
            in
                merge (List.rev rankedChildren, h')
            end
                
    end








