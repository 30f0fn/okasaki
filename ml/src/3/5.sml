(* within BinomiaHeap, define findMin directly rather than via a call to removeMinTree *)

use "../../src/2/ORDERED.sml";
use "../../src/3/HEAP.sml";

functor BinomialHeap (Element : ORDERED) : HEAP =
    struct
        structure Elem = Element

        datatype Tree = Node of int * Elem.T * Tree list
        type Heap = Tree list

        val empty = []
        fun isEmpty ts = null ts

        fun rank (Node (r, _, _)) = r
        fun root (Node (_, x, _)) = x

        (* combine two trees, maintaining heap order *)
        fun link (t1 as Node (r, x1, c1),
                  t2 as Node (_, x2, c2)) =
            if Elem.leq (x1, x2)
            then Node (r + 1, x1, t2 :: c1)
            else Node (r + 1, x2, t1 :: c2)

        (* inserting an element into a heap *)
        fun insTree (t, []) = [t]
          | insTree (t, ts as t' :: ts') =
            if rank t < rank t'
            then t :: ts
            else insTree (link (t, t'), ts')
        fun insert (x, ts) = insTree (Node (0, x, []), ts)


        (* merge two heaps *)
        fun merge (ts1, []) = ts1
          | merge ([], ts2) = ts2
          | merge (ts1 as t1 :: ts1',
                   ts2 as t2 :: ts2') =
            if rank t1 < rank t2
            then t1 :: merge (ts1', ts2)
            else (if rank t2 < rank t1
                  then t2 :: merge (ts1, ts2')
                  else insTree (link (t1, t2), merge (ts1', ts2')))

        exception EmptyHeap

        fun removeMinTree [] = raise EmptyHeap
          | removeMinTree [t] = (t, [])
          | removeMinTree (t :: ts) =
            let
                val (t', ts') = removeMinTree ts
            in
                if Elem.leq (root t, root t') then (t, ts) else (t', t :: ts')
            end

        (* fun findMin h [] = raise EmptyHeap *)
        (*   | findMin [t] = (t, []) *)
        (*   | findMin (t :: ts) =  *)


        fun findMin [] = raise EmptyHeap
          | findMin (t :: ts) =
            let
                fun aux m [] = m
                  | aux m (t' :: ts') = 
                    let
                        val m' = if Elem.leq (m, root t') then m else root t'
                    in 
                        aux m' ts'
                    end
            in
                aux (root t) ts
            end

        (* remove the minimum tree t from H, then return to H the children of t
           note that the reversal of the children-list of a binomial tree is a heap! *)

        fun deleteMin h =
            let 
                val (t, h') = removeMinTree h
                val Node (_, _, children) = t
            in
                merge (List.rev children, h')
            end
                
    end








