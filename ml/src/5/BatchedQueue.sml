use "../../src/5/QUEUE.sml";

structure BatchedQueue : QUEUE =
    struct
        type 'a Queue = 'a list * 'a list
        exception EMPTY
        val empty = ([], [])
        fun isEmpty (f, r) = null f
        fun checkf ([], r) = (rev r, [])
          | checkf q = q
        fun snoc ((f, r), x) = checkf (f, x :: r)
        fun head ([], _) = raise EMPTY
          | head (x :: f, r) = x
        fun tail ([], _) = raise EMPTY
          | tail (x :: f, r) = checkf (f, r)
    end
