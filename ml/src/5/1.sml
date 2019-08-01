use "../../src/5/DEQUE.sml";

structure BatchedDeque : DEQUE =
    struct
        type 'a Queue = 'a list * 'a list
        exception EMPTY
        val empty = ([], [])
        fun isEmpty (f, r) = null f
        fun revTake (0, l) = ([], l)
          | revTake (n, l) =
            let
                val (first, x :: second) = revTake (n-1, l)
            in
                (x :: first, second)
            end
        fun halves l = revTake (((length l) div 2), l)
        fun check ([], r) =
            let
                val (first, second) = halves (rev r)
            in
                (first, second)
            end
          | check (r, []) =
            let
                val (first, second) = halves r
            in
                (rev first, rev second)
            end
          | check q = q
        fun cons (x, (f, r)) = check (x :: f, r)
        fun head ([], _) = raise EMPTY
          | head (x :: _, _) = x
        fun tail ([], _) = raise EMPTY
          | tail (x :: f, r) = check (f, r)

        fun snoc ((f, r), x) = check (f, x :: r)
        fun last (_, []) = raise EMPTY
          | last (_, x :: _) = x
        fun init (_, []) = raise EMPTY
          | init (f, x :: r) = check (f, r)
    end
