structure DupInts : ORDERED =
    struct
        type T = int
        fun eq (x,y) = x = y
        fun lt (x,y) = x < y
        fun leq (x,y) = x <= y
    end
