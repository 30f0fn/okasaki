use "../../src/5/QUEUE.sml";

signature DEQUE =
    sig
        type 'a Queue
        val empty : 'a Queue
        val isEmpty : 'a Queue -> bool

        val cons : 'a * 'a Queue -> 'a Queue
        val head : 'a Queue -> 'a
        val tail : 'a Queue -> 'a Queue
                                  
        val snoc : 'a Queue * 'a -> 'a Queue
        val last : 'a Queue -> 'a
        val init : 'a Queue -> 'a Queue
    end
