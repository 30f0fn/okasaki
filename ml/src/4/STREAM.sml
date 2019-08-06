Control.lazysml := true;
open Lazy;

signature STREAM =
    sig
        datatype 'a StreamCell = NIL | CONS of 'a * 'a Stream
        withtype 'a Stream = 'a StreamCell susp
        val strcat : 'a Stream -> 'a Stream -> 'a Stream
        val take : int * 'a Stream -> 'a Stream
        val drop : int * 'a Stream -> 'a Stream
        val reverse : 'a Stream -> 'a Stream
    end
