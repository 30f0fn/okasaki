use "../../src/4/STREAM.sml";

Control.lazysml := true;
open Lazy;



structure Stream : STREAM =
    struct
        datatype 'a StreamCell = NIL | CONS of 'a * 'a Stream
        withtype 'a Stream = 'a StreamCell susp

        fun lazy strcat ($NIL) t = t
          | strcat ($(CONS(x, s))) t = $(CONS(x, strcat s t))

        fun lazy take (0, s) = $NIL
          | take (n, $NIL) = $NIL
          | take (n, ($(CONS(x, s)))) = $(CONS(x, take (n - 1, s)))

        fun lazy drop (n, s) =
            let
                fun drop' (0, s) = s
                  | drop' (n, $NIL) = $NIL
                  | drop' (n, ($(CONS(x, s)))) = drop'(n-1, s)
            in
                drop' (n, s)
            end
                
        fun lazy reverse s =
            let
                fun reverse' ($NIL,  t) = t
                  | reverse' (($(CONS(x, s))), t) = reverse'(s, $(CONS(x, t)))
            in
                reverse' (s, $NIL)
            end
    end
