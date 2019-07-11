use "../../src/2/5.sml";

fun test x 0 = NONE 
  | test x n = case (test x (n - 1)) of
                 SOME t => SOME t
                | NONE => case check (completer x n) of
                             false => SOME (completer x n)
                           | true => NONE

