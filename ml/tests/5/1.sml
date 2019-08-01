use "../../src/5/1.sml";

open BatchedDeque

val q = empty;
val q0 = cons (0, q)
val q01 = cons (1, q0)
val q012 = cons (2, q01)
val q0123 = cons (3, q012)
val q012' = tail q0123
val v3 = head q0123
val q123 = init q0123
val v1 = last q123
val q1234 = snoc (q123, 4)
val v4 = last q1234


val qq0 = snoc (q, 0)
val qq01 = snoc (qq0, 1)
val qq012 = snoc (qq01, 2)
val v0 = head qq012
val v1 = head (tail qq012)
val v2 = last qq012
