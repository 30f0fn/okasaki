Control.Print.printDepth := 1024;

datatype lexp = Var of int
              | Abs of int * lexp
              | App of lexp * lexp

fun toString (Var k) = "Var " ^ Int.toString(k) ^ " "
  | toString (Abs (k, e)) = "Abs " ^ Int.toString(k) ^ " " ^ toString(e) ^ " "
  | toString (App (e, f)) = "App " ^ toString(e) ^ toString(f) ^ " "

fun lprint lexp = (print(toString(lexp)); print("\n"))

fun max_var (Var k) = k
  | max_var (Abs (k, e)) = Int.max(k, max_var e)
  | max_var (App (e1, e2)) = Int.max(max_var e1, max_var e2)

fun next_var exp = (max_var exp) + 1

fun reletter (Var k) l m = if k = l
                           then Var m
                           else Var k
  | reletter (Abs (k, e)) l m = if k = l
                                then Abs (m, (reletter e l m))
                                else Abs (k, (reletter e l m))
  | reletter (App (e, f)) l m = App ((reletter e l m), (reletter f l m))


fun inc (Var k) i = Var k
  | inc (Abs (k, e)) i = reletter (Abs (k, inc e i)) k (k + i)
  | inc (App (e, f)) i = App (inc e i, inc f i)


fun alpha_min exp nexp = Int.max(next_var exp, next_var nexp)

fun alpha exp nexp = inc exp (alpha_min exp nexp)

fun subst (Var k) l exp = if k = l then exp else Var k
          | subst (Abs (k, e)) l exp = if k = l
                                       then Abs (k, e)
                                       else (Abs (k, (subst e l exp)))
          | subst (App (e, f)) l exp = App ((subst e l exp),
                                            (subst f l exp))
 
fun beta (App (Abs(k, e), f)) = beta (subst (alpha e f) k f)
  | beta a = a


(* fun eval_CBV (App (Abs (k, e), f)) = *)
(*     let *)
(*         val ev = eval_CBV e *)
(*         val fv = eval_CBV f *)
(*     in *)
(*         eval_CBV (beta (App (ev, fv))) *)
(*     end *)
(*   | eval_CBV (App (e, f)) = App (eval_CBV e, eval_CBV f) *)
(*   | eval_CBV (Abs (k, e)) = Abs (k, eval_CBV e) *)
(*   | eval_CBV (Var k) = Var k *)

(* fun eval_CBV (App (e, f)) = *)
(*     ( *)
(*       case e of *)
(*         Abs _ => eval_CBV (beta (App (e, f))) *)
(*        | _ => App (eval_CBV e, eval_CBV f) *)
(*     ) *)
(*   | eval_CBV (Abs (k, e)) = Abs (k, eval_CBV e) *)
(*   | eval_CBV (Var k) = Var k *)


(* closest yet? *)
(* fun eval_CBV exp = *)
(*     case exp of *)
(*         App (e, f) => *)
(*         let *)
(*             val ev = eval_CBV e *)
(*             val fv = eval_CBV f *)
(*         in *)
(*             case ev of *)
(*                 Abs _ =>  eval_CBV (beta (App (ev, fv))) *)
(*               | _ => App (ev, fv) *)
(*         end *)
(*       | Abs (v, e) => *)
(*          Abs (v, eval_CBV e) *)
(*       | Var k => *)
(*          Var k *)

fun eval_CBV exp =
    (print ("evaluating " ^ (toString exp) ^ "\n"); 
     case exp of
         App (e, f) =>
         let
             val ev = eval_CBV e
             val fv = eval_CBV f
         in
             case ev of
                 Abs _ =>  eval_CBV (beta (App (ev, fv)))
               | _ => App (ev, fv)
         end
       | Abs (v, e) =>
         Abs (v, eval_CBV e)
       | Var k =>
         Var k
    )


(* (***********) *)
(* (* TESTING *) *)
(* (***********) *)




(* (* http://pages.cs.wisc.edu/~horwitz/CS704-NOTES/2.LAMBDA-CALCULUS-PART2.html *) *)

(* (* sugar *) *)
fun APP f e = App (f, e)
fun APP2 f e1 e2 = App (App (f, e1), e2)
fun APP3 f e1 e2 e3 = App (App (App (f, e1), e2), e3)
fun ABS k e = Abs (k, e)
fun ABS2 k1 k2 e = Abs (k1, Abs (k2, e))
fun ABS3 k1 k2 k3 e = Abs (k1, Abs (k2, Abs (k3, e)))


(* booleans *)
val FIRST = Abs (0, Abs (1, Var 0))
val TRUE = FIRST
val SECOND = Abs (0, Abs (1, Var 1))
val FALSE = SECOND

val COND = ABS3 0 1 2 (APP2 (Var 0) (Var 1) (Var 2))
val NEG = (ABS 3 (ABS2 1 2 (APP2 (Var 3) (Var 2) (Var 1))))
val CONJ = (ABS2 0 1 (APP2 (Var 0) (Var 1) FALSE))
val DISJ = (ABS2 0 1 (APP2 (Var 0) TRUE (Var 1)))

 



(* val test_exps = [ *)
(*     (Var 1), *)
(*     (Var 4), *)
(*     (ABS 1 (Var 1)), *)
(*     (ABS 1 (Var 4)), *)
(*     (ABS 1 (Var 5)), *)
(*     APP (ABS 0 (Var 0)) (Var 0), *)
(*     APP (ABS 0 (APP (Var 0) (Var 1))) (ABS 0 (Var 0)), *)
(*     ABS 0 (APP (Var 0) (Var 1)), *)
(*     APP (Var 0) (Var 1), *)
(*     ABS 0 (APP (ABS 3 (Var 3)) (Var 0)), *)
(*     ABS2 0 1 (APP (ABS 0 (Var 1)) (APP (Var 0) (Var 1))), *)
(*     ABS2 3 4 (APP (ABS 3 (Var 3)) (APP (Var 3) (Var 4))) *)
(* ] *)

(* fun test_exp_fn_to_lexp fcn test_exps extra = *)
(*     let *)
(*         fun run_test exp = *)
(*             (print extra; *)
(*              print "\n"; *)
(*              lprint exp; *)
(*              lprint (fcn exp); *)
(*              print "\n") *)
(*     in *)
(*         map run_test test_exps *)
(*     end *)

(* fun test_exp_fn_to_int fcn test_exps extra = *)
(*     let *)
(*         fun run_test exp = *)
(*             (print extra; *)
(*              print "\n"; *)
(*              lprint exp; *)
(*              print (Int.toString (fcn exp)); *)
(*              print "\n") *)
(*     in *)
(*         map run_test test_exps *)
(*     end *)


(* val test_max_var = test_exp_fn_to_int max_var test_exps "max_var" *)

(* val test_reletter = test_exp_fn_to_lexp *)
(*                         (fn exp => reletter exp 1 9) *)
(*                          test_exps *)
(*                         "reletter all 1's to 9's" *)


(* val inc_test1 = test_exp_fn_to_lexp *)
(*                     (fn exp => inc exp 1) *)
(*                     test_exps *)
(*                     "incrementing from 1" *)

(* val inc_test4 = test_exp_fn_to_lexp *)
(*                     (fn exp => inc exp 4) *)
(*                     test_exps *)
(*                     "incrementing from 4" *)

(* val alpha_test1 = *)
(*     let *)
(*         val exp2 = (APP2 (Var 0) (ABS 1 (Var 1)) (ABS 3 (Var 3))) *)
(*     in *)
(*         test_exp_fn_to_lexp *)
(*             (fn exp1 => alpha exp1 exp2) *)
(*              test_exps *)
(*             ("alpha with " ^ toString(exp2)) *)
(*     end *)
        

(* val subst_test1 = *)
(*     let *)
(*         val exp2 = (APP2 (Var 9) (ABS 11 (Var 11)) (ABS 12 (Var 12))) *)
(*     in *)
(*         test_exp_fn_to_lexp *)
(*             (fn exp1 => (subst exp1 1 exp2)) *)
(*             test_exps *)
(*             ("substituting " ^ (toString exp2) ^ " for 1") *)
(*     end *)

(* val beta_test_exps = [ *)
(*     APP (ABS 1 (Var 1)) (Var 2), *)
(*     APP (ABS2 1 2 (Var 1)) (Var 3), *)
(*     APP3 COND TRUE (Var 0) (Var 1), *)
(*     APP3 COND FALSE (Var 0) (Var 1) *)
(* ] *)

(* val beta_test1 = *)
(*     test_exp_fn_to_lexp *)
(*         beta *)
(*         beta_test_exps *)
(*         "applying beta" *)

                       

(* val cbv0 = eval_CBV ( APP (ABS 0 (Var 0)) (Var 0)) *)
(* val cbv1 = eval_CBV ( APP (ABS 0 (APP (Var 0) (Var 1))) (ABS 0 (Var 0))) *)
(* val cbv2 = eval_CBV (ABS 0 (APP (Var 0) (Var 1))) *)
(* val cbv3 = eval_CBV (APP (Var 0) (Var 1)) *)
(* val cbv4 = eval_CBV (ABS 0 (APP (ABS 3 (Var 3)) (Var 0))) *)
(* val cbv5 = eval_CBV (ABS2 3 4 (APP (ABS 3 (Var 3)) (APP (Var 3) (Var 4)))) *)


(* (* the conditional applies its first arg to its second and third args *) *)

(* val FIRST_0_1 = eval_CBV (APP2 FIRST (Var 0) (Var 1)) *)
(* val SECOND_0_1 = eval_CBV (APP2 SECOND (Var 0) (Var 1)) *)


val CBV_COND_TRUE_0_1 = eval_CBV (APP3 COND TRUE (Var 0) (Var 1))
val CBV_COND_FALSE_0_1 = eval_CBV (APP3 COND FALSE (Var 0) (Var 1))


(* val NEG = Abs(0, (APP2 (Var 0) FALSE TRUE)) *)

(* (* val REV_TRUE = eval_CBV  (ABS2 1 2 (APP2 TRUE (Var 2) (Var 1))) *) *)
(* (* val REV_FALSE = eval_CBV (ABS2 1 2 (APP2 FALSE (Var 2) (Var 1))) *) *)

val CBV_NEG_TRUE = eval_CBV (APP NEG TRUE)
val CBV_NEG_FALSE = eval_CBV (APP NEG FALSE)

(* (* val REV_TRUE_0_1 = eval_CBV (APP2 (ABS 1 (ABS 2 (APP (APP TRUE (Var 2)) (Var 1)))) (Var 0)) (Var 1) *) *)

(* (* neg v = lambda 0 1 v rev 0 1 *) *)

val CBV_CONJ_TRUE_TRUE = eval_CBV (APP2 CONJ TRUE TRUE)
val CBV_CONJ_TRUE_FALSE = eval_CBV (APP2 CONJ TRUE FALSE)
val CBV_CONJ_FALSE_TRUE = eval_CBV (APP2 CONJ FALSE TRUE)
val CBV_CONJ_FALSE_FALSE = eval_CBV (APP2 CONJ FALSE FALSE)

val CBV_DISJ_TRUE_TRUE = eval_CBV (APP2 DISJ TRUE TRUE)
val CBV_DISJ_TRUE_FALSE = eval_CBV (APP2 DISJ TRUE FALSE)
val CBV_DISJ_FALSE_TRUE = eval_CBV (APP2 DISJ FALSE TRUE)
val CBV_DISJ_FALSE_FALSE = eval_CBV (APP2 DISJ FALSE FALSE)

val CBV_DISJ_DUAL = ABS2 0 1 (APP NEG (APP2 DISJ (APP NEG (Var 0)) (APP NEG (Var 1))))
val CBV_DISJ_DUAL_TRUE_TRUE = eval_CBV (APP2 CBV_DISJ_DUAL TRUE TRUE)
val CBV_DISJ_DUAL_TRUE_FALSE = eval_CBV (APP2 CBV_DISJ_DUAL TRUE FALSE)
val CBV_DISJ_DUAL_FALSE_TRUE = eval_CBV (APP2 CBV_DISJ_DUAL FALSE TRUE)
val CBV_DISJ_DUAL_FALSE_FALSE = eval_CBV (APP2 CBV_DISJ_DUAL FALSE FALSE)
(* lists *)

val CONS = ABS2 1 2 (ABS 0 (APP2 (Var 0) (Var 1) (Var 2)))
(* a list is a function which takes a selector function as arg, and returns the result of applying the selector to the pair head, tail *)
val HEAD = ABS 0 (APP (Var 0) TRUE)
(* to get head of a list, apply the list to the TRUE selector function *)
val TAIL = ABS 0 (APP (Var 0) FALSE)
(* to get tail of a list, apply the list to the FALSE selector function *)
val ISEMPTY = ABS 0 (APP (Var 0) (ABS2 1 2 FALSE))
val NIL = ABS 0 TRUE

val IS_EMPTY_NIL = eval_CBV (APP ISEMPTY NIL)
val IS_EMPTY_CONS_0_NIL = eval_CBV (APP ISEMPTY (APP2 CONS (Var 0) NIL))

(* church numerals *)

(* zero x y = y *)
(* F = lambda x : FALSE *)
(* iszero = lambda f : f F TRUE *)

val ZERO = ABS2 0 1 (Var 1)
val ONE = ABS2 0 1 (APP (Var 0) (Var 1))
val TWO = ABS2 0 1 (APP (Var 0) (APP (Var 0) (Var 1)))
(* want ISZERO to return result of applying its argument to X, Y *)
(* applying ZERO to X, Y returns Y, so we should pick Y=TRUE *)
(* applying nonzero number to X, Y returns the result of some application of X, so we should pick X = lambda x : FALSE *)

val ISZERO = ABS 0 (APP2 (Var 0) (ABS 1 FALSE) TRUE)

val ZEROISZERO = eval_CBV (APP ISZERO ZERO)
val ONEISZERO = eval_CBV (APP ISZERO ONE)
val TWOISZERO = eval_CBV (APP ISZERO TWO)

(* succ lambda x,y : xxxy = lambda x, y : xxxxy *)
(* succ f = lambda x, y : x(f(x,y)) *)
(* succ = lambda f : lambda x, y : x(f(x,y)) *)
val SUCC = ABS 0 (ABS2 1 2 (APP2 (Var 0) (Var 1) (APP (Var 1) (Var 2))))
val SUCC_ZERO = eval_CBV (APP SUCC ZERO)

(* val SUCC1 = ABS 0 (ABS2 1 2 (APP (Var 1) (APP2 (Var 0) (Var 1) (Var 2)) )) *)
(* val SUCC1_ZERO = eval_CBV (APP SUCC1 ZERO) *)


