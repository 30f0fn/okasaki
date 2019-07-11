Control.Print.printDepth := 1024;

datatype lexp = Var of int
              | Abs of int * lexp
              | App of lexp * lexp

fun toString (Var k) = "(Var " ^ Int.toString(k) ^ ")"
  | toString (Abs (k, e)) = "(Abs " ^ Int.toString(k) ^ ", " ^ toString(e) ^ ")"
  | toString (App (e, f)) = "(App " ^ toString(e) ^ ", " ^ toString(f) ^ ")"

fun lsize (Var k) = 1
  | lsize (App (e, f)) = 1 + (lsize e) + (lsize f)
  | lsize (Abs (k, e)) = 1 + (lsize e)

(* fun toStringSize lexp = String.size (toString lexp) *)

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


fun eval exp =
    case exp of
        App (e, f) =>
        let
            val ev = eval e
            val fv = eval f
        in
            case ev of
                Abs _ =>  eval (beta (App (ev, fv)))
              | _ => App (ev, fv)
        end
      | _ => exp

fun eval_extra exp extra_fcn =
    (extra_fcn exp;
     case exp of
         App (e, f) =>
         let
             val ev = eval_extra e extra_fcn
             val fv = eval_extra f extra_fcn
         in
             case ev of
                 Abs _ =>  eval_extra (beta (App (ev, fv))) extra_fcn
               | _ => App (ev, fv)
         end
       | _ => exp
    )

fun eval_verbosely exp =
    let
        fun f e = print ("evaluating " ^ (toString e) ^ "\n")
    in
        eval_extra exp f
    end

fun eval_sizecounter exp =
    let
        (* fun f e = print ("evaluating string of size " ^ (Int.toString (lsize e)) ^ "\n") *)
        fun f e = print ((Int.toString (max_var e)) ^ " ")
    in
        eval_extra exp f
    end
        


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
(* val NEG = (ABS 3 (ABS2 1 2 (APP2 (Var 3) (Var 2) (Var 1)))) *)
val NEG = Abs(0, (APP2 (Var 0) FALSE TRUE))
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

               

(* val cbv0 = eval ( APP (ABS 0 (Var 0)) (Var 0)) *)
(* val cbv1 = eval ( APP (ABS 0 (APP (Var 0) (Var 1))) (ABS 0 (Var 0))) *)
(* val cbv2 = eval (ABS 0 (APP (Var 0) (Var 1))) *)
(* val cbv3 = eval (APP (Var 0) (Var 1)) *)
(* val cbv4 = eval (ABS 0 (APP (ABS 3 (Var 3)) (Var 0))) *)
(* val cbv5 = eval (ABS2 3 4 (APP (ABS 3 (Var 3)) (APP (Var 3) (Var 4)))) *)


(* (* the conditional applies its first arg to its second and third args *) *)

(* val FIRST_0_1 = eval (APP2 FIRST (Var 0) (Var 1)) *)
(* val SECOND_0_1 = eval (APP2 SECOND (Var 0) (Var 1)) *)


val CBV_COND_TRUE_0_1 = eval (APP3 COND TRUE (Var 0) (Var 1))
val CBV_COND_FALSE_0_1 = eval (APP3 COND FALSE (Var 0) (Var 1))




(* (* val REV_TRUE = eval  (ABS2 1 2 (APP2 TRUE (Var 2) (Var 1))) *) *)
(* (* val REV_FALSE = eval (ABS2 1 2 (APP2 FALSE (Var 2) (Var 1))) *) *)

val CBV_NEG_TRUE = eval (APP NEG TRUE)
val CBV_NEG_FALSE = eval (APP NEG FALSE)

(* (* val REV_TRUE_0_1 = eval (APP2 (ABS 1 (ABS 2 (APP (APP TRUE (Var 2)) (Var 1)))) (Var 0)) (Var 1) *) *)

(* (* neg v = lambda 0 1 v rev 0 1 *) *)

val CBV_CONJ_TRUE_TRUE = eval (APP2 CONJ TRUE TRUE)
val CBV_CONJ_TRUE_FALSE = eval (APP2 CONJ TRUE FALSE)
val CBV_CONJ_FALSE_TRUE = eval (APP2 CONJ FALSE TRUE)
val CBV_CONJ_FALSE_FALSE = eval (APP2 CONJ FALSE FALSE)

val CBV_DISJ_TRUE_TRUE = eval (APP2 DISJ TRUE TRUE)
val CBV_DISJ_TRUE_FALSE = eval (APP2 DISJ TRUE FALSE)
val CBV_DISJ_FALSE_TRUE = eval (APP2 DISJ FALSE TRUE)
val CBV_DISJ_FALSE_FALSE = eval (APP2 DISJ FALSE FALSE)

val CBV_DISJ_DUAL = ABS2 0 1 (APP NEG (APP2 DISJ (APP NEG (Var 0)) (APP NEG (Var 1))))
val CBV_DISJ_DUAL_TRUE_TRUE = eval (APP2 CBV_DISJ_DUAL TRUE TRUE)
val CBV_DISJ_DUAL_TRUE_FALSE = eval (APP2 CBV_DISJ_DUAL TRUE FALSE)
val CBV_DISJ_DUAL_FALSE_TRUE = eval (APP2 CBV_DISJ_DUAL FALSE TRUE)
val CBV_DISJ_DUAL_FALSE_FALSE = eval (APP2 CBV_DISJ_DUAL FALSE FALSE)
(* lists *)

val CONS = ABS2 1 2 (ABS 0 (APP2 (Var 0) (Var 1) (Var 2)))
(* a list is a function which takes a selector function as arg, and returns the result of applying the selector to the pair head, tail *)
val HEAD = ABS 0 (APP (Var 0) TRUE)
(* to get head of a list, apply the list to the TRUE selector function *)
val TAIL = ABS 0 (APP (Var 0) FALSE)
(* to get tail of a list, apply the list to the FALSE selector function *)
val ISEMPTY = ABS 0 (APP (Var 0) (ABS2 1 2 FALSE))
val NIL = ABS 0 TRUE

val IS_EMPTY_NIL = eval (APP ISEMPTY NIL)
val IS_EMPTY_CONS_0_NIL = eval (APP ISEMPTY (APP2 CONS (Var 0) NIL))
(* val TRUE_TRUE_FALSE = eval APP2 CONS TRUE (APP2 CONS TRUE FALSE) *)
(* val list_0_1_2 = (APP2 CONS (Var 88) (Var 99)) *)
(* val HEAD_TAIL_list_77_88_99 = eval (APP HEAD (APP TAIL list_0_1_2)) *)

(* church numerals *)

(* zero x y = y *)
(* F = lambda x : FALSE *)
(* iszero = lambda f : f F TRUE *)

val ZERO = ABS2 0 1 (Var 1)
val ONE = ABS2 0 1 (APP (Var 0) (Var 1))
val TWO = ABS2 0 1 (APP (Var 0) (APP (Var 0) (Var 1)))
val THREE = ABS2 0 1 (APP (Var 0)(APP (Var 0) (APP (Var 0) (Var 1))))
(* want ISZERO to return result of applying its argument to X, Y *)
(* applying ZERO to X, Y returns Y, so we should pick Y=TRUE *)
(* applying nonzero number to X, Y returns the result of some application of X, so we should pick X = lambda x : FALSE *)

val ISZERO = ABS 0 (APP2 (Var 0) (ABS 1 FALSE) TRUE)

val ZEROISZERO = eval (APP ISZERO ZERO)
val ONEISZERO = eval (APP ISZERO ONE)
val TWOISZERO = eval (APP ISZERO TWO)

(* succ lambda x,y : xxxy = lambda x, y : xxxxy *)
(* succ f = lambda x, y : x(f(x,y)) *)
(* succ = lambda f : lambda x, y : x(f(x,y)) *)
val SUCC = ABS 0 (ABS2 1 2 (APP2 (Var 0) (Var 1) (APP (Var 1) (Var 2))))
val SUCC_ZERO = eval (APP SUCC ZERO) 
val SUCC_SUCC_SUCC_ZERO = eval (APP SUCC (APP SUCC (APP SUCC ZERO)))

                               (* val THREE_OF_SUCC_ZERO = eval (APP2 THREE SUCC ZERO) *)

                               (* val f = ABS 0 (APP2 CONS (APP TAIL (Var 0)) (APP SUCC (APP TAIL (Var 0)))) *)
                               (* val pc0 = APP2 CONS ZERO ZERO *)
                               (* val PRED = ABS 0 (APP HEAD (APP2 (Var 0) f pc0)) *)



                               (* val pred_succ_zero = eval (APP PRED (APP SUCC ZERO)) *)
                               (* val pred_three = eval (APP PRED THREE) *)

                               (* val D = (ABS 1 (APP2 (Var 0) (Var 1) (Var 1))) *)
                               (* val Y = (ABS 0 (APP D D)) *)

                               (*             (* lambda F *) *)
                               (*             (* ( *) *)
                               (*             (*   lambda x y (APP *) *)
                               (*             (*                   (ISZERO x) *) *)
                               (*             (*                   x *) *)
                               (*             (*                   (APP SUCC (APP2 F *) *)
                               (*             (*                                   (Var 0) *) *)
                               (*             (*                                   (APP PRED (Var 1)))) *) *)
                               (*             (*              ) *) *)
                               (*             (* ) *) *)

                               (* val REC_FOR_ADD = ABS 2 ( *)
                               (*         ABS2 0 1 ( *)
                               (*             APP2 (APP ISZERO (Var 1)) *)
                               (*                  (Var 0) *)
                               (*                  (APP SUCC (APP2 (Var 2) *)
                               (*                                  (Var 0) *)
                               (*                                  (APP PRED (Var 1)))))) *)

                               (* val ADD = APP Y REC_FOR_ADD *)

                               (* (* for some reason this doesn't reduce :( *) *)
                               (* val ONE_PLUS_TWO = eval_sizecounter (APP2 ADD ONE TWO) *)
                               
