Control.Print.printDepth := 1024;

datatype lexp = Var of int
              | Abs of int * lexp
              | App of lexp * lexp

datatype value = Null
               | IntVal of int
               | Closure of int * lexp * value list

structure Env = struct
(* type t = value list *)
val empty = [] : value list
fun null_vals_list k = if k = 0 then [] else Null :: null_vals_list (k - 1)
fun extend l k = l @ (null_vals_list k)
fun extend_to l new_len = extend l (Int.max(0, new_len - length l))
fun lookup l i = if i < length l then List.nth (l, i) else Null
fun insert l i v =
    let
        val ll = extend_to l (i+1)
    in
        List.take (ll, i) @ v :: List.drop(ll, i + 1)
    end
end
            
fun eval_rel (Var (k : int)) env = Env.lookup env k
  | eval_rel (Abs (k, e)) env = Closure (k, e, env)
  | eval_rel (App (e, f)) env =
    let
        val ev = eval_rel e env
        val fv = eval_rel f env
    in
        case ev of
            Closure (k, e1, env1) => 
            let
                val env2 = Env.insert env1 k fv
            in
                eval_rel e1 env2
            end
         | _ => ev
    end 

fun eval exp = eval_rel exp Env.empty


(* (***********) *)
(* (* TESTING *) *)
(* (***********) *)

(* val env = [IntVal 8, IntVal 9, IntVal 10] *)
(* val var_0_in_env_is_8 = eval_rel (Var 0) env *)
(* val var_1_in_env_is_9 = eval_rel (Var 1) env *)
(* val var_2_in_env_is_10 = eval_rel (Var 2) env *)
(* val var_3_in_env_is_neg = eval_rel (Var 3) env *)
(* val env1 = Env.insert env 3 (IntVal 11) *)
(* val var_3_in_env1_is_11 = eval_rel (Var 3) env1 *)
(* val env2 = Env.insert env 99 (IntVal 999) *)
(* val var_99_in_env2_is_999 = eval_rel (Var 99) env2 *)
(* val var_0_in_env2_is_8 = eval_rel (Var 0) env *)






(* (* http://pages.cs.wisc.edu/~horwitz/CS704-NOTES/2.LAMBDA-CALCULUS-PART2.html *) *)

(* (* (* sugar *) *) *)
fun APP f e = App (f, e)
fun APP2 f e1 e2 = App (App (f, e1), e2)
fun APP3 f e1 e2 e3 = App (App (App (f, e1), e2), e3)
fun ABS k e = Abs (k, e)
fun ABS2 k1 k2 e = Abs (k1, Abs (k2, e))
fun ABS3 k1 k2 k3 e = Abs (k1, Abs (k2, Abs (k3, e)))

val identity_of_99 = eval_rel (APP (ABS 1 (Var 1)) (Var 0)) [IntVal 99]

(* (* booleans *) *)
val FIRST = Abs (0, Abs (1, Var 0))
val TRUE = FIRST
val SECOND = Abs (0, Abs (1, Var 1))
val FALSE = SECOND

val first_of_88_99 = eval_rel (APP2 FIRST (Var 0) (Var 1)) [IntVal 88, IntVal 99]
val second_of_88_99 = eval_rel (APP2 SECOND (Var 0) (Var 1)) [IntVal 88, IntVal 99]

val eval_TRUE = eval TRUE
val identity_of_TRUE = eval (APP (ABS 1 (Var 1)) TRUE)
val first_of = eval (ABS2 0 1 (Var 0))
val FIRST_of_TRUE_FALSE_rel = eval_rel (APP2 FIRST (Var 0) (Var 1)) [Closure (0, TRUE, []), Closure(0, TRUE, [])]
val FIRST_of_TRUE_FALSE = eval (APP2 FIRST TRUE FALSE)

(* (* val SECOND_0_1 = eval (APP2 SECOND (Var 0) (Var 1)) *) *)

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

                       

(* val cbv0 = eval ( APP (ABS 0 (Var 0)) (Var 0)) *)
(* val cbv1 = eval ( APP (ABS 0 (APP (Var 0) (Var 1))) (ABS 0 (Var 0))) *)
(* val cbv2 = eval (ABS 0 (APP (Var 0) (Var 1))) *)
(* val cbv3 = eval (APP (Var 0) (Var 1)) *)
(* val cbv4 = eval (ABS 0 (APP (ABS 3 (Var 3)) (Var 0))) *)
(* val cbv5 = eval (ABS2 3 4 (APP (ABS 3 (Var 3)) (APP (Var 3) (Var 4)))) *)


(* (* the conditional applies its first arg to its second and third args *) *)

(* val FIRST_0_1 = eval (APP2 FIRST (Var 0) (Var 1)) *)
(* val SECOND_0_1 = eval (APP2 SECOND (Var 0) (Var 1)) *)


val CBV_COND_TRUE_88_99 = eval_rel (APP3 COND TRUE (Var 0) (Var 1)) [IntVal 88, IntVal 99]
val CBV_COND_FALSE_88_99 = eval_rel (APP3 COND FALSE (Var 0) (Var 1)) [IntVal 88, IntVal 99]


val NEG = Abs(0, (APP2 (Var 0) FALSE TRUE))

(* (* (* val REV_TRUE = eval  (ABS2 1 2 (APP2 TRUE (Var 2) (Var 1))) *) *) *)
(* (* (* val REV_FALSE = eval (ABS2 1 2 (APP2 FALSE (Var 2) (Var 1))) *) *) *)

val CBV_NEG_TRUE = eval (APP NEG TRUE)
val CBV_NEG_FALSE = eval (APP NEG FALSE)

(* (* (* val REV_TRUE_0_1 = eval (APP2 (ABS 1 (ABS 2 (APP (APP TRUE (Var 2)) (Var 1)))) (Var 0)) (Var 1) *) *) *)

(* (* (* neg v = lambda 0 1 v rev 0 1 *) *) *)

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
(* (* lists *) *)

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


val f = ABS 0 (APP2 CONS (APP TAIL (Var 0)) (APP SUCC (APP TAIL (Var 0))))
val pc0 = APP2 CONS ZERO ZERO
val PRED = ABS 0 (APP HEAD (APP2 (Var 0) f pc0))

val pred_succ_zero = eval (APP PRED (APP SUCC ZERO))
val pred_three = eval (APP PRED THREE)

val D = (ABS 1 (APP2 (Var 0) (Var 1) (Var 1)))
val Y = (ABS 0 (APP D D))

            (* lambda F *)
            (* ( *)
            (*   lambda x y (APP *)
            (*                   (ISZERO x) *)
            (*                   x *)
            (*                   (APP SUCC (APP2 F *)
            (*                                   (Var 0) *)
            (*                                   (APP PRED (Var 1)))) *)
            (*              ) *)
            (* ) *)

val REC_FOR_ADD = ABS 2 (
        ABS2 0 1 (
            APP2 (APP ISZERO (Var 1))
                 (Var 0)
                 (APP SUCC (APP2 (Var 2)
                                 (Var 0)
                                 (APP PRED (Var 1))))))

val ADD = APP Y REC_FOR_ADD

(* (* for some reason this doesn't reduce :( *) *)
(* val ONE_PLUS_TWO = eval (APP2 ADD ONE TWO) *)
