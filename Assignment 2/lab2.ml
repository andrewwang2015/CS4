open Num
let ni = num_of_int

(* A.1 *)

(* The space complexity would be O(n). We see that we evaluate fib(n-1) before
   fib (n-2) and fib(n-1) would take at most n frames (we do not get more than
   n levels of recursion). Once fib(n-1) is calculated and the value returned,
   those frames go back to memory and we now evaluate fib(n-2) which does not
   take more than the number of frames for fib(n-1). This is different than
   time complexity because unlike time complexity, for space complexity, all
   memory used in evaluating an expression once the expression is done
   evaluating is returned to the system. For time complexity, we have to wait
   for all recursive calls to finish.
*)

(* A.2 *)

(* We have to evaluate it 5 times as 12.15 / 3^5 < 0.1 *)
(* The space complexity is O(log(a)) because the amount of memory required is
   how tall the recursive stack is and in this case, it's how many times 3
   divides a until a is less than 0.1. Clearly this requires log(a) frames. For
   the time complexity, we look at how many times the function is called, and as
   evident in the first part of this problem, this number is the smallest n
   such that a/3^n < 0.1. From this, we can tell that it is O(log(a)) time
   complexity. *)

(* A.3 *)

let rec fast_expt b n=
  let is_even m = m mod 2 = 0 in
  let square m = m * m in
  match n with
  | 0 -> 1
  | _ when is_even n -> square (fast_expt b (n/2))
  | _ -> b * fast_expt b (n-1)

let ifast_expt b n =
  let is_even m = m mod 2 = 0 in
  let rec iter a x y =
    match y with
    | 0 -> a
    | _ when is_even y -> iter a (x*x) (y/2)
    | _ -> iter (a*x) x (y-1)
in iter 1 b n

(* A.4 *)

let rec fast_mult a b =
  let double m = m * 2 in
  let halve m = m / 2 in
  let is_even m = m mod 2 = 0 in
  match b with
  | 0 -> 0
  | _ when is_even b -> double(fast_mult a (halve b))
  | _ -> a + fast_mult a (b-1)

(* A. 5 *)

let ifast_mult a b =
  let double m = m * 2 in
  let halve m=  m / 2 in
  let is_even m = m mod 2 = 0 in
  let rec iter sum a b =
    match b with
    | 0 -> sum
    | _ when is_even b -> iter sum (double a) (halve b)
    | _ -> iter (sum + a) a (b-1)
  in iter 0 a b

(* A. 6 *)

(* For time complexity, we have two calls of foo per single calling of foo
   until the base case. Thus, if we draw this function out in tree format, we
   can see that for the ith level of the tree, there will be 2^ i calls for that
   level. There are log(n) levels of the tree so the total complexity is O(n).

   For space complexity, we have at most O(log n) pending operations because
   n gets divided by two for successive calling of foo.*)

(* A. 7 *)

(* This is a linear recursive function because we have a buildup process where
   the last_two for n relies on those of (n-1). The time complexity is O(n)
   because each call to last_two relies on the computation of last_two(n-1)
   which relies on the computation of last_two(n-2) .... all the way down to
   last_two(0) and because each function call takes constant time to calculate,
   the total time is O(n).

   The space complexity is O(n) as each call to last_two merely relies on
   last_two (n-1) which relies on last_two(n-2) .... all the way to last_two(0),
   so it only requires O(n) extra space. *)

(* B.1 *)

(*    (fun x y -> x * (2+y)) 20 (2*4)  *)

(*    (fun a b c -> b *. b -. 4.0 *. a *. c) 1.0 20.0 3.0)    *)

(*    (( fun x -> ((fun y -> (( fun z -> x * y * z) 3 )) 2 )) 1)    *)

(*     (( fun x -> ((fun x -> (( fun x -> x * x * x) 3 )) 2 )) 1)    *)

(* B.2 *)

(*   (fun x y -> (( fun y-> (( fun z -> x * y *z) 22 )) 14 )) (2*10) (3+4)
    Evaluate 3 + 4
    -> 7
    Evaluate 2 * 10
     -> 20

     Apply fun x y -> (( fun y-> (( fun z -> x * y *z) 22 )) 14 ) to 20, 7
     Substitute 20 for x, y for 7
     -> (( fun y-> (( fun z -> 20 * y *z) 22 )) 14 (inner y shielded)
      14 -> 14
     Evaluate fun y-> (( fun z -> 20 * y *z) 22 ) 14
     Apply fun y-> (( fun z -> 20 * y *z) 22 ) to 14
     Substitute 14 for y
     -> (( fun z -> 20 * 14 *z) 22 )
      Evaluate (fun z -> x * y * z) 22
        22 -> 22
        Apply fun z -> 20 * 14 * z to 22
      Substitute 22 for z
     -> 20 * 14 * 22
    -> 6160
*)

(* B.3 *)
(* ( fun x y z -> x + y + z) 10 (x * 2) (y + 3)) *)
(* This function complains that it is bounded because x is indeed not defined.
   It is just a placeholder for 10.*)

(* To fix:
   let x = 10 in
   let y = x * 2 in
   let z = y + 3 in
   x + y + z
*)

(* C.1 *)

let isum term a next b =
  let rec iter a result =
    if a > b
    then result
    else iter (next(a)) (result +/ term(a))
  in iter a (ni(0))

(* C.2 *)

let rec product_rec term a next b =
  if a > b
  then (ni 1)
  else term a */ (product_rec term (next a) next b)

let product_iter term a next b =
  let rec iter a result =
    if a > b
    then result
    else iter (next(a)) (result */ term(a))
  in iter a (ni(1))

let factorial_iter n = product_iter (fun x->x) (ni 1) (fun n -> n +/ (ni 1)) n
let factorial_rec n = product_rec (fun x->x) (ni 1) (fun n -> n +/ (ni 1)) n

let pi_product n =
  let numerator x = (quo_num x (ni(2))) */ ni(2) +/ ni(2) in
  let denominator x = (quo_num (x +/ ni(1)) (ni(2))) */ ni(2) +/ ni(1) in
  let next x = x +/ ni(1) in
  let totalTop = product_iter numerator (ni(1)) next n in
  let totalBottom = product_iter denominator (ni(1)) next n in
  (ni 4) */ totalTop // totalBottom

let pi_approx = float_of_num(pi_product (ni 1000))

(* C.3 *)

let rec accumulate_rec combiner null_value term a next b =
  if a > b then null_value
  else combiner (term a)(accumulate_rec combiner null_value term (next a)next b)

let accumulate_iter combiner null_value term a next b =
  let rec iter a result =
    if a > b then result
    else iter (next a) (combiner result (term a))
in iter a null_value

let sum term a next b = accumulate_iter ( +/ ) (ni 0) term a next b
let product term a next b = accumulate_iter ( */ ) (ni 1) term a next b

(* C. 4 *)

let compose f g = (fun x -> (f ( g x)))


(* C.5 *)

let repeated f n =
  let rec iter fun1 a = if a > n then fun1 else iter (compose fun1 f) (a+1) in
  iter (fun x -> x) 1

(* C.6 *)

let smooth f dx = (fun x -> ( f (x -. dx) +. f x +. f (x +. dx))/. 3.)

let nsmoothed f n dx =
  let smooth_dx = smooth f dx in
  repeated smooth_dx n

(* D.1 *)
let is_prime n =
  match n < 2 with
  | true -> false
  | _ -> let isFactor m = n mod m = 0 in
    let sqrt a = int_of_float(sqrt(float_of_int a)) in
    let rec check start end1 =
      match start > end1 with
      | true -> true
      | _ -> if isFactor start && start <> end1 then false
        else check (start+1) end1
    in check 2 ((sqrt n) + 1)

(* D.2 *)
let smallest_prime_factor n =
  match n < 2 with
  | true -> invalid_arg "n too small"
  | _ ->
    let isFactor m = n mod m = 0 in
    let sqrt a = int_of_float(sqrt(float_of_int a)) in
    let rec check start end1 =
      match start > end1 with
      | true -> invalid_arg "prime"
      | _ -> if (is_prime start && isFactor start) then start
        else check (start + 1) end1
    in check 1 ((sqrt n) + 1)
