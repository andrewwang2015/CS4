(* A.1 *)

(*
1. - : int = 10
2. - : float = 10.
3. - : int = 12
4. Error: Here, we are using the "+" operator which is reserved for integers on
   two floats.
5. Error: Here, we are using the "+." operator which is reserved for floats on
   two integers.
6. Error: Here, we are using the "+" operator which is expecting two integers on
   an expression containing one float.
7. Error: Here, we are using the "+." operator which is expecting two floats on
   an expression containing one int.
8. - : float = 7.2
9. - : int = 5
10. - : int = 7
11. val a : int = 3
12. val b : int = 4
13. - : bool = false
14. - : bool = true
15. - : bool = false. This is different from the previous expression because
    while "=" evaluates for structural equivalence, "==" evaluates for
    identicality (same object in memory)
16. - : (int * int * int) list = [(1, 2, 3)]. To differentiate items in a list,
    we should use ";" instead of "," which is used for tuples. In this case,
    the interpretor thinks we want to instantiate a list of size 1, containing
    one tuple of 3 integers.
17. - : int = 4
18. Syntax error: This is not Python. "&&" is the proper way to use the boolean
    and.
19. - : int = 6
20. Type error: We can only leave off the else clause if the then clause returns
    the unit value (). In this case, it returns an int.

*)

(* A.2 *)

(* This function computes the sum of the squares of the two largest of the three
   input numbers *)
let sum_of_squares_of_two_largest num1 num2 num3 =
  if (num1 > num2 || num1 > num3) && num2 > num3 then num1 * num1 + num2 * num2
  else if (num3 > num2 || num3 > num1) && num2 > num1
  then num3 * num3 + num2 * num2 else num3 * num3 + num1 * num1

(* A.3 *)

(* We first evaluate if b > 0. If b > 0 then we will add a and b. If b <= 0,
   we subtract b from a. Essentially, this function returns a + absolute
   value of b.
*)

(* ====================================================================================== *)

(* B.1 *)
(* For normal order, the function will quickly return 0 because it never needs
   to evaluate p(). For the applicative order, it will evaluate p() immediately
   which is problematic ( due to p being a recursive function that keeps calling
   itself and thereby never returning/ stopping). In the end, no value will be returned.
*)

(* B.2 *)
(* The new-if statement first evaluates the then_clause and else_clause
   before evlauating the predicate. Because of this, we will have an infinite
   recursion loop where the else_clause keeps calling the recursive sqrt_iter
   function
*)

(* B.3 *)

(*
Evaluate add_a 2 5:
  2 -> 2, 5-> 5
	Evaluate add_a -> ( fun a b -> if a = 0 then b else inc (add_a (dec a) b))
	Apply fun a b to 2, 5
	Substitute 2, 5 for a, b in if a = 0 then b else inc (add_a(dec a)b)
	Evaluate if 2 = 0 then 5 else inc ( add_a (dec 2) 5)
		Evaluate the predicate: 2 = 0:
			2 -> 2
			0 -> 0
			= -> =
			Apply = to 2, 0 to get false
		Go to else clause: inc (add_a (dec 2) 5)
		Evaluate inc (add_a (dec 2) 5):
			Evaluate (add_a (dec 2) 5):
				Evaluate (dec 2):
					2 -> 2:
					Evaluate dec -> fun a -> a - 1
					Apply fun a to 2 to get 1
				5 -> 5
				Evaluate add_a -> (fun a b -> if a = 0 then b else inc (add_a (dec a) b))
				Apply fun a b -> if a = 0 then b else inc ( add_a (dec a) b) to 1, 5
        Substitute 1,5 for a,b in if a = 0 then b else inc ( add_a (dec a) b) to 1, 5
				Evaluate if 1 = 0 then 5 else inc ( add_a (dec 1) 5):
					Evaluate 1 = 0:
						1 -> 1
						0 -> 0
						= -> =
						Apply = to 1, 0 to get false
					Go to else clause: inc ( add_a (dec 1) 5)
					Evaluate inc( add_a (dec 1) 5):
						Evaluate (add_a (dec 1) 5):
							Evaluate (dec 1):
								 -> 1
								Evaluate dec -> fun a -> a - 1
								Apply fun a to 1 to get 0
							5 -> 5
							Evaluate add_a -> ( fun a b -> if a = 0 then b else inc (add_a (dec a) b))
							Apply fun a b -> if a = 0 then b else inc ( add_a (dec a) b) to 0, 5
							Substitute 0,5 for a,b in if a = 0 then b else inc (add_a (dec a) b)
							Evaluate if 0 = 0 then 5 else inc ( add_a (dec 0) 5):
								0 -> 0
								0 -> 0
								= -> =
								Apply = to 0, 0
                Go to else clause (b) to get 5
						Evaluate inc -> fun a -> a + 1
						Apply fun a to 5 to get 6
			Evaluate inc -> fun a -> a + 1
			Apply fun a to 6 to get 7
Return value: 7

Evaluate add_b 2 5:
	2 -> 2
	5 -> 5
	Evaluate add_b -> fun a b -> if a = 0 then b else add_b (dec a) (inc b)
	Apply fun a b to 2, 5
	Substitute 2, 5 for a, b in if a = 0 then b else add_b (dec a) (inc b)
	Evaluate if 2 = 0 then 5 else add_b (dec 2) (inc 5):
		Evaluate 2 = 0:
			2 -> 2
			0 -> 0
			= -> =
			Apply = to 2, 0 to get false
    Go to else clause: add_b (dec 2) (inc 5)
		Evaluate add_b (dec 2) (inc 5):
			Evaluate (dec 2):
				2 -> 2:
				Evaluate dec -> fun a -> a - 1
				Apply inc a to 2 to get 1
			Evaluate (inc 5):
				5 -> 5:
				Evaluate inc -> fun a -> a + 1
				Apply inc to 5 to get 6
			Evaluate add_b -> fun a b -> if a = 0 then b else add_b (dec a) (inc b)
      Substitute 1,6 for a,b in fun a b -> if a = 0 then b else add_b (dec a) (inc b)
			Evaluate if 1 = 0 then 6 else add_b (dec 1) (inc 6):
				Evaluate 1 = 0:
					1 -> 1
					0 -> 0
					= -> =
					Apply = to 1, 0 to get false
        Go to else clause: add_b (dec 1) (inc 6)
				Evaluate add_b (dec 1) (inc 6):
					Evaluate (dec 1):
						1 -> 1
						Evaluate dec -> fun a -> a - 1
						Apply dec to 1 to get 0
					Evaluate (inc 6):
						6 -> 6
						Evaluate inc -> fun a -> a + 1
						Apply inc to 6 to get 7
					Evaluate add_b -> fun a b -> if a = 0 then b else add_b (dec a) (inc b)
          Substitute 0,7 for a,b in fun a b -> if a = 0 then b else add_b (dec a) (inc b)
					Evaluate if 0 = 0 then 7 else add_b (dec 0) (inc 7):
						Evaluate 0 = 0:
							0 -> 0
							0 -> 0
							= -> =
							Apply = to 0, 0 to get true
          Go to then clause: with true clause 7
    Return value: 7

add_a is linear recursive while add_b is linear iterative.
*)

(* ====================================================================================== *)

(* This function computes the factorial of the input number, which for a number
   n is equal to n * (n-1) * ... * 1. *)
let rec factorial n =
  if n = 0 then 1 else n * factorial(n-1)

(* C.1.a. *)

(* This function takes a non-negative integer argument and computes that term
   of the infinite series expansion of e. *)

let e_term term =
  1.0 /. float_of_int(factorial(term))

(* C.1.b *)

(* This helper recursive function takes three arguments and computes the sum of the first
   max terms of the series approximation for e *)
let rec e_approx_helper current max sum =
  if current > max
  then sum
  else e_approx_helper (current + 1) max (sum +. e_term current)

(* This is the function that calls the helper function *)
let e_approximation termNumber =
  e_approx_helper 0 termNumber 0.

(* C.1.c *)

(* e_approximation 20 | - : float = 2.71828182845904553 *)
(* exp 1.0            | - : float = 2.71828182845904509 *)

(* C.1.d *)
(* When we sum up to a large number of terms, we also need to compute the
   factorial of these large numbers which due to limited space evaluates to 0
   and thus the sum of 1 over these large numbers goes to infinity *)

(* C.2 *)
(* This function is a mutually recursive function that tells whether a given
   number is odd or even *)
let rec is_even num = match num with
  | 0 -> true
  | _ -> is_odd(num-1)
and is_odd num = match num with
  | 0 -> false
  | _ -> is_even(num-1)

(* C.3 *)

(* Recursive definition for f(n) = n if n < 3 and f(n) = f(n-1) + 2f(n-2) +
   3f(n-3)) if n >= 3 *)
let rec f_rec num =
  if num < 3 then num
  else f_rec (num-1) + 2 * f_rec (num-2) + 3 * f_rec (num-3)

(* Iterative definition for f(n) = n if n < 3 and f(n) = f(n-1) + 2f(n-2) +
   3f(n-3)) if n >= 3. We need helper function that keeps track of last three
  terms of sequence *)
let rec helper_f num1 num2 num3 current max =
  if current = max then num3 + 2 * num2 + 3 * num1
  else helper_f (num2) (num3) (num3 + 2 * num2 + 3 * num1) (current+1) max

let f_iter num =
  if num < 3 then num
  else helper_f 0 1 2 3 num

(* C.4 *)

(* This function takes a row and index and recursively calculates the pascal triangle
   number at that row and index *)

let rec pascal_coefficient row index = match row > 0 with
  | false -> failwith "invalid arguments"   (* If negative arguments *)
  | true ->
    match index with
    | 1 -> 1
    | _ when index = row -> 1 (* last index is always 1 per row *)
    | _ when 1 < index && index < row -> pascal_coefficient (row-1)(index-1) + pascal_coefficient (row-1) (index)
    | _ -> failwith "invalid arguments"   (* if index > row or index is negative *)
