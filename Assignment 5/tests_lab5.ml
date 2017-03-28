(* Tests for lab5.ml *)

open OUnit2
open Lab5

let eps = 0.00000001
let assert_float msg f1 f2 = assert_bool msg (cmp_float ~epsilon:eps f1 f2)
let ungram (`Gram f) = f
let unsecond (`Second s) = s

(* Test function for bubble sort. *)
let test_bs arr = bubble_sort arr; arr

(* Definitions for problem C.2 *)
(*
let n0  = make_number 0
let n1  = make_number 1
let n2  = make_number 2
let x   = make_variable "x"
let s1  = make_sum n1 n2
let s2  = make_sum n1 n0
let s3  = make_sum x n1
let s4  = make_sum (make_variable "y") (make_number 4)
let p1  = make_product n2 n2
let p2  = make_product x n0
let p3  = make_product x n2
let p4  = make_product x s1
let sl1 = make_sum p3 (make_sum p4 s3)
let p5  = make_product n2 p4
let p6  = make_product x s3
let ap1 = make_sum p3 p4
let pa1 = make_product s3 s4
let pl1 = make_product s2 (make_product s4 sl1)
*)

(*
show (p3#derive "x")  (* 2 *)
show (p3#derive "y")  (* 0 *)
show (p6#derive "x")  (* ((x + 1) + x) *)
show (pl1#derive "x") (* ((y + 4) * 6) *)
show (pl1#derive "y") (* ((x * 2) + ((x * 3) + (x + 1))) *)
show (s3#derive "x")  (* 1 *)
*)

(* g represents 5*x + x*y + 7*y *)
(*
let g =
  make_sum 
    (make_product 
      (make_number 5)
      (make_variable "x"))
    (make_sum
      (make_product 
        (make_variable "x")
        (make_variable "y"))
      (make_product 
        (make_number 7)
        (make_variable "y")))

g#show
(g#evaluate "x" 2)#show
(g#evaluate "y" 3)#show
((g#evaluate "x" 2)#evaluate "y" 3)#show
(g#derive "x")#show  ;;
(g#derive "y")#show;;
(g#derive "z")#show ;;
(((g#derive "x")#evaluate "x" 2)#evaluate "y" 3)#show;;
*)

(*

(* f = x^3*y + 3*x^2*y^2 + y^2 + 2 *)

let f =
  make_sum
   (make_product 
    (make_variable "x")
    (make_product 
     (make_variable "x")
     (make_product
      (make_variable "x")
      (make_variable "y"))))
   (make_sum
    (make_product 
     (make_number 3)
     (make_product
      (make_variable "x")
      (make_product
       (make_variable "x")
       (make_product
        (make_variable "y")
        (make_variable "y")))))
    (make_sum
     (make_product 
      (make_variable "y")
      (make_variable "y"))
     (make_number 2)))

show (differentiate f "x")
show (evaluate f "x" 3)
show (evaluate (evaluate f "x" 3) "y" 4)
let dfdx = differentiate f "x"
show (evaluate (evaluate dfdx "x" 3) "y" 4)

*)

let all_tests = "all" >:::
[ 
  "Problem A.1" >:: (fun c ->
     assert_equal (fibonacci 0) 0;
     assert_equal (fibonacci 1) 1;
     assert_equal (fibonacci 2) 1;
     assert_equal (fibonacci 3) 2;
     assert_equal (fibonacci 5) 5;
     assert_equal (fibonacci 10) 55;
     assert_equal (fibonacci 40) 102334155;
     assert_equal (fibonacci2 0) 0;
     assert_equal (fibonacci2 1) 1;
     assert_equal (fibonacci2 2) 1;
     assert_equal (fibonacci2 3) 2;
     assert_equal (fibonacci2 5) 5;
     assert_equal (fibonacci2 10) 55;
     assert_equal (fibonacci2 40) 102334155
  );

  "Problem A.2" >:: (fun c ->
     assert_equal (test_bs [| |]) [| |];
     assert_equal (test_bs [| 1 |]) [| 1 |];
     assert_equal (test_bs [| 1; 1 |]) [| 1; 1 |];
     assert_equal (test_bs [| 2; 1 |]) [| 1; 2 |];
     assert_equal (test_bs [| 1; 1; 1 |]) [| 1 ; 1; 1 |];
     assert_equal (test_bs [| 1; 2; 3; 4; 5 |]) [| 1 ; 2; 3; 4; 5 |];
     assert_equal (test_bs [| 5; 2; 3; 1; 4 |]) [| 1 ; 2; 3; 4; 5 |];
     assert_equal (test_bs [| 5; 3; 3; 5; 1 |]) [| 1 ; 3; 3; 5; 5 |];
     assert_equal (test_bs [| "foo"; "bar"; "baz" |])
      [| "bar"; "baz"; "foo" |]
  );

  "Problem B.1" >:: (fun c ->
     assert_float "get_meters 1"   (get_meters   (`Meter  1.0)) 1.0;
     assert_float "get_meters 2"   (get_meters   (`Foot   2.0)) 0.6096;
     assert_float "get_meters 3"   (get_meters   (`Inch   3.0)) 0.0762;
     assert_float "get_grams 1"    (get_grams    (`Gram   1.0)) 1.0;
     assert_float "get_grams 2"    (get_grams    (`Kilo   2.0)) 2000.0;
     assert_float "get_grams 3"    (get_grams    (`Slug   3.0)) 43781.709609;
     assert_float "get_seconds 1"  (get_seconds  (`Second 1.0)) 1.0;
     assert_float "get_seconds 2"  (get_seconds  (`Minute 2.0)) 120.0;
     assert_float "get_seconds 3"  (get_seconds  (`Hour   3.0)) 10800.0;
     assert_float "get_seconds 4"  (get_seconds  (`Day    4.0)) 345600.0;
     assert_float "mass_add 1" (ungram (mass_add (`Gram 1.0) (`Slug 2.0)))
       29188.806406;
     assert_float "mass_add 2" (ungram (mass_add (`Gram 1.0) (`Kilo 2.0)))
       2001.0;
     assert_float "mass_add 3" (ungram (mass_add (`Slug 1.0) (`Kilo 2.0)))
      16593.903203;
     assert_float "time_add 1" (unsecond (time_add (`Second 1.0) (`Minute 2.0)))
       121.0;
     assert_float "time_add 2" (unsecond (time_add (`Minute 1.0) (`Hour 2.0)))
       7260.0;
     assert_float "time_add 3" (unsecond (time_add (`Hour 1.0) (`Day 2.0)))
       176400.0;
     assert_float "time_add 4" (unsecond (time_add (`Day 1.0) (`Second 2.0)))
       86402.0
  );
]

let _ = run_test_tt_main all_tests

