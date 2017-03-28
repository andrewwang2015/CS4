(* A.1 *)

let fibonacci n =
  match n with
  | 0 -> 0
  | 1 -> 1
  | _ -> let term1 = ref 0 in
    let term2 = ref 1 in
    let count = ref 1 in
    let result = ref 0 in
    while !count < n do
      result := !term1 + !term2;
      term1 := !term2;
      term2 := !result;
      count := !count + 1
    done;
    !result

let fibonacci2 n =
  match n with
  | 0 -> 0
  | 1 -> 1
  | _ -> let term1 = ref 0 in
    let term2 = ref 1 in
    let result = ref 0 in
    for count = 1 to (n-1) do
      result := !term1 + !term2;
      term1 := !term2;
      term2 := !result;
    done;
    !result


(* A.2 *)

let bubble_sort arr =
  let swap i j = begin
    let temp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- temp;
  end in
  for i = 1 to (Array.length arr - 1) do
    for i = 1 to (Array.length arr - 1) do
      if arr.(i-1) > arr.(i) then begin
        swap (i-1) (i)
      end
    done;
  done;
;;


(* B.1 *)

let meters_per_foot = 0.3048
let inches_per_foot = 12.0

let get_meters len =
  match len with
  | `Meter m -> m
  | `Foot f -> f *. meters_per_foot
  | `Inch i -> i /. inches_per_foot *. meters_per_foot

let length_add a b = `Meter (get_meters a +. get_meters b)

(* B.2 *)
let grams_per_kilogram = 1000.0
let grams_per_slug = 14593.903203

let get_grams mass = match mass with
  | `Gram g -> g
  | `Kilo k -> k *. grams_per_kilogram
  | `Slug s -> s *. grams_per_slug

let mass_add a b = `Gram (get_grams a +. get_grams b)

let secs_per_minute = 60.0
let hrs_per_day = 24.0
let mins_per_hr = 60.0

let get_seconds t = match t with
| `Second s -> s
| `Minute m -> m *. secs_per_minute
| `Hour h -> h *. mins_per_hr *. secs_per_minute
| `Day d -> d *. hrs_per_day *. mins_per_hr *. secs_per_minute

let time_add a b  = `Second(get_seconds a +. get_seconds b)

(* B.3 *)

let unit_add a b =
  match (a,b) with
  | (`Length a, `Length b) -> `Length(length_add a b)
  | (`Mass a, `Mass b) -> `Mass(mass_add a b)
  | (`Time a, `Time b) -> `Time(time_add a b)
  | _ -> failwith "Incompatible types"


(* We do not get a combinatorial explosion because in this case, we can
   check if the types of the two operands are the same type. Clearly, if there
   are n types, then we only need to check n times, so it's linear. *)

(* C.1 *)
let rec make_gram g =
   object
     method get_grams = g
     method get_slugs = g /. grams_per_slug
     method unit_type = `Gram
     method compatible other = match other#unit_type with
       | `Gram -> true
       | `Slug -> true
       | _ -> false
      method add other = match other#unit_type with
        | `Gram
        | `Slug -> make_gram (g +. other#get_grams)
   		  | _ -> failwith "incompatible types"
   end

(* C.2 *)

(* Define a number as a message-passing object. *)
(* "i" is an int. *)
let rec make_number i =
object
 method value = i
 method show = string_of_int i
 method is_zero = i = 0
 method is_number = true
 method evaluate _ _ = make_number i  (* must evaluate to an object *)
 method derive _ = make_number 0  (* derivative of a number is 0 *)
end

(* Define a variable as a message-passing object. *)
(* "v" is a string. *)
let rec make_variable v =
object
 method value = failwith "variable has no numerical value"
 method show  = v
 method is_zero = false
 method is_number = false
 method evaluate v' n =
   if v = v'
     then make_number n
     else make_variable v
 method derive v' =
   if v = v'
     then make_number 1  (* d/dx(x) = 1 *)
     else make_number 0  (* d/dx(y) = 0 *)
end

(* Define a sum as a message-passing object. *)
let rec make_sum expr1 expr2 =
match () with
 | _ when expr1#is_zero -> expr2  (* expr + 0 = expr *)
 | _ when expr2#is_zero -> expr1  (* 0 + expr = expr *)
 | _ when expr1#is_number && expr2#is_number ->  (* add numbers *)
       make_number (expr1#value + expr2#value)
 | _ ->  (* create a new object representing the sum *)
       object
         method value = failwith "sum expression has no numerical value"
         method show = "(" ^ expr1#show ^ " + " ^ expr2#show ^ ")"
         method is_zero = false
         method is_number = false
         method evaluate v n =
           make_sum (expr1#evaluate v n) (expr2#evaluate v n)
         method derive v =
           make_sum (expr1#derive v) (expr2#derive v)
       end

(* Evaluate a message-passing expression with a number
substituted for a variable. *)
let evaluate expr v n = expr#evaluate v n

(* Return the string representation of an expression. *)
let show expr = expr#show

(* Return the derivative of an expression. *)
let differentiate expr v = expr#derive v

(* Part a *)
let rec make_product expr1 expr2 =
  match () with
  | _ when expr1#is_zero || expr2#is_zero -> make_number 0
  | _ when expr1#is_number && expr1#value = 1 -> expr2
  | _ when expr2#is_number && expr2#value = 1 -> expr1
  | _ when expr1#is_number && expr2#is_number ->
    make_number(expr1#value * expr2#value)
  | _ ->
    object
      method value = failwith "prod epression has no numerical value"
      method show = "(" ^ expr1#show ^ " * " ^ expr2#show ^ ")"
      method is_zero = false
      method is_number = false
      method evaluate v n = make_product(expr1#evaluate v n)
          (expr2#evaluate v n)
      method derive v =
        let first_term = make_product(expr1#derive  v) (expr2) in
        let second_term = make_product(expr2#derive v) expr1 in
        make_sum first_term second_term
    end




(* Part b *)

(* 1 *)
(*
val f :
  < derive : string -> 'a; evaluate : string -> int -> 'a; is_number :
      bool; is_zero : bool; show : string; value : int >
  as 'a = <obj>
*)

(* 2 *)

(* val dfdx :
   < derive : string -> 'a; evaluate : string -> int -> 'a; is_number :
    bool; is_zero : bool; show : string; value : int >
   as 'a = <obj>
*)

(* 3 *)

(*

- : string =
"(((x * (x * y)) + (((x * y) + (y * x)) * x)) + (((x * (y * y)) +
((y * y) * x)) * 3))"

*)

(* 4 *)

(*
- : string =
"((3 * (3 * (3 * y))) + ((3 * (3 * (3 * (y * y)))) + ((y * y) + 2)))"
*)

(* 5 *)

(*
- : string = "558"
*)

(* 6 *)

(*
- : string = "396"
*)
