(* A.1.a *)

type mobile = Mobile of branch * branch  (* left and right branches *)
and branch =
  | Weight    of int * int     (* length and weight *)
  | Structure of int * mobile  (* length and sub-mobile *)

let make_mobile l r = Mobile (l, r)
let make_weight l w = Weight (l, w)
let make_structure l m = Structure (l, m)

let left_branch = function Mobile (l, _) -> l

let right_branch = function Mobile (_, r) -> r

let branch_length = function
  | Weight(l, w) -> l
  | Structure(l, s) -> l

let branch_structure = function
  | Weight(l, w) -> `Weight w
  | Structure(l, s) -> `Structure s

(* A.1.b *)

let rec branch_weight1 = function
  | Weight(l,w) -> w
  | Structure(l, s) -> total_weight1 s
and total_weight1 s = match s with
  | Mobile(l, r) -> (branch_weight1 l) + (branch_weight1 r)

let rec branch_weight2 bs = match branch_structure bs with
  |`Weight w -> w
  | `Structure s-> total_weight2 s
and total_weight2 s = branch_weight2 (left_branch s) +
                      branch_weight2 (right_branch s)

(* A.1.c *)
let rec is_balanced m =
  let test = (branch_length (left_branch m)) * (branch_weight2 (left_branch m))
             = (branch_length (right_branch m)) *
               (branch_weight2 (right_branch m)) in
  match (branch_structure (left_branch m), branch_structure (right_branch m))
  with
  | (`Weight w, `Weight w1) -> test
  | (`Weight w, `Structure s)
  | (`Structure s, `Weight w) -> test && is_balanced s
  | (`Structure s, `Structure s1) -> test && is_balanced s && is_balanced s1

(* A.1.d *)
type mobile'  = { left: branch'; right: branch' }
and  branch'  = Branch' of int * contents
and  contents = Weight' of int | Structure' of mobile'

let make_mobile' left right = {left; right}
let make_weight' l w = Branch' (l, Weight' w)
let make_structure' l s = Branch' (l, Structure' s)

let left_branch' {left; right} = left
let right_branch' {left; right} = right

let branch_length' = function
  | Branch' (l, _) -> l

let branch_structure' (Branch' (_, c)) = match c with
  | Structure' s -> `Structure s
  | Weight' w -> `Weight w

let rec branch_weight' b = match branch_structure' b with
  | `Structure s -> total_weight' s
  | `Weight w -> w
and total_weight' m =
  (branch_weight' (left_branch' m)) + (branch_weight' (right_branch' m))

let rec is_balanced' m =
  let test = (branch_length' (left_branch' m)) * (branch_weight'
                                                    (left_branch' m))
               = (branch_length' (right_branch' m)) *
                 (branch_weight' (right_branch' m)) in
  match (branch_structure' (left_branch' m), branch_structure'
           (right_branch' m))
    with
    | (`Weight w, `Weight w1) -> test
    | (`Weight w, `Structure s)
    | (`Structure s, `Weight w) -> test && is_balanced' s
    | (`Structure s, `Structure s1) -> test && is_balanced' s && is_balanced' s1

(* A.2 *)


type tree = Tree of elem list
and elem =
  | Num of int
  | Sub of tree

let rec square_tree t1 =
  match t1 with
  | Tree []-> Tree []
  | Tree((Num h) :: t) -> let (Tree tempTree) = square_tree (Tree t) in
    Tree(Num (h * h) :: tempTree)
  | Tree ((Sub h) :: t) -> let (Tree tempTree) = square_tree (Tree t) in
    Tree(Sub (square_tree h) :: tempTree)

let rec square_tree' treeLst = match treeLst with
  | Tree treeLst ->
    let mappingF = function
      | Num n -> Num (n*n)
      | Sub s -> Sub(square_tree' s)
    in
    Tree (List.map mappingF treeLst)

(* A.3 *)

let tree_map g treeLst = match treeLst with
  | Tree treeLst ->
    let mappingF = function
      | Num n -> Num (g n)
      | Sub s -> Sub(square_tree' s)
    in
    Tree (List.map mappingF treeLst)


let square_tree'' tree = tree_map (fun n -> n * n) tree

(* A.4 *)

let rec subsets = function
  | [] -> [[]]
  | h::t -> let rest = subsets t in rest @ (List.map (fun x-> h:: x) rest )

(* This works because to get subsets recursively, we can think of it as
   considering whether to add the particular element (h) or not. By doing this
   for the each element of the set (deciding whether to include it or not), we
   are able to generate all subsets *)

(* A.5 *)
let rec accumulate op initial sequence =
  match sequence with
  | [] -> initial
  | h :: t -> op h (accumulate op initial t)

let map p sequence =
  accumulate (fun x r -> (p x) :: r) [] sequence

let append seq1 seq2 =
  accumulate (fun x r -> x :: r) seq2 seq1

let length sequence =
  accumulate (fun x r -> r + 1) 0 sequence

(* A.6 *)
let rec accumulate_n op init seqs =
  match seqs with
  | [] -> failwith "empty list"
  | [] :: _ -> []
  | h :: t -> accumulate op init (List.map List.hd seqs) ::
              accumulate_n op init (List.map List.tl seqs)

(* A.7 *)
let rec map2 f x y =
  match (x, y) with
  | ([], []) -> []
  | ([], _) -> failwith "unequal lists"
  | (_, []) -> failwith "unequal lists"
  | (lh::lt, rh:: rt) -> (f lh rh) :: map2 f lt rt

let dot_product v w = accumulate (+) 0 (map2 ( * ) v w)
let matrix_times_vector m v = accumulate(fun x y -> (dot_product x v) :: y) [] m

let transpose mat = accumulate_n (fun x y -> x::y) [] mat

let matrix_times_matrix m n =
  let cols = transpose n in
  map (fun r -> matrix_times_vector cols r) m

(* B.1 *)
let rec filter predicate sequence =
match sequence with
	| [] -> []
	| h :: t when predicate h -> h :: filter predicate t
 | _ :: t -> filter predicate t

let rec quicksort lst cmp = match lst with
  | [] -> []
  | h:: t ->
    quicksort(filter (fun x -> cmp x h) t) cmp @ [h] @
    quicksort(filter(fun x -> not(cmp x h)) t) cmp

(* B.2 *)

(* This quicksort is an instance of generative recursion because we are
   "generating" new lists for the left and right halves. The original lst
   is not modified *)

(* B.3 *)

(* We have looping recursion because without this base case with one element,
   in the case where the input list is [x], there is no way for [x] to be
   further reduced. *)

(* B.4 *)
let rec insert_in_order new_result a_list cmp =
  match a_list with
  | [] -> [new_result]
  | h :: t when cmp new_result h -> new_result :: a_list
  | h :: t ->  h :: insert_in_order new_result t cmp

let rec insertion_sort a_list cmp =
  match a_list with
  | [] -> []
  | h :: t -> insert_in_order h (insertion_sort t cmp) cmp

(* This is structural recursion because we are breaking the input into a head
   and tail and then inserting the head into a properly sorted tail. No new
   lists are being generated, but rather the input is being portioned. *)

(* C.1 *)

type expr =
	| Int of int
	| Var of string
	| Add of expr * expr
	| Mul of expr * expr
 | Pow of expr * int

let rec simplify expr =
  let e = simplify1 expr in
  if expr = e
  then expr
  else simplify e
and simplify1 expr =
  let ( ** ) b n =
  let is_even m = m mod 2 = 0 in
  let rec iter a x y =
    match y with
    | 0 -> a
    | _ when is_even y -> iter a (x*x) (y/2)
    | _ -> iter (a*x) x (y-1)
  in iter 1 b n in
  match expr with
  | Add(Int a', Int b') -> Int (a' + b')
  | Mul (Int a', Int b') -> Int (a' * b')
  | Pow (Int a', b') -> Int (a' ** b')
  | Add (Int 0, a) -> simplify a
  | Add (a, Int 0) -> simplify a
  | Mul (a, Int 0) -> Int 0
  | Mul (Int 0, a) -> Int 0
  | Mul (Int 1, a) -> simplify a
  | Mul (a, Int 1) -> simplify a
  | Pow (a, 0) -> Int 1
  | Pow (a, 1) -> a
  | Add (a,b) -> (Add (simplify1 a, simplify1 b))
  | Mul (a,b) -> (Mul (simplify1 a, simplify1 b))
  | Pow (a,b) -> (Pow (simplify1 a, b))
  | _ -> expr     (* Base case *)

(* C.2 *)

let rec deriv expr var =
  match expr with
  | Int a' -> Int 0
  | Var x -> if x = var then Int 1 else Int 0
  | Add(a, b) -> Add ((deriv a var), (deriv b var))
  | Mul(a, b) -> Add (Mul(a, (deriv b var)), Mul((deriv a var), b))
  | Pow(a, b) -> Mul(Mul((Int b), Pow(a, b-1)),(deriv a var))


let derivative expr var =
  let e = simplify expr in
  let d = deriv e var in
  simplify d
