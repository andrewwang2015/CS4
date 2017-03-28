(* name: Andrew Wang *)
(* login: azwang *)

(* 1.a *)
(* The time complexity is O(x). We see that this function returns z when x < 0
   and x decreases by 1 each call, so thus it is linear with respect to x. *)

(* 1.b *)
(* The time complexity is O(log y). Here, we see that for large x and large y,
   y decreases much faster so we focus on y. This function returns z when y is
   less than 1 and because y gets divided by 2 each iteration, it's basically
   calls itself n times where y / 2^n < 1. Thus, we have O(log y) time
   complexity. *)

(* 1.c *)
(* The time complexity is O(xmax * ymax). We see that the function returns once
   x > xmax. x only increases when y > ymax and y increases each recursive call.
   However, we note that x increases once per y iterations for y > ymax and
   that once y > ymax, y gets reset to 0. Thus because x starts at 0, we need ~
   xmax times for y > ymax for x to be > xmax. Thus, our time complexity is
   O(xmax * ymax). *)

(* 1.d *)
(* The time complexity is O(a). We see that we have a  tree structure with
   three recursive calls per call to jerky. The height of our tree would be
   log (base 3) of a because a gets divided by 3 each call and our function
   returns when a = 1. Thus, the total time complexity is 3 ^ log(base 3) of a.
   This is equivalent to O(a).
*)

(* 1.e *)

(* This is an example of infinite recursion, so one could say O(infinity).
   We see that there is
   no base case for returning and that once m = n,
   quirky n m will keep getting called. *)

(* 2.1.a *)

let split n lst =
  let rec iter m take rest =
    match rest with
      | _ when m = 0 -> (List.rev take, rest)
      | [] -> failwith "split: not enough elements in list"
      | h :: t -> iter (m - 1) (h::take) t
  in
  iter n [] lst

(* 2.1.b *)

let rec groupN n lst =
  match lst with
  | [] -> []
  | _ -> let (lst1, rest) = split n lst in
    lst1 :: groupN n rest

(* 2.1.c *)

(* This is structural recursion because we are making recursive calls on
   partitions of the original list. When we recursively call the groupN
   function, we are doing it on the "rest" portion of the input lst, not
   new generated lists. *)

(* 2.2.a *)

let rec even_and_odd_halves_rec lst =
  match lst with
  | [] -> [], []
  | [x] -> [x], []
  | a::b::c -> let (even, odd) = even_and_odd_halves_rec c in a::even, b::odd

(* 2.2.b *)

let even_and_odd_halves_iter lst =
  let rec iter num lst1 lst2 lst =
    match lst with
    | [] -> (lst1, lst2)
    | a::b -> if num mod 2 = 0 then iter (num+1) (a::lst1) lst2 b else
        iter (num+1) (lst1) (a::lst2) b
  in iter 0 [] [] lst

(* 2.2.c *)

let rec merge_in_order lst1 lst2 cmp =
  match (lst1, lst2) with
  | ([], _) -> lst2
  | (_, []) -> lst1
  | (h1 :: t1, h2 :: _) when cmp h1 h2 ->
    h1 :: merge_in_order t1 lst2 cmp
  | (_, h2 :: t2) ->
    h2 :: merge_in_order lst1 t2 cmp

let rec merge_sort a_list cmp =
  match a_list with
  | [] -> a_list
  | [x] -> a_list
  | _ -> let (even, odd) = even_and_odd_halves_rec a_list in
    merge_in_order (merge_sort even cmp) (merge_sort odd cmp) cmp

(* 2.3.a *)

let bubble lst cmp =
  let rec iter newLst lst cmp =
    match lst with
    | [] -> List.rev(newLst)
    | [x] -> List.rev(x::newLst)
    | a::b::c -> if cmp a b then iter (a:: newLst) (b::c) cmp
      else iter (b:: newLst) (a::c) cmp
  in iter [] lst cmp

(* 2.3.b *)

let bubble2 lst cmp =
  let rec iter newLst lst change cmp =
    match lst with
    | [] -> (List.rev(newLst), change)
    | [x] -> (List.rev(x::newLst), change)
    | a::b::c -> if cmp a b then iter (a:: newLst) (b::c) change cmp
      else iter (b:: newLst) (a::c) true cmp
  in iter [] lst false cmp

(* 2.3.c *)

let rec bubble_sort lst cmp =
  let (onePass, change) = bubble2 lst cmp in
  match change with
  | false -> onePass
  | true -> bubble_sort onePass cmp

(* 3.1 *)

let rec zipWith fct lst1 lst2 =
    match (lst1, lst2) with
      | ([], []) -> []
      | ([], h::t) -> []
      | (h::t, []) -> []
      | (h::t, h1::t1) -> (fct h h1) :: (zipWith fct t t1)

(* 3.2 *)

(* note, due to floating point error, this function was having trouble
   converging when I used the = operator for currentX and targetX. Thus,
   I did a similar test in comparing if their difference is < stepSize
   for the stopping condition *)

let euler_solve_de g f0 dx x =
  let rec iter stepSize targetX currentX currentY fct =
    if (targetX -. currentX) < stepSize then currentY else
      iter stepSize targetX (currentX +. stepSize)
                       (currentY +. stepSize *. fct currentX currentY) fct in
  iter dx x 0.0 f0 g

(* 3.3 *)
let tolerance = 1e-7
let square x = x *. x
let average x y = 0.5 *. (x +. y)
let rec iterative_improve good improvement =
  fun x ->
    if good x then x else iterative_improve good improvement (improvement x)

let my_sqrt num = if num = 0.0 then 0.0 else
    let good_enough = fun guess-> if abs_float(square guess -. num) < tolerance
then true else false in
let improve guess = average (num /. guess) guess in iterative_improve
  good_enough improve num



(* 4.1 *)
type tree =
  | Leaf
  | Node2 of tree * int * tree   (* left tree, value, right tree *)
  | Node3 of tree * int * tree * int * tree
  (* left tree, left value, middle tree, right value, right tree *)

let rec tree_search num = function
  | Leaf -> false
  | Node2(left, value, right) -> value = num ||
                                 (num < value && tree_search num left) ||
                                 (num > value && tree_search num right)
  | Node3(left, value1, middle, value2, right) ->
    num = value1 || num = value2 ||  (num < value1 && tree_search num left)
    || (num > value2 && tree_search num right) || (tree_search num middle)

(* 4.2 *)
type insertion =
  | Ok of tree   (* we absorbed the int into the existing nodes *)
  | Split of tree * int * tree   (* we had to split a node *)

let rec insert_helper i t =
  match t with

  (* Base cases. *)

  | Leaf -> Ok (Node2 (Leaf, i, Leaf))

  | Node2 (_, j, _) when i = j -> Ok t  (* i is already in tree*)
  | Node3 (_, j, _, k, _) when i = j || i = k -> Ok t  (* ditto *)

  | Node2 (Leaf, j, Leaf) when i < j ->
    (* add i to tree; change 2-node to 3-node *)
    Ok (Node3 (Leaf, i, Leaf, j, Leaf))
  | Node2 (Leaf, j, Leaf) ->   (* i > j *)
    Ok (Node3 (Leaf, j, Leaf, i, Leaf))

  | Node3 (Leaf, j, Leaf, k, Leaf) when i < j ->
    (* split; watch the order! *)
    Split (Node2 (Leaf, i, Leaf), j, Node2 (Leaf, k, Leaf))
  | Node3 (Leaf, j, Leaf, k, Leaf) when i > j && i < k ->
    Split (Node2 (Leaf, j, Leaf), i, Node2 (Leaf, k, Leaf))
  | Node3 (Leaf, j, Leaf, k, Leaf) ->   (* i > k *)
    Split (Node2 (Leaf, j, Leaf), k, Node2 (Leaf, i, Leaf))

  (* Recursive cases. *)

  | Node2 (t1, j, t2) when i < j ->  (* insert into t1 *)
    begin
      match insert_helper i t1 with
      | Ok t1' ->
        Ok (Node2 (t1', j, t2))
      | Split (t1a, i', t1b) ->
        Ok (Node3 (t1a, i', t1b, j, t2))
    end

  | Node2 (t1, j, t2) ->  (* i > j; insert into t2 *)
    begin
      match insert_helper i t2 with
      | Ok t2' ->
        Ok (Node2 (t1, j, t2'))
      | Split (t1a, i', t1b) ->
        Ok (Node3 (t1, j , t1a, i', t1b))
    end

  | Node3 (t1, j, t2, k, t3) when i < j ->  (* insert into t1 *)
    begin
      match insert_helper i t1 with
      | Ok t1' -> Ok (Node3 (t1', j, t2, k, t3))
      | Split (t1a, i', t1b) ->  (* split nodes *)
        let left = Node2 (t1a, i', t1b) in
        let right = Node2 (t2, k, t3) in
        Split (left, j, right)
    end

  | Node3 (t1, j, t2, k, t3) when i > j && i < k ->  (* insert into t2 *)
    begin
      match insert_helper i t2 with
      | Ok t2' -> Ok (Node3 (t1, j, t2', k, t3))
      | Split (t2a, i', t2b) ->  (* split nodes *)
        let left = Node2 (t1, j, t2a) in
        let right = Node2 (t2b, k, t3) in
        Split (left, i', right)
    end

  | Node3 (t1, j, t2, k, t3) ->  (* i > k; insert into t3 *)
    begin
      match insert_helper i t3 with
      | Ok t3' -> Ok (Node3 (t1, j, t2, k, t3'))
      | Split (t3a, i', t3b) ->  (* split nodes *)
        let left = Node2 (t1, j , t2) in
        let right = Node2 (t3a, i', t3b) in
        Split (left, k, right)
    end

let tree_insert i t =
  match insert_helper i t with
  | Ok t' -> t'
  | Split (t1, j, t2) -> Node2 (t1, j, t2)
