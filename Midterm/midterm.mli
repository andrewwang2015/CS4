(* midterm.mli: interface file for the CS 4 midterm exam, 2017 *)

(* 2.1 *)

val split : int -> 'a list -> 'a list * 'a list
val groupN : int -> 'a list -> 'a list list

(* 2.2 *)

val even_and_odd_halves_rec : 'a list -> 'a list * 'a list
val even_and_odd_halves_iter : 'a list -> 'a list * 'a list
val merge_in_order : 'a list -> 'a list -> ('a -> 'a -> bool) -> 'a list
val merge_sort : 'a list -> ('a -> 'a -> bool) -> 'a list

(* 2.3 *)

val bubble : 'a list -> ('a -> 'a -> bool) -> 'a list
val bubble2 : 'a list -> ('a -> 'a -> bool) -> 'a list * bool
val bubble_sort : 'a list -> ('a -> 'a -> bool) -> 'a list

(* 3.1 *)

val zipWith : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list

(* 3.2 *)

val euler_solve_de :
  (float -> float -> float) -> float -> float -> float -> float

(* 3.3 *)

val iterative_improve : ('a -> bool) -> ('a -> 'a) -> 'a -> 'a
val average : float -> float -> float
val square : float -> float
val tolerance : float
val my_sqrt : float -> float

(* 4.1 *)

type tree =
    Leaf
  | Node2 of tree * int * tree
  | Node3 of tree * int * tree * int * tree

val tree_search : int -> tree -> bool

(* 4.2 *)

type insertion = Ok of tree | Split of tree * int * tree

val insert_helper : int -> tree -> insertion

val tree_insert : int -> tree -> tree

