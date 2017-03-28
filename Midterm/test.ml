
type tree =
  | Leaf
  | Node2 of tree * int * tree   (* left tree, value, right tree *)
  | Node3 of tree * int * tree * int * tree
  (* left tree, left value, middle tree, right value, right tree *)

let tree7a =
  Node3 (Node3 (Leaf, 10, Leaf, 30, Leaf), 40, Node2 (Leaf, 50, Leaf), 60,
         Node3 (Leaf, 70, Leaf, 90, Leaf))
let tree7c =
  Node2
    (Node2 (Node3 (Leaf, 10, Leaf, 30, Leaf), 40, Node2 (Leaf, 50, Leaf)), 51,
     Node2 (Node2 (Leaf, 52, Leaf), 60, Node3 (Leaf, 70, Leaf, 90, Leaf)))
let tree7b =
  Node3 (Node3 (Leaf, 10, Leaf, 30, Leaf), 40,
         Node3 (Leaf, 50, Leaf, 52, Leaf), 60, Node3 (Leaf, 70, Leaf, 90, Leaf))
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

let tree7bguess = tree_insert 52 tree7a
let tree7cguess = tree_insert 51 tree7b
