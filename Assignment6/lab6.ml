(* A.1 *)

(*

  FRAME 0 (initial environment)
    parent: none
    bindings:
      - : [primitive function -]
      * : [primitive function *]

  FUNCTION 0 (fun n -> let rec iter m r ...)
    env: FRAME 0
    param: n
    body: let rec iter m r ...

  FRAME 1 (let factorial n = FUNCTION 0 in ...)
      parent: FRAME 0
      bindings:
        factorial: FUNCTION 0

  FRAME 2 (FUNCTION 0 applied to 3)
      parent: FRAME 0
      bindings:
        n: 3

  FUNCTION 1 (fun m r -> if m ....)
    env: FRAME 2
    param: m r
    body: if m = 0 then r ...

  FRAME 3 (let rec iter m r = FUNCTION 1)
    parent: FRAME 0
    bindings:
      iter : FUNCTION 1

  FRAME 4 (FUNCTION 1 applied to 3 1)
    parent: FRAME 3
    bindings:
      m: 3
      r : 1

  FRAME 5 (FUNCTION 1 applied to 2 3)
  parent: FRAME 3
  bindings:
    m: 2
    r : 3


    FRAME 6 (FUNCTION 1 applied to 1 6)
    parent: FRAME 3
    bindings:
      m: 1
      r : 6

    FRAME 7 (FUNCTION 1 applied to 0 6)
    parent: FRAME 3
    bindings:
      m: 0
      r : 6

*)

(* A.2 *)

let factorial =
  let f = ref (fun n -> 0) in
  f:= begin function
    | 0 -> 1
    | x -> x * !f(x-1)
    end;
  !f

(* B.1 *)


exception Stat_error of string

let make_stat_1() = object
  val mutable sum = 0.
  val mutable sumsq = 0.
  val mutable n = 0

  method append value =
    sum <- sum +. value;
    sumsq <- sumsq +. (value *. value);
    n <- n + 1

  method mean = match n with
    | 0 -> raise (Stat_error "need at least one value for mean")
    | _ -> sum /. (float_of_int n)

  method variance = match n with
    | 0 -> raise (Stat_error "need at least one value for variance")
    | _ -> (sumsq -. sum *. sum /. float_of_int n) /. float_of_int n

  method stdev = match n with
    | 0 -> raise (Stat_error "need at least one value for stdev")
    | _ -> sqrt((sumsq -. sum *. sum /. float_of_int n) /. float_of_int n)

  method clear =
    sum <- 0.;
    sumsq <- 0.;
    n <- 0
end


(* B.2 *)
let make_stat_2() = object(self)
  val mutable sum = 0.
  val mutable sumsq = 0.
  val mutable n = 0

  method append value =
    sum <- sum +. value;
    sumsq <- sumsq +. (value *. value);
    n <- n + 1

  method private _variance =
    (sumsq -. sum *. sum /. float_of_int n) /. float_of_int n


  method mean = match n with
    | 0 -> raise (Stat_error "need at least one value for mean")
    | _ -> sum /. (float_of_int n)

  method variance = match n with
    | 0 -> raise (Stat_error "need at least one value for variance")
    | _ -> self#_variance

  method stdev = match n with
    | 0 -> raise (Stat_error "need at least one value for stdev")
    | _ -> sqrt(self#_variance)

  method clear =
    sum <- 0.;
    sumsq <- 0.;
    n <- 0
end

(* C.1 *)
module type PRIORITY_QUEUE =
  sig
    exception Empty

    type elem      (* Abstract type of elements of queue. *)
    type t         (* Abstract type of queue. *)

    val empty      : t                (* The empty queue.         *)
    val is_empty   : t -> bool        (* Check if queue is empty. *)
    val insert     : t -> elem -> t   (* Insert item into queue.  *)
    val find_min   : t -> elem        (* Return minimum element.  *)
    val delete_min : t -> t           (* Delete minimum element.  *)
    val from_list  : elem list -> t   (* Convert list to queue.   *)
  end

module PriorityQueue: (PRIORITY_QUEUE with type elem = int) =
struct
  exception Empty
  type elem = int

  type t =
    | Leaf
    | Node of elem * int * t * t

  let empty = Leaf
  let is_empty q = q = empty

  let merge_helper q1 q2 value = match (q1, q2) with
    | (Leaf, Leaf) -> Leaf
    | (Leaf, Node(v, r, left, right)) -> Node(value, 1, q2, Leaf)
    | (Node(v, r, left, right), Leaf) -> Node(value, 1, q1, Leaf)
    | (Node(v,r, left, right), Node(v1, r1, left1, right1)) when r <= r1
      -> Node(value, r+1, q2, q1)
    | (Node(v,r, left, right), Node(v1, r1, left1, right1))
      -> Node(value, r1+1, q1, q2)



let rec merge q1 q2 =
  match (q1, q2) with
  | (Leaf, Leaf) -> Leaf
  | (Leaf, _) -> q2
  | (_, Leaf) -> q1
  | (Node(v, r, left, right), Node(v1, r1, left1, right1)) when v <= v1
    -> merge_helper  left (merge q2 right) v
  | (Node(v, r, left, right), Node(v1, r1, left1, right1))
    -> merge_helper left1 (merge q1 right1) v1



let insert q element = merge q (Node(element, 0, empty, empty))

let rec find_min = function
  | Leaf -> raise Empty
  | Node (v, r, left, right) -> v

  let delete_min q = match q with
    | Leaf -> raise Empty
    | Node (v, r, left, right) -> merge left right

  let from_list lst =
    let rec iter q l = match l with
      | [] -> q
      | x :: t -> iter (insert q x) t
    in iter empty lst
end

let heap_sort lst =
  let initial = PriorityQueue.from_list lst in
  let rec iter l q =
    if PriorityQueue.is_empty q then l else
      iter (PriorityQueue.find_min q :: l) (PriorityQueue.delete_min q)
  in List.rev (iter [] initial)


(* C.2 *)

(* Type for ordered comparisons. *)
type comparison = LT | EQ | GT

(* Signature for ordered objects. *)
module type ORDERED =
  sig
    type t
    val cmp: t -> t -> comparison
  end




module MakePriorityQueue (Elt : ORDERED)
  : (PRIORITY_QUEUE with type elem = Elt.t) =
struct
  exception Empty
  type elem = Elt.t

  type t =
    | Leaf
    | Node of elem * int * t * t

  let empty = Leaf
  let is_empty q = q = empty


  let merge_helper q1 q2 value =  match (q1, q2) with
    | (Leaf, Leaf) -> Leaf
    | (Leaf, Node(v, r, left, right)) -> Node(value, 1, q2, Leaf)
    | (Node(v, r, left, right), Leaf) -> Node(value, 1, q1, Leaf)
    | (Node(v,r, left, right), Node(v1, r1, left1, right1)) when
        r <= r1
      -> Node(value, r+1, q2, q1)
    | (Node(v,r, left, right), Node(v1, r1, left1, right1))
      -> Node(value, r1+1, q1, q2)


  let rec merge q1 q2 =
    match (q1, q2) with
    | (Leaf, Leaf) -> Leaf
    | (Leaf, _) -> q2
    | (_, Leaf) -> q1
    | (Node(v, r, left, right), Node(v1, r1, left1, right1)) when
        Elt.cmp v v1 = LT || Elt.cmp v v1 = EQ
      -> merge_helper left (merge q2 right) v
    | (Node(v, r, left, right), Node(v1, r1, left1, right1))
      -> merge_helper left1 (merge q1 right1) v1



  let insert q element = merge q (Node(element, 0, empty, empty))

  let rec find_min = function
    | Leaf -> raise Empty
    | Node (v, r, left, right) -> v

    let delete_min q = match q with
      | Leaf -> raise Empty
      | Node (v, r, left, right) -> merge left right

  let from_list lst =
    let rec iter q l = match l with
      | [] -> q
      | x :: t -> iter (insert q x) t
    in iter empty lst
end

module OrderedString =
struct
  type t = string
  let cmp x y =
    if x = y then EQ else if x < y then LT else GT
end

module StringPQ = MakePriorityQueue(OrderedString)

let heap_sort_2 lst =
  let initial = StringPQ.from_list lst in
  let rec iter l q =
    if StringPQ.is_empty q then l else
      iter (StringPQ.find_min q :: l) (StringPQ.delete_min q)
  in List.rev (iter [] initial)
