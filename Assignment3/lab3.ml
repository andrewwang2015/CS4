  (* A.1 *)

  type point = {x: float ; y: float}
  type segment = {startp: point; endp: point}

  let make_point x y = {x; y}
  let make_segment startp endp = {startp; endp}
  let get_coords {x; y} = (x, y)
  let get_points {startp; endp} = (startp, endp)
  let midpoint_segment {startp ; endp} = make_point ((startp.x +. endp.x) /. 2.)
      ((startp.y +. endp.y) /. 2.)

  let segment_length {startp; endp} =
    let x1 = startp.x -. endp.x in
    let y1 = startp.y -. endp.y in
    sqrt(x1 *. x1 +. y1 *. y1)

  let print_point {x; y} = Printf.printf "(%g,%g)" x y

  (* A.2 *)

  type rectangle = {lowerLeft: point; upperRight: point}
  let rectangle_lower_segment {lowerLeft; upperRight} =
    let downRightPoint = make_point upperRight.x lowerLeft.y in
    make_segment lowerLeft downRightPoint

  let rectangle_upper_segment {lowerLeft; upperRight} =
    let upperLeftPoint = make_point lowerLeft.x upperRight.y in
    make_segment upperLeftPoint upperRight

  let rectangle_left_segment {lowerLeft; upperRight} =
    let upperLeftPoint = make_point lowerLeft.x upperRight.y in
    make_segment lowerLeft upperLeftPoint

  let rectangle_right_segment {lowerLeft; upperRight} =
    let downRightPoint = make_point upperRight.x lowerLeft.y in
    make_segment downRightPoint upperRight

  let rectangle_perimeter r = segment_length (rectangle_lower_segment r) +. segment_length (rectangle_upper_segment r)
                              +. segment_length (rectangle_right_segment r) +. segment_length (rectangle_left_segment r)

  let rectangle_area r = segment_length (rectangle_left_segment r) *. segment_length (rectangle_lower_segment r)

  type rectangle2 = {xMin: float; xMax: float; yMin:float; yMax: float}
  let rectangle_lower_segment2 {xMin; xMax; yMin; yMax} =
    make_segment (make_point xMin yMin) (make_point xMax yMin)
  let rectangle_upper_segment2 {xMin; xMax; yMin; yMax} =
    make_segment (make_point xMin yMax) (make_point xMax yMax)
  let rectangle_left_segment2 {xMin; xMax; yMin; yMax} =
    make_segment (make_point xMin yMin) (make_point xMin yMax)
  let rectangle_right_segment2 {xMin; xMax; yMin; yMax} =
    make_segment (make_point xMax yMin) (make_point xMax yMax)

  let rectangle_perimeter2 r = segment_length (rectangle_lower_segment2 r) +. segment_length (rectangle_upper_segment2 r)
                               +. segment_length (rectangle_right_segment2 r) +. segment_length (rectangle_left_segment2 r)

  let rectangle_area2 r2 = segment_length (rectangle_left_segment2 r2) *. segment_length (rectangle_lower_segment2 r2)

  let make_rectangle lowerLeft upperRight = {lowerLeft; upperRight}
  let make_rectangle2 xMin xMax yMin yMax = {xMin; xMax; yMin; yMax}

  (* A.3 *)

  let make_pair x y = fun m -> m x y
  let first z = z (fun x y -> x)
  let second z = z (fun x y -> y)

  (*
  Evaluation first (make_pair x y):
    Evaluate make_pair x y:
      x -> x
       -> y
    Evaluate make_pair -> fun x y -> (fun m -> m x y)
      Apply fun x y -> (fun m -> m x y) to x, y
      Substitute x and y into (fun m -> m x y)
      -> Result: (fun m -> m x y)
    Evaluate first ->  fun z -> z (fun x y -> x)
      Apply fun z -> z (fun x y -> x) to (fun m -> m x y)
      Substitute (fun m -> m x y) for z in z (fun x y -> x)
      Evaluate (fun m -> m x y) (fun x y -> x):
      Apply (fun m -> m x y) to (fun x y -> x)
      Substitute (fun x y -> x) for m in m x y
    Evaluate (fun x y -> x) x y:
      x -> x
      y -> y
  Apply (fun x y -> x) to x y
  -> Result: x

  Evaluate second (make_pair 1 2):
    Evaluate make_pair 1 2:
      1 -> 1
      2 -> 2
    Evaluate make_pair -> fun x y -> (fun m -> m x y)
    Apply fun x y -> (fun m -> m x y) to 1, 2
    Substitute 1,2 for x, y in (fun m -> m x y)
      -> Result: (fun m -> m 1 2)
    Evaluate second -> fun z -> z (fun x y -> y)
    Apply fun z -> z (fun x y -> y) to (fun m -> m 1 2)
    Substitute (fun m -> m 1 2)  for z in z (fun x y -> y)
    Evaluate (fun m -> m 1 2) (fun x y -> y):
    Evaluate (fun x y -> y) -> (fun x y -> y)
    Evaluate (fun m -> m 1 2) -> (fun m -> m 1 2)
    Apply (fun m -> m 1 2) to (fun x y -> y)
  Substiute (fun x y -> y) for m in m 1 2
  -> Result: (fun x y -> y) 1 2
  Evaluate (fun x y -> y) 1 2:
    1 -> 1
    2 -> 2
  Apply (fun x y -> y) to 1, 2
  Substitute 1 for x, 2 for y in y
  -> Result: 2
  *)

  (* A.4 *)

  let pow x y =
    if y = 0 then 1 else
    let rec iter ans x y =
      if y = 1 then ans else iter (ans * x) x (y-1) in
    iter x x y

  let int_log x y =
    if y mod x <> 0 then 0
    else let rec iter num x y =
           if y mod x <> 0 || y = 1 then num
           else iter (num + 1) x (y/x) in
      iter 0 x y

  let make_pairi a b = (pow 2 a) * (pow 3 b)
  let firsti p = int_log 2 p
  let secondi p = int_log 3 p

  (* A.5 *)
  let zero = []

  let is_zero = function
    | [] -> true
    | () :: _ -> false

  let succ u = () :: u

  let prev u = match u with
    | [] -> invalid_arg "No unary integer less than zero"
    | ():: after -> after

  let integer_to_unary num =
    let rec iter ans num =
      if num = 0 then ans
      else iter (succ ans) (num-1)
    in iter zero num

  let unary_to_integer u =
    let rec iter u num =
      if u = zero then num
      else iter (prev u) (num+1) in
    iter u 0

  let unary_add a b =
    let rec iter ans b =
      if b = zero then ans
      else iter (succ ans) (prev b) in
    iter a b

  type nat = Zero | Succ of nat

  let zero' = Zero

  let is_zero' = function
    | Zero -> true
    | Succ _ -> false

  let succ' u = Succ u

  let prev' = function
    | Zero -> invalid_arg "No unary integer less than zero"
    | Succ after -> after

  let unary_to_integer' u =
    let rec iter' u num =
      if is_zero' u then num
      else iter' (prev' u) (num+1) in
    iter' u 0

  let integer_to_unary' num =
    let rec iter' ans num =
      if num = 0 then ans
      else iter' (succ' ans) (num-1)
    in iter' zero' num

  let unary_add' a b =
    let rec iter' ans b =
      if is_zero' b then ans
      else iter' (succ' ans) (prev' b) in
    iter' a b

  (* No, the other definitions do not have to change *)

  (* A.6 *)

  let zero = fun s -> fun z -> z
  let add1 n = fun s -> fun z -> s (n s z)

  let one = fun s -> fun z -> s z
  let two = fun s -> fun z -> s (s z)
  let three = fun s -> fun z -> s (s (s z))
  let four = fun s -> fun z -> s (s (s (s z)))
  let five = fun s -> fun z -> s (s (s (s (s z))))
  let six = fun s -> fun z -> s (s (s (s (s (s z)))))
  let seven = fun s -> fun z -> s (s (s (s (s (s (s z))))))
  let eight = fun s -> fun z -> s (s (s (s (s (s (s (s z)))))))
  let nine = fun s -> fun z -> s (s (s (s (s (s (s (s (s z))))))))
  let ten = fun s -> fun z -> s (s (s (s (s (s (s (s (s (s z)))))))))

  let add m n s z = m s (n s z)
  let church_to_integer n = n (fun x-> x + 1) (0)

  (* A.7 *)

  (* For zero:
     zero is of the form 'a ->'b ->'b while church_to_integer is of form int ->
     int -> int -> 'a -> 'a'. Thus 'a is an int-> int and 'b' is an int. Therefore,
     it must return a type of int

     For one:
     one is of the form ('a -> 'b) -> 'a -> 'b while church_to_integer is of form
     int -> int -> int -> 'a -> 'a'. Thus 'a -> 'b is of form int->int and 'a is an
     int and thus the return value is an int.
  *)

  (* B.1 *)
  let rec last_sublist = function
    | [] -> invalid_arg "last_sublist: empty list"
    | [h] -> [h]
    | h :: t -> last_sublist t

  (* B.2 *)
  let reverse lst =
    let rec iter lst tail =
      match tail with
      | [] -> lst
      | h :: t -> iter (h::lst) t
    in iter [] lst

  (* B.3 *)
  let rec square_list = function
    | [] -> []
    | h :: t -> h * h :: square_list t

  let square_list2 items = List.map (fun x -> x * x) items

  (* B.4 *)

  (* This does not work because as we go through the things list, we are updating
     the answer list by appending the square of the head item of things. If we do
     this step by step, answer will first get updated to be the square of the first item,
     but on the next iteration the square of the next item will be added before
     the square of the first item. From this, we see that it produces the desired
     list in reverse order *)

  (* In this case, Louis is trying ot use the :: operator to append lists to the left of
    the answers list. However, :: can only be used for element :: list, and to fix this
     we change h * h element to a list and can simply use the @ concatenation operator for lists .
  *)



  let square_list items =
    let rec iter things answer =
      match things with
      | [] -> answer
      | h :: t -> iter t (answer @ [h * h])
    in iter items []

  (* The resulting solution is not efficient. For each element we want to add,
     we need to create a new list of that element. In addition, the @ operator for
     appending lists is not efficient as we need to create a new list that is able
     to hold the number of items that both lists have. *)

  (* B.5.1 *)

  let count_negative_numbers lst =
    let rec iter lst num =
      match lst with
      | [] -> num
      | h:: t -> if h < 0 then iter t (num+1) else iter t num in
    iter lst 0

  (* B.5.2 *)

  let power_of_two_list n =
    if n = 0 then [] else
    let rec iter lst n =
      if n-1 = 0 then 1 :: lst
      else iter (pow 2 (n-1) :: lst) (n-1)
    in iter [] (n)

  (* B.5.3 *)

  let prefix_sum lst =
    let rec iter lst newLst sum =
      match lst with
      | [] -> newLst
      | [x] -> (sum+x) :: newLst
      | h::t -> iter t ((sum + h) :: newLst) (sum+h)
    in reverse(iter lst [] 0)

  (* B.6 *)
  let reverse lst =
    let rec iter lst tail =
      match tail with
      | [] -> lst
      | h :: t -> iter (h::lst) t
    in iter [] lst

  let rec deep_reverse lst =
    let rec iter lst newLst = match lst with
      | [] -> newLst
      | h :: t -> iter t (reverse h :: newLst) in
    iter lst []

  (* B.7 *)

  type 'a nested_list =
    | Value of 'a
    | List of 'a nested_list list



  let rec deep_reverse_nested lst =
    let reverse1 l =
      let rec iter lst newLst = match lst with
        | [] -> newLst
        | h :: t -> iter t ((deep_reverse_nested h) :: newLst)
      in iter l []
    in
    match lst with
    | Value x -> Value x
    | List l -> List (reverse1 l)
