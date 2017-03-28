(* klotski.ml: core functionality of the Klotski game. *)
(* Student name:    Andrew Wang            *)
(* CMS cluster login name:    azwang  *)

(* ----------------------------------------------------------------------
 * Types.
 * ---------------------------------------------------------------------- *)

type loc = int * int
type dir = Up | Down | Left | Right
type move = char * dir * int

module LocM =
  struct
    type t = loc
    let compare = Pervasives.compare
  end

module LocSet : Set.S with type elt = loc = Set.Make(LocM)

(* Sets of LocSets.  Used locally only. *)

module LocSetM =
  struct
    type t = LocSet.t
    let compare = LocSet.compare
  end

module LocSetSet = Set.Make(LocSetM)

module CharM =
  struct
    type t = char
    let compare = Pervasives.compare
  end

module CharMap : Map.S with type key = char = Map.Make(CharM)

type piece = LocSet.t
type t = { pieces : piece CharMap.t ; unoccupied : LocSet.t }

(* ----------------------------------------------------------------------
 * Functions.
 * ---------------------------------------------------------------------- *)

(* Create a board from a string. *)
let read s =
  let rec iter p u r c =
    match () with
      | _ when r = 5 -> { pieces = p; unoccupied = u }
      | _ when c = 4 -> iter p u (r + 1) 0
      | _ ->
        let i = r * 4 + c in
        let ch = s.[i] in
          if ch = '.'  (* unoccupied location; add to unoccupied set *)
            then iter p (LocSet.add (r, c) u) r (c + 1)
            else  (* occupied; add to appropriate piece set *)
              try
                let cs  = CharMap.find ch p in     (* old piece set *)
                let cs' = LocSet.add (r, c) cs in  (* add new location *)
                let p'  = CharMap.add ch cs' p in  (* store back into map *)
                  iter p' u r (c + 1)
              with
                Not_found ->  (* new piece; create a new piece set *)
                  let cs = LocSet.singleton (r, c) in
                  let p' = CharMap.add ch cs p in
                    iter p' u r (c + 1)
  in
    if String.length s <> 20
      then failwith "read: invalid input string length"
      else iter CharMap.empty LocSet.empty 0 0

(* Convert the board to a string representation suitable for printing. *)
let show b =
  let string_of_char_list = function
    | [a;b;c;d] -> Printf.sprintf "%c%c%c%c" a b c d
    | _ -> failwith "invalid char list"
  in
  let char_at board loc =
    let rec iter = function
      | [] -> raise Not_found
      | (c, locs) :: t ->
        if LocSet.mem loc locs then c else iter t
    in
    if LocSet.mem loc board.unoccupied
      then '.'
      else iter (CharMap.bindings board.pieces)
  in
  (String.concat "\n"
     (List.map (fun r ->
        string_of_char_list
          (List.map (char_at b)
            (List.map (fun c -> (r, c)) [0; 1; 2; 3])))
        [0; 1; 2; 3; 4])) ^ "\n"

(* Tests to see if a given board is solved *)
let is_solved b =
  let solved = LocSet.of_list [(3,1); (3,2); (4,1); (4,2)] in
  CharMap.exists (fun c locs -> LocSet.equal solved locs) (b.pieces)


(* Helper function used to return a LocSetSet / Set of set of locations
   corresponding to occupancies of pieces in a board *)

let fillSet a =
  let rec iter a ultimateSt =
    match a with
    | [] -> ultimateSt
    | (c1, locs1) :: t1 -> iter t1 (LocSetSet.add locs1 ultimateSt)
  in iter a LocSetSet.empty

(* Compares two boards. Return 1 if first is larger than second, 0 if equal,
   and -1 if second is larger than first *)
let compare b1 b2 =
  let x = fillSet (CharMap.bindings b1.pieces) in
  let y = fillSet (CharMap.bindings b2.pieces) in
  LocSetSet.compare x y

(* Removes a piece from board *)
let remove c ({ pieces = p; unoccupied = u } as b) =
  try
    let currentBinding = CharMap.find c b.pieces in
    let x = LocSet.union currentBinding b.unoccupied in
    let y = CharMap.remove c b.pieces in
    { pieces = y; unoccupied = x }
  with Not_found -> b

(* Returns the difference between two sets *)
let returnDiff x y =
  let rec iter a b =
    match a with
    | [] -> b
    | h::t -> iter t (LocSet.remove h b)
  in iter x y

(* Places a piece on board if posisble *)
let add (c, p) { pieces = ps; unoccupied = u } =
  if CharMap.mem c ps || not (LocSet.subset p u)
  then None
  else let newPieces = CharMap.add c p ps in
    let newUnoc = returnDiff (LocSet.elements p) u in
    Some {pieces = newPieces; unoccupied = newUnoc}

(* Given a direction and set of locations for a piece, changes coordinates
   of locations to match the direction *)
let makeOneMove currentMoves d =
  match d with
  | Up -> LocSet.map (fun (a,b) -> (a-1, b)) currentMoves
  | Down -> LocSet.map (fun (a,b) -> (a+1, b)) currentMoves
  | Right -> LocSet.map (fun (a,b) -> (a, b+1)) currentMoves
  | Left -> LocSet.map (fun (a,b) -> (a, b-1)) currentMoves

(* Given a piece label, direction, and integer representing number of steps,
   make the move on the board if possible. *)
let make_move (c, d, i) b =
  if i < 1 || not(CharMap.mem c b.pieces) then None
  else
    let rec iter (c1,d1,i1) b1 =
      let moves = CharMap.find c1 b1.pieces in
      let boardWithRemoved = remove c1 b1 in
      match i1 with
      | 0 -> Some b1
      | _ -> let added = add (c1, makeOneMove moves d1) boardWithRemoved in
        match added with
        | None -> None
        | Some x -> iter (c1, d1, i1-1) x
    in iter (c, d, i) b

(* Gets all piece labels on board *)
let getAllChars b=
  let allBindings = CharMap.bindings b.pieces in
    let rec iter lst b = match b with
      | (c, locs) :: t -> iter (c::lst) t
      | [] -> lst
  in iter [] allBindings

(* Given a piece label, direction, number of steps, and a board,
   returns a list of all possible board configurations resulting from
   moving that piece from 1 to number of steps in the specific direction *)

let getPossibleForOneChar c direction numSteps board=
  let rec iter ch dir steps lst b =
    if steps = 0 then lst else
      match make_move(ch, dir, steps) b with
      | None -> iter ch dir (steps - 1) lst b
      | Some x -> iter c dir (steps -1) (x::lst) b
  in iter c direction numSteps [] board

(* Compiles all possible board configurations from moving a specific piece.
   It considers all directions and assumes that the maximum number of steps
   to move vertically is 4 and max. steps for horizontal movement is 3. This
   is true given that the smallest piece is 1x1 and the board is 5 x 4. *)

let getAllPossibleForOneChar c b =
  getPossibleForOneChar c Up 4 b @ getPossibleForOneChar c Down 4 b
  @ getPossibleForOneChar c Right 3 b @ getPossibleForOneChar c Left 3 b

(* Figures out al possible moves on board, makes moves starting
   from the board, adn collects all resulting boards into a list *)
let next b =
  let allLabels = getAllChars b in
  let rec iter finalLst labels board = match labels with
    | h::t -> iter (getAllPossibleForOneChar h board @ finalLst) t board
    | [] -> finalLst
  in iter [] allLabels b


(* Function to interactively test the "next" function.
 * Useful for debugging. *)
let test_next b =
  let bs = next b in
    begin
      print_string (show b ^ "\n");
      List.iter
        (fun b -> print_string ("----\n\n" ^ show b ^ "\n"))
        bs
    end
