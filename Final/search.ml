(* search.ml: search strategies *)
(* Student name:   Andrew Wang          *)
(* CMS cluster login name:   azwang   *)

module type Storage =
  sig
    type 'a t
    exception Empty

    val create : unit -> 'a t
    val push : 'a -> 'a t -> unit
    val pop : 'a t -> 'a
    val is_empty : 'a t -> bool
  end

module type Domain =
  sig
    type t
    val show : t -> string
    val is_solved : t -> bool
    val compare : t -> t -> int
    val next : t -> t list
  end

module Search (S : Storage) (D : Domain) =
  struct
    module DS = Set.Make(D)

  (* Returns a list of boards representing new histories to
   add to stack/queue *)

    let generateNewHistories nextOptions history visited =
      let rec iter options hist newHistories =
        match options with
        | [] -> newHistories
        | h::t -> if DS.mem h visited then iter t hist newHistories
          else iter t hist ((h::hist)::newHistories)
      in iter nextOptions history []

    (* Adds histories/list of boards to stack/queue... calls
    generateNewHistories *)

    let addHistoriesToStorage board history store visited =
      let historiesToAdd = generateNewHistories board history visited in
      let rec iter his stor t=
        match his with
        | [] -> stor
        | h:: t -> let x = S.push h stor in iter t stor x
      in iter historiesToAdd store ()

  (* Updates the set of visited boards so that all boards which are used in
     the updating of histories are put in the 'visited' set *)

    let addToVisited nextOptions visited =
      let rec iter options v =
        match options with
        | [] -> v
        | h::t -> iter t (DS.add h v)
      in iter nextOptions visited

    (* Main search function: Finds a solution using a BFS or DFS given
    an initial configuration of a Klotski board *)
    let search init =
      let initialStorage = S.create () in let x = S.push [init] initialStorage
      in let setSeen = DS.empty in let setSeen = DS. add init setSeen in
      let rec iter histories visited unit1 =
        match S.is_empty histories with
        | true -> raise Not_found
        | false -> let popped = S.pop histories in
          let first = List.hd popped in
          match D.is_solved first with
          | true -> popped
          | false ->  let next1 = D.next first in iter
              (addHistoriesToStorage next1 popped histories visited)
                       (addToVisited next1 visited) unit1
      in iter initialStorage setSeen x

    (* Outputs history of boards leading up to solution *)
    let show_history hist =
      (String.concat "\n----\n\n" (List.map D.show (List.rev hist))) ^ "\n"
  end

(* Thank you for the great course *)
