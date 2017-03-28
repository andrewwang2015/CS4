let generateNewHistories lst history =
  let rec iter options hist newHistories =
    match options with
    | [] -> newHistories
    | h::t -> iter t hist ((h::hist)::newHistories)
  in iter lst history []

let lst1 = [1;2;3]
let lst2 = [4;5]

let test lst1 lst2 =
  let options = generateNewHistories lst1 lst2 in
  let rec iter options newLst=
    match options with
    | [] -> newLst
    | h::t -> iter t (h::newLst)
in iter options []
