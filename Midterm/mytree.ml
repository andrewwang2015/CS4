(* Convert a tree to a string in "dot" format. *)
let tree_to_string t =
  let sp = Printf.sprintf in
  let rec iter t i =
    match t with
      | Leaf -> (sp "  %d[shape=\"point\",width=0.2]\n" i, i + 1)
      | Node2 (t1, c, t2) ->
          let (t1s, j) = iter t1 (i + 1) in
          let (t2s, k) = iter t2 j in
          let curr =
            sp "  %d[label=\"%d\"]\n" i c ^
            sp "  %d -> %d\n" i (i + 1) ^
            sp "  %d -> %d\n" i j in
          let body = curr ^ t1s ^ t2s in
            (body, k)
      | Node3 (t1, c1, t2, c2, t3) ->
          let (t1s, j) = iter t1 (i + 1) in
          let (t2s, k) = iter t2 j in
          let (t3s, l) = iter t3 k in
          let curr =
            sp "  %d[label=\"%d %d\"]\n" i c1 c2 ^
            sp "  %d -> %d\n" i (i + 1) ^
            sp "  %d -> %d\n" i j ^
            sp "  %d -> %d\n" i k in
          let body = curr ^ t1s ^ t2s ^ t3s in
            (body, l)
  in
  let header = "digraph tree {\n" in
  let (body, _) = iter t 0 in
    header ^ body ^ "}\n"

(* Print a tree to a file in "dot" format. *)
let print_tree_to_file filename t =
  let outfile = open_out (filename ^ ".dot") in
    begin
      Printf.fprintf outfile "%s" (tree_to_string t);
      close_out outfile
    end
