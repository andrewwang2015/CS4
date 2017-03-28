(* Tests for midterm.ml *)

open OUnit2
open Midterm

let assert_raises_failure msg x =
  assert_bool msg 
    (try 
       begin
          ignore (x ()); 
          false
       end
     with
       | Failure _ -> true
       | _ -> false)

(* Convert a list to a tree. *)
let tree_of_list lst =
  let rec iter tree rest =
    match rest with
      | [] -> tree
      | h :: t -> iter (tree_insert h tree) t
  in
    iter Leaf lst

(* Some sample 2-3 trees. *)

let tree0 = Leaf

let tree1 = Node2 (Leaf, 4, Leaf)

let tree2 = Node3 (Leaf, 4, Leaf, 6, Leaf)

let tree3 = 
  Node2 (Node2 (Leaf, 3, Leaf), 4, Node2 (Leaf, 6, Leaf))

let tree4 = 
  Node2 (Node2 (Leaf, 3, Leaf), 4, Node3 (Leaf, 5, Leaf, 6, Leaf))

let tree5 = 
  Node3 (Node2 (Leaf, 3, Leaf), 4, Node2 (Leaf, 5, Leaf), 6, Node2 (Leaf, 7, Leaf))

let tree6 =
  Node3 (Node2 (Leaf, 3, Leaf), 4, Node2 (Leaf, 5, Leaf), 6,
    Node3 (Leaf, 7, Leaf, 9, Leaf))

let tree7 =
  Node3 (Node3 (Leaf, 1, Leaf, 3, Leaf), 4, Node2 (Leaf, 5, Leaf), 6,
    Node3 (Leaf, 7, Leaf, 9, Leaf))

let tree7a =
  Node3 (Node3 (Leaf, 10, Leaf, 30, Leaf), 40, Node2 (Leaf, 50, Leaf), 60,
    Node3 (Leaf, 70, Leaf, 90, Leaf))

let tree7b = 
  Node3 (Node3 (Leaf, 10, Leaf, 30, Leaf), 40,
    Node3 (Leaf, 50, Leaf, 52, Leaf), 60, Node3 (Leaf, 70, Leaf, 90, Leaf))

let tree7c = 
  Node2
    (Node2 (Node3 (Leaf, 10, Leaf, 30, Leaf), 40, Node2 (Leaf, 50, Leaf)), 51,
       Node2 (Node2 (Leaf, 52, Leaf), 60, Node3 (Leaf, 70, Leaf, 90, Leaf)))

let tree8 =
  Node2 (Node2 (Node2 (Leaf, 1, Leaf), 2, Node2 (Leaf, 3, Leaf)), 4,
    Node2 (Node2 (Leaf, 5, Leaf), 6, Node3 (Leaf, 7, Leaf, 9, Leaf)))

let tree9 =
  Node2 (Node2 (Node2 (Leaf, 1, Leaf), 2, Node2 (Leaf, 3, Leaf)), 4,
    Node3 (Node2 (Leaf, 5, Leaf), 6, Node2 (Leaf, 7, Leaf), 9,
      Node2 (Leaf, 10, Leaf)))

let tree10 =
  Node2 (Node2 (Node2 (Leaf, 1, Leaf), 2, Node2 (Leaf, 3, Leaf)), 4,
    Node3 (Node2 (Leaf, 5, Leaf), 6, Node3 (Leaf, 7, Leaf, 8, Leaf), 9,
      Node2 (Leaf, 10, Leaf)))

let tree11 =
  Node2 (Node2 (Node2 (Leaf, 1, Leaf), 2, Node2 (Leaf, 3, Leaf)), 4,
    Node3 (Node2 (Leaf, 5, Leaf), 6, Node3 (Leaf, 7, Leaf, 8, Leaf), 9,
      Node3 (Leaf, 10, Leaf, 11, Leaf)))

let tree12 =
  Node3 (Node2 (Node2 (Leaf, 1, Leaf), 2, Node2 (Leaf, 3, Leaf)), 4,
    Node2 (Node2 (Leaf, 5, Leaf), 6, Node3 (Leaf, 7, Leaf, 8, Leaf)), 9,
      Node2 (Node2 (Leaf, 10, Leaf), 11, Node2 (Leaf, 12, Leaf)))

let test_tree_insert i t =
  match insert_helper i t with
    | Ok t' -> t'
    | Split (t1, j, t2) -> Node2 (t1, j, t2)


let all_tests = "all" >:::
[ 
  "Problem 2.1: split" >:: (fun c ->
    assert_equal 
      (split 4 [1; 2; 3; 4; 5; 6; 7; 8; 9; 10])
      ([1; 2; 3; 4], [5; 6; 7; 8; 9; 10]);

    assert_equal (split 0 [1; 2; 3]) ([], [1; 2; 3]);
    assert_equal (split 0 []) ([], []);
    assert_equal (split 1 [1; 2; 3]) ([1], [2; 3]);
    assert_equal (split 2 [1; 2; 3]) ([1; 2], [3]);
    assert_equal (split 3 [1; 2; 3]) ([1; 2; 3], []);
    assert_raises_failure "split" 
      (fun () -> split 4 [1; 2; 3]);
  );

  "Problem 2.1: groupN" >:: (fun c ->
    assert_equal (groupN 0 []) [];
    assert_equal (groupN 1 []) [];
    assert_equal (groupN 1 [1]) [[1]];
    assert_raises_failure "groupN" (fun () -> groupN 2 [1]);
    assert_equal (groupN 2 [1;2]) [[1; 2]];
    assert_raises_failure "groupN" (fun () -> groupN 2 [1; 2; 3]);
    assert_equal (groupN 2 [1;2;3;4]) [[1; 2]; [3; 4]];
    assert_equal (groupN 3 []) [];
    assert_equal (groupN 3 [1;2;3]) [[1; 2; 3]];
    assert_equal (groupN 3 [1;2;3;4;5;6]) [[1; 2; 3]; [4; 5; 6]];
    assert_equal 
      (groupN 3 [1;2;3;4;5;6;7;8;9]) 
      [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]];
    assert_raises_failure "groupN" (fun () -> groupN 3 [1;2;3;4;5;6;7;8])
  );

  "Problem 2.2: even_and_odd_halves_rec" >:: (fun c ->
    assert_equal(even_and_odd_halves_rec []) ([], []);
    assert_equal(even_and_odd_halves_rec [0]) ([0], []);
    assert_equal(even_and_odd_halves_rec [0;1]) ([0], [1]);
    assert_equal(even_and_odd_halves_rec [0;1;2]) ([0;2], [1]);
    assert_equal(even_and_odd_halves_rec [0;1;2;3]) ([0;2], [1;3]);
    assert_equal(even_and_odd_halves_rec [0;1;2;3;4]) ([0;2;4], [1;3]);
    assert_equal(even_and_odd_halves_rec [0;1;2;3;4;5]) ([0;2;4], [1;3;5]);
    assert_equal(even_and_odd_halves_rec [0;1;2;3;4;5;6]) ([0;2;4;6], [1;3;5]);
  );

  "Problem 2.2: even_and_odd_halves_iter" >:: (fun c ->
    assert_equal(even_and_odd_halves_iter []) ([], []);
    assert_equal(even_and_odd_halves_iter [0]) ([0], []);
    assert_equal(even_and_odd_halves_iter [0;1]) ([0], [1]);
    assert_equal(even_and_odd_halves_iter [0;1;2]) ([2;0], [1]);
    assert_equal(even_and_odd_halves_iter [0;1;2;3]) ([2;0], [3;1]);
    assert_equal(even_and_odd_halves_iter [0;1;2;3;4]) ([4;2;0], [3;1]);
    assert_equal(even_and_odd_halves_iter [0;1;2;3;4;5]) ([4;2;0], [5;3;1]);
    assert_equal(even_and_odd_halves_iter [0;1;2;3;4;5;6]) ([6;4;2;0], [5;3;1]);
  );

  "Problem 2.2: merge_sort" >:: (fun c ->
     assert_equal (merge_sort [] (<)) [];
     assert_equal (merge_sort [5;4;3;2;1] (<)) [1;2;3;4;5];
     assert_equal (merge_sort [5;4;3;2;1;2;3;4;5] (<)) [1;2;2;3;3;4;4;5;5];
  );

  "Problem 2.3: bubble" >:: (fun c ->
    assert_equal (bubble [] (<=)) [];
    assert_equal (bubble [1] (<=)) [1];
    assert_equal (bubble [1; 2] (<=)) [1; 2];
    assert_equal (bubble [1; 1] (<=)) [1; 1];
    assert_equal (bubble [1; 2; 3] (<=)) [1; 2; 3];
    assert_equal (bubble [2; 1] (<=)) [1; 2];
    assert_equal (bubble [3; 1; 2] (<=))  [1; 2; 3];
    assert_equal (bubble [3; 2; 1] (<=))  [2; 1; 3];
    assert_equal (bubble [5; 1; 3; 2; 4] (<=))  [1; 3; 2; 4; 5];
    assert_equal (bubble [1; 3; 2; 4; 5] (<=))  [1; 2; 3; 4; 5];
    assert_equal (bubble [5; 4; 3; 2; 1] (<=))  [4; 3; 2; 1; 5];
    assert_equal (bubble [4; 3; 2; 1; 5] (<=))  [3; 2; 1; 4; 5];
    assert_equal (bubble [3; 2; 1; 4; 5] (<=))  [2; 1; 3; 4; 5];
    assert_equal (bubble [2; 1; 3; 4; 5] (<=))  [1; 2; 3; 4; 5];
    assert_equal (bubble [1; 2; 3; 4; 5] (<=))  [1; 2; 3; 4; 5];
  );

  "Problem 2.3: bubble2" >:: (fun c ->
    assert_equal (bubble2 [] (<=)) ([], false);
    assert_equal (bubble2 [1] (<=)) ([1], false);
    assert_equal (bubble2 [1; 2] (<=)) ([1; 2], false);
    assert_equal (bubble2 [1; 2; 3] (<=)) ([1; 2; 3], false);
    assert_equal (bubble2 [2; 1] (<=)) ([1; 2], true);
    assert_equal (bubble2 [3; 1; 2] (<=)) ([1; 2; 3], true);
    assert_equal (bubble2 [3; 2; 1] (<=)) ([2; 1; 3], true);
    assert_equal (bubble2 [1; 3; 2; 4; 5] (<=)) ([1; 2; 3; 4; 5], true);
    assert_equal (bubble2 [5; 1; 3; 2; 4] (<=)) ([1; 3; 2; 4; 5], true);
    assert_equal (bubble2 [1; 3; 2; 4; 5] (<=)) ([1; 2; 3; 4; 5], true);
    assert_equal (bubble2 [5; 4; 3; 2; 1] (<=)) ([4; 3; 2; 1; 5], true);
    assert_equal (bubble2 [4; 3; 2; 1; 5] (<=)) ([3; 2; 1; 4; 5], true);
    assert_equal (bubble2 [3; 2; 1; 4; 5] (<=)) ([2; 1; 3; 4; 5], true);
    assert_equal (bubble2 [2; 1; 3; 4; 5] (<=)) ([1; 2; 3; 4; 5], true);
    assert_equal (bubble2 [1; 2; 3; 4; 5] (<=)) ([1; 2; 3; 4; 5], false);
    assert_equal (bubble2 [1; 1] (<=)) ([1; 1], false);
    assert_equal 
      (bubble2 [1; 1; 1; 1; 1; 1; 1] (<=)) 
      ([1; 1; 1; 1; 1; 1; 1], false);
    assert_equal 
      (bubble2 [1; 1; 2; 2; 3; 3; 4; 4; 5; 5] (<=)) 
      ([1; 1; 2; 2; 3; 3; 4; 4; 5; 5], false);
  );

  "Problem 2.3: bubble_sort" >:: (fun c ->
    assert_equal (bubble_sort [] (<=)) [];
    assert_equal (bubble_sort [1] (<=)) [1];
    assert_equal (bubble_sort [1; 2] (<=)) [1; 2];
    assert_equal (bubble_sort [2; 1] (<=)) [1; 2];
    assert_equal (bubble_sort [5; 1; 2; 3; 4] (<=)) [1; 2; 3; 4; 5];
    assert_equal (bubble_sort [5; 1; 3; 2; 4] (<=)) [1; 2; 3; 4; 5];
    assert_equal (bubble_sort [5; 4; 3; 2; 1] (<=)) [1; 2; 3; 4; 5];
    assert_equal 
      (bubble_sort [5; 1; 3; 2; 3; 4; 2; 1; 5] (<=)) 
      [1; 1; 2; 2; 3; 3; 4; 5; 5];
    assert_equal (bubble_sort [1; 1] (<=)) [1; 1];
    assert_equal 
      (bubble_sort [1; 1; 1; 1; 1; 1; 1] (<=)) 
      [1; 1; 1; 1; 1; 1; 1];
    assert_equal 
      (bubble_sort [1; 2; 3; 4; 5; 4; 3; 2; 1] (<=)) 
      [1; 1; 2; 2; 3; 3; 4; 4; 5];
  );

  "Problem 3.1: zipWith" >:: (fun c ->
    assert_equal (zipWith (fun x y -> (x, y)) [] []) [];
    assert_equal (zipWith (fun x y -> (x, y)) [] [1; 2; 3]) [];
    assert_equal (zipWith (fun x y -> (x, y)) [1; 2; 3] []) [];
    assert_equal 
      (zipWith (fun x y -> (x, y)) [1; 2; 3] [4; 5; 6]) 
      [(1, 4); (2, 5); (3, 6)];
    assert_equal (zipWith ( * ) [1; 2; 3] [4; 5; 6]) [4; 10; 18];
    assert_equal (zipWith ( * ) [1; 2; 3] [4; 5; 6; 7]) [4; 10; 18];
    assert_equal (zipWith ( * ) [1; 2; 3; 4] [4; 5; 6]) [4; 10; 18];
  );
  
  "Problem 3.2: euler_solve_de" >:: (fun c ->
     let g x fx = fx in
     let f = euler_solve_de g 1.0 in
     let eprox = f 1.0e-6 1.0 in
     let diff = abs_float (exp 1.0 -. eprox) in
       assert_bool "euler_solve_de" (diff < 0.00001)
  );

  "Problem 3.3: iterative_improve" >:: (fun c ->
     let diff x = abs_float (sqrt x -. my_sqrt x) in
     let tol = 0.000001 in
       begin
         assert_bool "iterative_improve 1" (diff 2.0 < tol);
         assert_bool "iterative_improve 2" (diff 3.0 < tol);
         assert_bool "iterative_improve 3" (diff 5.0 < tol)
       end
  );

  "Problem 4.1: tree_search" >:: (fun c ->
    assert_bool "test0" (not (tree_search 4 tree0));

    assert_bool "test1" (tree_search 4 tree1);

    assert_bool "test2" (tree_search 4 tree2);
    assert_bool "test2" (tree_search 6 tree2);

    assert_bool "test3" (tree_search 4 tree3);
    assert_bool "test3" (tree_search 6 tree3);
    assert_bool "test3" (tree_search 3 tree3);

    assert_bool "test4" (tree_search 4 tree4);
    assert_bool "test4" (tree_search 6 tree4);
    assert_bool "test4" (tree_search 3 tree4);
    assert_bool "test4" (tree_search 5 tree4);

    assert_bool "test5" (tree_search 4 tree5);
    assert_bool "test5" (tree_search 6 tree5);
    assert_bool "test5" (tree_search 3 tree5);
    assert_bool "test5" (tree_search 5 tree5);
    assert_bool "test5" (tree_search 7 tree5);

    assert_bool "test6" (tree_search 4 tree6);
    assert_bool "test6" (tree_search 6 tree6);
    assert_bool "test6" (tree_search 3 tree6);
    assert_bool "test6" (tree_search 5 tree6);
    assert_bool "test6" (tree_search 7 tree6);
    assert_bool "test6" (tree_search 9 tree6);

    assert_bool "test7" (tree_search 4 tree7);
    assert_bool "test7" (tree_search 6 tree7);
    assert_bool "test7" (tree_search 3 tree7);
    assert_bool "test7" (tree_search 5 tree7);
    assert_bool "test7" (tree_search 7 tree7);
    assert_bool "test7" (tree_search 9 tree7);
    assert_bool "test7" (tree_search 1 tree7);

    assert_bool "test8" (tree_search 4 tree8);
    assert_bool "test8" (tree_search 6 tree8);
    assert_bool "test8" (tree_search 3 tree8);
    assert_bool "test8" (tree_search 5 tree8);
    assert_bool "test8" (tree_search 7 tree8);
    assert_bool "test8" (tree_search 9 tree8);
    assert_bool "test8" (tree_search 1 tree8);
    assert_bool "test8" (tree_search 2 tree8);

    assert_bool "test9" (tree_search 4 tree9);
    assert_bool "test9" (tree_search 6 tree9);
    assert_bool "test9" (tree_search 3 tree9);
    assert_bool "test9" (tree_search 5 tree9);
    assert_bool "test9" (tree_search 7 tree9);
    assert_bool "test9" (tree_search 9 tree9);
    assert_bool "test9" (tree_search 1 tree9);
    assert_bool "test9" (tree_search 2 tree9);
    assert_bool "test9" (tree_search 10 tree9);

    assert_bool "test10" (tree_search 4 tree10);
    assert_bool "test10" (tree_search 6 tree10);
    assert_bool "test10" (tree_search 3 tree10);
    assert_bool "test10" (tree_search 5 tree10);
    assert_bool "test10" (tree_search 7 tree10);
    assert_bool "test10" (tree_search 9 tree10);
    assert_bool "test10" (tree_search 1 tree10);
    assert_bool "test10" (tree_search 2 tree10);
    assert_bool "test10" (tree_search 10 tree10);
    assert_bool "test10" (tree_search 8 tree10);
  );

  "Problem 4.2: insert_helper" >:: (fun c ->
    assert_equal (test_tree_insert 4 tree0) tree1;
    assert_equal (test_tree_insert 6 tree1) tree2;
    assert_equal (test_tree_insert 3 tree2) tree3;
    assert_equal (test_tree_insert 5 tree3) tree4;
    assert_equal (test_tree_insert 7 tree4) tree5;
    assert_equal (test_tree_insert 9 tree5) tree6;
    assert_equal (test_tree_insert 1 tree6) tree7;
    assert_equal (test_tree_insert 2 tree7) tree8;
    assert_equal (test_tree_insert 10 tree8) tree9;
    assert_equal (test_tree_insert 8 tree9) tree10;
    assert_equal (test_tree_insert 11 tree10) tree11;
    assert_equal (test_tree_insert 12 tree11) tree12;
    assert_equal (test_tree_insert 52 tree7a) tree7b;
    assert_equal (test_tree_insert 51 tree7b) tree7c;
  );

]

let _ = run_test_tt_main all_tests

