open OUnit2
open Prefix_tree_dict

let test_insert_find _ =
  let tree = PrefixTree.init () in
  let tree = PrefixTree.insert "hello" 42 tree in
  let tree = PrefixTree.insert "world" 99 tree in
  assert_equal (Some 42) (PrefixTree.find "hello" tree);
  assert_equal (Some 99) (PrefixTree.find "world" tree);
  assert_equal None (PrefixTree.find "notfound" tree)

let test_remove _ =
  let tree = PrefixTree.init () in
  let tree = PrefixTree.insert "hello" 42 tree in
  let tree = PrefixTree.insert "world" 99 tree in
  let tree = PrefixTree.remove "hello" tree in
  assert_equal None (PrefixTree.find "hello" tree);
  assert_equal (Some 99) (PrefixTree.find "world" tree)

let test_filter _ =
  let tree = PrefixTree.init () in
  let tree = PrefixTree.insert "goodbye" 10 tree in
  let tree = PrefixTree.insert "world" 99 tree in
  let tree = PrefixTree.insert "hello" 42 tree in
  (* Применяем фильтрацию: оставляем только те узлы, у которых значение больше 50 *)
  let filtered_tree = PrefixTree.filter_tree (fun x -> x > 50) tree in

  (* Проверяем, что остался только узел с значением 99 *)
  assert_equal (Some 99) (PrefixTree.find "world" filtered_tree);
  assert_equal None (PrefixTree.find "hello" filtered_tree);
  (* "hello" должно быть удалено *)
  assert_equal None (PrefixTree.find "goodbye" filtered_tree);

  (* "goodbye" должно быть удалено *)

  (* Проверим, что "notfound" не существует в дереве *)
  assert_equal None (PrefixTree.find "notfound" filtered_tree)

let test_map _ =
  let tree = PrefixTree.init () in
  let tree = PrefixTree.insert "hello" 42 tree in
  let tree = PrefixTree.insert "world" 99 tree in
  let mapped_tree = PrefixTree.map (fun x -> x * 2) tree in
  assert_equal (Some 84) (PrefixTree.find "hello" mapped_tree);
  assert_equal (Some 198) (PrefixTree.find "world" mapped_tree);
  assert_equal None (PrefixTree.find "notfound" mapped_tree)

let test_fold_left _ =
  let tree = PrefixTree.init () in
  let tree = PrefixTree.insert "hello" 42 tree in
  let tree = PrefixTree.insert "world" 99 tree in
  let result = PrefixTree.fold_left ( + ) 0 tree in
  assert_equal 141 result

let test_fold_right _ =
  let tree = PrefixTree.init () in
  let tree = PrefixTree.insert "hello" 42 tree in
  let tree = PrefixTree.insert "world" 99 tree in
  let result = PrefixTree.fold_right ( + ) tree 0 in
  assert_equal 141 result

let test_merge_empty _ =
  let empty_tree = PrefixTree.empty in
  let merged = PrefixTree.merge empty_tree empty_tree in
  assert_equal PrefixTree.empty merged

let test_merge_with_empty _ =
  let tree = PrefixTree.insert "a" 1 PrefixTree.empty in
  let empty_tree = PrefixTree.empty in
  let merged = PrefixTree.merge tree empty_tree in
  assert_equal tree merged

let test_merge_with_itself _ =
  let tree = PrefixTree.insert "a" 1 PrefixTree.empty in
  let merged = PrefixTree.merge tree PrefixTree.empty in
  (* Printf.printf "\n%s" (PrefixTree.to_string string_of_int tree 0);
     Printf.printf "\n%s" (PrefixTree.to_string string_of_int merged 0); *)
  assert_equal tree merged

let test_merge_different_keys _ =
  let tree1 = PrefixTree.insert "a" 1 PrefixTree.empty in
  let tree2 = PrefixTree.insert "b" 2 PrefixTree.empty in
  let merged = PrefixTree.merge tree1 tree2 in
  let expected =
    PrefixTree.insert "a" 1 (PrefixTree.insert "b" 2 PrefixTree.empty)
  in
  assert_equal expected merged

let test_merge_same_key _ =
  let tree1 = PrefixTree.insert "a" 1 PrefixTree.empty in
  let tree2 = PrefixTree.insert "a" 2 PrefixTree.empty in
  let merged = PrefixTree.merge tree1 tree2 in
  let expected = PrefixTree.insert "a" 1 PrefixTree.empty in
  assert_equal expected merged

let suite =
  "PrefixTree Tests"
  >::: [
         "test_insert_find" >:: test_insert_find;
         "test_remove" >:: test_remove;
         "test_filter" >:: test_filter;
         "test_fold_left" >:: test_fold_left;
         "test_fold_right" >:: test_fold_right;
         "test_map" >:: test_map;
         "test_merge_empty" >:: test_merge_empty;
         "test_merge_with_empty" >:: test_merge_with_empty;
         "test_merge_with_itself" >:: test_merge_with_itself;
         "test_merge_different_keys" >:: test_merge_different_keys;
         "test_merge_same_key" >:: test_merge_same_key;
       ]

let () = run_test_tt_main suite
