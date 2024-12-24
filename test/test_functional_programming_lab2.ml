open OUnit2
open Prefix_tree_dict

module StringDict = Dict (struct
  type t = string

  let to_string key = key
  let of_string str = str
end)

module IntDict = Dict (struct
  type t = int

  let to_string key = string_of_int key
  let of_string str = int_of_string str
end)

let test_insert_and_find _ =
  let dict = StringDict.init () in
  let dict = StringDict.insert "key1" 42 dict in
  let dict = StringDict.insert "key2" 84 dict in
  assert_equal (Some 42) (StringDict.find "key1" dict);
  assert_equal (Some 84) (StringDict.find "key2" dict);
  assert_equal None (StringDict.find "key3" dict)

let test_remove _ =
  let dict = StringDict.init () in
  let dict = StringDict.insert "key1" 42 dict in
  let dict = StringDict.insert "key2" 84 dict in
  let dict = StringDict.remove "key1" dict in
  assert_equal None (StringDict.find "key1" dict);
  assert_equal (Some 84) (StringDict.find "key2" dict)

let test_map _ =
  let dict = StringDict.init () in
  let dict = StringDict.insert "key1" 42 dict in
  let dict = StringDict.insert "key2" 84 dict in
  let dict = StringDict.map (( + ) 1) dict in
  assert_equal (Some 43) (StringDict.find "key1" dict);
  assert_equal (Some 85) (StringDict.find "key2" dict)

let test_fold _ =
  let dict = StringDict.init () in
  let dict = StringDict.insert "key1" 42 dict in
  let dict = StringDict.insert "key2" 84 dict in
  let sum = StringDict.fold_left ( + ) 0 dict in
  assert_equal 126 sum

let test_filter _ =
  let dict = StringDict.init () in
  let dict = StringDict.insert "key1" 42 dict in
  let dict = StringDict.insert "key2" 84 dict in
  let dict = StringDict.filter (fun v -> v > 50) dict in
  assert_equal None (StringDict.find "key1" dict);
  assert_equal (Some 84) (StringDict.find "key2" dict)

let test_merge _ =
  let dict1 =
    StringDict.init ()
    |> StringDict.insert "key1" 42
    |> StringDict.insert "key2" 84
  in
  let dict2 =
    StringDict.init ()
    |> StringDict.insert "key2" 100
    |> StringDict.insert "key3" 200
  in
  let merged = StringDict.merge dict1 dict2 in
  assert_equal (Some 42) (StringDict.find "key1" merged);
  assert_equal (Some 84) (StringDict.find "key2" merged);
  assert_equal (Some 200) (StringDict.find "key3" merged)

let test_merge_empty _ =
  let empty_tree = StringDict.init () in
  let merged = StringDict.merge empty_tree empty_tree in
  assert_bool "" (StringDict.equal StringDict.empty merged)

let test_merge_with_empty _ =
  let tree = StringDict.insert "a" 1 StringDict.empty in
  let empty_tree = StringDict.empty in
  let merged = StringDict.merge tree empty_tree in
  assert_bool "" (StringDict.equal tree merged)

let test_merge_with_itself _ =
  let tree = StringDict.insert "a" 1 StringDict.empty in
  let merged = StringDict.merge tree StringDict.empty in
  (* Printf.printf "\n%s" (PrefixTree.to_string string_of_int tree 0);
     Printf.printf "\n%s" (PrefixTree.to_string string_of_int merged 0); *)
  assert_bool "" (StringDict.equal tree merged)

let test_merge_different_keys _ =
  let tree1 = StringDict.insert "a" 1 StringDict.empty in
  let tree2 = StringDict.insert "b" 2 StringDict.empty in
  let merged = StringDict.merge tree1 tree2 in
  let expected =
    StringDict.init () |> StringDict.insert "a" 1 |> StringDict.insert "b" 2
  in
  assert_bool "" (StringDict.equal expected merged)

let test_non_string_key _ =
  let dict = IntDict.init () in
  let dict = IntDict.insert 1 42 dict in
  let dict = IntDict.insert 2 84 dict in
  assert_equal (Some 42) (IntDict.find 1 dict);
  assert_equal (Some 84) (IntDict.find 2 dict);
  assert_equal None (IntDict.find 3 dict);
  let dict = IntDict.remove 1 dict in
  assert_equal None (IntDict.find 1 dict);
  assert_equal (Some 84) (IntDict.find 2 dict)

let test_map_change_value_int_to_string _ =
  let target_dict =
    StringDict.init ()
    |> StringDict.insert "key1" "42"
    |> StringDict.insert "key2" "84"
  in
  let dict =
    StringDict.init ()
    |> StringDict.insert "key1" 42
    |> StringDict.insert "key2" 84
  in
  let out_dict = StringDict.map string_of_int dict in
  assert_bool "" (StringDict.equal target_dict out_dict)

let test_map_change_value_string_to_int _ =
  let target_dict =
    StringDict.init ()
    |> StringDict.insert "key1" 42
    |> StringDict.insert "key2" 52
    |> StringDict.insert "key3" 84
  in
  let dict =
    StringDict.init ()
    |> StringDict.insert "key1" "42"
    |> StringDict.insert "key2" "52"
    |> StringDict.insert "key3" "84"
  in
  let out_dict = StringDict.map int_of_string dict in
  assert_bool "" (StringDict.equal target_dict out_dict)

let suite =
  "PrefixTree Tests"
  >::: [
         "test_insert_and_find" >:: test_insert_and_find;
         "test_remove" >:: test_remove;
         "test_map" >:: test_map;
         "test_fold" >:: test_fold;
         "test_filter" >:: test_filter;
         "test_merge_empty" >:: test_merge_empty;
         "test_merge_with_empty" >:: test_merge_with_empty;
         "test_merge_different_keys" >:: test_merge_different_keys;
         "test_merge_with_itself" >:: test_merge_with_itself;
         "test_merge" >:: test_merge;
         "test_non_string_key" >:: test_non_string_key;
         "test_map_change_value_int_to_string"
         >:: test_map_change_value_int_to_string;
         "test_map_change_value_string_to_int"
         >:: test_map_change_value_string_to_int;
       ]

let () = run_test_tt_main suite
