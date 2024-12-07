open Prefix_tree_dict

(* let () =
   let tree = PrefixTree.init () in
   let tree = PrefixTree.insert "hello" 1 tree in
   let tree = PrefixTree.insert "hellol" 2 tree in
   let tree = PrefixTree.insert "helloo" 3 tree in
   let tree = PrefixTree.insert "world" 2 tree in
   Printf.printf "Tree after inserts: %s\n"
     (PrefixTree.to_string string_of_int tree 0);
   match PrefixTree.find "hello" tree with
   | Some v -> Printf.printf "Found value: %d\n" v
   | None ->
       Printf.printf "Key not found\n";
       let tree = PrefixTree.remove "hello" tree in
       Printf.printf "Tree after remove: %s\n"
         (PrefixTree.to_string string_of_int tree 0) *)

let () =
  let tree = PrefixTree.init () in

  let tree = PrefixTree.insert "hello" 42 tree in
  let tree = PrefixTree.insert "world" 99 tree in
  let tree = PrefixTree.insert "goodbye" 10 tree in
  Printf.printf "Tree after inserts: %s\n"
    (PrefixTree.to_string string_of_int tree 0);
  let filtered_tree = PrefixTree.filter_tree (fun x -> x > 20) tree in
  Printf.printf "Tree after filter: %s\n"
    (PrefixTree.to_string string_of_int filtered_tree 0);
  Printf.printf "Tree after filter: %s\n"
    (PrefixTree.to_string string_of_int tree 0)
