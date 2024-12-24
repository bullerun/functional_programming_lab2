open QCheck
open Prefix_tree_dict

module StringDict = Dict (struct
  type t = string

  let to_string key = key
  let of_string str = str
end)

(* Генератор деревьев *)
let gen_trie =
  let gen_entry = Gen.pair Gen.string Gen.int in
  let gen_trie_list =
    Gen.list gen_entry
    |> Gen.map
         (List.fold_left
            (fun acc (k, v) -> StringDict.insert k v acc)
            StringDict.empty)
  in
  QCheck.make gen_trie_list

(* Ассоциативность *)
let prop_monoid_assoc =
  Test.make ~name:"Monoid associativity" (triple gen_trie gen_trie gen_trie)
    (fun (t1, t2, t3) ->
      StringDict.equal
        (StringDict.merge (StringDict.merge t1 t2) t3)
        (StringDict.merge t1 (StringDict.merge t2 t3)))

(* Идентичность после map *)
let prop_map_id =
  Test.make ~name:"Map identity" gen_trie (fun trie ->
      let mapped = StringDict.map (fun x -> x) trie in
      StringDict.equal trie mapped)

(* fold_left и fold_right *)
let prop_fold_commutative =
  Test.make ~name:"Fold commutativity" gen_trie (fun trie ->
      let sum_left = StringDict.fold_left ( + ) 0 trie in
      let sum_right = StringDict.fold_right ( + ) trie 0 in
      sum_left = sum_right)

let () =
  let tests = [ prop_monoid_assoc; prop_map_id; prop_fold_commutative ] in
  QCheck_runner.run_tests_main tests
