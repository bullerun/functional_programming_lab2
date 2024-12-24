module type StringableKey = sig
  type t

  val to_string : t -> string
  val of_string : string -> t
end

module PrefixTree = struct
  module CharMap = Map.Make (Char)

  type 'a trie = Node of { value : 'a option; children : 'a trie CharMap.t }

  let empty = Node { value = None; children = CharMap.empty }
  let init () : 'a trie = empty
  let find_child ch children = CharMap.find_opt ch children

  let rec insert key idx value trie =
    match trie with
    | Node { value = current_value; children } ->
        if idx >= String.length key then Node { value = Some value; children }
        else
          let key_head = key.[idx] in
          let updated_child =
            match find_child key_head children with
            | None -> insert key (idx + 1) value empty
            | Some child -> insert key (idx + 1) value child
          in
          Node
            {
              value = current_value;
              children = CharMap.add key_head updated_child children;
            }

  let insert key value trie = insert key 0 value trie

  let rec find key idx trie =
    match trie with
    | Node { value; children } -> (
        if idx >= String.length key then value
        else
          let key_head = key.[idx] in
          match find_child key_head children with
          | None -> None
          | Some child -> find key (idx + 1) child)

  let find key trie = find key 0 trie

  let rec remove key idx trie =
    match trie with
    | Node { value; children } ->
        if idx >= String.length key then Node { value = None; children }
        else
          let key_head = key.[idx] in
          let updated_children =
            match find_child key_head children with
            | None -> children
            | Some child ->
                let updated_child = remove key (idx + 1) child in
                if updated_child = empty then CharMap.remove key_head children
                else CharMap.add key_head updated_child children
          in
          Node { value; children = updated_children }

  let remove key trie = remove key 0 trie

  let rec map f trie =
    match trie with
    | Node { value; children } ->
        let mapped_value = Option.map f value in
        let mapped_children = CharMap.map (map f) children in
        Node { value = mapped_value; children = mapped_children }

  let rec fold_left f acc trie =
    match trie with
    | Node { value; children } ->
        let acc = match value with Some v -> f acc v | None -> acc in
        CharMap.fold (fun _ child acc -> fold_left f acc child) children acc

  let rec fold_right f trie acc =
    match trie with
    | Node { value; children } -> (
        let acc =
          CharMap.fold (fun _ child acc -> fold_right f child acc) children acc
        in
        match value with Some v -> f v acc | None -> acc)

  let rec to_string string_of_value trie count =
    match trie with
    | Node { value; children } ->
        let indent = String.make count ' ' in
        let value_str =
          match value with
          | None -> ""
          | Some v -> Printf.sprintf "[%s]" (string_of_value v)
        in
        let children_str =
          CharMap.bindings children
          |> List.map (fun (ch, child) ->
                 Printf.sprintf "\n%s%c -> %s" indent ch
                   (to_string string_of_value child (count + 1)))
          |> String.concat ""
        in
        Printf.sprintf "{%s%s}" value_str children_str

  let rec filter_tree predicate trie =
    match trie with
    | Node { value; children } ->
        let filtered_value =
          match value with Some v when predicate v -> Some v | _ -> None
        in
        let filtered_children =
          CharMap.filter_map
            (fun _ child ->
              let filtered_child = filter_tree predicate child in
              match filtered_child with
              | Node { value = None; children } when CharMap.is_empty children
                ->
                  None
              | _ -> Some filtered_child)
            children
        in
        Node { value = filtered_value; children = filtered_children }

  let rec merge t1 t2 =
    match (t1, t2) with
    | Node { value = v1; children = c1 }, Node { value = v2; children = c2 } ->
        let merged_value =
          match (v1, v2) with
          | None, None -> None
          | Some v, None | None, Some v -> Some v
          | Some v1, Some _ -> Some v1
        in
        let merged_children =
          CharMap.merge
            (fun _ c1_opt c2_opt ->
              match (c1_opt, c2_opt) with
              | None, None -> None
              | Some c, None | None, Some c -> Some c
              | Some c1, Some c2 -> Some (merge c1 c2))
            c1 c2
        in
        Node { value = merged_value; children = merged_children }

  let rec equal t1 t2 =
    match (t1, t2) with
    | Node { value = v1; children = c1 }, Node { value = v2; children = c2 } ->
        v1 = v2 && CharMap.equal equal c1 c2
end

module Dict (Key : StringableKey) = struct
  module Trie = PrefixTree

  type 'a t = 'a Trie.trie

  let init () = Trie.init ()
  let empty = Trie.empty

  let insert key value dict =
    let key_str = Key.to_string key in
    Trie.insert key_str value dict

  let find key dict =
    let key_str = Key.to_string key in
    Trie.find key_str dict

  let remove key dict =
    let key_str = Key.to_string key in
    Trie.remove key_str dict

  let map f dict = Trie.map f dict
  let fold_left f acc dict = Trie.fold_left f acc dict
  let fold_right f dict acc = Trie.fold_right f dict acc
  let to_string string_of_value dict = Trie.to_string string_of_value dict 0
  let filter predicate dict = Trie.filter_tree predicate dict
  let merge dict1 dict2 = Trie.merge dict1 dict2
  let equal dict1 dict2 = Trie.equal dict1 dict2
end
