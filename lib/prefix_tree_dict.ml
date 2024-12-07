module PrefixTree = struct
  type 'a trie =
    | Node of { value : 'a option; children : (char * 'a trie) list }

  let empty = Node { value = None; children = [] }
  let init () : 'a trie = empty

  let rec find_child ch children =
    match children with
    | [] -> None
    | (c, child) :: rest -> if c = ch then Some child else find_child ch rest

  let rec insert key value trie =
    match trie with
    | Node { value = current_value; children } ->
        if String.length key = 0 then Node { value = Some value; children }
        else
          let key_head = String.get key 0 in
          let key_tail = String.sub key 1 (String.length key - 1) in
          let updated_children =
            let child =
              match find_child key_head children with
              | None -> empty
              | Some child -> child
            in
            let updated_child = insert key_tail value child in
            (key_head, updated_child) :: List.remove_assoc key_head children
          in
          Node { value = current_value; children = updated_children }

  let rec find key trie =
    match trie with
    | Node { value; children } -> (
        if String.length key = 0 then value
        else
          let key_head = String.get key 0 in
          let key_tail = String.sub key 1 (String.length key - 1) in
          match find_child key_head children with
          | None -> None
          | Some child -> find key_tail child)

  let rec remove key trie =
    match trie with
    | Node { value; children } ->
        if String.length key = 0 then Node { value = None; children }
        else
          let key_head = String.get key 0 in
          let key_tail = String.sub key 1 (String.length key - 1) in
          let updated_children =
            match find_child key_head children with
            | None -> children
            | Some child ->
                let updated_child = remove key_tail child in
                if updated_child = empty then
                  List.remove_assoc key_head children
                else
                  (key_head, updated_child)
                  :: List.remove_assoc key_head children
          in
          Node { value; children = updated_children }

  let rec map f trie =
    match trie with
    | Node { value; children } ->
        let mapped_value =
          match value with Some v -> Some (f v) | None -> None
        in
        let mapped_children =
          children |> List.map (fun (ch, child) -> (ch, map f child))
        in
        Node { value = mapped_value; children = mapped_children }

  let rec fold_left f acc trie =
    match trie with
    | Node { value; children } ->
        let acc = match value with Some v -> f acc v | None -> acc in
        List.fold_left
          (fun acc (_, child) -> fold_left f acc child)
          acc children

  let rec fold_right f trie acc =
    match trie with
    | Node { value; children } -> (
        let acc =
          List.fold_right
            (fun (_, child) acc -> fold_right f child acc)
            children acc
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
          children
          |> List.map (fun (ch, child) ->
                 Printf.sprintf "\n%s%c -> %s" indent ch
                   (to_string string_of_value child (count + 1)))
          |> String.concat ";"
        in
        Printf.sprintf "{%s %s}" value_str children_str

  let rec filter_tree predicate trie =
    match trie with
    | Node { value; children } ->
        let filtered_value =
          match value with Some v when predicate v -> Some v | _ -> None
        in
        let filtered_children =
          children
          |> List.map (fun (ch, child) -> (ch, filter_tree predicate child))
          |> List.filter (fun (_, child) ->
                 match child with
                 | Node { value = Some _; _ } -> true
                 | Node { value = None; children = [] } -> false
                 | _ -> true)
        in
        if filtered_value <> None || filtered_children <> [] then
          Node { value = filtered_value; children = filtered_children }
        else Node { value = None; children = [] }

  let rec merge t1 t2 =
    match (t1, t2) with
    | Node { value = v1; children = c1 }, Node { value = v2; children = c2 } ->
        let merged_value =
          match (v1, v2) with
          | None, None -> None
          | Some v, None | None, Some v -> Some v
          | Some v1, Some _ -> Some v1
        in
        let all_keys =
          List.map fst c1 @ List.map fst c2 |> List.sort_uniq Char.compare
        in
        let merged_children =
          all_keys
          |> List.map (fun k ->
                 let child1 = find_child k c1 |> Option.value ~default:empty in
                 let child2 = find_child k c2 |> Option.value ~default:empty in
                 (k, merge child1 child2))
        in
        Node { value = merged_value; children = merged_children }

  let rec equal t1 t2 =
    match (t1, t2) with
    | Node { value = v1; children = c1 }, Node { value = v2; children = c2 } ->
        v1 = v2
        && List.length c1 = List.length c2
        && List.for_all2 (fun (k1, t1) (k2, t2) -> k1 = k2 && equal t1 t2) c1 c2
end
