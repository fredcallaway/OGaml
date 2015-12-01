let list_max comp (lst : 'a list) : 'a = 
  let folder (best, best_val) elt =
    let elt_val = comp elt in
    if elt_val > best_val then elt, elt_val else best, best_val in
  match lst with 
  | [] -> failwith "empty list"
  | hd::tl -> List.fold_left folder (hd, comp hd) tl |> fst
