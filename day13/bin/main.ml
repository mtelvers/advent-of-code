type 'a node = One of 'a | Many of 'a node list

let to_node_list txt =
  let rec parse (lst : Str.split_result list) =
    match lst with
    | Text t :: tl -> One (int_of_string t) :: parse tl
    | Delim "," :: tl -> parse tl
    | Delim "[" :: tl -> (
        let rec parselist n (left : Str.split_result list)
            (right : Str.split_result list) =
          match right with
          | Delim "]" :: tl ->
              if n = 1 then (left, tl)
              else parselist (n - 1) (left @ [ Delim "]" ]) tl
          | Delim "[" :: tl -> parselist (n + 1) (left @ [ Delim "[" ]) tl
          | hd :: tl -> parselist n (left @ [ hd ]) tl
          | [] -> (left, [])
        in
        match parselist 1 [] tl with
        | sub, rest -> Many (parse sub) :: parse rest)
    | Delim "]" :: _ -> assert false
    | _ -> []
  in
  parse (Str.full_split (Str.regexp "[],[]") txt)

(*
let from_node_list lst =
  let rec loop acc lst =
    List.fold_left
      (fun acc el ->
        match el with
        | One x -> acc ^ string_of_int x ^ ","
        | Many x -> acc ^ "[" ^ loop "" x ^ "],")
      acc lst
  in
  loop "" lst
*)

let c le ri =
  let rec loop le ri =
    match (le, ri) with
    | One lh :: lt, One rh :: rt ->
        let r = compare lh rh in
        if r = 0 then loop lt rt else r
    | Many lh :: lt, Many rh :: rt ->
        let r = loop lh rh in
        if r = 0 then loop lt rt else r
    | One lh :: lt, Many rh :: rt ->
        let r = loop [ One lh ] rh in
        if r = 0 then loop lt rt else r
    | Many lh :: lt, One rh :: rt ->
        let r = loop lh [ One rh ] in
        if r = 0 then loop lt rt else r
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
  in
  loop le ri

let comparisons =
  let ic = open_in "input" in
  let rec loop input pair acc =
    try
      let left = input_line input in
      let left_nodes = to_node_list left in
      let right = input_line input in
      let right_nodes = to_node_list right in
      let result = if c left_nodes right_nodes < 0 then pair else 0 in
      let _ = input_line input in
      loop input (pair + 1) (acc + result)
    with End_of_file -> acc
  in
  loop ic 1 0

let () = Printf.printf "Total %i\n" comparisons

let comparisons =
  let ic = open_in "input" in
  let rec loop input =
    try
      let line = input_line input in
      if String.length line = 0 then loop input
      else to_node_list line :: loop input
    with End_of_file -> []
  in
  loop ic

let () =
  let dividers =
    List.fold_left
      (fun acc txt -> to_node_list txt :: acc)
      [] [ "[[2]]"; "[[6]]" ]
  in
  let sorted = List.sort c (comparisons @ dividers) in
  let _, total =
    List.fold_left
      (fun (i, t) el ->
        if List.mem el dividers then (i + 1, t * i) else (i + 1, t))
      (1, 1) sorted
  in
  Printf.printf "Total %i\n" total
