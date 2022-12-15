type 'a node = One of 'a | Many of 'a node list

let to_node_list txt =
            let () = Printf.printf "\ntxt: %s\n" txt in
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
        | sub, rest ->
            let print x =
                  match (x : Str.split_result) with
                  | Delim d -> Printf.printf " %s " d
                  | Text t -> Printf.printf " %s " t in
            let () = Printf.printf "tl: " in
            let () = List.iter (print) tl in 
            let () = Printf.printf "\n" in
            let () = Printf.printf "sub: " in
            let () = List.iter (print) sub in
            let () = Printf.printf "\n" in
            let () = Printf.printf "rest: " in
            let () = List.iter (print) rest in
            let () = Printf.printf "\n\n" in

            Many (parse sub) :: parse rest)
    | Delim "]" :: _ -> assert false
    | _ -> []
  in
  parse (Str.full_split (Str.regexp "[],[]") txt)

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

let c le ri =
  let rec loop le ri =
    match (le, ri) with
    | One lh :: lt, One rh :: rt ->
        let () = Printf.printf "O , O " in
        let () = Printf.printf "%i , %i\n" lh rh in
        let r = compare lh rh in
        if r = 0 then loop lt rt else r
    | Many lh :: lt, Many rh :: rt ->
        let () = Printf.printf "M , M\n" in
        let r = loop lh rh in
        if r = 0 then loop lt rt else r
    | One lh :: lt, Many rh :: rt ->
        let () = Printf.printf "O , M " in
        let () = Printf.printf "%i\n" lh in
        let r = loop [ One lh ] rh in
        if r = 0 then loop lt rt else r
    | Many lh :: lt, One rh :: rt ->
        let () = Printf.printf "M , O " in
        let () = Printf.printf "%i\n" rh in
        let r = loop lh [ One rh ] in
        if r = 0 then loop lt rt else r
    | [], _ ->
        let () = Printf.printf "*true*\n" in
        -1
    | _, [] ->
        let () = Printf.printf "*false*\n" in
        1
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
      let () =
        Printf.printf "\nPair %i\n left: %s\n left: %s\nright: %s\nright: %s\n"
          pair left
          (from_node_list left_nodes)
          right
          (from_node_list right_nodes)
      in
      let result =
        if c left_nodes right_nodes < 0 then
          let () = Printf.printf "true\n" in
          pair
        else
          let () = Printf.printf "false\n" in
          0
      in
      let _ = input_line input in
      loop input (pair + 1) (acc + result)
    with End_of_file -> acc
  in
  loop ic 1 0

let () = Printf.printf "Total %i\n" comparisons
