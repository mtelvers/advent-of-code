let () = print_endline "Hello, World!"

let input_list =
  let ic = open_in "input" in
  let rec loop input =
    try
      let line = input_line input in
      let num = int_of_string line in
      num :: loop input
    with End_of_file ->
      close_in input;
      []
  in
  loop ic

let () = List.iter (Printf.printf "%i,") input_list
let () = Printf.printf "\n"

exception Failure of string

let rec find x lst =
  match lst with
  | [] -> raise (Failure "Not Found")
  | h :: t -> if x = h then 0 else 1 + find x t

let rec split_at n acc l =
  if n = 0 then (List.rev acc, l)
  else
    match l with
    | [] -> (List.rev acc, [])
    | h :: t -> split_at (n - 1) (h :: acc) t

let print_list str lst =
  let () = Printf.printf "%s: " str in
  let () = List.iter (Printf.printf "%i,") lst in
  Printf.printf "\n"

let output_list =
  List.fold_left
    (fun acc number ->
      let index = find number acc in
      let len = List.length acc in
      let num = number mod len in
      let num = num + number / len in
      let dir, delta =
        if index + num >= len then (`Left, index + num - len + 1)
        else if index + num < 0 then (`Right, len + num - 1)
        else
          ((if num = 0 then `Fixed else if num > 0 then `Right else `Left), index + num)
      in
      let () = Printf.printf "%i %i %i\n" num index delta in
      match dir with
      | `Fixed -> acc
      | `Left ->
          let left, right = split_at index [] acc in
          let () = print_list "left" left in
          let () = print_list "right" right in
          let left_left, left_right = split_at delta [] left in
          let () = print_list "left_left" left_left in
          let () = print_list "left_right" left_right in
          let result = left_left @ [ number ] @ left_right @ List.tl right in
          let () = print_list "result" result in
          result
      | `Right ->
          let left, right = split_at index [] acc in
          let () = print_list "left" left in
          let () = print_list "right" right in
          let right_left, right_right = split_at delta [] (List.tl right) in
          let () = print_list "right_left" right_left in
          let () = print_list "right_right" right_right in
          let result = left @ right_left @ [ number ] @ right_right in
          let () = print_list "result" result in
          result)
    input_list input_list

let () = print_list "result" output_list
let index_of_zero = find 0 output_list
let () = Printf.printf "%i\n" index_of_zero
let () = Printf.printf "Sum of 1000th (%i) + 2000th (%i) + 3000th (%i) = %i\n" 
(List.nth output_list ((1000 + index_of_zero) mod (List.length output_list)))
(List.nth output_list ((2000 + index_of_zero) mod (List.length output_list)))
(List.nth output_list ((3000 + index_of_zero) mod (List.length output_list)))
((List.nth output_list ((1000 + index_of_zero) mod (List.length output_list))) +
(List.nth output_list ((2000 + index_of_zero) mod (List.length output_list))) +
(List.nth output_list ((3000 + index_of_zero) mod (List.length output_list))))
