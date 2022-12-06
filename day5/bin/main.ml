let stacks = Array.make 9 []

let load st =
  let ic = open_in "input" in
  let rec loop input =
    try
      let line = input_line input in
      let columns = List.init (String.length line) (String.get line) in
      let len = List.length columns in
      let rec cols n =
        let col = 1 + (4 * n) in
        if col < len then (
          let ch = List.nth columns col in
          match ch with
          | ' ' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
              cols (n + 1)
          | c ->
              st.(n) <- st.(n) @ [ c ];
              cols (n + 1))
        else loop input
      in
      if len > 0 then cols 0
    with End_of_file -> ()
  in
  let rec loop2 input =
    try
      let line = input_line input in
      let moves = String.split_on_char ' ' line in
      ( int_of_string (List.nth moves 1),
        int_of_string (List.nth moves 3),
        int_of_string (List.nth moves 5) )
      :: loop2 input
    with End_of_file ->
      close_in input;
      []
  in
  loop ic;
  loop2 ic

let moves = load stacks

let () =
  List.iter
    (fun (n, f, t) ->
      let rec mv n =
        let tmp = List.hd stacks.(f - 1) in
        let () = Array.set stacks (f - 1) (List.tl stacks.(f - 1)) in
        let () = Array.set stacks (t - 1) (tmp :: stacks.(t - 1)) in
        if n > 1 then mv (n - 1)
      in
      mv n)
    moves

let () = Printf.printf "\nPart one tops: "

let () =
  Array.iter
    (fun lst -> if List.length lst > 0 then Printf.printf "%c" (List.hd lst))
    stacks

let stacks = Array.make 9 []
let moves = load stacks

let () =
  List.iter
    (fun (n, f, t) ->
      let rec rm n lst =
        let tmp = List.hd stacks.(f - 1) in
        let () = Array.set stacks (f - 1) (List.tl stacks.(f - 1)) in
        if n > 1 then tmp :: rm (n - 1) lst else tmp :: lst
      in
      Array.set stacks (t - 1) (rm n [] @ stacks.(t - 1)))
    moves

let () = Printf.printf "\nPart two tops: "

let () =
  Array.iter
    (fun lst -> if List.length lst > 0 then Printf.printf "%c" (List.hd lst))
    stacks
