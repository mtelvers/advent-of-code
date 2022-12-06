type play = [ `Rock | `Paper | `Scissors ] 

let games =
  let ic = open_in "input" in
  let rec loop input =
    try
      let line = input_line input in
      let plays = String.split_on_char ' ' line in
      let play_list =
        List.map
          (fun x ->
            match x with
            | "A" | "X" -> `Rock
            | "B" | "Y" -> `Paper
            | "C" | "Z" -> `Scissors
            | _ -> assert false)
          plays
      in
      match play_list with
      | f :: s :: _ -> (f, s) :: loop input
      | _ -> loop input
    with End_of_file -> []
  in
  loop ic

let play (g : play * play) =
  match g with
  | `Rock, `Rock -> 1 + 3
  | `Rock, `Paper -> 2 + 6
  | `Rock, `Scissors -> 3 + 0
  | `Paper, `Rock -> 1 + 0
  | `Paper, `Paper -> 2 + 3
  | `Paper, `Scissors -> 3 + 6
  | `Scissors, `Rock -> 1 + 6
  | `Scissors, `Paper -> 2 + 0
  | `Scissors, `Scissors -> 3 + 3

let n = List.fold_left (fun i x -> i + x) 0 (List.map play games)
let () = Printf.printf "Part one %i\n" n


type outcome = [ `Win | `Lose | `Draw ]

let games =
  let ic = open_in "input" in
  let rec loop input =
    try
      let line = input_line input in
      let plays = String.split_on_char ' ' line in
      let opening =
        match List.nth plays 0 with
        | "A" -> `Rock
        | "B" -> `Paper
        | "C" -> `Scissors
        | _ -> assert false
      in
      let result =
        match List.nth plays 1 with
        | "X" -> `Lose
        | "Y" -> `Draw
        | "Z" -> `Win
        | _ -> assert false
      in
      (opening, result) :: loop input
    with End_of_file -> []
  in
  loop ic

let play (g : play * outcome) = 
  match g with
  | `Rock, `Win -> 6 + 2
  | `Rock, `Lose -> 0 + 3
  | `Rock, `Draw -> 3 + 1
  | `Paper, `Win -> 6 + 3
  | `Paper, `Lose -> 0 + 1
  | `Paper, `Draw -> 3 + 2
  | `Scissors, `Win -> 6 + 1
  | `Scissors, `Lose -> 0 + 2
  | `Scissors, `Draw -> 3 + 3

let n = List.fold_left (fun i x -> i + x) 0 (List.map play games)
let () = Printf.printf "Part two %i\n" n
