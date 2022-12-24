let () = print_endline "Hello, World!"

type coord = { x : int; y : int }

module Board = Map.Make (struct
  type t = coord

  let compare = compare
end)

let blizzards, directions =
  let ic = open_in "input" in
  let rec loop input y b d =
    try
      let line = input_line input in
      let chars = List.init (String.length line) (String.get line) in
      let _, b2, d2 =
        List.fold_left
          (fun (x, b, d) c ->
            match c with
            | '>' -> (x + 1, { x; y } :: b, { x = 1; y = 0 } :: d)
            | '<' -> (x + 1, { x; y } :: b, { x = -1; y = 0 } :: d)
            | 'v' -> (x + 1, { x; y } :: b, { x = 0; y = 1 } :: d)
            | '^' -> (x + 1, { x; y } :: b, { x = 0; y = -1 } :: d)
            | '.' -> (x + 1, b, d)
            | '#' -> (x + 1, b, d)
            | _ -> assert false)
          (0, b, d) chars
      in
      loop input (y + 1) b2 d2
    with End_of_file ->
      close_in input;
      (b, d)
  in
  loop ic 0 [] []

let top_left = List.fold_left (fun acc b -> { x = min acc.x b.x; y = min acc.y b.y }) (List.hd blizzards) blizzards
let bottom_right = List.fold_left (fun acc b -> { x = max acc.x b.x; y = max acc.y b.y }) (List.hd blizzards) blizzards
let target = { x = bottom_right.x; y = bottom_right.y + 1 }
let origin = { x = 1; y = 0 }

let draw b d o =
  let board =
    List.fold_left2
      (fun acc b d ->
        if Board.mem b acc then
          let ioc = int_of_char (Board.find b acc) in
          let ch = if ioc > int_of_char '9' then int_of_char '2' else ioc + 1 in
          Board.add b (char_of_int ch) acc
        else Board.add b (match d with { x = 1; y = 0 } -> '>' | { x = -1; y = 0 } -> '<' | { x = 0; y = 1 } -> 'v' | { x = 0; y = -1 } -> '^' | _ -> '*') acc)
      Board.empty b d
  in
  let board = List.fold_left (fun acc op -> Board.add op 'E' acc) board o in
  let () = Printf.printf " 01234567\n" in
  for y = top_left.y - 1 to bottom_right.y + 1 do
    let () = Printf.printf "%i" y in
    for x = top_left.x - 1 to bottom_right.x + 1 do
      if Board.mem { x; y } board then
        let c = Board.find { x; y } board in
        Printf.printf "%c" c
      else Printf.printf "."
    done;
    Printf.printf "\n"
  done;
  flush stdout

let () = Printf.printf "top left %i %i\n" top_left.x top_left.y
let () = Printf.printf "bottom right %i %i\n" bottom_right.x bottom_right.y

let blow b d =
  List.fold_left2
    (fun acc b d ->
      let x = b.x + d.x in
      let y = b.y + d.y in
      acc
      @ [
          {
            x = (if x < top_left.x then bottom_right.x else if x > bottom_right.x then top_left.x else x);
            y = (if y < top_left.y then bottom_right.y else if y > bottom_right.y then top_left.y else y);
          };
        ])
    [] b d

let best = [| 900 |]

let rec simulate time blizzards directions expedition =
  let () = Printf.printf "%i %i\n" time best.(0) in
  let () = flush stdout in
  if expedition = target then
    if time < best.(0) then
      let _ = Array.set best 0 time in
      time
    else time
  else if time > best.(0) then time
  else
    (*  let () = draw blizzards directions [ expedition ] in *)
    let blizzards = blow blizzards directions in
    let board = List.fold_left (fun acc b -> Board.add b '#' acc) Board.empty blizzards in
    let options =
      List.map
        (fun vector -> { x = expedition.x + vector.x; y = expedition.y + vector.y })
        [
{ x = 1; y = 0 };
{ x = 0; y = 1 };
{ x = -1; y = 0 };
{ x = 0; y = -1 };
{ x = 0; y = 0 }
]
    in
    let options =
      List.filter
        (fun new_position ->
          let blizzard = Board.mem new_position board in
          let out_of_bounds =
            new_position.x < top_left.x || new_position.y < top_left.y || new_position.x > bottom_right.x || new_position.y > bottom_right.y
          in
          let the_end = new_position = target in
          let the_start = new_position = origin in
          ((not blizzard) && not out_of_bounds) || the_end || the_start)
        options
    in
    (*
    let () = List.iter (fun v -> Printf.printf "%i, %i\n" v.x v.y) options in
    let () = draw blizzards directions options in
*)
    List.fold_left (fun tt o -> min (simulate (time + 1) blizzards directions o) tt) (time + 1_000_000) options

let minimum_step = simulate 0 blizzards directions origin
let () = Printf.printf "%i\n" minimum_step
