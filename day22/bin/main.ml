let () = print_endline "Hello, World!"

type coord = { x : int; y : int }

module Board = Map.Make (struct
  type t = coord

  let compare = compare
end)

let board, route =
  let ic = open_in "input" in
  let rec loop input y board route map =
    try
      let line = input_line input in
      match (String.length line, map) with
      | 0, _ -> loop input y board route false
      | _, true ->
          let chars = List.init (String.length line) (String.get line) in
          let _, updated = List.fold_left (fun (x, b) c -> if c <> ' ' then (x + 1, Board.add { x; y } c b) else (x + 1, b)) (1, board) chars in
          loop input (y + 1) updated route true
      | _, false ->
          let r = Str.full_split (Str.regexp "[RL]") line in
          loop input y board r false
    with End_of_file ->
      close_in input;
      (board, route)
  in
  loop ic 1 Board.empty [] true

let draw b pos =
  for y = 1 to 20 do
    for x = 1 to 20 do
      if Board.mem { x; y } b then
        let c = Board.find { x; y } b in
        if pos.x = x && pos.y = y then
        Printf.printf "*"
        else
        Printf.printf "%c" c
      else Printf.printf " "
    done;
    Printf.printf "\n"
  done;
  flush stdout

let () = draw board { x = 0; y = 0 }
let () = List.iter (fun (r : Str.split_result) -> match r with Delim d -> Printf.printf "%s," d | Text t -> Printf.printf "%i," (int_of_string t)) route
let () = Printf.printf "\n\n"
let directions = [| { x = 1; y = 0 }; { x = 0; y = 1 }; { x = -1; y = 0 }; { x = 0; y = -1 } |]
let board_min, _ = Board.min_binding board
let board_min = Board.fold (fun k _ acc -> { x = min acc.x k.x; y = min acc.y k.y }) board board_min
let board_max, _ = Board.max_binding board
let board_max = Board.fold (fun k _ acc -> { x = max acc.x k.x; y = max acc.y k.y }) board board_max

let (final_pos, final_dir) =
  List.fold_left
    (fun (pos, dir) (r : Str.split_result) ->
      match r with
      | Delim d ->
let () = Printf.printf "%s\n" d in let () = flush stdout in
if d = "R" then (pos, (dir + 1) mod 4) else (pos, (dir + 3) mod 4)
      | Text t ->
          let dist = int_of_string t in
let () = Printf.printf "dir = %i, %i\n" dir dist in let () = flush stdout in
let () = draw board pos in
          let rec loop p n =
            let rec move p =
              let new_pos = { x = p.x + directions.(dir).x; y = p.y + directions.(dir).y } in
              if Board.mem new_pos board then new_pos
              else
                let rec find_edge np = if Board.mem np board then np else
let () = Printf.printf "%i, %i\n" np.x np.y in
find_edge { x = np.x + directions.(dir).x; y = np.y + directions.(dir).y } in
                match dir with
                | 0 -> find_edge { x = board_min.x; y = new_pos.y }
                | 1 -> find_edge { x = new_pos.x; y = board_min.y }
                | 2 -> find_edge { x = board_max.x; y = new_pos.y }
                | 3 -> find_edge { x = new_pos.x; y = board_max.y }
                | _ -> assert false
            in
            let moved = move p in
            let cell = Board.find moved board in
            if cell = '#' then p else if n = 1 then moved else loop moved (n - 1)
          in
          (loop pos dist, dir))
    ({ x = 9; y = 1 }, 0)
    route

let () = Printf.printf "Final pos in (%i, %i) in dir %i = %i\n" final_pos.x final_pos.y final_dir (final_pos.y * 1000 + final_pos.x * 4 + final_dir)
