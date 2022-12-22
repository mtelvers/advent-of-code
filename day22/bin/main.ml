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

(*
let draw b pos =
  for y = 1 to 20 do
    for x = 1 to 20 do
      if Board.mem { x; y } b then
        let c = Board.find { x; y } b in
        if pos.x = x && pos.y = y then Printf.printf "*" else Printf.printf "%c" c
      else Printf.printf " "
    done;
    Printf.printf "\n"
  done;
  flush stdout

let () = draw board { x = 0; y = 0 }
*)

let () = List.iter (fun (r : Str.split_result) -> match r with Delim d -> Printf.printf "%s," d | Text t -> Printf.printf "%i," (int_of_string t)) route
let () = Printf.printf "\n\n"
let directions = [| { x = 1; y = 0 }; { x = 0; y = 1 }; { x = -1; y = 0 }; { x = 0; y = -1 } |]
let board_min, _ = Board.min_binding board
let board_min = Board.fold (fun k _ acc -> { x = min acc.x k.x; y = min acc.y k.y }) board board_min
let board_max, _ = Board.max_binding board
let board_max = Board.fold (fun k _ acc -> { x = max acc.x k.x; y = max acc.y k.y }) board board_max

let final_pos, final_dir =
  List.fold_left
    (fun (pos, dir) (r : Str.split_result) ->
      match r with
      | Delim d ->
          if d = "R" then (pos, (dir + 1) mod 4) else (pos, (dir + 3) mod 4)
      | Text t ->
          let dist = int_of_string t in
          let rec loop p n =
            let move p =
              let new_pos = { x = p.x + directions.(dir).x; y = p.y + directions.(dir).y } in
              if Board.mem new_pos board then new_pos
              else
                let rec find_edge np =
                  if Board.mem np board then np
                  else
                    find_edge { x = np.x + directions.(dir).x; y = np.y + directions.(dir).y }
                in
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
    ({ x = 51; y = 1 }, 0)
    route

let () = Printf.printf "Final pos in (%i, %i) in dir %i = %i\n" final_pos.x final_pos.y final_dir ((final_pos.y * 1000) + (final_pos.x * 4) + final_dir)

let final_pos, final_dir =
  List.fold_left
    (fun (pos, dir) (r : Str.split_result) ->
      match r with
      | Delim d ->
          if d = "R" then (pos, (dir + 1) mod 4) else (pos, (dir + 3) mod 4)
      | Text t ->
          let dist = int_of_string t in
          let rec loop p d n =
            let move p d =
              let remap n r1 r2 r3 r4 = if r3 < r4 then n - r1 + r3 else r2 - n + r4 in
              let new_pos = { x = p.x + directions.(d).x; y = p.y + directions.(d).y } in
              if Board.mem new_pos board then (new_pos, d)
              else if
                (* Sample net
                                if new_pos.x = 13 && 1 <= new_pos.y && new_pos.y <= 4 then
                                  ({ x = 16; y = remap new_pos.y 1 4 12 9 }, 2) else
                                if new_pos.x = 13 && 5 <= new_pos.y && new_pos.y <= 8 then
                                  ({ x = remap new_pos.y 5 8 16 13; y = 9 }, 1) else
                                if new_pos.x = 17 && 9 <= new_pos.y && new_pos.y <= 12 then
                                  ({ x = 12; y = remap new_pos.y 9 12 4 1 }, 2) else

                                if new_pos.x = 8 && 1 <= new_pos.y && new_pos.y <= 4 then
                                  ({ x = remap new_pos.y 1 4 8 5; y = 5 }, 1) else
                                if new_pos.x = 0 && 5 <= new_pos.y && new_pos.y <= 8 then
                                  ({ x = remap new_pos.y 5 8 16 13; y = 12 }, 3) else
                                if new_pos.x = 8 && 9 <= new_pos.y && new_pos.y <= 12 then
                                  ({ x = remap new_pos.y 9 12 8 5; y = 8 }, 3) else

                                if new_pos.y = 4 && 1 <= new_pos.x && new_pos.x <= 4 then
                                  ({ x = remap new_pos.x 1 4 9 12; y = 1 }, 1) else
                                if new_pos.y = 4 && 5 <= new_pos.x && new_pos.x <= 8 then
                                  ({ x = 9; y = remap new_pos.x 5 8 1 4 }, 0) else
                                if new_pos.y = 0 && 9 <= new_pos.x && new_pos.x <= 12 then
                                  ({ x = remap new_pos.x 9 12 4 1; y = 5 }, 1) else
                                if new_pos.y = 8 && 13 <= new_pos.x && new_pos.x <= 16 then
                                  ({ x = 12; y = remap new_pos.x 13 16 8 5 }, 2) else

                                if new_pos.y = 9 && 1 <= new_pos.x && new_pos.x <= 4 then
                                  ({ x = remap new_pos.x 1 4 12 9; y = 12 }, 3) else
                                if new_pos.y = 9 && 5 <= new_pos.x && new_pos.x <= 8 then
                                  ({ x = 12; y = remap new_pos.x 5 8 12 9 }, 0) else
                                if new_pos.y = 13 && 9 <= new_pos.x && new_pos.x <= 12 then
                                  ({ x = remap new_pos.x 9 12 4 1; y = 8 }, 3) else
                                if new_pos.y = 13 && 13 <= new_pos.x && new_pos.x <= 16 then
                                  ({ x = 1; y = remap new_pos.x 13 16 8 5 }, 0) else
                *)
                new_pos.y = 0 && 51 <= new_pos.x && new_pos.x <= 100
              then (* A *)
                ({ x = 1; y = remap new_pos.x 51 100 151 200 }, 0)
              else if new_pos.x = 0 && 151 <= new_pos.y && new_pos.y <= 200 then (* A *)
                ({ x = remap new_pos.y 151 200 51 100; y = 1 }, 1)
              else if new_pos.y = 0 && 101 <= new_pos.x && new_pos.x <= 150 then (* B *)
                ({ x = remap new_pos.x 101 150 1 50; y = 200 }, 3)
              else if new_pos.y = 201 && 1 <= new_pos.x && new_pos.x <= 50 then (* B *)
                ({ x = remap new_pos.x 1 50 101 150; y = 1 }, 1)
              else if new_pos.y = 51 && 101 <= new_pos.x && new_pos.x <= 150 then (* C *)
                ({ x = 100; y = remap new_pos.x 101 150 51 100 }, 2)
              else if new_pos.x = 101 && 51 <= new_pos.y && new_pos.y <= 100 then (* C *)
                ({ x = remap new_pos.y 51 100 101 150; y = 50 }, 3)
              else if new_pos.x = 151 && 1 <= new_pos.y && new_pos.y <= 50 then (* D *)
                ({ x = 100; y = remap new_pos.y 1 50 150 101 }, 2)
              else if new_pos.x = 101 && 101 <= new_pos.y && new_pos.y <= 150 then (* D *)
                ({ x = 150; y = remap new_pos.y 101 150 50 1 }, 2)
              else if new_pos.y = 151 && 51 <= new_pos.x && new_pos.x <= 100 then (* E *)
                ({ x = 50; y = remap new_pos.x 51 100 151 200 }, 2)
              else if new_pos.x = 51 && 151 <= new_pos.y && new_pos.y <= 200 then (* E *)
                ({ x = remap new_pos.y 151 200 51 100; y = 150 }, 3)
              else if new_pos.x = 50 && 1 <= new_pos.y && new_pos.y <= 50 then (* F *)
                ({ x = 1; y = remap new_pos.y 1 50 150 101 }, 0)
              else if new_pos.x = 0 && 101 <= new_pos.y && new_pos.y <= 150 then (* F *)
                ({ x = 51; y = remap new_pos.y 101 150 50 1 }, 0)
              else if new_pos.x = 50 && 51 <= new_pos.y && new_pos.y <= 100 then (* G *)
                ({ x = remap new_pos.y 51 100 1 50; y = 101 }, 1)
              else if new_pos.y = 100 && 1 <= new_pos.x && new_pos.x <= 50 then (* G *)
                ({ x = 51; y = remap new_pos.x 1 50 51 100 }, 0)
              else assert false
            in
            let moved, directed = move p d in
            let cell = Board.find moved board in
            if cell = '#' then (p, d) else if n = 1 then (moved, directed) else loop moved directed (n - 1)
          in
          loop pos dir dist)
    ({ x = 51; y = 1 }, 0)
    route

let () = Printf.printf "Final pos in (%i, %i) in dir %i = %i\n" final_pos.x final_pos.y final_dir ((final_pos.y * 1000) + (final_pos.x * 4) + final_dir)
