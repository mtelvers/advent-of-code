type coord = { x : int; y : int }

module Terrain = Map.Make (struct
  type t = coord

  let compare = compare
end)

let load () =
  let ic = open_in "input" in
  let rec loop input row terrain =
    try
      let line = input_line input in
      let chars = List.init (String.length line) (String.get line) in
      let indexed_chars =
        List.mapi
          (fun col ch ->
            let h =
              if ch = 'S' then (0, 0)
              else if ch = 'E' then (27, -1)
              else (int_of_char ch - int_of_char '`', -1)
            in
            (col, h))
          chars
      in
      let new_terrain =
        List.fold_left
          (fun f (col, height) -> Terrain.add { x = col; y = row } height f)
          terrain indexed_chars
      in
      loop input (row + 1) new_terrain
    with End_of_file ->
      close_in input;
      terrain
  in
  loop ic 0 Terrain.empty

let terrain = load ()

let draw t =
  for y = 0 to 45 do
    for x = 0 to 169 do
      if Terrain.mem { x; y } t then
        let h, d = Terrain.find { x; y } t in
        if d = -1 then Printf.printf "%c" (char_of_int (96 + h))
        else Printf.printf "%c" (char_of_int (65 + (d / 21)))
      else Printf.printf " "
    done;
    Printf.printf "\n"
  done;
  flush stdout

let find n =
  Terrain.fold
    (fun k (h, _) i -> if h = n then k else i)
    terrain { x = 0; y = 0 }

let the_start = find 0
let the_end = find 27
let () = Printf.printf "the_start (%i, %i)\n" the_start.x the_start.y
let () = Printf.printf "the_end (%i, %i)\n" the_end.x the_end.y
let () = draw terrain

let rank_neighbours t distance =
  let () = draw t in
  let all_at_distance =
    Terrain.bindings (Terrain.filter (fun _ (_, d) -> d = distance) t)
  in
  let neighbours =
    List.fold_left
      (fun acc (start, (start_height, _)) ->
        acc
        @ List.filter
            (fun query ->
              if Terrain.mem query t then
                let query_height, query_distance = Terrain.find query t in
                (start_height >= query_height || start_height + 1 = query_height)
                && query_distance = -1
              else false)
            [
              { x = start.x - 1; y = start.y };
              { x = start.x + 1; y = start.y };
              { x = start.x; y = start.y - 1 };
              { x = start.x; y = start.y + 1 };
            ])
      [] all_at_distance
  in
  List.fold_left
    (fun acc n ->
      let h, _ = Terrain.find n acc in
      Terrain.add n (h, distance + 1) acc)
    t neighbours

let rec update t n =
  let t2 = rank_neighbours t n in
  let _, steps = Terrain.find the_end t2 in
  if steps = -1 then update t2 (n + 1) else t2

let terrain = update terrain 0
let _, steps = Terrain.find the_end terrain
let () = Printf.printf "the_end (%i, %i) %i steps\n" the_end.x the_end.y steps
