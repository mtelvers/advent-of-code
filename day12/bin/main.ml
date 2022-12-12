type coord = { x : int; y : int }

module Terrain = Map.Make (struct
  type t = coord

  let compare = compare
end)

let terrain =
  let ic = open_in "input" in
  let rec loop input row terrain =
    try
      let line = input_line input in
      let chars = List.init (String.length line) (String.get line) in
      let indexed_chars =
        List.mapi
          (fun col ch ->
            let h =
              if ch = 'S' then 0
              else if ch = 'E' then 27
              else int_of_char ch - int_of_char '`'
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

let () =
  for y = 0 to 5 do
    for x = 0 to 8 do
      if Terrain.mem { x; y } terrain then
        Printf.printf "%2i:" (Terrain.find { x; y } terrain)
      else Printf.printf "  "
    done;
    Printf.printf "\n"
  done

let rec path route =
  let start = List.hd route in
  let compare_height p1 p2 = let h1 = Terrain.find p1 terrain in let h2 = Terrain.find p2 terrain in (compare h1 h2) in
  let choices =
    List.filter
      (fun query ->
        if Terrain.mem query terrain then
          let s = Terrain.find start terrain in
          let h = Terrain.find query terrain in
          (s = h || s + 1 = h) && not (List.mem query route)
        else false)
      [
        { x = start.x - 1; y = start.y };
        { x = start.x + 1; y = start.y };
        { x = start.x; y = start.y - 1 };
        { x = start.x; y = start.y + 1 };
      ]
  in
  if List.length choices = 0 then [ route ]
  else
   let sorted_choices = List.rev (List.sort compare_height choices) in
    (*  let () = List.iter (fun p -> Printf.printf "(%i,%i)\n" p.x p.y) choices in *)
    List.fold_left (fun acc t -> acc @ path (t :: route)) [] sorted_choices

let all_routes = path [ { x = 0; y = 0 } ]

let successful_routes =
  List.filter
    (fun route -> Terrain.find (List.hd route) terrain == 27)
    all_routes

let () =
  List.iteri
    (fun i s ->
      let () = Printf.printf "%i: " i in
      let () = List.iter (fun p -> Printf.printf "(%i,%i)" p.x p.y) s in
      Printf.printf "\n")
    successful_routes

let route_lengths = List.map (fun l -> List.length l - 1) successful_routes
let shortest_route = List.hd (List.sort compare route_lengths)
let () = Printf.printf "Shortest route is %i\n" shortest_route
