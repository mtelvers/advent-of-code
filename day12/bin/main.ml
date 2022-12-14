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

let draw route =
  for y = 0 to 47 do
    for x = 0 to 169 do
      if Terrain.mem { x; y } terrain then
        if List.length (List.filter (fun r -> { x; y } = r) route) > 0 then
          Printf.printf "*"
        else
          Printf.printf "%c" (char_of_int (96 + Terrain.find { x; y } terrain))
      else Printf.printf " "
    done;
    Printf.printf "\n"
  done;
  flush stdout

let the_start, _ =
  List.split (Terrain.bindings (Terrain.filter (fun _ v -> v = 0) terrain))

let the_end =
  Terrain.fold (fun k v i -> if v = 27 then k else i) terrain { x = 0; y = 0 }

let () = Printf.printf "the_end (%i, %i)\n" the_end.x the_end.y

let rec path route next_height =
  let () = draw route in
  let start = List.hd route in
  if start = the_end then route else
  let distance p1 p2 =
    Float.sqrt
      ((float_of_int (p2.x - p1.x) ** 2.) +. (float_of_int (p2.y - p1.y) ** 2.))
  in
  let choices =
    List.filter
      (fun query ->
        if Terrain.mem query terrain then
          let s = Terrain.find start terrain in
          let h = Terrain.find query terrain in
          (((s > 15) && (s > h)) || s = h || s + 1 = h) && not (List.mem query route)
        else false)
      [
        { x = start.x - 1; y = start.y };
        { x = start.x + 1; y = start.y };
        { x = start.x; y = start.y - 1 };
        { x = start.x; y = start.y + 1 };
      ]
  in
  let len = List.length choices in
  if len = 0 then []
  else
    let all_targets = Terrain.filter (fun _ v -> v = next_height) terrain in
    let target_distance_to_the_end = Terrain.mapi (fun k _ -> distance the_end k) all_targets in
(*    let () = Terrain.iter (fun k v -> Printf.printf "(%i,%i) distance %f\n" k.x k.y v) target_distance_to_the_end in *)
    let _, nearest_the_end = Terrain.fold (fun k v (acc, res) -> if v < acc then (v, k) else (acc, res)) target_distance_to_the_end ((distance start the_end), the_end) in
(*    let () = Printf.printf "nearest %i square is (%i,%i)\n" next_height nearest_the_end.x nearest_the_end.y in *)
    let score_choices =
      List.map
        (fun x ->
          let h0 = Terrain.find start terrain in
          let h1 = Terrain.find x terrain in
          let h = if h1 > h0 then 1. else if h1 = h0 then 0. else float_of_int (h0 - h1) in
          (x, h +. (1. /. distance x nearest_the_end)))
        choices
    in
    let sorted_choices, _ =
      List.split
        (List.rev
           (List.sort
              (fun c1 c2 ->
                let _, s1 = c1 in
                let _, s2 = c2 in
                compare s1 s2)
              score_choices))
    in
    List.fold_left
      (fun acc t -> if List.length acc > 0 then acc else path (t :: route) (let h = Terrain.find start terrain in let h1 = Terrain.find t terrain in if h1 > h then (next_height + 1) else next_height))
      [] sorted_choices

let () = Printf.printf "length %i\n" (List.length the_start)
let a_route = path the_start 1

(*
let a_route =
  [
    { x = 138; y = 20 };
    { x = 139; y = 20 };
    { x = 140; y = 20 };
    { x = 140; y = 21 };
    { x = 140; y = 22 };
    { x = 140; y = 23 };
    { x = 139; y = 23 };
    { x = 139; y = 24 };
    { x = 139; y = 25 };
    { x = 139; y = 26 };
    { x = 140; y = 26 };
    { x = 141; y = 26 };
    { x = 142; y = 26 };
    { x = 142; y = 25 };
    { x = 141; y = 25 };
    { x = 141; y = 24 };
    { x = 142; y = 24 };
    { x = 143; y = 24 };
    { x = 143; y = 25 };
    { x = 144; y = 25 };
    { x = 145; y = 25 };
    { x = 146; y = 25 };
    { x = 146; y = 24 };
    { x = 147; y = 24 };
    { x = 147; y = 23 };
    { x = 147; y = 22 };
    { x = 146; y = 22 };
    { x = 146; y = 21 };
    { x = 146; y = 20 };
    { x = 147; y = 20 };
    { x = 147; y = 19 };
    { x = 147; y = 18 };
    { x = 147; y = 17 };
    { x = 147; y = 16 };
    { x = 147; y = 15 };
    { x = 146; y = 15 };
    { x = 146; y = 16 };
    { x = 145; y = 16 };
    { x = 145; y = 15 };
    { x = 144; y = 15 };
    { x = 144; y = 16 };
    { x = 143; y = 16 };
    { x = 143; y = 15 };
    { x = 142; y = 15 };
    { x = 142; y = 14 };
    { x = 141; y = 14 };
    { x = 141; y = 13 };
    { x = 140; y = 13 };
    { x = 139; y = 13 };
    { x = 138; y = 13 };
    { x = 138; y = 14 };
    { x = 139; y = 14 };
    { x = 139; y = 15 };
    { x = 138; y = 15 };
    { x = 137; y = 15 };
    { x = 137; y = 16 };
    { x = 138; y = 16 };
    { x = 138; y = 17 };
    { x = 137; y = 17 };
    { x = 136; y = 17 };
    { x = 136; y = 18 };
    { x = 135; y = 18 };
    { x = 134; y = 18 };
    { x = 134; y = 19 };
    { x = 134; y = 20 };
    { x = 134; y = 21 };
    { x = 134; y = 22 };
    { x = 135; y = 22 };
    { x = 135; y = 23 };
    { x = 136; y = 23 };
    { x = 137; y = 23 };
    { x = 137; y = 24 };
    { x = 137; y = 25 };
    { x = 137; y = 26 };
    { x = 137; y = 27 };
    { x = 137; y = 28 };
    { x = 138; y = 28 };
    { x = 138; y = 29 };
    { x = 139; y = 29 };
    { x = 140; y = 29 };
    { x = 140; y = 28 };
    { x = 141; y = 28 };
    { x = 142; y = 28 };
    { x = 142; y = 27 };
    { x = 143; y = 27 };
    { x = 144; y = 27 };
    { x = 145; y = 27 };
    { x = 146; y = 27 };
    { x = 147; y = 27 };
    { x = 148; y = 27 };
    { x = 148; y = 26 };
    { x = 149; y = 26 };
    { x = 149; y = 25 };
    { x = 149; y = 24 };
    { x = 149; y = 23 };
    { x = 149; y = 22 };
    { x = 148; y = 22 };
    { x = 148; y = 21 };
    { x = 149; y = 21 };
    { x = 149; y = 20 };
    { x = 150; y = 20 };
    { x = 150; y = 19 };
    { x = 150; y = 18 };
    { x = 150; y = 17 };
    { x = 150; y = 16 };
    { x = 150; y = 15 };
    { x = 149; y = 15 };
    { x = 149; y = 14 };
    { x = 148; y = 14 };
    { x = 148; y = 13 };
    { x = 147; y = 13 };
    { x = 147; y = 14 };
    { x = 146; y = 14 };
    { x = 146; y = 13 };
    { x = 146; y = 12 };
    { x = 145; y = 12 };
    { x = 145; y = 13 };
    { x = 145; y = 14 };
    { x = 144; y = 14 };
    { x = 144; y = 13 };
    { x = 143; y = 13 };
    { x = 143; y = 12 };
    { x = 143; y = 11 };
    { x = 142; y = 11 };
    { x = 141; y = 11 };
    { x = 141; y = 10 };
    { x = 140; y = 10 };
    { x = 140; y = 9 };
    { x = 139; y = 9 };
    { x = 138; y = 9 };
    { x = 137; y = 9 };
    { x = 137; y = 10 };
    { x = 138; y = 10 };
    { x = 139; y = 10 };
    { x = 139; y = 11 };
    { x = 138; y = 11 };
    { x = 137; y = 11 };
    { x = 136; y = 11 };
    { x = 136; y = 12 };
    { x = 137; y = 12 };
    { x = 137; y = 13 };
    { x = 136; y = 13 };
    { x = 136; y = 14 };
    { x = 136; y = 15 };
    { x = 135; y = 15 };
    { x = 135; y = 16 };
    { x = 134; y = 16 };
    { x = 133; y = 16 };
    { x = 133; y = 17 };
    { x = 132; y = 17 };
    { x = 131; y = 17 };
    { x = 131; y = 18 };
    { x = 131; y = 19 };
    { x = 131; y = 20 };
    { x = 131; y = 21 };
    { x = 131; y = 22 };
    { x = 131; y = 23 };
    { x = 131; y = 24 };
    { x = 132; y = 24 };
    { x = 132; y = 23 };
    { x = 133; y = 23 };
    { x = 133; y = 24 };
    { x = 134; y = 24 };
    { x = 134; y = 25 };
    { x = 134; y = 26 };
    { x = 135; y = 26 };
    { x = 135; y = 27 };
    { x = 135; y = 28 };
    { x = 135; y = 29 };
    { x = 135; y = 30 };
    { x = 135; y = 31 };
    { x = 136; y = 31 };
    { x = 137; y = 31 };
    { x = 138; y = 31 };
    { x = 139; y = 31 };
    { x = 140; y = 31 };
    { x = 141; y = 31 };
    { x = 142; y = 31 };
    { x = 142; y = 30 };
    { x = 143; y = 30 };
    { x = 143; y = 29 };
    { x = 144; y = 29 };
    { x = 144; y = 30 };
    { x = 145; y = 30 };
    { x = 146; y = 30 };
    { x = 147; y = 30 };
    { x = 148; y = 30 };
    { x = 149; y = 30 };
    { x = 150; y = 30 };
    { x = 150; y = 29 };
    { x = 151; y = 29 };
    { x = 151; y = 28 };
    { x = 152; y = 28 };
    { x = 152; y = 27 };
    { x = 152; y = 26 };
    { x = 152; y = 25 };
    { x = 152; y = 24 };
    { x = 151; y = 24 };
    { x = 151; y = 23 };
    { x = 151; y = 22 };
    { x = 151; y = 21 };
    { x = 151; y = 20 };
    { x = 152; y = 20 };
    { x = 152; y = 19 };
    { x = 152; y = 18 };
    { x = 153; y = 18 };
    { x = 153; y = 17 };
    { x = 153; y = 16 };
    { x = 153; y = 15 };
    { x = 153; y = 14 };
    { x = 152; y = 14 };
    { x = 152; y = 13 };
    { x = 151; y = 13 };
    { x = 151; y = 12 };
    { x = 151; y = 11 };
    { x = 150; y = 11 };
    { x = 150; y = 12 };
    { x = 149; y = 12 };
    { x = 149; y = 11 };
    { x = 149; y = 10 };
    { x = 148; y = 10 };
    { x = 148; y = 11 };
    { x = 148; y = 12 };
    { x = 147; y = 12 };
    { x = 147; y = 11 };
    { x = 146; y = 11 };
    { x = 145; y = 11 };
    { x = 145; y = 10 };
    { x = 144; y = 10 };
    { x = 144; y = 9 };
    { x = 143; y = 9 };
    { x = 143; y = 8 };
    { x = 142; y = 8 };
    { x = 141; y = 8 };
    { x = 141; y = 7 };
    { x = 140; y = 7 };
    { x = 139; y = 7 };
    { x = 138; y = 7 };
    { x = 138; y = 8 };
    { x = 137; y = 8 };
    { x = 136; y = 8 };
    { x = 136; y = 9 };
    { x = 136; y = 10 };
    { x = 135; y = 10 };
    { x = 135; y = 11 };
    { x = 134; y = 11 };
    { x = 133; y = 11 };
    { x = 133; y = 12 };
    { x = 134; y = 12 };
    { x = 134; y = 13 };
    { x = 133; y = 13 };
    { x = 133; y = 14 };
    { x = 132; y = 14 };
    { x = 131; y = 14 };
    { x = 131; y = 15 };
    { x = 130; y = 15 };
    { x = 129; y = 15 };
    { x = 128; y = 15 };
    { x = 128; y = 16 };
    { x = 128; y = 17 };
    { x = 128; y = 18 };
    { x = 128; y = 19 };
    { x = 128; y = 20 };
    { x = 128; y = 21 };
    { x = 128; y = 22 };
    { x = 128; y = 23 };
    { x = 129; y = 23 };
    { x = 129; y = 24 };
    { x = 130; y = 24 };
    { x = 130; y = 25 };
    { x = 131; y = 25 };
    { x = 131; y = 26 };
    { x = 132; y = 26 };
    { x = 132; y = 27 };
    { x = 132; y = 28 };
    { x = 132; y = 29 };
    { x = 132; y = 30 };
    { x = 133; y = 30 };
    { x = 133; y = 31 };
    { x = 133; y = 32 };
    { x = 134; y = 32 };
    { x = 134; y = 33 };
    { x = 134; y = 34 };
    { x = 135; y = 34 };
    { x = 136; y = 34 };
    { x = 137; y = 34 };
    { x = 138; y = 34 };
    { x = 139; y = 34 };
    { x = 140; y = 34 };
    { x = 141; y = 34 };
    { x = 141; y = 33 };
    { x = 142; y = 33 };
    { x = 143; y = 33 };
    { x = 143; y = 32 };
    { x = 144; y = 32 };
    { x = 145; y = 32 };
    { x = 146; y = 32 };
    { x = 147; y = 32 };
    { x = 148; y = 32 };
    { x = 149; y = 32 };
    { x = 150; y = 32 };
    { x = 151; y = 32 };
    { x = 152; y = 32 };
    { x = 152; y = 31 };
    { x = 153; y = 31 };
    { x = 153; y = 30 };
    { x = 154; y = 30 };
    { x = 154; y = 29 };
    { x = 154; y = 28 };
    { x = 154; y = 27 };
    { x = 154; y = 26 };
    { x = 154; y = 25 };
    { x = 154; y = 24 };
    { x = 154; y = 23 };
    { x = 154; y = 22 };
    { x = 154; y = 21 };
    { x = 155; y = 21 };
    { x = 155; y = 20 };
    { x = 155; y = 19 };
    { x = 155; y = 18 };
    { x = 155; y = 17 };
    { x = 156; y = 17 };
    { x = 156; y = 16 };
    { x = 156; y = 15 };
    { x = 156; y = 14 };
    { x = 156; y = 13 };
    { x = 155; y = 13 };
    { x = 155; y = 12 };
    { x = 155; y = 11 };
    { x = 154; y = 11 };
    { x = 153; y = 11 };
    { x = 153; y = 10 };
    { x = 153; y = 9 };
    { x = 153; y = 8 };
    { x = 152; y = 8 };
    { x = 152; y = 9 };
    { x = 152; y = 10 };
    { x = 151; y = 10 };
    { x = 151; y = 9 };
    { x = 150; y = 9 };
    { x = 150; y = 8 };
    { x = 151; y = 8 };
    { x = 151; y = 7 };
    { x = 151; y = 6 };
    { x = 152; y = 6 };
    { x = 153; y = 6 };
    { x = 154; y = 6 };
    { x = 155; y = 6 };
    { x = 156; y = 6 };
    { x = 157; y = 6 };
    { x = 158; y = 6 };
    { x = 158; y = 7 };
    { x = 158; y = 8 };
    { x = 158; y = 9 };
    { x = 159; y = 9 };
    { x = 159; y = 10 };
    { x = 159; y = 11 };
    { x = 159; y = 12 };
    { x = 160; y = 12 };
    { x = 160; y = 13 };
    { x = 160; y = 14 };
    { x = 160; y = 15 };
    { x = 160; y = 16 };
    { x = 160; y = 17 };
    { x = 161; y = 17 };
    { x = 161; y = 18 };
    { x = 161; y = 19 };
    { x = 161; y = 20 };
    { x = 161; y = 21 };
    { x = 161; y = 22 };
    { x = 161; y = 23 };
    { x = 161; y = 24 };
    { x = 161; y = 25 };
    { x = 161; y = 26 };
    { x = 161; y = 27 };
    { x = 160; y = 27 };
    { x = 160; y = 28 };
    { x = 160; y = 29 };
    { x = 159; y = 29 };
    { x = 159; y = 28 };
    { x = 159; y = 27 };
    { x = 159; y = 26 };
    { x = 160; y = 26 };
    { x = 160; y = 25 };
    { x = 160; y = 24 };
    { x = 160; y = 23 };
    { x = 160; y = 22 };
    { x = 160; y = 21 };
    { x = 160; y = 20 };
    { x = 160; y = 19 };
    { x = 160; y = 18 };
    { x = 159; y = 18 };
    { x = 159; y = 17 };
    { x = 159; y = 16 };
    { x = 159; y = 15 };
    { x = 159; y = 14 };
    { x = 159; y = 13 };
    { x = 158; y = 13 };
    { x = 158; y = 14 };
    { x = 158; y = 15 };
    { x = 158; y = 16 };
    { x = 158; y = 17 };
    { x = 158; y = 18 };
    { x = 158; y = 19 };
    { x = 159; y = 19 };
    { x = 159; y = 20 };
    { x = 159; y = 21 };
    { x = 159; y = 22 };
    { x = 159; y = 23 };
    { x = 159; y = 24 };
    { x = 159; y = 25 };
    { x = 158; y = 25 };
    { x = 158; y = 24 };
    { x = 158; y = 23 };
    { x = 158; y = 22 };
    { x = 158; y = 21 };
    { x = 158; y = 20 };
    { x = 157; y = 20 };
    { x = 157; y = 21 };
    { x = 157; y = 22 };
    { x = 157; y = 23 };
    { x = 157; y = 24 };
    { x = 157; y = 25 };
    { x = 157; y = 26 };
    { x = 158; y = 26 };
    { x = 158; y = 27 };
    { x = 158; y = 28 };
    { x = 158; y = 29 };
    { x = 158; y = 30 };
    { x = 159; y = 30 };
    { x = 159; y = 31 };
    { x = 159; y = 32 };
    { x = 159; y = 33 };
    { x = 158; y = 33 };
    { x = 158; y = 34 };
    { x = 158; y = 35 };
    { x = 158; y = 36 };
    { x = 157; y = 36 };
    { x = 157; y = 35 };
    { x = 157; y = 34 };
    { x = 157; y = 33 };
    { x = 156; y = 33 };
    { x = 156; y = 34 };
    { x = 156; y = 35 };
    { x = 156; y = 36 };
    { x = 156; y = 37 };
    { x = 156; y = 38 };
    { x = 156; y = 39 };
    { x = 155; y = 39 };
    { x = 154; y = 39 };
    { x = 153; y = 39 };
    { x = 152; y = 39 };
    { x = 151; y = 39 };
    { x = 150; y = 39 };
    { x = 149; y = 39 };
    { x = 149; y = 40 };
    { x = 148; y = 40 };
    { x = 147; y = 40 };
    { x = 146; y = 40 };
    { x = 145; y = 40 };
    { x = 144; y = 40 };
    { x = 143; y = 40 };
    { x = 143; y = 39 };
    { x = 144; y = 39 };
    { x = 145; y = 39 };
    { x = 146; y = 39 };
    { x = 147; y = 39 };
    { x = 148; y = 39 };
    { x = 148; y = 38 };
    { x = 149; y = 38 };
    { x = 150; y = 38 };
    { x = 151; y = 38 };
    { x = 152; y = 38 };
    { x = 153; y = 38 };
    { x = 154; y = 38 };
    { x = 155; y = 38 };
    { x = 155; y = 37 };
    { x = 155; y = 36 };
    { x = 155; y = 35 };
    { x = 154; y = 35 };
    { x = 154; y = 36 };
    { x = 154; y = 37 };
    { x = 153; y = 37 };
    { x = 152; y = 37 };
    { x = 151; y = 37 };
    { x = 150; y = 37 };
    { x = 149; y = 37 };
    { x = 149; y = 36 };
    { x = 150; y = 36 };
    { x = 151; y = 36 };
    { x = 152; y = 36 };
    { x = 153; y = 36 };
    { x = 153; y = 35 };
    { x = 153; y = 34 };
    { x = 152; y = 34 };
    { x = 152; y = 35 };
    { x = 151; y = 35 };
    { x = 150; y = 35 };
    { x = 149; y = 35 };
    { x = 148; y = 35 };
    { x = 148; y = 36 };
    { x = 148; y = 37 };
    { x = 147; y = 37 };
    { x = 147; y = 38 };
    { x = 146; y = 38 };
    { x = 145; y = 38 };
    { x = 144; y = 38 };
    { x = 143; y = 38 };
    { x = 143; y = 37 };
    { x = 142; y = 37 };
    { x = 142; y = 38 };
    { x = 142; y = 39 };
    { x = 141; y = 39 };
    { x = 141; y = 40 };
    { x = 140; y = 40 };
    { x = 139; y = 40 };
    { x = 138; y = 40 };
    { x = 137; y = 40 };
    { x = 137; y = 39 };
    { x = 136; y = 39 };
    { x = 135; y = 39 };
    { x = 134; y = 39 };
    { x = 133; y = 39 };
    { x = 132; y = 39 };
    { x = 132; y = 38 };
    { x = 131; y = 38 };
    { x = 131; y = 37 };
    { x = 130; y = 37 };
    { x = 130; y = 36 };
    { x = 130; y = 35 };
    { x = 131; y = 35 };
    { x = 131; y = 36 };
    { x = 132; y = 36 };
    { x = 132; y = 37 };
    { x = 133; y = 37 };
    { x = 133; y = 38 };
    { x = 134; y = 38 };
    { x = 135; y = 38 };
    { x = 136; y = 38 };
    { x = 137; y = 38 };
    { x = 137; y = 37 };
    { x = 136; y = 37 };
    { x = 135; y = 37 };
    { x = 134; y = 37 };
    { x = 134; y = 36 };
    { x = 133; y = 36 };
    { x = 133; y = 35 };
    { x = 132; y = 35 };
    { x = 132; y = 34 };
    { x = 131; y = 34 };
    { x = 131; y = 33 };
    { x = 131; y = 32 };
    { x = 130; y = 32 };
    { x = 130; y = 33 };
    { x = 130; y = 34 };
    { x = 129; y = 34 };
    { x = 129; y = 35 };
    { x = 129; y = 36 };
    { x = 128; y = 36 };
    { x = 127; y = 36 };
    { x = 126; y = 36 };
    { x = 125; y = 36 };
    { x = 124; y = 36 };
    { x = 123; y = 36 };
    { x = 123; y = 35 };
    { x = 123; y = 34 };
    { x = 123; y = 33 };
    { x = 123; y = 32 };
    { x = 123; y = 31 };
    { x = 122; y = 31 };
    { x = 122; y = 30 };
    { x = 122; y = 29 };
    { x = 122; y = 28 };
    { x = 122; y = 27 };
    { x = 123; y = 27 };
    { x = 123; y = 28 };
    { x = 123; y = 29 };
    { x = 123; y = 30 };
    { x = 124; y = 30 };
    { x = 124; y = 29 };
    { x = 124; y = 28 };
    { x = 124; y = 27 };
    { x = 125; y = 27 };
    { x = 125; y = 28 };
    { x = 125; y = 29 };
    { x = 125; y = 30 };
    { x = 126; y = 30 };
    { x = 126; y = 29 };
    { x = 126; y = 28 };
    { x = 126; y = 27 };
    { x = 127; y = 27 };
    { x = 127; y = 28 };
    { x = 128; y = 28 };
    { x = 128; y = 29 };
    { x = 128; y = 30 };
    { x = 129; y = 30 };
    { x = 130; y = 30 };
    { x = 130; y = 29 };
    { x = 129; y = 29 };
    { x = 129; y = 28 };
    { x = 129; y = 27 };
    { x = 128; y = 27 };
    { x = 128; y = 26 };
    { x = 127; y = 26 };
    { x = 126; y = 26 };
    { x = 125; y = 26 };
    { x = 124; y = 26 };
    { x = 123; y = 26 };
    { x = 122; y = 26 };
    { x = 121; y = 26 };
    { x = 120; y = 26 };
    { x = 119; y = 26 };
    { x = 118; y = 26 };
    { x = 117; y = 26 };
    { x = 116; y = 26 };
    { x = 115; y = 26 };
    { x = 114; y = 26 };
    { x = 113; y = 26 };
    { x = 112; y = 26 };
    { x = 112; y = 25 };
    { x = 112; y = 24 };
    { x = 112; y = 23 };
    { x = 111; y = 23 };
    { x = 111; y = 22 };
    { x = 111; y = 21 };
    { x = 111; y = 20 };
    { x = 110; y = 20 };
    { x = 109; y = 20 };
    { x = 108; y = 20 };
    { x = 107; y = 20 };
    { x = 106; y = 20 };
    { x = 105; y = 20 };
    { x = 104; y = 20 };
    { x = 103; y = 20 };
    { x = 102; y = 20 };
    { x = 101; y = 20 };
    { x = 100; y = 20 };
    { x = 99; y = 20 };
    { x = 98; y = 20 };
    { x = 97; y = 20 };
    { x = 96; y = 20 };
    { x = 95; y = 20 };
    { x = 94; y = 20 };
    { x = 93; y = 20 };
    { x = 93; y = 21 };
    { x = 92; y = 21 };
    { x = 91; y = 21 };
    { x = 90; y = 21 };
    { x = 90; y = 22 };
    { x = 90; y = 23 };
    { x = 91; y = 23 };
    { x = 91; y = 24 };
    { x = 90; y = 24 };
    { x = 90; y = 25 };
    { x = 90; y = 26 };
    { x = 91; y = 26 };
    { x = 92; y = 26 };
    { x = 93; y = 26 };
    { x = 93; y = 27 };
    { x = 92; y = 27 };
    { x = 91; y = 27 };
    { x = 90; y = 27 };
    { x = 89; y = 27 };
    { x = 88; y = 27 };
    { x = 87; y = 27 };
    { x = 86; y = 27 };
    { x = 85; y = 27 };
    { x = 84; y = 27 };
    { x = 83; y = 27 };
    { x = 82; y = 27 };
    { x = 81; y = 27 };
    { x = 80; y = 27 };
    { x = 80; y = 28 };
    { x = 80; y = 29 };
    { x = 80; y = 30 };
    { x = 80; y = 31 };
    { x = 80; y = 32 };
    { x = 79; y = 32 };
    { x = 78; y = 32 };
    { x = 77; y = 32 };
    { x = 76; y = 32 };
    { x = 75; y = 32 };
    { x = 74; y = 32 };
    { x = 73; y = 32 };
    { x = 72; y = 32 };
    { x = 71; y = 32 };
    { x = 70; y = 32 };
    { x = 69; y = 32 };
    { x = 69; y = 33 };
    { x = 69; y = 34 };
    { x = 68; y = 34 };
    { x = 67; y = 34 };
    { x = 66; y = 34 };
    { x = 65; y = 34 };
    { x = 64; y = 34 };
    { x = 63; y = 34 };
    { x = 62; y = 34 };
    { x = 61; y = 34 };
    { x = 60; y = 34 };
    { x = 59; y = 34 };
    { x = 59; y = 33 };
    { x = 59; y = 32 };
    { x = 58; y = 32 };
    { x = 57; y = 32 };
    { x = 56; y = 32 };
    { x = 55; y = 32 };
    { x = 54; y = 32 };
    { x = 53; y = 32 };
    { x = 53; y = 31 };
    { x = 52; y = 31 };
    { x = 51; y = 31 };
    { x = 50; y = 31 };
    { x = 49; y = 31 };
    { x = 48; y = 31 };
    { x = 47; y = 31 };
    { x = 47; y = 30 };
    { x = 46; y = 30 };
    { x = 45; y = 30 };
    { x = 44; y = 30 };
    { x = 43; y = 30 };
    { x = 42; y = 30 };
    { x = 42; y = 29 };
    { x = 41; y = 29 };
    { x = 40; y = 29 };
    { x = 40; y = 28 };
    { x = 39; y = 28 };
    { x = 39; y = 27 };
    { x = 39; y = 26 };
    { x = 38; y = 26 };
    { x = 38; y = 25 };
    { x = 37; y = 25 };
    { x = 37; y = 24 };
    { x = 37; y = 23 };
    { x = 37; y = 22 };
    { x = 37; y = 21 };
    { x = 38; y = 21 };
    { x = 38; y = 22 };
    { x = 38; y = 23 };
    { x = 38; y = 24 };
    { x = 39; y = 24 };
    { x = 39; y = 25 };
    { x = 40; y = 25 };
    { x = 40; y = 24 };
    { x = 40; y = 23 };
    { x = 39; y = 23 };
    { x = 39; y = 22 };
    { x = 39; y = 21 };
    { x = 40; y = 21 };
    { x = 40; y = 22 };
    { x = 41; y = 22 };
    { x = 41; y = 21 };
    { x = 41; y = 20 };
    { x = 40; y = 20 };
    { x = 39; y = 20 };
    { x = 38; y = 20 };
    { x = 37; y = 20 };
    { x = 36; y = 20 };
    { x = 35; y = 20 };
    { x = 34; y = 20 };
    { x = 33; y = 20 };
    { x = 32; y = 20 };
    { x = 31; y = 20 };
    { x = 31; y = 19 };
    { x = 31; y = 18 };
    { x = 30; y = 18 };
    { x = 29; y = 18 };
    { x = 28; y = 18 };
    { x = 28; y = 17 };
    { x = 27; y = 17 };
    { x = 26; y = 17 };
    { x = 25; y = 17 };
    { x = 24; y = 17 };
    { x = 23; y = 17 };
    { x = 22; y = 17 };
    { x = 21; y = 17 };
    { x = 20; y = 17 };
    { x = 19; y = 17 };
    { x = 18; y = 17 };
    { x = 17; y = 17 };
    { x = 16; y = 17 };
    { x = 15; y = 17 };
    { x = 14; y = 17 };
    { x = 13; y = 17 };
    { x = 13; y = 18 };
    { x = 13; y = 19 };
    { x = 13; y = 20 };
    { x = 13; y = 21 };
    { x = 12; y = 21 };
    { x = 11; y = 21 };
    { x = 11; y = 22 };
    { x = 11; y = 23 };
    { x = 10; y = 23 };
    { x = 9; y = 23 };
    { x = 8; y = 23 };
    { x = 7; y = 23 };
    { x = 6; y = 23 };
    { x = 5; y = 23 };
    { x = 4; y = 23 };
    { x = 3; y = 23 };
    { x = 2; y = 23 };
    { x = 2; y = 22 };
    { x = 2; y = 21 };
    { x = 1; y = 21 };
    { x = 0; y = 21 };
    { x = 0; y = 20 };
  ]
*)

let print route =
  let () = List.iter (fun p -> Printf.printf "(%i,%i)" p.x p.y) route in
  Printf.printf ": length %i\n" (List.length route)

let () = print a_route

let rec optimise route =
  match route with
  | hd :: tl ->
      let touches =
        List.fold_left
          (fun acc step ->
            let filtered =
              List.filter
                (fun neighbour ->
                  step = neighbour
                  && abs (Terrain.find hd terrain - Terrain.find neighbour terrain) <= 1)
                [
                  { x = hd.x - 1; y = hd.y };
                  { x = hd.x + 1; y = hd.y };
                  { x = hd.x; y = hd.y - 1 };
                  { x = hd.x; y = hd.y + 1 };
                ]
            in
            if List.length filtered > 0 then filtered @ acc
            else acc)
          [] tl
      in
      if List.length touches > 0 then
        let rec find x lst =
          match lst with
          | [] -> assert false
          | h :: t -> if x = h then 0 else 1 + find x t in
        let indicies = List.map (fun x -> find x tl) touches in
        let max_i = List.fold_left (fun acc x -> if x > acc then x else acc) 0 indicies in
        let new_tl = List.filteri (fun i _ -> i >= max_i) tl in
        hd :: optimise new_tl
      else hd :: optimise tl
  | [] -> []

let () = draw a_route
let optimised = optimise a_route
let () = Printf.printf "Now %i\n" (List.length optimised)
let () = draw optimised

