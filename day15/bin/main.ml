type coord = { x : int; y : int }

module Cave = Map.Make (struct
  type t = coord

  let compare = compare
end)

let row = 2_000_000

let data =
  let ic = open_in "input" in
  let rec loop input lst =
    try
      let line = input_line input in
      let tkn = Str.split (Str.regexp "[ =:,]+") line in
      let tkn =
        List.filter
          (fun test ->
            try
              let _ = int_of_string test in
              true
            with Failure _ -> false)
          tkn
      in
      let tkn = List.map int_of_string tkn in
      let r =
        match tkn with
        | sensor_x :: sensor_y :: beacon_x :: beacon_y :: _ ->
            ({ x = sensor_x; y = sensor_y }, { x = beacon_x; y = beacon_y })
        | _ -> assert false
      in
      r :: loop input lst
    with End_of_file ->
      close_in input;
      lst
  in
  loop ic []

let draw c =
  for y = -5 to 30 do
    Printf.printf "%2i: " y;
    for x = -5 to 30 do
      if Cave.mem { x; y } c then Printf.printf "%c" (Cave.find { x; y } c)
      else Printf.printf " "
    done;
    Printf.printf "\n"
  done;
  flush stdout

let cave =
  List.fold_left
    (fun acc (sensor, beacon) ->
      let () =
        Printf.printf "Sensor (%i, %i) Beacon (%i, %i)\n" sensor.x sensor.y
          beacon.x beacon.y
      in
      let () = flush stdout in
      let delta = abs (beacon.x - sensor.x) + abs (beacon.y - sensor.y) in
      let nc =
        let rec iloop acc n =
          if n >= 0 then
            let rec xloop acc x y ux =
              if (*y = row && *) x <= ux then
                let acc = Cave.add { x; y } '#' acc in
                xloop acc (x + 1) y ux
              else acc
            in
            let acc =
              xloop acc
                (sensor.x - (delta - n))
                (sensor.y - n)
                (sensor.x + (delta - n))
            in
            let acc =
              xloop acc
                (sensor.x - (delta - n))
                (sensor.y + n)
                (sensor.x + (delta - n))
            in
            iloop acc (n - 1)
          else acc
        in
        iloop acc delta
      in
      nc)
    Cave.empty data

let cave =
  List.fold_left
    (fun acc (sensor, beacon) ->
      let () =
        Printf.printf "Sensor (%i, %i) Beacon (%i, %i)\n" sensor.x sensor.y
          beacon.x beacon.y
      in
      let () = flush stdout in
      let acc = Cave.add { x = sensor.x; y = sensor.y } 'S' acc in
      let acc = Cave.add { x = beacon.x; y = beacon.y } 'B' acc in
      acc)
    cave data

let () = draw cave

let count c y =
  Cave.fold
    (fun k v acc -> if k.y = y then if v = '#' then acc + 1 else acc else acc)
    c 0

let () = Printf.printf "Row %i has %i\n" row (count cave row)
let rows = Array.make 4000001 []

let merge lst =
  let () = List.iter (fun (l, h) -> Printf.printf "%i -> %i, " l h ) lst in
  let () = Printf.printf " ==> " in
  let rec loop lst =
    match lst with
    | (l1, h1) :: (l2, h2) :: tl ->
        if h1 + 1 >= l2 then loop ((l1, max h1 h2) :: tl)
        else (l1, h1) :: loop ((l2, h2) :: tl)
    | hd :: [] -> hd :: loop []
    | [] -> []
  in
  let merged =
  loop (List.sort compare lst)
  in 
  let () = List.iter (fun (l, h) -> Printf.printf "%i -> %i," l h ) lst in
  let () = Printf.printf "\n" in
   merged

let _ =
  List.iter
    (fun (sensor, beacon) ->
      let () =
        Printf.printf "Sensor (%i, %i) Beacon (%i, %i)\n" sensor.x sensor.y
          beacon.x beacon.y
      in
      let () = flush stdout in
      let delta = abs (beacon.x - sensor.x) + abs (beacon.y - sensor.y) in
      for n = 0 to delta do
        let () =
          if sensor.y - n >= 0 && sensor.y - n <= 4_000_000 then
            Array.set rows (sensor.y - n)
              (merge
                 (List.sort (compare) ((sensor.x - (delta - n), sensor.x + (delta - n))
                 :: rows.(sensor.y - n))))
          else ()
        in
        if sensor.y + n >= 0 && sensor.y + n <= 4_000_000 then
          Array.set rows (sensor.y + n)
            (merge
               (List.sort (compare) ((sensor.x - (delta - n), sensor.x + (delta - n))
               :: rows.(sensor.y + n))))
        else ()
      done)
    data

let () =
  for n = 0 to 4000000 do
    if List.length rows.(n) > 1 then
      List.iter (fun (l, h) -> Printf.printf "%i: %i -> %i\n" n l h) rows.(n)
  done

(*
let my_lst = [ (8, 10); (-3, 10) ]
let () = Printf.printf "list\n"
let () = List.iter (fun (l, h) -> Printf.printf "%i -> %i\n" l h ) my_lst
let () = Printf.printf "sorted\n"
let () = List.iter (fun (l, h) -> Printf.printf "%i -> %i\n" l h ) (List.sort (compare) my_lst)
let () = Printf.printf "results in\n"
let () = List.iter (fun (l, h) -> Printf.printf "%i -> %i\n" l h ) (merge my_lst)
*)
