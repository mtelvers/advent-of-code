type coord = { x : int; y : int }

module Cave = Map.Make (struct
  type t = coord

  let compare = compare
end)

let ( -- ) i j =
  let rec aux n acc = if n < i then acc else aux (n - 1) (n :: acc) in
  aux j []

let cave =
  let ic = open_in "input" in
  let rec loop input cave =
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
      let rec plot c l =
        match l with
        | sensor_x :: sensor_y :: beacon_x :: beacon_y :: tail ->
            let () =
              Printf.printf "Sensor (%i, %i) Beacon (%i, %i)\n" sensor_x
                sensor_y beacon_x beacon_y
            in
            let () = flush stdout in
            let delta = abs (beacon_x - sensor_x) + abs (beacon_y - sensor_y) in
            let nc =
              List.fold_left
                (fun acc y ->
                  List.fold_left
                    (fun acc x ->
                      let distance = abs (x - sensor_x) + abs (y - sensor_y) in
                      if distance <= delta then
                        if Cave.mem { x; y } acc then acc
                        else Cave.add { x; y } '#' acc
                      else acc)
                    acc
                    (sensor_x - delta -- (sensor_x + delta)))
                c
                (sensor_y - delta -- (sensor_y + delta))
            in
            let nc = Cave.add { x = sensor_x; y = sensor_y } 'S' nc in
            let nc = Cave.add { x = beacon_x; y = beacon_y } 'B' nc in
            plot nc tail
        | _ -> c
      in
      let new_cave = plot cave tkn in
      loop input new_cave
    with End_of_file ->
      close_in input;
      cave
  in
  loop ic Cave.empty

let draw t =
  for y = -5 to 30 do
    Printf.printf "%2i: " y;
    for x = -5 to 30 do
      if Cave.mem { x; y } t then Printf.printf "%c" (Cave.find { x; y } t)
      else Printf.printf " "
    done;
    Printf.printf "\n"
  done;
  flush stdout

let () = draw cave

let count c y =
  Cave.fold
    (fun k v acc -> if k.y = y then if v = '#' then acc + 1 else acc else acc)
    c 0

let () = Printf.printf "Row 2000000 has %i\n" (count cave 2_000_000)
