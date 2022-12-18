type coord = { x : int; y : int; z : int }

module Droplet = Map.Make (struct
  type t = coord

  let compare = compare
end)

let droplet =
  let ic = open_in "input" in
  let rec loop input d =
    try
      let line = input_line input in
      let numbers = String.split_on_char ',' line in
      loop input
        (Droplet.add
           {
             x = int_of_string (List.nth numbers 0);
             y = int_of_string (List.nth numbers 1);
             z = int_of_string (List.nth numbers 2);
           }
           0 d)
    with End_of_file ->
      close_in input;
      d
  in
  loop ic Droplet.empty

let count_sides d =
  Droplet.fold
    (fun k _ acc ->
      acc
      + List.fold_left
          (fun acc neighbour ->
            if Bool.not (Droplet.mem neighbour d) then acc + 1 else acc)
          0
          [
            { x = k.x + 1; y = k.y; z = k.z };
            { x = k.x - 1; y = k.y; z = k.z };
            { x = k.x; y = k.y + 1; z = k.z };
            { x = k.x; y = k.y - 1; z = k.z };
            { x = k.x; y = k.y; z = k.z + 1 };
            { x = k.x; y = k.y; z = k.z - 1 };
          ])
    d 0

let () = Printf.printf "Surface Area %i\n" (count_sides droplet)

let min_coord =
  let min_binding, _ = Droplet.min_binding droplet in
  Droplet.fold
    (fun k _ mi -> { x = min mi.x k.x; y = min mi.y k.y; z = min mi.z k.z })
    droplet min_binding

let max_coord =
  let max_binding, _ = Droplet.max_binding droplet in
  Droplet.fold
    (fun k _ mi -> { x = max mi.x k.x; y = max mi.y k.y; z = max mi.z k.z })
    droplet max_binding

let rec water d p =
      List.fold_left
          (fun d vec ->
              let q = { x = p.x + vec.x; y = p.y + vec.y; z = p.z + vec.z } in
              let bounded =
                q.x >= min_coord.x - 1
                && q.y >= min_coord.y - 1
                && q.z >= min_coord.z - 1
                && q.x <= max_coord.x + 1
                && q.y <= max_coord.y + 1
                && q.z <= max_coord.z + 1
              in
              let another_droplet = Droplet.mem q d in
              match (bounded, another_droplet) with
              | true, true -> d
              | true, false -> water (Droplet.add q 1 d) q
              | false, _ -> d )
          d
          [
            { x = 1; y = 0; z = 0 };
            { x = -1; y = 0; z = 0 };
            { x = 0; y = 1; z = 0 };
            { x = 0; y = -1; z = 0 };
            { x = 0; y = 0; z = 1 };
            { x = 0; y = 0; z = -1 };
          ]

let droplet = water droplet min_coord

let draw d =
  for z = min_coord.z to max_coord.z do
    Printf.printf "%2i:            1        2\n    0123456789012345678901\n" z;
  for y = min_coord.y to max_coord.y do
    Printf.printf "%2i: " y;
    for x = min_coord.x to max_coord.x do
      if Droplet.mem { x; y; z } d then Printf.printf "%i" (Droplet.find { x; y; z} d)
      else Printf.printf " "
    done;
    Printf.printf "\n"
  done;
    Printf.printf "\n"
  done;
  flush stdout

let count_sides d =
  Droplet.fold
    (fun k v acc ->
      if v = 0 then
      acc
      + List.fold_left
          (fun acc neighbour ->
            if Droplet.mem neighbour d && Droplet.find neighbour d = 1 then acc + 1 else acc)
          0
          [
            { x = k.x + 1; y = k.y; z = k.z };
            { x = k.x - 1; y = k.y; z = k.z };
            { x = k.x; y = k.y + 1; z = k.z };
            { x = k.x; y = k.y - 1; z = k.z };
            { x = k.x; y = k.y; z = k.z + 1 };
            { x = k.x; y = k.y; z = k.z - 1 };
          ] else acc)
    d 0

let () = draw droplet

let () = Printf.printf "Surface Area %i\n" (count_sides droplet)
