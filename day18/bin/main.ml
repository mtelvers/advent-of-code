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
    (fun k v acc ->
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
