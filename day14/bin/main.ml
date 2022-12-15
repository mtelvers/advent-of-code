let () = print_endline "Hello, World!"

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
      let tkn = Str.split (Str.regexp "[ ,>-]+") line in
      let tkni = List.map int_of_string tkn in
      let rec plot c l =
        match l with
        | x1 :: y1 :: x2 :: y2 :: tail ->
            let nc =
              if x1 = x2 then
                List.fold_left
                  (fun acc y -> Cave.add { x = x1; y } '#' acc)
                  c
                  (min y1 y2 -- max y1 y2)
              else
                List.fold_left
                  (fun acc x -> Cave.add { x; y = y1 } '#' acc)
                  c
                  (min x1 x2 -- max x1 x2)
            in
            plot nc (x2 :: y2 :: tail)
        | _ -> c
      in
      let new_cave = plot cave tkni in
      loop input new_cave
    with End_of_file ->
      close_in input;
      cave
  in
  loop ic Cave.empty

let cave_max, _ = Cave.max_binding cave
let () = Printf.printf "Cave_Max (%i, %i)\n" cave_max.x cave_max.y

let draw t =
  for y = 0 to 200 do
    for x = 450 to 550 do
      if Cave.mem { x; y } t then Printf.printf "%c" (Cave.find { x; y } t)
      else Printf.printf " "
    done;
    Printf.printf "\n"
  done;
  flush stdout

let () = draw cave

let rec add_sand c pos =
  let options =
    [
      { x = pos.x; y = pos.y + 1 };
      { x = pos.x - 1; y = pos.y + 1 };
      { x = pos.x + 1; y = pos.y + 1 };
    ]
  in
  let valid =
    List.filter
      (fun new_pos ->
        try
          let ch = Cave.find new_pos c in
          if ch = '#' || ch = 'o' then false else true
        with Not_found -> true)
      options
  in
  let () =
    List.iter (fun x -> Printf.printf "could move to (%i,%i)\n" x.x x.y) valid
  in
  if List.length valid = 0 then Cave.add pos 'o' c
  else add_sand c (List.hd valid)

let rec loop c n =
if n > 0 then
let c = add_sand c { x = 500; y = 0 } in
 let () = draw c in loop c (n - 1)
else c

let _ = loop cave 900
