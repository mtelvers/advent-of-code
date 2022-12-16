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

let cave_min, _ = Cave.min_binding cave

let cave_min =
  Cave.fold
    (fun k _ acc -> { x = min acc.x k.x; y = min acc.y k.y })
    cave cave_min

let cave_max, _ = Cave.max_binding cave

let cave_max =
  Cave.fold
    (fun k _ acc -> { x = max acc.x k.x; y = max acc.y k.y })
    cave cave_max

let draw t =
  for y = 0 to cave_max.y + 5 do
    for x = cave_min.x - 20 to cave_max.x + 20 do
      if Cave.mem { x; y } t then Printf.printf "%c" (Cave.find { x; y } t)
      else Printf.printf " "
    done;
    Printf.printf "\n"
  done;
  flush stdout

let () = draw cave

exception Abyss of int
exception Blocked of int

let rec add_sand c pos max_y =
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
  if List.length valid = 0 then
    if pos.y = 0 then raise (Blocked 0) else Cave.add pos 'o' c
  else if (List.hd valid).y > max_y then raise (Abyss 0)
  else add_sand c (List.hd valid) max_y

let rec loop c n =
  try
    let c = add_sand c { x = 500; y = 0 } cave_max.y in
    let () = draw c in
    loop c (n + 1)
  with Abyss _ -> (c, n)

let cave, iterations = loop cave 0
let () = Printf.printf "Iterations %i\n" iterations
let cave_max = { x = cave_max.x; y = cave_max.y + 2 }

let add_floor c =
  List.fold_left
    (fun c i -> Cave.add { x = i; y = cave_max.y } '#' c)
    c (1 -- 1000)

let cave = add_floor cave

let rec loop c n =
  try
    let c = add_sand c { x = 500; y = 0 } cave_max.y in
    loop c (n + 1)
  with Blocked _ -> (c, n)

let _, iterations = loop cave iterations
let () = Printf.printf "Iterations %i\n" (iterations + 1)
