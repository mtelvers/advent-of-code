type junction = { n : int; corridors : string list }

module Chambers = Map.Make (struct
  type t = string

  let compare = compare
end)

type route = { start : string; finish : string }

module Paths = Map.Make (struct
  type t = route

  let compare = compare
end)

let chambers =
  let ic = open_in "input" in
  let rec loop input tnl =
    try
      let line = input_line input in
      let tkn = Str.split (Str.regexp "[ :=,;]+") line in
      let t =
        match tkn with
        | _ :: valve :: _ :: _ :: _ :: num :: _ :: _ :: _ :: _ :: corridors ->
            let n = int_of_string num in
            Chambers.add valve { n; corridors } tnl
        | _ -> tnl
      in
      loop input t
    with End_of_file ->
      close_in input;
      tnl
  in
  loop ic Chambers.empty

let () =
  Chambers.iter
    (fun k j ->
      let () = Printf.printf "%s %i: " k j.n in
      let () = List.iter (Printf.printf "%s,") j.corridors in
      Printf.printf "\n")
    chambers

let rec bfs start xxx path n =
  let s = Chambers.find xxx chambers in
  let new_t, corridors =
    List.fold_left
      (fun (acc, cor) finish ->
        if not (Paths.mem { start; finish } acc) then
          (Paths.add { start; finish } n acc, s.corridors @ cor)
        else (acc, cor))
      (path, []) s.corridors
  in
  List.fold_left (fun acc el -> bfs start el acc (n + 1)) new_t corridors

let paths = Chambers.fold ( fun ch v pa -> bfs ch ch (Paths.add { start = ch; finish = ch } 0 pa) 1 ) chambers Paths.empty
let () = Paths.iter (fun k j -> Printf.printf "%s %s: %i\n" k.start k.finish j) paths



