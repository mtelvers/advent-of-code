type junction = { n : int; corridors : string list }

module Tunnels = Map.Make (struct
  type t = string

  let compare = compare
end)

let tunnels =
  let ic = open_in "input" in
  let rec loop input tnl =
    try
      let line = input_line input in
      let tkn = Str.split (Str.regexp "[ :=,;]+") line in
      let t =
        match tkn with
        | _ :: valve :: _ :: _ :: _ :: num :: _ :: _ :: _ :: _ :: corridors ->
            let n = int_of_string num in
            Tunnels.add valve { n; corridors } tnl
        | _ -> tnl
      in
      loop input t
    with End_of_file ->
      close_in input;
      tnl
  in
  loop ic Tunnels.empty

let () =
  Tunnels.iter
    (fun k j ->
      let () = Printf.printf "%s %i: " k j.n in
      let () = List.iter (Printf.printf "%s,") j.corridors in
      Printf.printf "\n")
    tunnels

let rec bfs k t n =
  let s = Tunnels.find k tunnels in
  let new_t, corridors =
    List.fold_left
      (fun (acc, cor) el ->
        if not (Tunnels.mem el acc) then
          (Tunnels.add el n acc, s.corridors @ cor)
        else (acc, cor))
      (t, []) s.corridors
  in
  List.fold_left (fun acc el -> bfs el acc (n + 1)) new_t corridors

let t = bfs "AA" (Tunnels.add "AA" 0 Tunnels.empty) 1
let () = Tunnels.iter (fun k j -> Printf.printf "%s: %i\n" k j) t
