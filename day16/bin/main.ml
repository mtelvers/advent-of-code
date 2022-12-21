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

let rec bfs start dest path n =
  let s = Chambers.find dest chambers in
  let new_t, corridors =
    List.fold_left
      (fun (acc, cor) finish ->
        if not (Paths.mem { start; finish } acc) then
          (Paths.add { start; finish } n acc, finish :: cor)
        else (acc, cor))
      (path, []) s.corridors
  in
 let () = Printf.printf "corridors: " in
 let () = List.iter (Printf.printf "%s," ) corridors in
 let () = Printf.printf "\n"
  in
  List.fold_left (fun acc el -> bfs start el acc (n + 1)) new_t corridors

let paths =
  Chambers.fold
    (fun ch v pa -> bfs ch ch (Paths.add { start = ch; finish = ch } 0 pa) 1)
    chambers Paths.empty

let () =
  Paths.iter (fun k j -> Printf.printf "%s %s: %i\n" k.start k.finish j) paths

let working_valves =
  Chambers.fold (fun ch j lst -> if j.n > 0 then ch :: lst else lst) chambers []

let () = List.iter print_endline working_valves

let rec simulate time pos visited water =
  if time > 30 then
  let () = Printf.printf "water %i\n" water in
water
  else
    let flow = Chambers.fold (fun _ n total -> total + n) visited 0 in
    if Chambers.cardinal visited = List.length working_valves then
       simulate (time + 1) pos visited (water + flow)
    else
    List.fold_left
      (fun bst dest ->
        if not (Chambers.mem dest visited) then
          let () = Printf.printf "time %i: " time in
          let () = Chambers.iter (fun k j -> Printf.printf "%s %i, " k j) visited in
          let () = Printf.printf "-> water %i flow %i\n" water flow in
          let j = Chambers.find dest chambers in
          let d = 1 + Paths.find { start = pos; finish = dest } paths in
          let nv = Chambers.add dest j.n visited in
          max bst (simulate (time + d) dest nv (water + (flow * d)))
        else bst)
      0 working_valves

let best = simulate 1 "AA" Chambers.empty 0
let () = Printf.printf "Best %i\n" best
