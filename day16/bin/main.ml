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

let rec bfs start dests path n =
  let new_p, corridors =
    List.fold_left
      (fun (acc, cor) finish ->
        if not (Paths.mem { start; finish } acc) then
          let chamber = Chambers.find finish chambers in
          (Paths.add { start; finish } n acc, chamber.corridors @ cor)
        else (acc, cor))
      (path, []) dests
  in
  if List.length corridors > 0 then
  bfs start corridors new_p (n + 1)
  else path

let paths =
  Chambers.fold
    (fun ch v pa -> bfs ch [ch] pa 0)
    chambers Paths.empty

let working_valves =
  Chambers.fold (fun ch j lst -> if j.n > 0 then ch :: lst else lst) chambers []

let () = List.iter print_endline working_valves

let rec simulate time pos visited water =
  if time = 0 then
water
  else
    let flow = Chambers.fold (fun _ n total -> total + n) visited 0 in
    if Chambers.cardinal visited = List.length working_valves then
       simulate 0 pos visited (water + flow * time)
    else
    List.fold_left
      (fun bst dest ->
        if not (Chambers.mem dest visited) then
          let j = Chambers.find dest chambers in
          let d = min (1 + Paths.find { start = pos; finish = dest } paths) time in
          let nv = Chambers.add dest j.n visited in
          max bst (simulate (time - d) dest nv (water + (flow * d)))
        else bst)
      0 working_valves

let best = simulate 30 "AA" Chambers.empty 0 "AA"
let () = Printf.printf "Best %i\n" best

