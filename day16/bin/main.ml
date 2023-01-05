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
  if List.length corridors > 0 then bfs start corridors new_p (n + 1) else path

let paths = Chambers.fold (fun ch v pa -> bfs ch [ ch ] pa 0) chambers Paths.empty
let working_valves = Chambers.fold (fun ch j lst -> if j.n > 0 then ch :: lst else lst) chambers []

let rec simulate time pos valves visited water =
  if time = 0 then water
  else
    let flow = Chambers.fold (fun _ n total -> total + n) visited 0 in
    if Chambers.cardinal visited = List.length valves then simulate 0 pos valves visited (water + (flow * time))
    else
      List.fold_left
        (fun bst dest ->
          if not (Chambers.mem dest visited) then
            let j = Chambers.find dest chambers in
            let d = min (1 + Paths.find { start = pos; finish = dest } paths) time in
            let nv = Chambers.add dest j.n visited in
            max bst (simulate (time - d) dest valves nv (water + (flow * d)))
          else bst)
        0 valves

let best = simulate 30 "AA" working_valves Chambers.empty 0
let () = Printf.printf "Best %i\n" best
let () = flush stdout

let rec combnk k lst =
  if k = 0 then [ [] ]
  else
    let rec inner = function [] -> [] | x :: xs -> List.map (fun z -> x :: z) (combnk (k - 1) xs) :: inner xs in
    List.concat (inner lst)

let best =
  List.fold_left
    (fun acc l ->
      let () = List.iter (Printf.printf "%s,") l in
      let n = simulate 26 "AA" l Chambers.empty 0 in
      let () = Printf.printf " = %i -- " n in
      let compliment = List.filter (fun x -> not (List.mem x l)) working_valves in
      let () = List.iter (Printf.printf "%s,") compliment in
      let m = simulate 26 "AA" compliment Chambers.empty 0 in
      let total = n + m in
      let () = Printf.printf " = %i -> %i\n" m total in
      if total > acc then total else acc)
    0
    (combnk (List.length working_valves / 2) working_valves)

let () = Printf.printf "Best %i\n" best
