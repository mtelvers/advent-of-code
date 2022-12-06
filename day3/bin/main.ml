let bags =
  let ic = open_in "input" in
  let rec loop input =
    try
      let line = input_line input in
      List.init (String.length line) (String.get line) :: loop input
    with End_of_file -> []
  in
  loop ic

let pockets =
  List.map
    (fun bag ->
      let nitems = List.length bag / 2 in
      ( List.filteri (fun i _ -> if i < nitems then true else false) bag,
        List.filteri (fun i _ -> if i >= nitems then true else false) bag ))
    bags

let common lst =
  List.map (fun (p1, p2) -> List.filter (fun x -> List.mem x p2) p1) lst

let priority lst =
  List.map
    (fun l ->
      let a = Char.code (List.hd l) in
      if a > Char.code 'Z' then a - 0x60 else a - 0x40 + 26)
    lst

let sum = List.fold_left (fun i e -> i + e) 0 (priority (common pockets))
let _ = Printf.printf "Part one sum: %i\n" sum

let shared lst =
  let rec loop x =
    match x with
    | one :: two :: three :: tail ->
        List.filter (fun x -> List.mem x two && List.mem x three) one
        :: loop tail
    | _ -> []
  in
  loop lst

let sum = List.fold_left (fun i e -> i + e) 0 (priority (shared bags))
let _ = Printf.printf "Part two sum: %i\n" sum
