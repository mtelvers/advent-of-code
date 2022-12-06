let load =
  let ic = open_in "input" in
  let rec loop input =
    try
      let line = input_line input in
      let ranges = String.split_on_char ',' line in
      let rec process l =
        match l with
        | hd :: tl ->
            let r = String.split_on_char '-' hd in
            let range i j =
              let rec loop n lst =
                if n < i then lst else loop (n - 1) (n :: lst)
              in
              loop j []
            in
            range
              (int_of_string (List.hd r))
              (int_of_string (List.hd (List.tl r)))
            :: process tl
        | [] -> []
      in
      process ranges :: loop input
    with End_of_file -> []
  in
  loop ic

let spare =
  List.fold_left
    (fun i e ->
      let loop l =
        match l with
        | elf1 :: elf2 :: _ ->
            let common =
              List.length (List.filter (fun e -> List.mem e elf2) elf1)
            in
            if List.length elf1 <= common || List.length elf2 <= common then
              i + 1
            else i
        | _ -> i
      in
      loop e)
    0 load

let () = Printf.printf "Spare elves %i\n" spare

let overlaps =
  List.fold_left
    (fun i e ->
      let loop l =
        match l with
        | elf1 :: elf2 :: _ ->
            let common =
              List.length (List.filter (fun e -> List.mem e elf2) elf1)
            in
            if common > 0 then i + 1 else i
        | _ -> i
      in
      loop e)
    0 load

let () = Printf.printf "Overlaps elves %i\n" overlaps
