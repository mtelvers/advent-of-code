let sorted_elves =
  let ic = open_in "input" in
  let rec process input =
    let rec process_elf input =
      try
        let line = input_line input in
        let n = int_of_string_opt line in
        match n with None -> [] | Some n -> n :: process_elf input
      with End_of_file -> []
    in
    let elf = process_elf input in
    match elf with
    | [] ->
        close_in input;
        []
    | l -> l :: process input
  in
  let each_elf =
    List.map (List.fold_left (fun init elem -> init + elem) 0) (process ic)
  in
  List.rev (List.sort compare each_elf)

let () =
  match sorted_elves with
  | first :: second :: third :: _ ->
      Printf.printf "Largest is %i\nTop three %i\n" first
        (first + second + third)
  | _ -> ()
