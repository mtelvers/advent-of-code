let program =
  let ic = open_in "input" in
  let rec loop input =
    try
      let line = input_line input in
      let cmds = String.split_on_char ' ' line in
      cmds @ loop input
    with End_of_file ->
      close_in input;
      []
  in
  loop ic

let pc, x, strength =
  List.fold_left
    (fun (acc, x, strength) s ->
      let num = try int_of_string s with Failure _ -> 0 in
      let ss = if (acc - 20) mod 40 = 0 then acc * x else 0 in
      (acc + 1, x + num, strength + ss))
    (1, 1, 0) program

let () = Printf.printf "pc = %i, x = %i, strength = %i\n" pc x strength

let pc, x, crt =
  List.fold_left
    (fun (acc, x, crt) s ->
      let num = try int_of_string s with Failure _ -> 0 in
      let column = (acc - 1) mod 40 in
      let c = if x - 1 <= column && column <= x + 1 then '#' else '.' in
      (acc + 1, x + num, crt @ [ c ]))
    (1, 1, []) program

let () = Printf.printf "pc = %i, x = %i\n" pc x

let () =
  List.iteri
    (fun i c ->
      if i mod 40 = 39 then Printf.printf "%c\n" c else Printf.printf "%c" c)
    crt
