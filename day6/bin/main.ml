let read len =
  let ic = open_in "input" in
  let rec loop input =
    try
      let line = input_line input in
      let tokens = List.init (String.length line) (String.get line) in
      let rec dup_in lst =
        match lst with [] -> false | hd :: tl -> List.mem hd tl || dup_in tl
      in
      let rec process i lst stk =
        match lst with
        | hd :: tl ->
            let tmp = stk @ [ hd ] in
            if List.length tmp < len then process (i + 1) tl tmp
            else if dup_in tmp then
              match tmp with _ :: t -> process (i + 1) tl t | [] -> -1
            else i
        | [] -> -1
      in
      process 1 tokens [] :: loop input
    with End_of_file ->
      close_in input;
      []
  in
  loop ic

let () = Printf.printf "Part 1: "
let () = List.iter (Printf.printf "%i, ") (read 4)
let () = Printf.printf "\nPart 2: "
let () = List.iter (Printf.printf "%i, ") (read 14)
let () = Printf.printf "\n"
