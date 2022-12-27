let tests = [ "1=-0-2"; "12111"; "2=0="; "21"; "2=01"; "111"; "20012"; "112"; "1=-1="; "1-12"; "12"; "1="; "122" ]

let int_of_snafu s =
  let chars = List.init (String.length s) (String.get s) in
  let result, _ =
    List.fold_right
      (fun ch (acc, p) -> ((acc + match ch with '2' -> 2 * p | '1' -> p | '0' -> 0 | '-' -> -1 * p | '=' -> -2 * p | _ -> 0), p * 5))
      chars (0, 1)
  in
  result

let rec snafu_of_int x =
  let r = x mod 5 in
  let d = x / 5 in
  let t, s = match r with 0 -> (d, "0") | 1 -> (d, "1") | 2 -> (d, "2") | 3 -> (d + 1, "=") | 4 -> (d + 1, "-") | _ -> (d, "?") in
  if t = 0 then s else snafu_of_int t ^ s

let () =
  List.iter
    (fun x ->
      let input = x in
      let output = int_of_snafu input in
      let original = snafu_of_int output in
      Printf.printf "%s -> %i -> %s\n" input output original)
    tests

let numbers =
  let ic = open_in "input" in
  let rec loop input =
    try
      let line = input_line input in
      line :: loop input
    with End_of_file -> []
  in
  loop ic

let total = List.fold_left (fun acc n -> acc + int_of_snafu n) 0 numbers
let () = Printf.printf "%i -> %s\n" total (snafu_of_int total)
