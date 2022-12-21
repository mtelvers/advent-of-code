let () = print_endline "Hello, World!"

let input_list =
  let ic = open_in "input" in
  let rec loop input =
    try
      let line = input_line input in
      let num = int_of_string line in
      num :: loop input
    with End_of_file ->
      close_in input;
      []
  in
  loop ic

let numbers = Array.make (List.length input_list) (0, 0)
let () = List.iteri (fun i number -> Array.set numbers i (i, number)) input_list
let rec find a x n = if a.(n) = x then n else find a x (n + 1)

let rec find_part a x n =
  let _, num = a.(n) in
  if num = x then n else find_part a x (n + 1)

let swap a i1 i2 =
  let n1 = a.(i1) in
  let n2 = a.(i2) in
  let () = Array.set a i2 n1 in
  Array.set a i1 n2

let mix count =
  for _ = 1 to count do
  List.iteri
    (fun i number ->
      let () = Printf.printf "%i %i\n" i number in
      let () = flush stdout in
      (* let () =
          let () = Array.iter (fun (i, n) -> Printf.printf "%i %i," i n) numbers in
          Printf.printf "\n"
         in *)
      let len = Array.length numbers in
      let rec loop num =
        if num = 0 then ()
        else
          let index = find numbers (i, number) 0 in
          if index + 1 = len then
            let n1 = numbers.(len - 1) in
            let () = Array.blit numbers 0 numbers 1 (len - 1) in
            let () = Array.set numbers 0 n1 in
            loop num
          else
            let () = swap numbers index (index + 1) in
            loop (num - 1)
      in
      loop
        (if number < 0 then (number mod (len - 1)) + len - 1
        else number mod (len - 1)))
    input_list
  done

(*
let () = mix 1

let () =
  let () =
    Array.iteri
      (fun ind (i, n) -> Printf.printf "%i: (%i, %i)\n" ind i n)
      numbers
  in
  Printf.printf "\n"

let index_of_zero = find_part numbers 0 0
let () = Printf.printf "%i\n" index_of_zero
let _, one = Array.get numbers ((1000 + index_of_zero) mod Array.length numbers)
let _, two = Array.get numbers ((2000 + index_of_zero) mod Array.length numbers)
let _, three =
  Array.get numbers ((3000 + index_of_zero) mod Array.length numbers)

let () =
  Printf.printf "Sum of 1000th (%i) + 2000th (%i) + 3000th (%i) = %i\n" one two
    three
    (one + two + three)
*)



let numbers = Array.make (List.length input_list) (0, 0)
let () = List.iteri (fun i number -> Array.set numbers i (i, number * 811589153)) input_list

let () = mix 10

let () =
  let () =
    Array.iteri
      (fun ind (i, n) -> Printf.printf "%i: (%i, %i)\n" ind i n)
      numbers
  in
  Printf.printf "\n"

let index_of_zero = find_part numbers 0 0
let () = Printf.printf "%i\n" index_of_zero
let _, one = Array.get numbers ((1000 + index_of_zero) mod Array.length numbers)
let _, two = Array.get numbers ((2000 + index_of_zero) mod Array.length numbers)
let _, three =
  Array.get numbers ((3000 + index_of_zero) mod Array.length numbers)

let () =
  Printf.printf "Sum of 1000th (%i) + 2000th (%i) + 3000th (%i) = %i\n" one two
    three
    (one + two + three)

