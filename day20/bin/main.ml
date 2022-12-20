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

let numbers = Array.of_list input_list
let rec find a x n = if a.(n) = x then n else find a x (n + 1)

let swap a i1 i2 =
  let n1 = a.(i1) in
  let n2 = a.(i2) in
  let () = Array.set a i2 n1 in
  Array.set a i1 n2

let () =
  List.iteri
    (fun i number ->
   let () = Printf.printf "%i %i\n" i number in
   let () = flush stdout in
      let () =
        let () = Array.iter (Printf.printf "%i,") numbers in
        Printf.printf "\n"
      in
      let len = Array.length numbers in
      let rec loop num =
        if num = 0 then
          ()
         else (
            let index = find numbers number 0 in
            if index + 1 = len then
                let n1 = numbers.(len - 1) in
                for i = len - 1 downto 1 do
                  swap numbers i (i - 1)
                done;
                let () = Array.set numbers 0 n1 in
                loop num
            else
                let () = swap numbers index (index + 1) in
                loop (num - 1))
      in
      loop (if number < 0 then (number mod len) + len - 1 else number mod len))
    input_list

let () =
  let () = Array.iter (Printf.printf "%i,") numbers in
  Printf.printf "\n"

let index_of_zero = find numbers 0 0
let () = Printf.printf "%i\n" index_of_zero
let () = Printf.printf "Sum of 1000th (%i) + 2000th (%i) + 3000th (%i) = %i\n" 
(Array.get numbers ((10 + index_of_zero) mod (Array.length numbers)))
(Array.get numbers ((20 + index_of_zero) mod (Array.length numbers)))
(Array.get numbers ((30 + index_of_zero) mod (Array.length numbers)))
((Array.get numbers ((10 + index_of_zero) mod (Array.length numbers))) +
(Array.get numbers ((20 + index_of_zero) mod (Array.length numbers))) +
(Array.get numbers ((30 + index_of_zero) mod (Array.length numbers))))
