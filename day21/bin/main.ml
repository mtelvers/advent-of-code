let () = print_endline "Hello, World!"

type operator = [ `Add | `Subtract | `Multiply | `Divide ]

type monkey = {
  name : string;
  left : string option;
  op : operator option;
  right : string option;
  l : int option;
  r : int option;
  n : int option;
}

let get_string = function
  | Some v -> v
  | None -> ""

let get_int = function
  | Some v -> v
  | None -> 0

let operator_of_string s =
  match s with
  | "+" -> Some `Add
  | "-" -> Some `Subtract
  | "*" -> Some `Multiply
  | "/" -> Some `Divide
  | _ -> None

let read_file =
  let ic = open_in "input" in
  let rec loop input lst =
    try
      let line = input_line input in
      let tkn = Str.split (Str.regexp "[ :]+") line in
      let r =
        match tkn with
        | aa :: bb :: cc :: dd :: _ ->
            {
              name = aa;
              left = Some bb;
              op = operator_of_string cc;
              right = Some dd;
              l = None;
              r = None;
              n = None;
            }
        | aa :: bb :: _ ->
            {
              name = aa;
              left = None;
              op = None;
              right = None;
              l = None;
              r = None;
              n = Some (int_of_string bb);
            }
        | _ -> assert false
      in
      r :: loop input lst
    with End_of_file ->
      close_in input;
      lst
  in
  loop ic []

let monkeys = Array.of_list read_file
let rec find a x n = if a.(n) = x then n else find a x (n + 1)

let rec find_name a str n =
  let m = a.(n) in
  if m.name = str then n else find_name a str (n + 1)

let rec subsitute () =
  let () = 
  Array.iter
    (fun m ->
      match m.n with
      | Some _ ->
          let () = Array.iteri
            (fun i mm ->
(*let () = Printf.printf "Comparing %s with %s and %s\n" m.name (get_string mm.left) (get_string mm.right) in *)
              if Some m.name = mm.left then
                Array.set monkeys i
                  {
                    name = mm.name;
                    left = mm.left;
                    op = mm.op;
                    right = mm.right;
                    l = m.n;
                    r = mm.r;
                    n = mm.n;
                  } else
              if Some m.name = mm.right then
                Array.set monkeys i
                  {
                    name = mm.name;
                    left = mm.left;
                    op = mm.op;
                    right = mm.right;
                    l = mm.l;
                    r = m.n;
                    n = mm.n;
                  })
            monkeys in
           Array.iteri (fun i mm -> match mm.l, mm.op, mm.r with
                 | Some x, Some `Add, Some y -> Array.set monkeys i { name = mm.name; left = mm.left; op = mm.op; right = mm.right; l = mm.l; r = mm.r; n = Some (x + y) }
                 | Some x, Some `Subtract, Some y -> Array.set monkeys i { name = mm.name; left = mm.left; op = mm.op; right = mm.right; l = mm.l; r = mm.r; n = Some (x - y) }
                 | Some x, Some `Multiply, Some y -> Array.set monkeys i { name = mm.name; left = mm.left; op = mm.op; right = mm.right; l = mm.l; r = mm.r; n = Some (x * y) }
                 | Some x, Some `Divide, Some y -> Array.set monkeys i { name = mm.name; left = mm.left; op = mm.op; right = mm.right; l = mm.l; r = mm.r; n = Some (x / y) }
                 | _, _, _ -> ()
          ) monkeys
      | None -> ())
    monkeys
  in let index = find_name monkeys "root" 0 in if monkeys.(index).n = None then subsitute () else monkeys.(index)

let m = subsitute ()
let () = Printf.printf "root monkey = %i\n" (get_int m.n)

(* let () = Array.iteri ( fun i m -> Printf.printf "%s: %s () %s aka %i () %i = %i\n" m.name (get_string m.left) (get_string m.right) (get_int m.l) (get_int m.r) (get_int m.n) ) monkeys*)




