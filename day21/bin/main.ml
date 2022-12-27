type operator = [ `Add | `Subtract | `Multiply | `Divide | `Equal ]
type monkey = { name : string; op : operator; n : int }

type 'a tree = Leaf | Node of 'a node
and 'a node = { value : 'a; left : 'a tree; right : 'a tree }

let operator_of_string s = match s with "+" -> `Add | "-" -> `Subtract | "*" -> `Multiply | "/" -> `Divide | _ -> `Equal

module Data = Map.Make (String)

(* ptdq: humn - dvpt *)
(* dvpt: 3           *)

let input_map =
  let ic = open_in "input" in
  let rec loop input data =
    try
      let line = input_line input in
      let tkn = Str.split (Str.regexp "[ :]+") line in
      loop input (match tkn with hd :: tl -> Data.add hd tl data | _ -> data)
    with End_of_file ->
      close_in input;
      data
  in
  loop ic Data.empty

let rec calculate = function
  | Leaf -> 0
  | Node { value; left; right } -> (
      match value.op with
      | `Add -> calculate left + calculate right
      | `Subtract -> calculate left - calculate right
      | `Multiply -> calculate left * calculate right
      | `Divide -> calculate left / calculate right
      | `Equal -> value.n)

let rec create_tree map name =
  let lst = Data.find name map in
  match lst with
  | node1 :: oper :: node2 :: _ -> Node { value = { name; op = operator_of_string oper; n = 0 }; left = create_tree map node1; right = create_tree map node2 }
  | num :: _ -> Node { value = { name; op = `Equal; n = int_of_string num }; left = Leaf; right = Leaf }
  | _ -> assert false

let mytree = create_tree input_map "root"

let rec print n = function
  | Leaf -> flush stdout
  | Node { value; left; right } ->
      let () = Printf.printf "%2i: %s %i\n" n value.name value.n in
      let () = print (n + 1) left in
      print (n + 1) right

let () = print 0 mytree
let () = Printf.printf "total = %i\n" (calculate mytree)

let rec guess map low high =
  let delta = high - low in
  let n = low + (delta / 2) in
  let map = Data.add "humn" [ string_of_int n ] map in
  let mytree = create_tree map "root" in
  let m = calculate mytree in
  let () = Printf.printf "guessing %i (%i %i) results in root monkey %i\n" n low high m in
  let () = flush stdout in
  if m = 0 then n else if m < 0 then guess map low (low + (delta / 2)) else guess map (high - (delta / 2)) high

let root = Data.find "root" input_map
let new_root = [ List.nth root 0; "-"; List.nth root 2 ]
let input_map = Data.add "root" new_root input_map
let _ = guess input_map 0 1_000_000_000_000_000
