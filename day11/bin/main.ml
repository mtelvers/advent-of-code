type operation = [ `Add | `Multiply | `Square ]

type monkey = {
  items : int list;
  op : operation;
  x : int;
  test : int;
  if_true : int;
  if_false : int;
}

module Troop = Map.Make (Int)

let load =
  let empty =
    { items = []; op = `Add; x = 0; test = 0; if_true = 0; if_false = 0 }
  in
  let ic = open_in "input" in
  let rec loop input troop m tmp =
    try
      let line = input_line input in
      let tokens = String.split_on_char ' ' line in
      let () = Printf.printf "tokens %i\n" (List.length tokens) in
      if List.length tokens < 2 then
        loop input (Troop.add m tmp troop) (m + 1) empty
      else
        let rec process t record =
          match t with
          | "items:" :: tl ->
              process tl
                {
                  items =
                    List.map
                      (fun str ->
                        int_of_string
                          (String.trim
                             (String.map
                                (fun c -> if c = ',' then ' ' else c)
                                str)))
                      tl;
                  op = record.op;
                  x = record.x;
                  test = record.test;
                  if_true = record.if_true;
                  if_false = record.if_false;
                }
          | "Operation:" :: tl ->
              let num =
                try int_of_string (List.hd (List.rev tl)) with Failure _ -> 0
              in
              process tl
                {
                  items = record.items;
                  op =
                    (if List.mem "+" tl then `Add
                    else if List.hd (List.rev tl) = "old" then `Square
                    else `Multiply);
                  x = num;
                  test = record.test;
                  if_true = record.if_true;
                  if_false = record.if_false;
                }
          | "Test:" :: tl ->
              process tl
                {
                  items = record.items;
                  op = record.op;
                  x = record.x;
                  test = int_of_string (List.hd (List.rev tl));
                  if_true = record.if_true;
                  if_false = record.if_false;
                }
          | "true:" :: tl ->
              process tl
                {
                  items = record.items;
                  op = record.op;
                  x = record.x;
                  test = record.test;
                  if_true = int_of_string (List.hd (List.rev tl));
                  if_false = record.if_false;
                }
          | "false:" :: tl ->
              process tl
                {
                  items = record.items;
                  op = record.op;
                  x = record.x;
                  test = record.test;
                  if_true = record.if_true;
                  if_false = int_of_string (List.hd (List.rev tl));
                }
          | hd :: tl ->
              let () = Printf.printf "%i: %s\n" m hd in
              process tl record
          | _ -> record
        in
        loop input troop m (process tokens tmp)
    with End_of_file ->
      close_in input;
      Troop.add m tmp troop
  in
  loop ic Troop.empty 0 empty

let _ =
  Troop.fold
    (fun k v i ->
      let () =
        Printf.printf "Monkey %i: test=%i x=%i t=%i f=%i: " k v.test v.x
          v.if_true v.if_false
      in
      let () = List.iter (Printf.printf "%i, ") v.items in
      let () = Printf.printf "\n" in
      i)
    load 0
