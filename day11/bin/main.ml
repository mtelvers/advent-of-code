open Big_int_Z

type operation = [ `Add | `Multiply | `Square ]

type monkey = {
  items : big_int list;
  op : operation;
  x : big_int;
  test : big_int;
  if_true : int;
  if_false : int;
  inspections : int;
}

let empty =
  {
    items = [];
    op = `Add;
    x = zero_big_int;
    test = zero_big_int;
    if_true = 0;
    if_false = 0;
    inspections = 0;
  }

let catalog troop =
  let ic = open_in "input" in
  let rec loop input m tmp =
    try
      let line = input_line input in
      let tokens = String.split_on_char ' ' line in
      if List.length tokens < 2 then
        let () = Array.set troop m tmp in
        loop input (m + 1) empty
      else
        let rec process t record =
          match t with
          | "items:" :: tl ->
              process tl
                {
                  items =
                    List.map
                      (fun str ->
                        big_int_of_string
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
                  inspections = record.inspections;
                }
          | "Operation:" :: tl ->
              let num =
                try big_int_of_string (List.hd (List.rev tl)) with Failure _ -> zero_big_int | Invalid_argument _ -> zero_big_int
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
                  inspections = record.inspections;
                }
          | "Test:" :: tl ->
              process tl
                {
                  items = record.items;
                  op = record.op;
                  x = record.x;
                  test = big_int_of_string (List.hd (List.rev tl));
                  if_true = record.if_true;
                  if_false = record.if_false;
                  inspections = record.inspections;
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
                  inspections = record.inspections;
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
                  inspections = record.inspections;
                }
          | _ :: tl ->
              process tl record
          | _ -> record
        in
        loop input m (process tokens tmp)
    with End_of_file ->
      close_in input;
      Array.set troop m tmp
  in
  loop ic 0 empty

let shenanigans troop n d =
  for i = 1 to n do
    Array.iteri
      (fun m mkey ->
        let () =
          List.iter
            (fun item ->
              let worry = mod_big_int (div_big_int
                (match mkey.op with
                | `Add -> add_big_int item mkey.x
                | `Multiply -> mult_big_int item mkey.x
                | `Square -> mult_big_int item item)
                d) (big_int_of_int 96577)
                (*d) (big_int_of_int 9699690)*)
              in
              let target =
                if eq_big_int (mod_big_int worry mkey.test) zero_big_int then mkey.if_true else mkey.if_false
              in
              let tmp = Array.get troop target in
              Array.set troop target
                {
                  items = tmp.items @ [ worry ];
                  op = tmp.op;
                  x = tmp.x;
                  test = tmp.test;
                  if_true = tmp.if_true;
                  if_false = tmp.if_false;
                  inspections = tmp.inspections;
                })
            mkey.items
        in
        let tmp = Array.get troop m in
        Array.set troop m
          {
            items = [];
            op = tmp.op;
            x = tmp.x;
            test = tmp.test;
            if_true = tmp.if_true;
            if_false = tmp.if_false;
            inspections = tmp.inspections + List.length mkey.items;
          })
      troop
; if List.mem i [1; 20; 1000; 2000; 10000] then

    Array.iteri
      (fun i v ->
        let () =
          Printf.printf "Monkey %i: test=%s x=%s t=%i f=%i inspections=%i: " i
            (string_of_big_int v.test) (string_of_big_int v.x) v.if_true v.if_false v.inspections
        in
        Printf.printf "\n")
      troop

  done

let () =
  let troop = Array.make 8 empty in
  let () = catalog troop in
  let () = shenanigans troop 20 (big_int_of_int 3) in
(*
  let () =
    Array.iteri
      (fun i v ->
        let () =
          Printf.printf "Monkey %i: test=%s x=%s t=%i f=%i inspections=%i: " i
            (string_of_big_int v.test) (string_of_big_int v.x) v.if_true v.if_false v.inspections
        in
        let () = List.iter (fun x -> Printf.printf "%s, " (string_of_big_int x)) v.items in
        Printf.printf "\n")
      troop
  in
*)
  let busy_list =
    Array.fold_left (fun acc v -> v.inspections :: acc) [] troop
  in
  let sorted_busy_list = List.rev (List.sort compare busy_list) in
  Printf.printf "Part one %i\n"
    (List.hd sorted_busy_list * List.nth sorted_busy_list 1)

let () =
  let troop = Array.make 8 empty in
  let () = catalog troop in
  let () = shenanigans troop 10000 unit_big_int in
(*
  let () =
    Array.iteri
      (fun i v ->
        let () =
          Printf.printf "Monkey %i: test=%s x=%s t=%i f=%i inspections=%i: " i
            (string_of_big_int v.test) (string_of_big_int v.x) v.if_true v.if_false v.inspections
        in
        let () = List.iter (fun x -> Printf.printf "%s, " (string_of_big_int x)) v.items in
        Printf.printf "\n")
      troop
  in
*)
  let busy_list =
    Array.fold_left (fun acc v -> v.inspections :: acc) [] troop
  in
  let sorted_busy_list = List.rev (List.sort compare busy_list) in
  Printf.printf "Part two %i\n" 
    (List.hd sorted_busy_list * List.nth sorted_busy_list 1)

