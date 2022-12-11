type operation = [ `Add | `Multiply | `Square ]

type monkey = {
  items : int list;
  op : operation;
  x : int;
  test : int;
  if_true : int;
  if_false : int;
  inspections : int;
}

let empty =
  { items = []; op = `Add; x = 0; test = 0; if_true = 0; if_false = 0; inspections = 0 }

let troop = Array.make 8 empty

let () =
  let ic = open_in "input" in
  let rec loop input m tmp =
    try
      let line = input_line input in
      let tokens = String.split_on_char ' ' line in
      let () = Printf.printf "tokens %i\n" (List.length tokens) in
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
                  inspections = record.inspections;
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
                  inspections = record.inspections;
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
          | hd :: tl ->
              let () = Printf.printf "%i: %s\n" m hd in
              process tl record
          | _ -> record
        in
        loop input m (process tokens tmp)
    with End_of_file ->
      close_in input;
      Array.set troop m tmp
  in
  loop ic 0 empty

let run n d =
  for _ = 1 to n do
    Array.iteri
      (fun m mkey ->
        let () =
          List.iter
            (fun item ->
              let worry =
                (match mkey.op with
                | `Add -> item + mkey.x
                | `Multiply -> item * mkey.x
                | `Square -> item * item)
                / d
              in
              let target =
                if worry mod mkey.test = 0 then mkey.if_true else mkey.if_false
              in
              let () =
                Printf.printf "worry %i therefore target %i\n" worry target
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
  done

let () = run 20 3

let () =
  Array.iteri
    (fun i v ->
      let () =
        Printf.printf "Monkey %i: test=%i x=%i t=%i f=%i inspections=%i: " i v.test v.x
          v.if_true v.if_false v.inspections
      in
      let () = List.iter (Printf.printf "%i, ") v.items in
      Printf.printf "\n")
    troop

let busy_list =
  Array.fold_left
    (fun acc v ->
          v.inspections :: acc
      ) []
    troop

let sorted_busy_list = List.rev (List.sort compare busy_list)

let () = Printf.printf "%i\n" ((List.hd sorted_busy_list) * (List.nth sorted_busy_list 1))

