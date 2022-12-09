type coord = { x : int; y : int }

module Forest = Map.Make (struct
  type t = coord

  let compare = compare
end)

let forest =
  let ic = open_in "input" in
  let rec loop input head tail forest =
    try
      let line = input_line input in
      let cmd = String.split_on_char ' ' line in
      let dir =
        match List.hd cmd with
        | "R" -> { x = 1; y = 0 }
        | "L" -> { x = -1; y = 0 }
        | "U" -> { x = 0; y = 1 }
        | "D" -> { x = 0; y = -1 }
        | _ -> assert false
      in
      let num = int_of_string (List.hd (List.tl cmd)) in
      let new_forest, new_head, new_tail =
        let rec move h t sq f =
          let new_head = { x = h.x + dir.x; y = h.y + dir.y } in
          let dx = new_head.x - t.x in
          let dy = new_head.y - t.y in
          let unit_size x =
            if x = 0 then 0 else if x > 0 then x / x else x / Int.abs x
          in
          let drag =
            if Int.abs dx > 1 || Int.abs dy > 1 then
              { x = unit_size dx; y = unit_size dy }
            else { x = 0; y = 0 }
          in
          let new_tail = { x = t.x + drag.x; y = t.y + drag.y } in
          let tmp = Forest.add new_tail sq f in
          if sq > 1 then move new_head new_tail (sq - 1) tmp
          else (tmp, new_head, new_tail)
        in
        move head tail num forest
      in
      loop input new_head new_tail new_forest
    with End_of_file ->
      close_in input;
      forest
  in
  let origin = { x = 0; y = 0 } in
  loop ic origin origin (Forest.add origin 0 Forest.empty)

let n = Forest.fold (fun _ v i -> if v > 0 then i + 1 else i) forest 0
let () = Printf.printf "Positions visited %i\n" n

let _ =
  for y = 0 to 5 do
    let () =
      for x = 0 to 5 do
        if Forest.mem { x; y } forest then
          Printf.printf "%i" (Forest.find { x; y } forest)
        else Printf.printf " "
      done
    in
    Printf.printf "\n"
  done
