type coord = { x : int; y : int }

module Forest = Map.Make (struct
  type t = coord

  let compare = compare
end)

let forest =
  let ic = open_in "input" in
  let rec loop input worm forest =
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
      let () = Printf.printf "%i\n" num in
      let new_forest, new_worm =
        let move h t f =
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
          let tmp = Forest.add new_tail 1 f in
          (tmp, new_head, new_tail)
        in
        let rec process w f =
          match w with
          | hd :: nx :: tl ->
              let f, updated_hd, updated_nx = move hd nx f in
              let f, wrm = process (updated_nx :: tl) f in
              (f, updated_hd :: wrm)
          | hd :: [] -> (f, [ hd ])
          | [] -> (f, [])
        in
        let rec step n w f =
          let f, w = process w f in
          if n > 1 then step (n - 1) w f else (f, w)
        in
        step num worm forest
      in
      loop input new_worm new_forest
    with End_of_file ->
      close_in input;
      forest
  in
  let origin = { x = 0; y = 0 } in
  let worm = [ origin; origin ] in
  loop ic worm (Forest.add origin 0 Forest.empty)

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
