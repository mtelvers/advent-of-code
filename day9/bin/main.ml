type coord = { x : int; y : int }

module Board = Map.Make (struct
  type t = coord

  let compare = compare
end)

let origin = { x = 0; y = 0 }

let play worm =
  let ic = open_in "input" in
  let rec loop input worm board =
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
      let new_board, new_worm =
        let rec process w vec b =
          match w with
          | hd :: nx :: tl ->
              let new_hd = { x = hd.x + vec.x; y = hd.y + vec.y } in
              let dx = new_hd.x - nx.x in
              let dy = new_hd.y - nx.y in
              let unit_size x =
                if x = 0 then 0 else if x > 0 then x / x else x / Int.abs x
              in
              let drag =
                if Int.abs dx > 1 || Int.abs dy > 1 then
                  { x = unit_size dx; y = unit_size dy }
                else { x = 0; y = 0 }
              in
              let new_nx = { x = nx.x + drag.x; y = nx.y + drag.y } in
              let tmp =
                if List.length tl = 0 then Board.add new_nx 1 b else b
              in
              let tmp2, wrm = process (new_nx :: tl) origin tmp in
              (tmp2, new_hd :: wrm)
          | hd :: [] -> (b, [ hd ])
          | [] -> (b, [])
        in
        let rec step n w b =
          let b, w = process w dir b in
          if n > 1 then step (n - 1) w b else (b, w)
        in
        step num worm board
      in
      loop input new_worm new_board
    with End_of_file ->
      close_in input;
      board
  in
  loop ic worm (Board.add origin 0 Board.empty)

let () =
  List.iter
    (fun size ->
      let game = play (List.init size (fun _ -> origin)) in
      let n = Board.fold (fun _ v i -> if v > 0 then i + 1 else i) game 0 in
      Printf.printf "Positions visited at length %i: %i\n" size n)
    [ 2; 10 ]
