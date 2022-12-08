type coord = { x : int; y : int }

module Forest = Map.Make (struct
  type t = coord

  let compare = compare
end)

let forest =
  let ic = open_in "input" in
  let rec loop input row forest =
    try
      let line = input_line input in
      let chars = List.init (String.length line) (String.get line) in
      let indexed_chars =
        List.mapi (fun col ch -> (col, int_of_char ch - int_of_char '0')) chars
      in
      let new_forest =
        List.fold_left
          (fun f (col, height) -> Forest.add { x = col; y = row } height f)
          forest indexed_chars
      in
      loop input (row + 1) new_forest
    with End_of_file ->
      close_in input;
      forest
  in
  loop ic 0 Forest.empty

let visible_trees =
  Forest.fold
    (fun c h i ->
      let visible =
        List.fold_left
          (fun init vec ->
            let rec test_dir p =
              let q = { x = p.x + vec.x; y = p.y + vec.y } in
              if Forest.mem q forest then
                if h > Forest.find q forest then init || test_dir q else false
              else true
            in
            init || test_dir c)
          false
          [
            { x = 0; y = -1 };
            { x = 0; y = 1 };
            { x = -1; y = 0 };
            { x = 1; y = 0 };
          ]
      in
      if visible then i + 1 else i)
    forest 0

let () = Printf.printf "Visible trees: %i\n" visible_trees

let scenic_score =
  Forest.mapi
    (fun c h ->
      List.fold_left
        (fun init vec ->
          let rec test_dir p =
            let q = { x = p.x + vec.x; y = p.y + vec.y } in
            if Forest.mem q forest then
              if h > Forest.find q forest then 1 + test_dir q else 1
            else 0
          in
          init * test_dir c)
        1
        [
          { x = 0; y = -1 };
          { x = 0; y = 1 };
          { x = -1; y = 0 };
          { x = 1; y = 0 };
        ])
    forest

let most_scenic =
  Forest.fold (fun _ r acc -> if r > acc then r else acc) scenic_score 0

let () = Printf.printf "Most scenic: %i\n" most_scenic
