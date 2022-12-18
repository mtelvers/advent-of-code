type coord = { x : int; y : int }

module Chamber = Map.Make (struct
  type t = coord

  let compare = compare
end)

let wind =
  let ic = open_in "input" in
  let rec loop input =
    try
      let line = input_line input in
      List.init (String.length line) (String.get line) @ loop input
    with End_of_file ->
      close_in input;
      []
  in
  loop ic

let rec gravity lst =
  match lst with hd :: tl -> hd :: 'v' :: gravity tl | [] -> []

let wind = gravity wind

let rocks =
  [
    [ { x = 0; y = 0 }; { x = 1; y = 0 }; { x = 2; y = 0 }; { x = 3; y = 0 } ];
    [
      { x = 1; y = 0 };
      { x = 0; y = 1 };
      { x = 1; y = 1 };
      { x = 2; y = 1 };
      { x = 1; y = 2 };
    ];
    [
      { x = 0; y = 0 };
      { x = 1; y = 0 };
      { x = 2; y = 0 };
      { x = 2; y = 1 };
      { x = 2; y = 2 };
    ];
    [ { x = 0; y = 0 }; { x = 0; y = 1 }; { x = 0; y = 2 }; { x = 0; y = 3 } ];
    [ { x = 0; y = 0 }; { x = 1; y = 0 }; { x = 0; y = 1 }; { x = 1; y = 1 } ];
  ]

(*
let draw c =
  for y = 10 downto 0 do
    Printf.printf "%2i: " y;
    for x = 0 to 7 do
      if Chamber.mem { x; y } c then
        Printf.printf "%c" (Chamber.find { x; y } c)
      else Printf.printf "."
    done;
    Printf.printf "\n"
  done;
  flush stdout
*)

let apply (chamber, rcks, start, nrocks) w =
  if nrocks = 0 then (chamber, [], { x = 0; y = 0 }, 0)
  else
    let possible rock here =
      List.fold_left
        (fun acc stone ->
          let pos = { x = here.x + stone.x; y = here.y + stone.y } in
          acc && 0 <= pos.x && pos.x < 7 && pos.y >= 0
          && Chamber.mem pos chamber = false)
        true rock
    in
    let new_pos =
      match w with
      | '<' -> { x = start.x - 1; y = start.y }
      | '>' -> { x = start.x + 1; y = start.y }
      | 'v' -> { x = start.x; y = start.y - 1 }
      | _ -> start
    in
    (* let () =
         Printf.printf "%i,%i %c %s %s %i\n" start.x start.y w
           (if possible (List.hd rcks) start then "true" else "false")
           (if possible (List.hd rcks) new_pos then "true" else "false")
           nrocks
       in
       let () =
         draw
           (List.fold_left
              (fun acc stone ->
                Chamber.add
                  { x = start.x + stone.x; y = start.y + stone.y }
                  '@' acc)
              chamber (List.hd rcks))
       in *)
    match
      (possible (List.hd rcks) start, possible (List.hd rcks) new_pos, w)
    with
    | true, true, _ -> (chamber, rcks, new_pos, nrocks)
    | true, false, '<' | true, false, '>' -> (chamber, rcks, start, nrocks)
    | true, false, 'v' ->
        let uc =
          List.fold_left
            (fun acc stone ->
              Chamber.add
                { x = start.x + stone.x; y = start.y + stone.y }
                '#' acc)
            chamber (List.hd rcks)
        in
        let max_y = Chamber.fold (fun k _ y -> max y k.y) uc 0 in
        ( uc,
          (if List.length rcks > 1 then List.tl rcks else rocks),
          { x = 2; y = max_y + 4 },
          nrocks - 1 )
    | true, false, _ -> assert false
    | false, _, _ -> (chamber, [], { x = 0; y = 0 }, 0)

let rec calculate ch ro st nr =
  let c, r, s, n = List.fold_left apply (ch, ro, st, nr) wind in
  if n = 0 then (c, n) else calculate c r s n

let chamber, _ = calculate Chamber.empty rocks { x = 2; y = 3 } 2022
let max_y = 1 + Chamber.fold (fun k _ y -> max y k.y) chamber 0
let () = Printf.printf "Height after 2022 rocks %i\n" max_y

let chamber, r, s, nrocks1 =
  List.fold_left apply
    (Chamber.empty, rocks, { x = 2; y = 3 }, 1_000_000_000_000)
    wind

let max_y1 = Chamber.fold (fun k _ y -> max y k.y) chamber 0
let chamber, _, _, nrocks2 = List.fold_left apply (chamber, r, s, nrocks1) wind
let max_y2 = Chamber.fold (fun k _ y -> max y k.y) chamber 0

let () =
  Printf.printf "Height after %i rocks %i\n" (nrocks1 - nrocks2)
    (max_y2 - max_y1)

let remaining_rocks = 1_000_000_000_000 mod (nrocks1 - nrocks2)
let chamber, _ = calculate Chamber.empty rocks { x = 2; y = 3 } remaining_rocks
let max_y = 1 + Chamber.fold (fun k _ y -> max y k.y) chamber 0
let () = Printf.printf "Height %i\n" max_y

let height =
  (1_000_000_000_000 / (nrocks1 - nrocks2) * (max_y2 - max_y1)) + max_y

let () = Printf.printf "Height %i\n" height
