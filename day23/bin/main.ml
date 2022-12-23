let () = print_endline "Hello, World!"

type coord = { x : int; y : int }

module Board = Map.Make (struct
  type t = coord

  let compare = compare
end)

let board =
  let ic = open_in "input" in
  let rec loop input y board =
    try
      let line = input_line input in
      let chars = List.init (String.length line) (String.get line) in
      let _, updated = List.fold_left (fun (x, b) c -> if c = '#' then (x + 1, Board.add { x; y } c b) else (x + 1, b)) (0, board) chars in
      loop input (y + 1) updated
    with End_of_file ->
      close_in input;
      board
  in
  loop ic 0 Board.empty

let draw b =
  let () = Printf.printf "543210123456789012345\n" in
  for y = -5 to 15 do
    for x = -5 to 15 do
      if Board.mem { x; y } b then
        let c = Board.find { x; y } b in
        Printf.printf "%c" c
      else Printf.printf "."
    done;
    Printf.printf "\n"
  done;
  flush stdout

let () = draw board
let north = [ { x = -1; y = -1 }; { x = 0; y = -1 }; { x = 1; y = -1 } ]
let south = [ { x = -1; y = 1 }; { x = 0; y = 1 }; { x = 1; y = 1 } ]
let west = [ { x = -1; y = 1 }; { x = -1; y = 0 }; { x = -1; y = -1 } ]
let east = [ { x = 1; y = 1 }; { x = 1; y = 0 }; { x = 1; y = -1 } ]

let step board options =
  let moveable =
    Board.filter
      (fun elf _ ->
        let neighbours =
          [
            { x = -1; y = -1 }; { x = 0; y = -1 }; { x = 1; y = -1 }; { x = -1; y = 0 }; { x = 1; y = 0 }; { x = -1; y = 1 }; { x = 0; y = 1 }; { x = 1; y = 1 };
          ]
        in
        List.fold_left (fun acc neighbour -> if Board.mem { x = elf.x + neighbour.x; y = elf.y + neighbour.y } board then acc + 1 else acc) 0 neighbours > 0)
      board
  in

  let () = draw moveable in

  let proposal =
    Board.mapi
      (fun elf _ ->
        List.fold_left
          (fun acc (neighbours, direction) ->
            if
              acc = '#'
              && List.fold_left
                   (fun acc neighbour -> if Board.mem { x = elf.x + neighbour.x; y = elf.y + neighbour.y } board then acc + 1 else acc)
                   0 neighbours
                 = 0
            then direction
            else acc)
          '#' options)
      moveable
  in

  let () = draw proposal in

  let destinations =
    Board.fold
      (fun elf v acc ->
        let new_position =
          match v with
          | 'N' -> { x = elf.x; y = elf.y - 1 }
          | 'S' -> { x = elf.x; y = elf.y + 1 }
          | 'W' -> { x = elf.x - 1; y = elf.y }
          | 'E' -> { x = elf.x + 1; y = elf.y }
          | _ -> elf
        in
        if Board.mem new_position acc then Board.add new_position 'N' acc else Board.add new_position 'Y' acc)
      proposal Board.empty
  in

  let () = draw destinations in

  let moved =
    Board.fold
      (fun elf v acc ->
        if Board.mem elf proposal then
          let dir = Board.find elf proposal in
          let new_position =
            match dir with
            | 'N' -> { x = elf.x; y = elf.y - 1 }
            | 'S' -> { x = elf.x; y = elf.y + 1 }
            | 'W' -> { x = elf.x - 1; y = elf.y }
            | 'E' -> { x = elf.x + 1; y = elf.y }
            | _ -> elf
          in
          let can_move = Board.find new_position destinations in
          if can_move = 'Y' then Board.add new_position '#' acc else Board.add elf '#' acc
        else Board.add elf '#' acc)
      board Board.empty
  in

  let () = draw moved in
  moved

let board = step board [ (north, 'N'); (south, 'S'); (west, 'W'); (east, 'E') ]
let board = step board [ (south, 'S'); (west, 'W'); (east, 'E'); (north, 'N') ]
let board = step board [ (west, 'W'); (east, 'E'); (north, 'N'); (south, 'S') ]
let board = step board [ (east, 'E'); (north, 'N'); (south, 'S'); (west, 'W') ]
let board = step board [ (north, 'N'); (south, 'S'); (west, 'W'); (east, 'E') ]
let board = step board [ (south, 'S'); (west, 'W'); (east, 'E'); (north, 'N') ]
let board = step board [ (west, 'W'); (east, 'E'); (north, 'N'); (south, 'S') ]
let board = step board [ (east, 'E'); (north, 'N'); (south, 'S'); (west, 'W') ]
let board = step board [ (north, 'N'); (south, 'S'); (west, 'W'); (east, 'E') ]
let board = step board [ (south, 'S'); (west, 'W'); (east, 'E'); (north, 'N') ]

let min_coord =
  let min_binding, _ = Board.min_binding board in
  Board.fold
    (fun k _ mi -> { x = min mi.x k.x; y = min mi.y k.y})
    board min_binding

let max_coord =
  let max_binding, _ = Board.max_binding board in
  Board.fold
    (fun k _ mx -> { x = max mx.x k.x; y = max mx.y k.y})
    board max_binding

let () = Printf.printf "min %i %i\n" min_coord.x min_coord.y
let () = Printf.printf "max %i %i\n" max_coord.x max_coord.y
let () = Printf.printf "area = %i, %i elves -> %i\n"
((1 + (max_coord.x - min_coord.x)) * (1 + (max_coord.y - min_coord.y))) (Board.cardinal board)
(((1 + (max_coord.x - min_coord.x)) * (1 + (max_coord.y - min_coord.y))) - (Board.cardinal board))
