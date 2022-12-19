let () = print_endline "Hello, World!"

type quantity = { ore : int; clay : int; obsidian : int; geode : int }

type blueprint = {
  id : int;
  ore_robot : quantity;
  clay_robot : quantity;
  obsidian_robot : quantity;
  geode_robot : quantity;
}

let blueprints =
  let ic = open_in "input" in
  let rec loop input lst =
    try
      let line = input_line input in
      let tkn = Str.split (Str.regexp "[ :]+") line in
      let tkn =
        List.filter
          (fun test ->
            try
              let _ = int_of_string test in
              true
            with Failure _ -> false)
          tkn
      in
      let tkn = List.map int_of_string tkn in
      let r =
        match tkn with
        | id :: oo :: co :: no :: nc :: go :: gn :: _ ->
            {
              id;
              ore_robot = { ore = oo; clay = 0; obsidian = 0; geode = 0 };
              clay_robot = { ore = co; clay = 0; obsidian = 0; geode = 0 };
              obsidian_robot = { ore = no; clay = nc; obsidian = 0; geode = 0 };
              geode_robot = { ore = go; clay = 0; obsidian = gn; geode = 0 };
            }
        | _ -> assert false
      in
      r :: loop input lst
    with End_of_file ->
      close_in input;
      lst
  in
  loop ic []

type play =
  [ `BuyNothing
  | `BuyOreRobot
  | `BuyClayRobot
  | `BuyObsidianRobot
  | `BuyGeodeRobot ]

let rec search bp depth stock robots =
  if depth = 0 then stock
  else
(*
let () = if depth > 10 then Printf.printf "depth %i: Stock [ ore %i clay %i obsidian %i geode %i ] Robots [ ore %i clay %i obsidian %i geode %i ]\n" depth
stock.ore stock.clay stock.obsidian stock.geode 
robots.ore robots.clay robots.obsidian robots.geode  in *)
    let choices =
      List.filter
        (fun (choice : play) ->
          match choice with
          | `BuyNothing -> true
          | `BuyOreRobot -> stock.ore >= bp.ore_robot.ore
          | `BuyClayRobot -> stock.ore >= bp.clay_robot.ore
          | `BuyObsidianRobot ->
              stock.ore >= bp.obsidian_robot.ore
              && stock.clay >= bp.obsidian_robot.clay
          | `BuyGeodeRobot ->
              stock.ore >= bp.geode_robot.ore
              && stock.obsidian >= bp.geode_robot.obsidian)
        [
          `BuyNothing;
          `BuyOreRobot;
          `BuyClayRobot;
          `BuyObsidianRobot;
          `BuyGeodeRobot;
        ]
    in
    List.fold_left
      (fun acc choice ->
        match choice with
        | `BuyNothing ->
            let updated_stock =
              {
                ore = stock.ore + robots.ore;
                clay = stock.clay + robots.clay;
                obsidian = stock.obsidian + robots.obsidian;
                geode = stock.geode + robots.geode;
              }
            in
            let r = search bp (depth - 1) updated_stock robots in
            if r.geode > acc.geode then r else acc
        | `BuyOreRobot ->
            let updated_stock =
              {
                ore = stock.ore + robots.ore - bp.ore_robot.ore;
                clay = stock.clay + robots.clay;
                obsidian = stock.obsidian + robots.obsidian;
                geode = stock.geode + robots.geode;
              }
            in
            let updated_robots =
              {
                ore = robots.ore + 1;
                clay = robots.clay;
                obsidian = robots.obsidian;
                geode = robots.geode;
              }
            in
            let r = search bp (depth - 1) updated_stock updated_robots in
            if r.geode > acc.geode then r else acc
        | `BuyClayRobot ->
            let updated_stock =
              {
                ore = stock.ore + robots.ore - bp.clay_robot.ore;
                clay = stock.clay + robots.clay;
                obsidian = stock.obsidian + robots.obsidian;
                geode = stock.geode + robots.geode;
              }
            in
            let updated_robots =
              {
                ore = robots.ore;
                clay = robots.clay + 1;
                obsidian = robots.obsidian;
                geode = robots.geode;
              }
            in
            let r = search bp (depth - 1) updated_stock updated_robots in
            if r.geode > acc.geode then r else acc
        | `BuyObsidianRobot ->
            let updated_stock =
              {
                ore = stock.ore + robots.ore - bp.obsidian_robot.ore;
                clay = stock.clay + robots.clay - bp.obsidian_robot.clay;
                obsidian = stock.obsidian + robots.obsidian;
                geode = stock.geode + robots.geode;
              }
            in
            let updated_robots =
              {
                ore = robots.ore;
                clay = robots.clay;
                obsidian = robots.obsidian + 1;
                geode = robots.geode;
              }
            in
            let r = search bp (depth - 1) updated_stock updated_robots in
            if r.geode > acc.geode then r else acc
        | `BuyGeodeRobot ->
            let updated_stock =
              {
                ore = stock.ore + robots.ore - bp.geode_robot.ore;
                clay = stock.clay + robots.clay;
                obsidian =
                  stock.obsidian + robots.obsidian - bp.geode_robot.obsidian;
                geode = stock.geode + robots.geode;
              }
            in
            let updated_robots =
              {
                ore = robots.ore;
                clay = robots.clay;
                obsidian = robots.obsidian;
                geode = robots.geode + 1;
              }
            in
            let r = search bp (depth - 1) updated_stock updated_robots in
            if r.geode > acc.geode then r else acc)
      { ore = 0; clay = 0; obsidian = 0; geode = 0 }
      choices

(*
let jobs =
  List.fold_left
    (fun acc bp ->
      let () =  Printf.printf "%i: %i %i %i %i %i %i %i %i %i %i %i %i %i %i %i %i\n" bp.id
bp.ore_robot.ore bp.ore_robot.clay bp.ore_robot.obsidian bp.ore_robot.geode
bp.clay_robot.ore bp.clay_robot.clay bp.clay_robot.obsidian bp.clay_robot.geode
bp.obsidian_robot.ore bp.obsidian_robot.clay bp.obsidian_robot.obsidian bp.obsidian_robot.geode
bp.geode_robot.ore bp.geode_robot.clay bp.geode_robot.obsidian bp.geode_robot.geode in let () = flush stdout in
      acc @ [Domain.spawn (fun _ ->
          search bp 24
            { ore = 0; clay = 0; obsidian = 0; geode = 0 }
            { ore = 1; clay = 0; obsidian = 0; geode = 0 }) ])
    [] blueprints

let result =
  List.fold_left2
    (fun acc job bp ->
      let r = Domain.join job in
      let () =
        Printf.printf "%i: %i %i %i %i\n" bp.id r.ore r.clay r.obsidian r.geode
      in
      acc + (bp.id * r.geode))
    0 jobs blueprints

let () = Printf.printf "Part two: %i\n" result
*)

let blueprints = if List.length blueprints > 3 then
[(List.nth blueprints 0); (List.nth blueprints 1); (List.nth blueprints 2)]
else blueprints

let jobs =
  List.fold_left
    (fun acc bp ->
      let () = Printf.printf "%i: %i %i %i %i %i %i %i %i %i %i %i %i %i %i %i %i\n" bp.id
bp.ore_robot.ore bp.ore_robot.clay bp.ore_robot.obsidian bp.ore_robot.geode
bp.clay_robot.ore bp.clay_robot.clay bp.clay_robot.obsidian bp.clay_robot.geode
bp.obsidian_robot.ore bp.obsidian_robot.clay bp.obsidian_robot.obsidian bp.obsidian_robot.geode
bp.geode_robot.ore bp.geode_robot.clay bp.geode_robot.obsidian bp.geode_robot.geode in let () = flush stdout in
      acc @ [Domain.spawn (fun _ ->
          search bp 32
            { ore = 0; clay = 0; obsidian = 0; geode = 0 }
            { ore = 1; clay = 0; obsidian = 0; geode = 0 }) ])
    [] blueprints

let result =
  List.fold_left2
    (fun acc job bp ->
      let r = Domain.join job in
      let () =
        Printf.printf "%i: %i %i %i %i\n" bp.id r.ore r.clay r.obsidian r.geode
      in
      acc * r.geode)
    1 jobs blueprints

let () = Printf.printf "Part one: %i\n" result
