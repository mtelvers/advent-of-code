let () = print_endline "Hello, World!"

type quantity = { ore : int; clay : int; obsidian : int; geode : int }
type blueprint = { id : int; ore_robot : quantity; clay_robot : quantity; obsidian_robot : quantity; geode_robot : quantity }

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

type play = [ `BuyNothing | `BuyOreRobot | `BuyClayRobot | `BuyObsidianRobot | `BuyGeodeRobot ]

let rec search bp depth stock robots max_qty path =
  (*  let () = Printf.printf "%s: %i\n" path stock.geode in *)
  if depth = 0 then stock
  else
    let choices =
      List.filter
        (fun (choice : play) ->
          match choice with
          | `BuyNothing -> false
          | `BuyOreRobot -> stock.ore >= bp.ore_robot.ore && robots.ore <= max_qty.ore
          | `BuyClayRobot -> stock.ore >= bp.clay_robot.ore && robots.clay <= max_qty.clay
          | `BuyObsidianRobot -> stock.ore >= bp.obsidian_robot.ore && stock.clay >= bp.obsidian_robot.clay && robots.obsidian <= max_qty.obsidian
          | `BuyGeodeRobot -> stock.ore >= bp.geode_robot.ore && stock.obsidian >= bp.geode_robot.obsidian)
        [ `BuyOreRobot; `BuyClayRobot; `BuyObsidianRobot; `BuyGeodeRobot ]
    in
    let choices = if List.length choices = 0 then [ `BuyNothing ] else choices in
    let best =
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
              let r = search bp (depth - 1) updated_stock robots max_qty (path ^ "N") in
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
              let updated_robots = { ore = robots.ore + 1; clay = robots.clay; obsidian = robots.obsidian; geode = robots.geode } in
              let r = search bp (depth - 1) updated_stock updated_robots max_qty (path ^ "O") in
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
              let updated_robots = { ore = robots.ore; clay = robots.clay + 1; obsidian = robots.obsidian; geode = robots.geode } in
              let r = search bp (depth - 1) updated_stock updated_robots max_qty (path ^ "C") in
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
              let updated_robots = { ore = robots.ore; clay = robots.clay; obsidian = robots.obsidian + 1; geode = robots.geode } in
              let r = search bp (depth - 1) updated_stock updated_robots max_qty (path ^ "0") in
              if r.geode > acc.geode then r else acc
          | `BuyGeodeRobot ->
              let updated_stock =
                {
                  ore = stock.ore + robots.ore - bp.geode_robot.ore;
                  clay = stock.clay + robots.clay;
                  obsidian = stock.obsidian + robots.obsidian - bp.geode_robot.obsidian;
                  geode = stock.geode + robots.geode;
                }
              in
              let updated_robots = { ore = robots.ore; clay = robots.clay; obsidian = robots.obsidian; geode = robots.geode + 1 } in
              let r = search bp (depth - 1) updated_stock updated_robots max_qty (path ^ "G") in
              if r.geode > acc.geode then r else acc)
        { ore = 0; clay = 0; obsidian = 0; geode = 0 } choices
    in
    best

let result =
  List.fold_left
    (fun acc bp ->
      let max_qty =
        {
          ore = max (max bp.ore_robot.ore bp.clay_robot.ore) (max bp.obsidian_robot.ore bp.geode_robot.ore);
          clay = max (max bp.ore_robot.clay bp.clay_robot.clay) (max bp.obsidian_robot.clay bp.geode_robot.clay);
          obsidian = max (max bp.ore_robot.obsidian bp.clay_robot.obsidian) (max bp.obsidian_robot.obsidian bp.geode_robot.obsidian);
          geode = max (max bp.ore_robot.geode bp.clay_robot.geode) (max bp.obsidian_robot.geode bp.geode_robot.geode);
        }
      in
      let r = search bp 24 { ore = 0; clay = 0; obsidian = 0; geode = 0 } { ore = 1; clay = 0; obsidian = 0; geode = 0 } max_qty "" in
      let () = Printf.printf "%i: %i %i %i %i\n" bp.id r.ore r.clay r.obsidian r.geode in
      let () = flush stdout in
      acc + (bp.id * r.geode))
    0 blueprints

let () = Printf.printf "Part one: %i\n" result
let blueprints = if List.length blueprints > 3 then [ List.nth blueprints 0; List.nth blueprints 1; List.nth blueprints 2 ] else blueprints

let result =
  List.fold_left
    (fun acc bp ->
      let max_qty =
        {
          ore = max (max bp.ore_robot.ore bp.clay_robot.ore) (max bp.obsidian_robot.ore bp.geode_robot.ore);
          clay = max (max bp.ore_robot.clay bp.clay_robot.clay) (max bp.obsidian_robot.clay bp.geode_robot.clay);
          obsidian = max (max bp.ore_robot.obsidian bp.clay_robot.obsidian) (max bp.obsidian_robot.obsidian bp.geode_robot.obsidian);
          geode = max (max bp.ore_robot.geode bp.clay_robot.geode) (max bp.obsidian_robot.geode bp.geode_robot.geode);
        }
      in
      let r = search bp 32 { ore = 0; clay = 0; obsidian = 0; geode = 0 } { ore = 1; clay = 0; obsidian = 0; geode = 0 } max_qty "" in
      let () = Printf.printf "%i: %i %i %i %i\n" bp.id r.ore r.clay r.obsidian r.geode in
      let () = flush stdout in
      acc * r.geode)
    1 blueprints

let () = Printf.printf "Part two: %i\n" result
