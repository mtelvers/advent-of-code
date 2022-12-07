let files = Hashtbl.create 1

let () =
  let ic = open_in "input" in
  let rec loop input path =
    try
      let line = input_line input in
      let tokens = String.split_on_char ' ' line in
      match tokens with
      | "$" :: "cd" :: ".." :: _ ->
          let new_path =
            let fldrs = String.split_on_char '/' path in
            match fldrs with _ :: tl -> String.concat "/" tl | [] -> ""
          in
          let p = Hashtbl.find files path in
          let n = Hashtbl.find files new_path in
          let () = Hashtbl.replace files new_path (n + p) in
          loop input new_path
      | "$" :: "cd" :: fldr :: _ ->
          let new_path = fldr ^ "/" ^ path in
          let () = Hashtbl.add files new_path 0 in
          loop input new_path
      | "$" :: "ls" :: _ -> loop input path
      | "dir" :: _ :: _ -> loop input path
      | length :: _ :: _ ->
          let l = int_of_string length in
          let c = Hashtbl.find files path in
          let () = Hashtbl.replace files path (c + l) in
          loop input path
      | _ -> loop input path
    with End_of_file ->
      let rec up path =
        let new_path =
          let fldrs = String.split_on_char '/' path in
          match fldrs with _ :: tl -> String.concat "/" tl | [] -> ""
        in
        let p = Hashtbl.find files path in
        let n = Hashtbl.find files new_path in
        let () = Hashtbl.replace files new_path (n + p) in
        if String.length new_path > 2 then up new_path
      in
      let () = up path in
      close_in input
  in
  loop ic ""

let total =
  Hashtbl.fold
    (fun _ size init -> if size <= 100000 then init + size else init)
    files 0

let () = Printf.printf "Part one total: %i\n" total
let available = 70_000_000 - Hashtbl.find files "//"
let needed = 30_000_000 - available

let delete_me =
  Hashtbl.fold
    (fun _ size init ->
      if size > needed then if size < init then size else init else init)
    files 70_000_000

let () = Printf.printf "Part two total: %i\n" delete_me
