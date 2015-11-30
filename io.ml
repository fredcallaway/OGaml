
let rec get_input () : string * string =
  print_endline "\nWhats next?";
  let input = String.lowercase (input_line stdin) in
  let () = print_endline "\n" in
  let cmd_list = Str.bounded_split (Str.regexp(" ")) (String.lowercase input) 2 in

  match cmd_list with
  | cmd::arg::[] -> cmd, arg
  | cmd::[] -> cmd, ""
  | _ -> print_endline "Invalid command\n"; get_input ()