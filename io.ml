
exception Exit
exception Quit

let rec get_input () : string * string =
  print_endline "\nWhats next?";
  let input = String.lowercase (input_line stdin) in
  print_endline "\n";
  let cmd_list = Str.bounded_split (Str.regexp(" ")) (String.lowercase input) 2 in

  match cmd_list with
  | "quit"::_ -> raise Quit
  | "exit"::_ -> raise Exit
  | cmd::arg::[] -> cmd, arg
  | cmd::[] -> cmd, ""
  | _ -> print_endline "Invalid command\n"; get_input ()