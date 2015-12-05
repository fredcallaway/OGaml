
open Printf
let rec get_input () : string * string =
  print_endline "\nWhats next?";
  let input = String.lowercase (input_line stdin) in
  print_endline "\n";
  let cmd_list = Str.bounded_split (Str.regexp(" ")) (String.lowercase input) 2 in

  match cmd_list with
  | "quit"::_ -> failwith "how do we quit?"
  | cmd::arg::[] -> cmd, arg
  | cmd::[] -> cmd, ""
  | _ -> print_endline "Invalid command\n"; get_input ()

let rec print_map_line str =
  let map_size = 34 in
  let len = String.length str in
  let trimmed, rest = if len > map_size then (String.sub str 0 map_size),
  (String.sub str map_size (len-map_size)) else str, "" in
  let padding = if len > map_size then "" else String.make (map_size-len) ' ' in
  printf "  |  %s  |  \n" (trimmed^padding);
  if rest = "" then () else print_map_line rest

let print_map_header title =
  printf "   ________________________________________ \n";
  printf "  { }______________________________________}\n";
  printf "  |                                      |  \n";
  printf "  |                                      |  \n";
  print_map_line ("#######  "^title^"  #######");
  printf "  |                                      |  \n"

let print_map_footer () =
  printf "  |                                      |  \n";
  printf "{_}______________________________________/  \n"