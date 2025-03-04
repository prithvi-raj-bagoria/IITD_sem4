open Printf
open Lexer

let () = 
  if Array.length Sys.argv > 1 then
    (* Read from file *)
    let filename = Sys.argv.(1) in
    try
      let channel = open_in filename in
      print_all_tokens channel;
      close_in channel
    with
      | Sys_error msg -> printf "Error opening file: %s\n" msg
  else
    (* Read from stdin if no file provided *)
    (* printf "Enter input (Ctrl+D to finish):\n"; *)
    print_all_tokens stdin;
;;