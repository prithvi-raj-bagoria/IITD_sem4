(* main.ml - Driver program for Matrix/Vector DSL *)

open Ast
open Lexing
open Typecheck

(* Helper function to get position string for errors *)
let get_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  let line = pos.pos_lnum in
  let col = pos.pos_cnum - pos.pos_bol + 1 in
  Printf.sprintf "line %d, column %d" line col

(* Set filename in lexing buffer for error reporting *)
let set_filename lexbuf filename =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with pos_fname = filename }

(* Parse a file and return the AST *)
let parse_file filename =
  try
    (* Open the file and create a lexbuf *)
    let in_channel = open_in filename in
    let lexbuf = from_channel in_channel in
    
    (* Set the filename for error reporting *)
    set_filename lexbuf filename;
    
    (* Try to parse the program *)
    try
      let ast = Parser.program Lexer.token lexbuf in
      close_in in_channel;
      Printf.printf "Parsing of '%s' successful.\n" filename;
      ast
    with
    | Parser.Error ->
        close_in in_channel;
        Printf.eprintf "Syntax error at %s.\n" (get_position lexbuf);
        exit 1
    | Lexer.LexError msg ->
        close_in in_channel;
        Printf.eprintf "Lexical error at %s: %s\n" (get_position lexbuf) msg;
        exit 1
  with Sys_error msg ->
    Printf.eprintf "Error: %s\n" msg;
    exit 1

(* Type check program and report errors *)
let type_check_program ast =
  try
    typecheck_program ast;
    Printf.printf "Type checking successful.\n";
    true
  with Type_error msg ->
    Printf.eprintf "Type error: %s\n" msg;
    false

(* Main function to process a file *)
let process_file filename =
  let ast = parse_file filename in
  if type_check_program ast then
    Printf.printf "Program '%s' is valid.\n" filename
  else
    exit 1

(* Program entry point *)
let () =
  (* Check for command line arguments *)
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Usage: %s <filename>\n" Sys.argv.(0);
    exit 1
  end else
    (* Get the filename from command line arguments *)
    let filename = Sys.argv.(1) in
    process_file filename