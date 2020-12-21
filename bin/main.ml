open List
open Syntax
open Codegen

exception Argv_Num

let get_filename () =
  let argv = Array.to_list Sys.argv in
  if length argv <> 2 then raise Argv_Num
  else let filename = List.nth (Array.to_list Sys.argv) 1
  in Core.Filename.realpath filename

let read_whole_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let src = get_filename () |> read_whole_file
let b = Parser.commands Lexer.token (Lexing.from_string src)
let cmds = List.map nasm_of_nask b

let main () = cmds |> string_of_commands |> print_string;;

main ()
