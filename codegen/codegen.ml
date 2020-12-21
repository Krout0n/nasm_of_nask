open Syntax.Command

exception Unreachable
exception TODO

let nasm_of_nask command = match command with
  | Db _ | Dw _ | Dd _ -> command
  | Resb lit -> Times (lit, Db [(Decimal "0")])
  | _ -> raise Unreachable

let string_of_literal = function
  | Hex l | Decimal l -> l
  | HexWithDollar l -> l ^ "-($-$$)"
  | String s -> "\"" ^ s ^ "\""

let string_of_literals literals =
  let rec aux buf = function
    | [] -> buf
    | last :: [] -> 
      let s = string_of_literal last in
      aux (buf ^ s) []
    | head :: tail -> 
      let s = string_of_literal head in
      aux (buf ^ s ^ ", ") tail
  in aux "" literals

let rec string_of_command command = 
  let variant_of_command = function
    | Db _ -> "DB"
    | Dw _ -> "DW"
    | Dd _ -> "DD"
    | Resb _ -> "Resb"
    | Times _ -> "Times" in
  let s_cmd = variant_of_command command in
  match command with
    | Db args | Dw args | Dd args -> (
      let s_literals = string_of_literals args in
      s_cmd ^ " " ^ s_literals
    )
    | Times (lit, subcmd) -> (
      let s_lit = string_of_literal lit in
      let s_subcmd = string_of_command subcmd in
      s_cmd ^ " " ^ s_lit ^ " " ^ s_subcmd
    )
    | _ -> raise TODO

let string_of_commands commands =
  List.fold_left (fun acc cmd -> let s_cmd = string_of_command cmd in acc ^ s_cmd ^ "\n") "" commands
