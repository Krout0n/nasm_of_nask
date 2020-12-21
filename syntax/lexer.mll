{
open Parser
}

let digit = ['0'-'9']
let hex = digit | ['a'-'f' 'A'-'F']
let ws = ['\t' ' ' '\n']

rule token = parse
    | ws+ { token lexbuf }
    | "DB" { DB }
    | "DW" { DW }
    | "DD" { DD }
    | "RESB" { RESB }
    | "0x"hex+ as h"-$" { HEX_WITH_DOLLAR h }
    | "0x"hex+ as h { HEX h }
    | digit+ as n { DECIMAL n }
    | "," { COMMA }
    | ";" { comment lexbuf }
    | "\"" { str_literal (Buffer.create 0) lexbuf }
    | eof { EOF }
and comment = parse
    | "\n" { token lexbuf }
    | _ { comment lexbuf }
and str_literal buf = parse
    | '"'       { STRING (Buffer.contents buf) }
    | _ as s { Buffer.add_char buf s; str_literal buf lexbuf }
