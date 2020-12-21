%{
open Command
%}

%token DB DW DD RESB COMMA EOF
%token <string> HEX
%token <string> DECIMAL
%token <string> STRING
%token <string> HEX_WITH_DOLLAR

%start commands
%type <command list> commands
%%

commands:
    | c=command+ EOF {c}

command:
    | DB l = literals { Db l }
    | DW l = literals { Dw l }
    | DD l = literals { Dd l }
    | RESB l = literal { Resb l }

literals:
    | l = separated_list(COMMA, literal) { l }

literal:
    | d = DECIMAL { Decimal d }
    | h = HEX { Hex h }
    | s = STRING { String s }
    | h = HEX_WITH_DOLLAR { HexWithDollar h }
