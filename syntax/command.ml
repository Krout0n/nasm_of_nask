(* nask と nasm 共に同じ所に入れている、本当は分けたほうがいいんだけど共通部分があってめんどい... *)
type command =
  | Db of literal list
  | Dw of literal list
  | Dd of literal list
  | Resb of literal
  (* nasm のみの命令 *)
  | Times of literal * command
and literal =
  | Hex of string
  | Decimal of string
  | String of string
  | HexWithDollar of string [@@ deriving show, eq]

(* デバッグ用に入れてる *)
module Debug = struct
  let flush_commands commands = (
    let rec aux = function
      | [] -> ""
      | command :: [] -> show_command command
      | head :: tail -> show_command head ^ ", " ^ aux tail in
    print_string "[";
    aux commands |> print_string;
    print_string "]"
  )

  let flush_literals literals = List.iter (fun lit -> lit |> show_literal |> print_endline) literals
end
