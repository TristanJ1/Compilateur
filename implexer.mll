{
open Impparser
exception Eof
let tbl = Hashtbl.create 17
let assoc = [
         ("if", IF);
         ("else", ELSE);
         ("int", INT);  
         ("bool", BOOL);
         ("void", VOID);
         ("while", WHILE);
         ("for", FOR);
         ("return", RETURN);
         ("putchar", PUTCHAR)]
let _ = List.iter (fun (s, tok) -> Hashtbl.add tbl s tok) assoc
}

let num = ['0'-'9']+

let var = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse
| [' ' '\t' '\n'] { token lexbuf }
| '+'             { PLUS }
| '*'             { MUL }
| '<'             { LT } 
| ';'         { SEMI }
| '{'         { LBRACKET }
| '}'         { RBRACKET }
| '('         { LPAR }
| ')'         { RPAR }
| '='         { EQ }
| ','         { COMMA }
| num as i        { VINT (int_of_string i) }
| var as s        { match Hashtbl.find_opt tbl s with
                | Some tok -> tok
            | None -> VARIABLE s }

| '-'             { MINUS }
| "true"          { VBOOL true }
| "false"         { VBOOL false }

| '/'             { DIV }
| "=="            { EQQ }
| "!="            { NEQ }
| "<="            { LEQ }
| ">="            { GEQ }
| '>'             { GT }
| "||"            { OR }
| "&&"            { AND }

| eof             { EOF }

