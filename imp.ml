open Format

type typ =
  | Int
  | Bool
  | Void

type expr =
  | Cst  of int
  | Bool of bool
  | Add  of expr * expr
  | Mul  of expr * expr
  | Div  of expr * expr
  | Lt   of expr * expr
  | Gt   of expr * expr
  | LessOrEqual of expr * expr
  | GreaterOrEqual of expr * expr
  | Equal of expr * expr
  | NotEqual of expr * expr
  | Or of expr * expr
  | And of expr * expr
  | Get  of string
  | Call of string * expr list
  | Minus of expr * expr


type instr =
  | Putchar of expr
  | Set     of string * expr
  | If      of expr * seq * seq
  | While   of expr * seq
  | Return  of expr
  | Expr    of expr
and seq = instr list

type fun_def = {
  name:   string;
  params: (string * typ) list;
  return: typ;
  locals: (string * typ) list;
  code:   seq;
}

type prog = {
  globals:   (string * typ) list;
  functions: fun_def list;
}

exception TypeError

let rec type_expr e = match e with 
  | Cst _ -> Int
  | Bool _ -> Bool
  | Add (e1,e2) ->
      if type_expr e1 = Int
      && type_expr e2 = Int
      then Int
      else raise TypeError
  | Mul (e1,e2) ->
      if type_expr e1 = Int
      && type_expr e2 = Int
      then Int
      else raise TypeError
  | Minus (e1,e2) ->
      if type_expr e1 = Int
      && type_expr e2 = Int
      then Int
      else raise TypeError
  | Div (e1,e2) ->
      if type_expr e1 = Int
      && type_expr e2 = Int
      then Bool
      else raise TypeError
  | Lt (e1,e2) ->
      if type_expr e1 = Int
      && type_expr e2 = Int
      then Bool
      else raise TypeError
  | Gt (e1,e2) ->
      if type_expr e1 = Int
      && type_expr e2 = Int
      then Bool
      else raise TypeError
  | LessOrEqual (e1,e2) ->
      if type_expr e1 = Int
      && type_expr e2 = Int
      then Bool
      else raise TypeError
  | GreaterOrEqual (e1,e2) ->
      if type_expr e1 = Int
      && type_expr e2 = Int
      then Bool
      else raise TypeError
  | Equal (e1,e2) ->
      if type_expr e1 = Int
      && type_expr e2 = Int
      then Bool
      else raise TypeError
  | NotEqual (e1,e2) ->
      if type_expr e1 = Int
      && type_expr e2 = Int
      then Bool
      else raise TypeError
  | Or (e1,e2) ->
      if type_expr e1 = Bool
      && type_expr e2 = Bool
      then Bool
      else raise TypeError
  | And (e1,e2) ->
      if type_expr e1 = Bool
      && type_expr e2 = Bool
      then Bool
      else raise TypeError
  | _ -> raise TypeError 


let rec affiche_fonctions f = List.iter affiche_fonction f

and affiche_instrs l = List.iter affiche_instr l 

and affiche_instr = function 
  | Putchar e -> print_string(" putchar " ^ affiche e) 
  | Set (s,e) -> print_string( s ^ " = " ^ affiche e)
  | If (e,s1,s2) -> print_string(" if ( " ^ affiche e ^ " ) { "); affiche_instrs s1; print_string(" } else { "); affiche_instrs s2; print_string(" } ")
  | While (e,s) -> print_string(" while (" ^ affiche e ^ " ) {"); affiche_instrs s; print_string(" }")
  | Return e -> print_string(" return " ^ affiche e) 
  | Expr e -> print_string(affiche e) 
 

and affiche = function
  | Cst n -> string_of_int n
  | Bool b -> string_of_bool b
  | Add (e1,e2) -> affiche e1 ^ " + " ^ affiche e2
  | Mul (e1,e2) -> affiche e1 ^ " * " ^ affiche e2
  | Minus (e1,e2) -> affiche e1 ^ " - " ^ affiche e2
  | Div (e1,e2) -> affiche e1 ^ " / " ^ affiche e2
  | Lt (e1,e2) -> affiche e1 ^ " < " ^ affiche e2
  | Gt (e1,e2) -> affiche e1 ^ " > " ^ affiche e2
  | LessOrEqual (e1,e2) -> affiche e1 ^ " <= " ^ affiche e2
  | GreaterOrEqual (e1,e2) -> affiche e1 ^ " >= " ^ affiche e2
  | Equal (e1,e2) -> affiche e1 ^ " = " ^ affiche e2
  | NotEqual (e1,e2) -> affiche e1 ^ " != " ^ affiche e2
  | Or (e1,e2) -> affiche e1 ^ " || " ^ affiche e2
  | And (e1,e2) -> affiche e1 ^ " && " ^ affiche e2
  | Get e -> e
  | Call (s,el) -> s ^ "( " ^ " )" 

and affiche_params p = List.iter affiche_param p

and affiche_param = function
  | (s,t) -> print_string (affiche_typ t ^ " " ^ s)

and affiche_typ = function
  | Int -> "int"
  | Bool -> "bool"
  | Void -> "void"
 

and affiche_fonction = function
  | { name = v; params = p; return = t; locals = d; code = s; } ->
    print_string (affiche_typ t ^ " " ^ v ^ "( "); affiche_params p; print_string " ) { "; affiche_params d; affiche_instrs s; print_string " } "   

