%{
    open Imp
%}
%token<int> VINT
%token<bool> VBOOL
%token<string> VARIABLE
%token MUL EOF
%token PLUS MINUS OR AND
%token LT GT EQQ NEQ LEQ GEQ
%token IF ELSE DIV 
%token INT BOOL VOID SEMI LBRACKET RBRACKET LPAR RPAR WHILE RETURN PUTCHAR EQ COMMA FOR

%left PLUS
%left MINUS
%left DIV
%left MUL
%left LT
%left GT
%left EQQ
%left NEQ
%left LEQ
%left GEQ
%left OR
%left AND

%start prog
%type <unit> prog
%%

decl:
  | t=typ s=VARIABLE EQ expr SEMI { (s,t) } 
  | t=typ s=VARIABLE SEMI { (s,t) } 
  | decl t=typ s=VARIABLE EQ expr SEMI { (s,t) } 
  | decl t=typ s=VARIABLE SEMI { (s,t) } 
;

fun_def:
  | t=typ v=VARIABLE LPAR p=seqparam RPAR LBRACKET d=list(decl) s=seq RBRACKET {
    {
      name = v;
      params = p;
      return = t;
      locals = d;
      code = s;
   } 
}
;

prog:
  | l=decl f=list(fun_def) EOF {
  } 
;

seqexpr:
  | s=separated_list(COMMA,expr) { s }
;

expr:
  | i=VINT { Cst i }
  | b=VBOOL { Bool b }
  | MINUS i=VINT { Cst (-i)}
  | e1=expr PLUS e2=expr { Add (e1,e2)}
  | e1=expr MINUS e2=expr { Minus (e1,e2)}
  | e1=expr MUL e2=expr { Mul (e1,e2)}
  | e1=expr DIV e2=expr { Div (e1,e2)}
  | e1=expr LT e2=expr { Lt (e1,e2)}
  | e1=expr GT e2=expr { Gt (e1,e2)}
  | e1=expr LEQ e2=expr { LessOrEqual (e1,e2)}
  | e1=expr GEQ e2=expr { GreaterOrEqual (e1,e2)}
  | e1=expr EQQ e2=expr { Equal (e1,e2)}
  | e1=expr NEQ e2=expr { NotEqual (e1,e2)}
  | e1=expr OR e2=expr { Or (e1,e2)}
  | e1=expr AND e2=expr { And (e1,e2)}
  | s=VARIABLE { Get s }
  | s=VARIABLE LPAR e=seqexpr RPAR { Call (s,e)}
;

typ:
  | INT { Int } 
  | BOOL { Bool }
  | VOID { Void }
;

seq:
  | l=list(instr) {l}
;

optionalexpr:
  | e=expr { e }
  |        { Cst 0 }
;

instr:
  | IF LPAR e=expr RPAR LBRACKET s1=seq RBRACKET ELSE LBRACKET s2=seq RBRACKET
    { If (e,s1,s2) }
  | WHILE LPAR e=expr RPAR LBRACKET s=seq RBRACKET
    { While (e,s) } 
  | FOR LPAR e=expr RPAR LBRACKET s=seq RBRACKET
    { While (e,s)}
  | RETURN e=optionalexpr SEMI
    { Return e }
  | PUTCHAR LPAR e=expr RPAR SEMI
    { Putchar e }
  | v=VARIABLE EQ e=expr SEMI
    { Set (v, e) }
  | e=expr { Expr e }
;

seqparam:
  | s=separated_list(COMMA,param) {s}
;

param:
 | t=typ s=VARIABLE { (s,t) }
;
