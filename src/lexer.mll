{
  open Parser;;
  exception Lexical_error;;
}

rule token = parse
    [' ' '\t']  { token lexbuf }
  | "lambda"    { LAMBDA }
  | "L"         { LAMBDA }
  | "true"      { TRUE }
  | "false"     { FALSE }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "succ"      { SUCC }
  | "pred"      { PRED }
  | "iszero"    { ISZERO }
  | "let"       { LET }
  | "letrec"    { LETREC }
  | "quit"      { QUIT }
  | "exit"      { QUIT }
  | "in"        { IN }
  | "Bool"      { BOOL }
  | "Nat"       { NAT }
  | "String"    { TSTRING }
  | "clear" { CLEAR }
  | "nil"   { NIL }
  | "cons"  { CONS}
  | "isnil" { ISNIL }
  | "head"  { HEAD }
  | "tail"  { TAIL }
  | "List"  { LIST }
  | "case" { CASE }
  | "of"   { OF }
  | "as"   { AS }

  | '('         { LPAREN }
  | ')'         { RPAREN }
  | '.'         { DOT }
  | "=>"     { EQARROW }
  | '='         { EQ }
  | ':'         { COLON }
  | "->"        { ARROW }
  | "^"         { CONCAT }
  | '{'         { LCURLYB }
  | '}'         { RCURLYB }
  | '['         { LB }
  | ']'         { RB }
  | ','         { COMMA }
  | '<'      { LT }
  | '>'      { GT }
  | '|'      { BAR }



  | eof         { EOF }


  | '"' [^ '"' ]* '"' as s
        { STRING (String.sub s 1 (String.length s - 2)) }

  | ['0'-'9']+  { INTV (int_of_string (Lexing.lexeme lexbuf)) }

  | ['a'-'z']['a'-'z' 'A'-'Z' '_' '0'-'9']*
                { VAR_ID (Lexing.lexeme lexbuf) } (*terms bindings*)

  | ['A'-'Z'] ['A'-'Z' 'a'-'z' '_' '0'-'9']* (*mandatory A-Z at first letter for types binding*)
      { TYPE_ID (Lexing.lexeme lexbuf) }

  | _           { raise Lexical_error }
