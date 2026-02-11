%{
  open Terms
  open Types
  open Context
%}

%token LETREC
%token QUIT
%token LAMBDA
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token SUCC
%token PRED
%token ISZERO
%token LET
%token IN
%token BOOL
%token NAT
%token TSTRING
%token CONCAT
%token CLEAR
%token NIL
%token CONS
%token ISNIL
%token HEAD
%token TAIL
%token LIST
%token CASE
%token OF
%token AS

%token EOF

%token LPAREN
%token RPAREN
%token DOT
%token EQ
%token COLON
%token ARROW
%token LCURLYB RCURLYB COMMA RB LB
%token LT GT BAR EQARROW


%token <string> STRING
%token <int> INTV
%token <string> VAR_ID   (* id for vars *)
%token <string> TYPE_ID  (* id for types, mandatory A-Z first letter *)

(* Precedence declarations *)
%right ARROW

%start s
%type <Lambda.command> s
%type <Lambda.term> term
%type <Lambda.term> concat_term
%type <Lambda.term> appTerm
%type <Lambda.term> atomicTerm
%type <Lambda.ty> ty
%type <Lambda.ty> atomicTy
%type <Lambda.term list> tuple_terms
%type <Lambda.term list> more_terms
%type <Lambda.ty list> tuple_types
%type <Lambda.ty list> more_types
%type <(string * Lambda.term) list> recordFields
%type <(string * Lambda.ty) list> recordTyFields
%type <(string * Lambda.ty) list> variantTyFields
%type <string * string * Lambda.term> variantCase
%type <(string * string * Lambda.term) list> variantCases


%%

s:
    TYPE_ID EQ ty EOF
        { TypeBind ($1, $3) }        (* X = Nat, etc. *)

  | VAR_ID EQ term EOF
        { Bind ($1, $3) }            (* x = true, x = 5, ... *)

  | term EOF
        { Eval $1 }

  | CLEAR EOF
        { Clear }

  | QUIT EOF
        { Quit }

;

(* Split term into two levels: term handles CONCAT (lowest precedence), concat_term handles binding constructs *)
term :
    concat_term
        { $1 }

  | term CONCAT concat_term
        { TmConcat ($1, $3) }

;

concat_term :
    appTerm
        { $1 }

  | IF concat_term THEN concat_term ELSE concat_term
        { TmIf ($2, $4, $6) }

  | LAMBDA VAR_ID COLON ty DOT concat_term
        { TmAbs ($2, $4, $6) }

  | LET VAR_ID EQ term IN concat_term
        { TmLetIn ($2, $4, $6) }

  | LETREC VAR_ID COLON ty EQ term IN concat_term
        { TmLetIn ($2, TmFix (TmAbs ($2, $4, $6)), $8) }

  | CASE concat_term OF variantCases
        { TmCase ($2, $4) }


;

appTerm :
    atomicTerm
        { $1 }

  | SUCC atomicTerm
        { TmSucc $2 }

  | PRED atomicTerm
        { TmPred $2 }

  | ISZERO atomicTerm
        { TmIsZero $2 }

  | NIL LB ty RB
        { TmNil $3 }

  (* Changed CONS to take atomicTerm for second argument instead of concat_term to eliminate shift/reduce conflict *)
  | CONS LB ty RB atomicTerm atomicTerm
        { TmCons ($3, $5, $6) }

  | ISNIL LB ty RB atomicTerm
        { TmIsNil ($3, $5) }

  | HEAD LB ty RB atomicTerm
        { TmHead ($3, $5) }

  | TAIL LB ty RB atomicTerm
        { TmTail ($3, $5) }

  | appTerm DOT INTV
        { TmProj ($1, $3) }

  | appTerm DOT VAR_ID
        { TmProjRecord ($1, $3) }

  | appTerm atomicTerm
        { TmApp ($1, $2) }

;

atomicTerm :
    LPAREN term RPAREN
        { $2 }

  | TRUE
        { TmTrue }

  | FALSE
        { TmFalse }

  | VAR_ID
        { TmVar $1 }

  | INTV
        { let rec f = function 0 -> TmZero | n -> TmSucc (f (n-1)) in f $1 }

  | STRING
        { TmString $1 }

  (* records de términos: { x = t1, y = t2, ... } *)
  | LCURLYB recordFields RCURLYB
        { TmRecord $2 }

  (* tuplas de términos: { t1, t2, ... } - al menos 2 elementos *)
  | LCURLYB tuple_terms RCURLYB
        { TmTupl $2 }

  | LT VAR_ID EQ term GT AS atomicTy
        { TmTag ($2, $4, $7) }


;

ty :
    ty ARROW ty
        { TyArr ($1, $3) }

  | LIST LB ty RB
        { TyList $3 }

  | atomicTy
        { $1 }

;

atomicTy :
    LPAREN ty RPAREN
        { $2 }

  | BOOL
        { TyBool }

  | NAT
        { TyNat }

  | TSTRING
        { TyString }

  | TYPE_ID
        { TyName $1 }

  (* tuplas de tipos: { T1, T2, ... } *)
  | LCURLYB tuple_types RCURLYB
        { TyTupl $2 }

  (* records de tipos: { x : T1, y : T2, ... } *)
  | LCURLYB recordTyFields RCURLYB
        { TyRecord $2 }

  | LT variantTyFields GT
        { TyVariant $2 }

;


tuple_terms :
    term COMMA more_terms
        { $1 :: $3 }

more_terms :
    term
        { [$1] }
  | term COMMA more_terms
        { $1 :: $3 }

tuple_types :
    ty COMMA more_types
        { $1 :: $3 }

more_types :
    ty
        { [$1] }
  | ty COMMA more_types
        { $1 :: $3 }

recordFields :
    VAR_ID EQ term
        { [($1, $3)] }
  | VAR_ID EQ term COMMA recordFields
        { ($1, $3) :: $5 }

recordTyFields :
    VAR_ID COLON ty
        { [($1, $3)] }
  | VAR_ID COLON ty COMMA recordTyFields
        { ($1, $3) :: $5 }

variantTyFields :
    VAR_ID COLON ty
        { [($1, $3)] }

  | VAR_ID COLON ty COMMA variantTyFields
        { ($1, $3) :: $5 }

variantCases :
    variantCase
        { [$1] }

  | variantCase BAR variantCases
        { $1 :: $3 }

variantCase :
    LT VAR_ID EQ VAR_ID GT EQARROW concat_term
        { ($2, $4, $7) }


%%
