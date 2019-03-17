%{
  module A = Ast
  open Support.Error
%}

%token <int Support.Error.withinfo> NUM
%token <string Support.Error.withinfo> ID 
%token <Support.Error.info> TRUE FALSE
%token <Support.Error.info> FOR IF ELSE
%token <Support.Error.info> INT BOOL
%token <Support.Error.info> MAIN
%token <Support.Error.info> RETURN
%token <Support.Error.info> SEMICOLON
%token <Support.Error.info> LPAREN RPAREN
%token <Support.Error.info> LBRACE RBRACE
%token EOF
%token <Support.Error.info> PLUS MINUS TIMES DIV
%token <Support.Error.info> AND OR
%token <Support.Error.info> NOT
%token <Support.Error.info> ASSIGN NEQ LT GT LEQ GEQ EQ

%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT LEQ GT GEQ
%left PLUS MINUS 
%left TIMES DIV 
%nonassoc UMINUS
%right NOT

%start <Ast.stmt option> prog 
%%

prog : 
  | EOF                                           { Some A.Nop }
  | INT; MAIN; LPAREN; RPAREN; s=block EOF        { Some s }
  ;

block : 
  i=LBRACE; s=stmt_list; RBRACE                   { A.Seq(s,i) }

rev_stmt_list : 
  | (* empty *)                                   { [] }
  | l=rev_stmt_list; s=stmt                       { s::l }
  ;

stmt_list : 
  | l=rev_stmt_list                               { List.rev l }
  ;

stmt : 
  | s=simp; SEMICOLON                             { s }
  | s=control                                     { s }
  | s=block                                       { s }
  ;

simpopt : 
  |  (* Nothing *)                                { A.Nop }
  |  s=simp                                       { s }
  ;

simp : 
  | e=exp                                         { A.Exp(e, A.extract_info_exp e) }
  | t=ty; id=ID                                   { let {v;i}=id in A.Decl(v,t, A.extract_info_type t) }
  | t=ty; id=ID; i=ASSIGN; e=exp                  { let {v;i}=id in A.Seq
                                                    ([A.Decl(v,t, A.extract_info_type t); 
                                                    A.Assign(v,e,i)], A.extract_info_type t) }
  | v=lvalue; i=ASSIGN; e=exp                     { A.Assign(v,e,i) }
  | i=RETURN; e=exp                               { A.Return(e,i) }
  ;

ty : 
  | i=INT                                         { A.Int i }
  | i=BOOL                                        { A.Bool i }
  ;

lvalue : 
  | vi=ID                                        { let {v;i}=vi in v }
  (*| LPAREN; id=lvalue; RPAREN                     { id }*)
  ;

exp : 
  | LPAREN; e=exp; RPAREN                         { e }
  | vi=NUM                                     { let {v;i}=vi in A.Intconst(v,i) }
  | i=TRUE                                        { A.True(i) }
  | i=FALSE                                       { A.False(i) }
  | vi=ID                                      { let {v;i}=vi in A.Var(v,i) }
  | i=NOT; e=exp                                  { A.Un(A.Not,e,i) }
  | i=MINUS; e=exp %prec UMINUS                   { A.Bin(A.Intconst(0,dummyinfo),A.Minus,e,i) }
  | e1=exp; op=bop; e2=exp                        { A.Bin(e1,fst op,e2,snd op) } 
  | e1=exp; i=LEQ; e2=exp                         { A.Un(A.Not, A.Bin(e1,A.Gt,e2,i), i) }
  | e1=exp; i=GEQ; e2=exp                         { A.Un(A.Not, A.Bin(e1,A.Lt,e2,i), i) }
  | e1=exp; i=NEQ; e2=exp                         { A.Un(A.Not, A.Bin(e1,A.Eq,e2,i), i) }
  ;

%inline bop :
  | i=PLUS                                        { A.Plus,i }
  | i=MINUS                                       { A.Minus,i }
  | i=TIMES                                       { A.Times,i }
  | i=DIV                                         { A.Div,i }
  | i=LT                                          { A.Lt,i }
  | i=GT                                          { A.Gt,i }
  | i=EQ                                          { A.Eq,i }
  | i=AND                                         { A.And,i }
  | i=OR                                          { A.Or,i }
  ;

control : 
  | i=IF; LPAREN; e=exp; RPAREN; s=stmt; 
    sop=elseopt                                   { A.If(e,s,sop,i) }
  | i=FOR; LPAREN; sop1=simpopt;
    SEMICOLON; e=exp; SEMICOLON; sop2=simpopt;
    RPAREN; s=stmt                                { A.Seq([sop1;A.While(e, A.Seq([s; sop2], A.extract_info_stmt s), i)], A.extract_info_stmt sop1) }
  ;

elseopt : 
  | ELSE; s=stmt                                  { Some(s) }
  | (* Nothing *)                                 { None }
  ;

