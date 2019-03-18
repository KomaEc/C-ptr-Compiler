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
  | EOF                                           { None }
  | INT; MAIN; LPAREN; RPAREN; s=block EOF        { Some s }
  ;

block : 
  LBRACE; s=stmt_list; RBRACE                   { s }
  ;
stmt_list :
  | (* nothing *)                                 { A.Seq([],dummyinfo) }
  | d=decl; sl=stmt_list                          { d sl }
  | s=stmt; sl=stmt_list                          
    { A.cons s (A.extract_info_stmt s) sl}
  ;

decl : 
  | t=ty; id=ID; SEMICOLON;
    { let {v;i}=id in 
      fun s -> A.Decl(v,t,s,i) }
  | t=ty; id=ID; ii=ASSIGN; e=exp; SEMICOLON;
    { let {v;i}=id in 
      fun s -> A.Decl(v,t,A.cons (A.Assign(v,e,ii)) ii s,i) }
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
  | vi=lvalue; ii=ASSIGN; e=exp                   { let {v;i}=vi in A.Assign(v,e,ii) }
  | i=RETURN; e=exp                               { A.Return(e,i) }
  ;

ty : 
  | i=INT                                         { A.Int i }
  | i=BOOL                                        { A.Bool i }
  ;

lvalue : 
  | vi=ID                                        { vi }
  | LPAREN; vi=lvalue; RPAREN                     { vi }
  ;

exp : 
  | vi=lvalue                                     { let {v;i}=vi in A.Var(v,i) }
  | LPAREN; e=exp; RPAREN                         { e }
  | vi=NUM                                        { let {v;i}=vi in A.Intconst(v,i) }
  | i=TRUE                                        { A.True(i) }
  | i=FALSE                                       { A.False(i) }
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
    RPAREN; s=stmt                                
    { A.Seq([sop1;A.While(e, A.Seq([s; sop2], A.extract_info_stmt s), i)], A.extract_info_stmt sop1) }
  | i=FOR; LPAREN; d=decl; e=exp; SEMICOLON; sop2=simpopt;
    RPAREN; s=stmt                                
    { d (A.While(e, A.Seq([s; sop2], A.extract_info_stmt s), i)) }

  ;

elseopt : 
  | ELSE; s=stmt                                  { Some(s) }
  | (* Nothing *)                                 { None }
  ;

