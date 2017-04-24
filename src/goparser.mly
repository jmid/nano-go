%{
(*  let start_pos = Parsing.symbol_start_pos *)

(*	  
  let token_var = ref 0
  let token_pp = ref 0
   
  let find var_tab s = 
    try Hashtbl.find var_tab s 
    with Not_found -> 
      Hashtbl.add var_tab s !token_var;
      let p = !token_var in
	incr token_var;
	p

  let new_pp () = 
    let p = !token_pp in
      incr token_pp;
      p
  let vars_used var_tab = Hashtbl.fold (fun s p l -> p::l) var_tab []
*)
%}

%start prog
%type < Ast.exprog > prog

%%

prog:
 | PKG MAIN
   FUNC MAIN LPAR RPAR LBRACE chdecls funcs body RBRACE EOF { $8,$9,$10 }
 ;

chdecls:
 | IDENT COLONEQ MAKE LPAR CHAN INT RPAR opt_semi         { [$1] }
 | IDENT COLONEQ MAKE LPAR CHAN INT RPAR opt_semi chdecls { $1::$9 }
 ;

funcs:
 | GO FUNC LPAR RPAR brbody LPAR RPAR       { [$5] }
 | GO FUNC LPAR RPAR brbody LPAR RPAR funcs { $5::$8 }
 ;

body:  /* -> exproc */
 | VAR IDENT INT opt_semi body   { let vars,stmts = $5 in ($2::vars),stmts }
 | stmtlist                      { [],$1 }
;

brbody:  /* -> exproc */
 | LBRACE body RBRACE   { $2 }
 | LBRACE RBRACE        { [],[] }
 ;

block:  /* -> exblock */
/* | stmt                    { [$1] } */
 | LBRACE stmtlist RBRACE  { $2 }
 | LBRACE RBRACE           { [] }
 ;

opt_semi: 
 | SEMI {}
 |      {}
 ;

stmtlist: 
 | stmt opt_semi            { [$1] }
 | stmt SEMI stmtlist       { ($1::$3) }
 | LBRACE stmtlist RBRACE   { $2 }
 ;

opt_stmtlist:
 |             { [] }
 | stmtlist    { $1 }
;

expr:
 | INTLIT         { Ast.Num $1 }
 | LPAR expr RPAR { $2 }
 | IDENT          { Ast.Var $1}
 | SUB expr       { Ast.Binop (Ast.Num 0,Ast.Minus,$2) }
 | expr ADD expr  { Ast.Binop ($1,Ast.Plus,$3) }
 | expr SUB expr  { Ast.Binop ($1,Ast.Minus,$3) }
 | expr MULT expr { Ast.Binop ($1,Ast.Mult,$3) }
 | expr MOD expr  { Ast.Binop ($1,Ast.Mod,$3) }
 ;

test:
 | TRUE           { Ast.True }
 | FALSE          { Ast.False }
 | BANG test       { Ast.Not $2 }
 | LPAR test RPAR { $2 }
 | expr comp expr { Ast.Relop ($1,$2,$3) }
 | test AND test  { Ast.Conj ($1,$3) }
/* | test OR test   { Ast.Or ($1,$3) } */
 ;

comp:
 | EQ     { Ast.Eq }
/* | NEQ    { Ast.Neq } */
 | LE     { Ast.Leq }
 | LT     { Ast.Lt }
 ;

stmt:
 | IDENT ASSIGN expr                   { let pos = $startpos in
					 Ast.ExAssign (pos,$1,$3) }
 | recvstmt                            { let pos = $startpos in
					 let (var,ch) = $1 in Ast.ExChread (pos,var,ch) }
 | sendstmt                            { let pos = $startpos in
					 let (ch,exp) = $1 in Ast.ExChwrite (pos,ch,exp) }
 | IF test block ELSE block            { let pos = $startpos in
					 Ast.ExIf (pos,$2,$3,$5) }
 | FOR test block                      { let pos = $startpos in
					 Ast.ExFor (pos,$2,$3) }
 | FOR block                           { let pos = $startpos in
					 Ast.ExFor (pos,Ast.True,$2) }
 | SELECT LBRACE caselist RBRACE       { let pos = $startpos in
					 Ast.ExSelect (pos,$3) }
 | PRINT LPAR expr RPAR                { let pos = $startpos in
					 Ast.ExPrint (pos,$3) }
;

recvstmt:
 | IDENT ASSIGN ARROW IDENT { (Some $1,$4) }
 |              ARROW IDENT { (None,$2) }
;

sendstmt: IDENT ARROW expr        { ($1,$3) }
;

commcase:
 | CASE recvstmt                 { let pos = $startpos in
				   fun is -> let (var,ch) = $2 in (pos,Ast.ExReadCase (var,ch,is)) }
 | CASE sendstmt                 { let pos = $startpos in
				   fun is -> let (ch,exp) = $2 in (pos,Ast.ExWriteCase (ch,exp,is)) }
;
     
caselist:
 |                                           { [] }
 | commcase COLON opt_stmtlist caselist      { ($1 $3)::$4 }
 ;

%%
