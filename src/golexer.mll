{
  open Gotokens
      
  let keywords = Hashtbl.create 23 

  let _ = Array.iter (fun (str,tok) -> Hashtbl.add keywords str tok)
    [| ("func"   , FUNC)
     ; ("go"     , GO)
     ; ("var"    , VAR)
     ; ("package", PKG)
     ; ("main"   , MAIN)
     ; ("print"  , PRINT)
     ; ("make"   , MAKE)
     ; ("chan"   , CHAN)
     ; ("int"    , INT)
(*   ; ("while"  , WHILE) *)
     ; ("for"    , FOR)
     ; ("assert" , ASSERT)
     ; ("ensure" , ENSURE)
     ; ("if"     , IF)
     ; ("else"   , ELSE)
     ; ("not"    , NOT)
(*     ; ("and"    , AND)
     ; ("or"     , OR) *)
     ; ("true"   , TRUE)
     ; ("false"  , FALSE)
     ; ("select" , SELECT)
     ; ("case"   , CASE) |]

  let id_or_keyword s = try Hashtbl.find keywords s with Not_found -> IDENT(s) 

(*  let level = ref 0 *)
}

let letter  = ['a'-'z' 'A'-'Z']
let digit   = ['0'-'9']
let ident   = letter (letter | digit | '_' | '.')*
let integer = ['0'-'9']+
let float   = integer '.' ['0'-'9']*

let space = [' ' '\009']
let cr    = '\013'
let lf    = '\010'
let eol   = cr | lf | cr lf

rule nexttoken = parse
    space+             { nexttoken lexbuf }
  | eol                { Lexing.new_line lexbuf; nexttoken lexbuf }
  | "/*"               { (*level := 1;*) comment lexbuf }
  | '#'[^'\n']*['\n']  { nexttoken lexbuf }
  | ident              { id_or_keyword (Lexing.lexeme lexbuf) }
  | '{'                { LBRACE }
  | '}'                { RBRACE }
  | '('                { LPAR }
  | ')'                { RPAR }
  | ":="               { COLONEQ }
  | "<-"               { ARROW }
  | "=="               { EQ }
  | "<>"               { NEQ }
  | "<="               { LE }
  | '<'                { LT }
  | ">="               { GE }
  | '>'                { GT }
  | "&&"               { AND }
(*  | "||"               { OR } *)
  | ':'                { COLON }
  | ';'                { SEMI }
  | '+'                { ADD }
  | '-'                { SUB }
  | '*'                { MULT }
  | '%'                { MOD }
  | '='                { ASSIGN }
  | '!'                { BANG }
  | '|'                { BAR }
  | "//"               { comment' lexbuf }
  | integer            { INTLIT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof                { EOF }

and comment = parse
  | "*/"    { (*decr level; 
              if !level = 0 then*) nexttoken lexbuf 
	      (*else comment lexbuf *) }
(*  | "/*"    { incr level; comment lexbuf } *)
  | eol     { Lexing.new_line lexbuf; comment lexbuf }
  | _       { comment lexbuf }

and comment' = parse
  | eol     { Lexing.new_line lexbuf; nexttoken lexbuf }
  | _       { comment' lexbuf }

