(*  calc_pos : Lexing.position -> int * int  *)
let calc_pos pos =
  let line = pos.Lexing.pos_lnum in
  let col  = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
  (line,col)

(*  calc_pos_lexbuf : Lexing.lexbuf -> int * int  *)
let calc_pos_lexbuf lexbuf = calc_pos lexbuf.Lexing.lex_curr_p

(*  parse_lexbuf : -> string -> Lexing.lexbuf -> (unit -> unit) -> Ast.prog  *)
let parse_lexbuf f lexbuf close =
  try Goparser.prog Golexer.nexttoken lexbuf 
(*  with Parsing.Parse_error ->  *)
  with Goparser.Error
     | Failure _ -> 
    let line,col = calc_pos_lexbuf lexbuf in
    begin
      Printf.printf " Parse error in %s line %i, column %i\n" f line col;
      close();
      if !Sys.interactive then raise Parsing.Parse_error else exit(1);
    end

(*  parse_str : string -> Ast.prog  *)
let parse_str s =
  let lexbuf = Lexing.from_string s in
  parse_lexbuf "input string" lexbuf (fun () -> ())

(*  parse_file : string -> Ast.prog  *)
let parse_file f =
  let ch  = open_in f in
  let lexbuf = Lexing.from_channel ch in
  let curr_p = lexbuf.Lexing.lex_curr_p in
  let ()     = lexbuf.Lexing.lex_curr_p <- { curr_p with Lexing.pos_fname = f } in
  let stm = parse_lexbuf f lexbuf (fun () -> close_in ch) in
  let ()  = close_in ch in
  stm
