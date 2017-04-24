module Analyzer = Analyzer.Intanalyzer
module R = Analyzer.R

module JsUn = Js.Unsafe

(*  calc_pos : Lexing.lexbuf -> int * int  *)
(*let calc_pos lexbuf =
  let pos  = lexbuf.Lexing.lex_curr_p in
  let line = pos.Lexing.pos_lnum in
  let col  = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
  (line,col)*)

(*  build_error : string -> (int * int) -> js_obj  *)    
let build_error msg (line,col) =
  JsUn.obj [| ("line",   JsUn.inject line);
	      ("column", JsUn.inject col);
	      ("msg",    JsUn.inject (Js.string msg)); |]

(*  parse : js_string t -> js_obj *)
let parse s =
  let lexbuf = Lexing.from_string (Js.to_string s) in
  let error msg = JsUn.obj [| ("error",     JsUn.inject (build_error msg (Frontend.calc_pos_lexbuf lexbuf)));
			      ("warnings",  JsUn.inject (Js.array [| |])); |] in
  try
    let chs,procs,proc = Goparser.prog Golexer.nexttoken lexbuf in
(*    let chs,procs,proc = Frontend.parse_file fname in *)
    let p  = Ast.desugar_prog (chs,procs,proc) in
    let p' = Last.label_prog p in
    let scaches = Analyzer.eval_proclist (snd p') 10 Format.std_formatter in
    let ws = Warn.warn_prog (fst p',scaches) in
    let warnings =
      Js.array (Array.of_list
		  (List.map (fun (pos,msg) ->
		               let (line,_) = Frontend.calc_pos pos in
			       JsUn.obj [| ("line", JsUn.inject line);
					   ("msg",  JsUn.inject (Js.string msg)); |]) ws)) in
    JsUn.obj [| ("result",    JsUn.inject (Js.string "ok"));
		("warnings",  JsUn.inject warnings); |]
  with
    | Failure msg     -> error ("Failure in " ^ msg)
    | End_of_file     -> error "Parse error: unexpected end of string"
    | Goparser.Error  -> error "Parse error"
    
(*  interpret : js_string t -> js_obj *)
let interpret s =
  let () = Last.reset () in
  let () = Ast.reset_info () in
  let lexbuf = Lexing.from_string (Js.to_string s) in
  let error msg = JsUn.obj [| ("error", JsUn.inject (build_error msg (Frontend.calc_pos_lexbuf lexbuf))) |] in
  try
    let ss = Goparser.prog Golexer.nexttoken lexbuf in
    let () = Lexing.flush_input lexbuf in
    let () = Parsing.clear_parser () in
    let lss = Last.label_prog (Ast.desugar_prog ss) in
(*    let lss = List.map (fun s ->  (fst s, Last.label (Ast.desugar_proc (snd s)))) ss in *)
    let () = Format.fprintf Format.str_formatter "Channel name mapping:\n----------------------\n" in
    let () = Ast.print_info Format.str_formatter in
    let info = Format.flush_str_formatter () in
    let _ = Analyzer.eval_proclist (snd lss) 10 Format.str_formatter in
    let s  = Format.flush_str_formatter () in
    JsUn.obj [| ("result",    JsUn.inject (Js.string (info ^ "\n\n" ^ s))); |]
  with
    | Failure msg -> error ("Failure in " ^ msg)
    | End_of_file -> error "Parse error: unexpected end of string"
    | Goparser.Error  -> error "Parse error in process"

(* export interpret function to JS's global scope *)
let () = Js.Unsafe.global##interpret <- Js.wrap_callback interpret
let () = Js.Unsafe.global##parse     <- Js.wrap_callback parse

