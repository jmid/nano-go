module A = Ast
(*
module Analyzer = Analyzer.Intanalyzer
module R = Analyzer.R
module R' = Analyzer.R'
module I = Interval
*)
module GoLex = Golexer
module GoPar = Goparser
(*module RELexerInfo = Regexplexer.Make(Ast) (* RE lexer parameterized with Ast *) *)

(*let parse_pol s = Regexpparser.regexp RELexerInfo.nexttoken (Lexing.from_string s) *)

;;
print_endline ("Nano-Go analyzer, v.0.000000001\n");
;;
if !Sys.interactive
then ()
else
  let filenames = ref [] in
  let analyze   = ref true in
  let usagemsg  = "Usage: " ^ Sys.argv.(0) ^ " [options] <filename>" in
  let argspec   = 
    Arg.align
      [("-only-parse", Arg.Clear analyze, " Only parse program, don't attempt analysis");  ] in
  Arg.parse argspec (fun s -> filenames := s::(!filenames)) usagemsg;
  match !filenames with
    | [fname] ->
      let chs,procs,proc = Frontend.parse_file fname in
      let () = print_string "channel names: " in
      let _ = List.fold_left (fun a ch -> print_string (a ^ ch); ", ") "" chs in
    (*    let () = print_newline () in
	  let () = print_newline () in
	  let () = print_endline "processes:" in
	  let _  = List.iter (fun ch -> Ast.print_exproc ch) procs in
	  let () = Ast.print_exproc proc in *)
      let p  = Ast.desugar_prog (chs,procs,proc) in
      let p' = Last.label_prog p in
      let () = print_newline () in
      let () = print_newline () in
    (*let () = print_endline "analysis:" in*)
    (*let botcache = Analyzer.Intanalyzer.Cache.bot in
      let _ = Analyzer.Intanalyzer.ppprog Format.std_formatter p' (botcache,botcache) in*)
      if !analyze
      then
	let scaches = Analyzer.Intanalyzer.eval_proclist (snd p') 10 Format.std_formatter in
	let ws = Warn.warn_prog (fst p',scaches) in
	let () = Format.fprintf Format.std_formatter "\nWarnings:\n" in
	List.iter (fun (pos,w) ->
	  let (line,col) = Frontend.calc_pos pos in
	  Format.fprintf Format.std_formatter " line %i col %i:  %s\n" line col w) ws
      else ()
  (*    List.iter (fun (xs,p) -> Analyzer.Intanalyzer.ppstmt Format.std_formatter p (botcache,botcache)) ps*)
    | _ ->
      (Arg.usage argspec usagemsg;
       exit 1)
