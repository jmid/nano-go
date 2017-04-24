(**  A collection of examples for analysis.                *)
(**  Run, e.g., by #use "src/examples.ml" in the toplevel  *)

module CP = Constprop;;
module CPRE = Regexpdom.Make(CP);;
CPRE.leq (Regexpdom.Eps) (CPRE.kleene (Regexpdom.Letter (CP.const 42)));;
CPRE.leq (Regexpdom.Eps) (Regexpdom.Letter (CP.const 42));;

module I = Interval;;
module IRE = Regexpdom.Make(I);;
IRE.leq (IRE.concat (IRE.letter (I.Interval (I.LBound 1, I.UBound 2)),
                     IRE.union (IRE.letter (I.const 1),IRE.letter (I.const 2))))
        (IRE.union (IRE.concat (IRE.letter (I.const 1),IRE.letter (I.const 1)),
                    IRE.concat (IRE.letter (I.const 2),IRE.letter (I.const 2))));;

module SRE = Analyzer.Signanalyzer.R;;
module A = Ast;;
module SA = Analyzer.Signanalyzer;;
module IA = Analyzer.Intanalyzer;;
module IRE = Analyzer.Intanalyzer.R;;

(* A few simple examples of analyzing a single process *)
SA.eval_proc_pp A.Skip;;
SA.eval_proc_pp (A.Seq (A.Assign ("x",A.Num 42), A.Chwrite (0,A.Var "x")));;
SA.eval_proc_pp (A.Seq (A.Chwrite (0,A.Num 2), A.Chwrite (0,A.Num 4)));;
SA.eval_proc_pp (A.Chread (0,"x"));;
SA.eval_proc_pp (A.Seq (A.Chread (0,"x"),A.Chread (1,"y")));;
SA.eval_proc_pp (A.While (A.True, A.Skip));;
SA.eval_proc_pp (A.While (A.True, A.Assign ("x",A.Num 0)));;
SA.eval_proc_pp (A.While (A.False, A.Assign ("x",A.Num 0)));;
SA.eval_proc_pp (A.While (A.True, A.Chread (0,"x")));;  (* ditto *)

SA.eval_proc_pp (A.Seq (A.Assign ("x",A.Num 0),
 		       (A.Chwrite (0,A.Var "x"))));;

SA.eval_proc_pp (A.Seq (A.Assign ("x",A.Num 0),
 		  A.Seq (A.Chwrite (0,A.Var "x"),
  		   A.While (A.True, A.Seq (A.Assign ("x", A.Binop (A.Var "x",A.Plus,A.Num 1)),
			                   A.Chwrite (0,A.Var "x"))))));;

SA.eval_proc_pp (A.Seq (A.Assign ("x",A.Num 1),
                        A.While (A.Relop (A.Num 1,A.Leq,A.Var "x"), A.Chread (0,"x"))));;
SA.eval_proc_pp (A.Seq (A.Assign ("x",A.Num 1),
                        A.While (A.Relop (A.Num 0,A.Leq,A.Var "x"), A.Chread (0,"x"))));;
SA.eval_proc_pp (A.Seq (A.Assign ("x",A.Num 1),
                        A.While (A.Relop (A.Num 0,A.Leq,A.Var "x"), A.Skip)));;

(* A few simple examples of parsing and analyzing a single process *)
Main.eval_str_pp "spawn p() { }";;
Main.eval_str_pp "spawn p() { x = 42; ch!x }";;
Main.eval_str_pp "spawn p() { ch!2; ch!4 }";;
Main.eval_str_pp "spawn p() { ch?x }";;
Main.eval_str_pp "spawn p() { ch?x; ch?y }";;
Main.eval_str_pp "spawn p() { while (true) {} }";;
Main.eval_str_pp "spawn p() { while (true) { x = 0 } }";;
Main.eval_str_pp "spawn p() { while (false) { x = 0 } }";;
Main.eval_str_pp "spawn p() { while (true) { ch?x } }";;
Main.eval_str_pp "spawn p() { while (true) { ch!x } }";;
Main.eval_str_pp "spawn p() { x = 1; \
                              while (1 <= x) { ch?x } }";;

Main.eval_str_pp "spawn p() { count = 0; \
                              while (true) { choose { \
                                { incr?x; count = count + 1; } \
                              | { decr?x; count = count - 1; } \
                              | { curr?x; curr!count } } } }";;

(* Some simple examples of analyzing two processes communicating *)
IA.eval_twoproc_pp (A.Chread (0,"y"), A.Chwrite (0, A.Num 22));;
IA.eval_twoproc_pp (A.Chwrite (0, A.Num 33), A.Chread (0, "x"));;
IA.eval_twoproc_pp (A.Seq (A.Chwrite (0, A.Num 33),A.Chread (1,"y")),
                    A.Seq (A.Chread (0, "x"),A.Chwrite (1, A.Num 22)));;

(* The high score example *)
(*
open Analyzer;;
Main.eval_proc_policy_pp (A.desugar_exblock (fst (A.highscore 0)))
                         "(report?[0;+oo] + ask?[0;+oo] + hsc![0;+oo])*";;
Main.eval_proc_policy_pp (A.desugar_exblock (snd (A.highscore 0)))
                         "(report![0;+oo] + ask![0;+oo] + hsc?[0;+oo])*";;
let hs = A.highscore 0 in
Main.eval_twoproc_policy_pp (A.desugar_exblock (snd hs)) (A.desugar_exblock (fst hs))
                         "(report![0;+oo] + ask![0;+oo] + hsc?[0;+oo])*";;
*)
