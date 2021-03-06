module A = Ast
module LA = Last

module Make(Astore :
            sig
	      module Val : sig
		             include Sigs.EXTLATTICE
			     val const : int -> elem
	                   end
	      include Sigs.LATTICE
	      val empty           : elem
              val extend          : elem -> Ast.var -> Val.elem -> elem
	      val eval_aexp       : Ast.aexp -> elem -> Val.elem
	      val eval_bexp_true  : Ast.bexp -> elem -> elem
	      val eval_bexp_false : Ast.bexp -> elem -> elem
            end)  =
struct
  module Val = Astore.Val
  module Channame = struct
                      include Interval
		      let widening = join (* only finitely many channel names, widening not needed *)
                    end
  module ReadPair = struct
                      include Redpairdom.MakeExt(Channame)(Val)
		      let prefix = "?"
		      let fpprint fmt p = (Format.fprintf fmt "%s" prefix; fpprint fmt p)
		      let pprint e = (fpprint Format.std_formatter e; Format.print_flush ())
		      let to_string p = let s = to_string p in
					prefix ^ s
                    end
  module WritePair = struct
                       include Redpairdom.MakeExt(Channame)(Val)
		       let prefix = "!"
		       let fpprint fmt p = (Format.fprintf fmt "%s" prefix; fpprint fmt p)
		       let pprint e = (fpprint Format.std_formatter e; Format.print_flush ())
		       let to_string p = let s = to_string p in
					 prefix ^ s
                     end
  module Chandom = struct
                     include Pairdom.MakeExt(WritePair)(ReadPair)

                     (*  fpprint : Format.formatter -> elem -> unit  *)
		     let fpprint fmt ((c1,c2) as pair) =
		       if leq top pair then Format.fprintf fmt "Top"
		       else
			 let s1 = WritePair.to_string c1 in
			 let s2 = ReadPair.to_string c2 in
 		         match WritePair.leq c1 WritePair.bot,
			       ReadPair.leq c2 ReadPair.bot with
			   | false,true  -> Format.fprintf fmt "%s" s1
			   | true,false  -> Format.fprintf fmt "%s" s2
			   | _,_         -> Format.fprintf fmt "(%s, %s)" s1 s2

                     (*  pprint : elem -> unit  *)
		     let pprint e = (fpprint Format.std_formatter e; Format.print_flush ())

                     (*  to_string : L.elem -> string  *)
		     let to_string ((c1,c2) as pair) =
		       if leq top pair then "Top"
		       else
			 let s1 = WritePair.to_string c1 in
			 let s2 = ReadPair.to_string c2 in
 		         match WritePair.leq c1 WritePair.bot,
 			       ReadPair.leq c2 ReadPair.bot with
			   | false,true  -> s1
			   | true,false  -> s2
			   | _,_         -> "(" ^ s1 ^ ", " ^ s2 ^ ")"
  end

(*let writetag ch v = WritePair.pair(Channame.const ch, v)*)
  let writetag ch v = Chandom.pair (WritePair.pair(Channame.const ch, v), ReadPair.bot)
  let readtag ch v  = Chandom.pair (WritePair.bot, ReadPair.pair(Channame.const ch, v))

  module R = struct
                include Regexpdom.Make(Chandom)
		let widening = join

		let rec close r =
		  let (writepart,readpart) as part = range r in
		  let part' =
		    Chandom.fold_partition
		      (fun (writepart,readpart) eqcl -> match eqcl with
			| Chandom.FstEqCl eq1 -> (* a write *)
			  let _writepart',readpart' =
			    range (d (Chandom.pair(WritePair.repr eq1, ReadPair.bot)) r) in
			  (WritePair.overlay_partitions writepart readpart', readpart)
			         (* refine writepart with that of consecutive reads *)
			| Chandom.SndEqCl eq2 -> (* a read  *)
			  let writepart',_readpart' =
			    range (d (Chandom.pair(WritePair.bot, ReadPair.repr eq2)) r) in
			  (writepart, ReadPair.overlay_partitions readpart writepart')
			         (* refine readpart with that of consecutive writes *))
		      part part in
		  let r' =
		    Chandom.fold_partition (* now close LVRE based on refined part *)
		      (fun acc eqcl -> match eqcl with
			| Chandom.FstEqCl eq1 -> (* a write *)
			  let wr_atom = WritePair.repr eq1 in
			  let dwr_r = d (Chandom.pair(WritePair.bot, wr_atom))
			                (d (Chandom.pair(wr_atom, ReadPair.bot)) r) in
			  union (acc, dwr_r)
			| Chandom.SndEqCl eq2 -> (* a read  *)
			  let rd_atom = ReadPair.repr eq2 in
			  let drw_r = d (Chandom.pair(rd_atom, ReadPair.bot))
			                (d (Chandom.pair(WritePair.bot, rd_atom)) r) in
			  union (acc, drw_r)) r part' in
		  if eq r r'
		  then r
		  else close r'
		  
(*		let rec close r =
		  let (writepart,readpart) as part = range r in
		  let refpart = ReadPair.overlay_partitions writepart readpart in
		  let r' = ReadPair.fold_partition
  	 	             (fun acc eqcl(*(cheqc,valeqc)*) ->
			       let chrep,valrep = ReadPair.repr eqcl in
			       (match chrep with
 				 | Interval.Bot ->
				   failwith "close, FstEqCl: Bottom channel should not happen"
				 | Interval.Interval (Interval.MInf,_) ->
				   failwith "close, FstEqCl: MInf channel should not happen"
				 | Interval.Interval (Interval.LBound ch,_) ->
				   let drw_r = d (writetag ch valrep) (d (readtag ch valrep) r) in
				   let dwr_r = d (readtag ch valrep) (d (writetag ch valrep) r) in
				   union (union (acc, drw_r), dwr_r))) r refpart in
		  if eq r r'
		  then r
		  else close r' *)
  end

  let pp = ref false

  (*  time_apply : ('a -> 'b) -> 'a -> 'b  *)
  let time_apply f x = f x (* uncommented to make it compile with js_of_ocaml *)
(*  let start = Unix.gettimeofday () in
    let res = f x in
    let stop = Unix.gettimeofday () in
    let () = Printf.printf "Execution time: %fs\n%!" (stop -. start) in
    res *)

  module Proddom = Pairdom.Make(Astore)(R)
  module Cache = Cache.Make(Proddom)

  let lfp init func =
    let rec postfix prev =
      let next = func prev in
      let next' = (Cache.widening (fst prev) (fst next),
		   Cache.widening (snd prev) (snd next)) in
      let () = if !pp then
	  begin
	    Format.printf "prev: ";
	    Cache.pprint (fst prev); Format.printf ", "; Cache.pprint (snd prev);
            Format.printf "  next: ";
	    Cache.pprint (fst next); Format.printf ", "; Cache.pprint (snd next);
            Format.printf "  next': ";
	    Cache.pprint (fst next'); Format.printf ", "; Cache.pprint (snd next');
            Format.printf "\n";
	  end else () in
      if Cache.leq (fst next') (fst prev) && Cache.leq (snd next') (snd prev) then prev else postfix next'
    in
    (*
    let rec narrow prev =
      let next = func prev in
      let next' = (Cache.narrowing (fst prev) (fst next),
		   Cache.narrowing (snd prev) (snd next)) in
      let () = if !pp then
	  begin
	    Format.printf "prev: ";
	    Cache.pprint (fst prev); Format.printf ", "; Cache.pprint (snd prev);
            Format.printf "  next: ";
	    Cache.pprint (fst next); Format.printf ", "; Cache.pprint (snd next);
            Format.printf "  next': ";
	    Cache.pprint (fst next'); Format.printf ", "; Cache.pprint (snd next');
            Format.printf "\n";
	  end else () in
      if Cache.eq (fst next') (fst prev) && Cache.eq (snd next') (snd prev) then prev else narrow next'
    in *)
    (*narrow*) (postfix init)

  (* ************************************************************ *)
  (*         Version with futures + environment policies          *)
  (* ************************************************************ *)

  let absassign sigma x v = Astore.extend sigma x v

  (*  eval_stmt : Ast.stmt -> cache * cache -> cache * cache  *)
  let rec eval_stmt s (en,ex) = match s with
    | LA.Skip l         -> (en, Cache.extend ex l.LA.label (Cache.lookup en l.LA.label))
    | LA.Assign (l,x,a) ->
      let (sigma,f) = Cache.lookup en l.LA.label in
      (en, Cache.extend ex l.LA.label (absassign sigma x (Astore.eval_aexp a sigma),f))
    | LA.Seq (s1,s2)    ->
      let (en',ex') = eval_stmt s1 (en,ex) in
      let labels = LA.last s1 in
      let join = List.fold_left (fun acc l -> Proddom.join acc (Cache.lookup ex' l)) Proddom.bot labels in
      eval_stmt s2 (Cache.extend en' (LA.first s2) join,ex')
    | LA.If (l,b,s1,s2) ->
      let (sigma,f) = Cache.lookup en l.LA.label in
      let (en1,ex1) = eval_stmt s1 (Cache.extend en (LA.first s1) (Astore.eval_bexp_true b sigma,f),ex) in
      let (en2,ex2) = eval_stmt s2 (Cache.extend en (LA.first s2) (Astore.eval_bexp_false b sigma,f),ex) in
      let res = List.fold_left
  	               (fun acc l -> Proddom.join acc (Cache.lookup ex1 l)) Proddom.bot (LA.last s1) in
      let res = List.fold_left (fun acc l -> Proddom.join acc (Cache.lookup ex2 l)) res (LA.last s2) in
      (Cache.join en1 en2, Cache.extend (Cache.join ex1 ex2) l.LA.label res)
    | LA.For (l,b,s1)   ->
      let (en',ex') =
	lfp (en,ex)
	  (fun (en,ex) ->
	    let prev = Cache.lookup en l.LA.label in
	    let (sigma,f) as res =
	      List.fold_left (fun acc l1 -> Proddom.join acc (Cache.lookup ex l1)) prev (LA.last s1) in
	    eval_stmt s1 (Cache.extend
			   (Cache.extend en (LA.first s1) (Astore.eval_bexp_true b sigma,f))
			      l.LA.label res, ex)) in
      let (sigma',f') = Cache.lookup en' l.LA.label in
      (en', Cache.extend ex' l.LA.label (Astore.eval_bexp_false b sigma',f'))
    | LA.Select (l,cs) ->
      let (sigma,f) as entry = Cache.lookup en l.LA.label in
      let entries,ens,exs = List.fold_left (fun (ents,ens,exs) ((pos,c) as cp) ->
	                                      let en' = Cache.extend en pos.LA.label entry in
	                                      let (en,ex) = eval_case cp (en',ex) in
					      let ent = Cache.lookup ex pos.LA.label in
					      ent::ents,en::ens,ex::exs) ([],[],[]) cs in
      let ens' = List.fold_left (fun acc ent -> Proddom.join ent acc) Proddom.bot entries in
      let en'  = List.fold_left (fun acc en -> Cache.join en acc) Cache.bot ens in
      let ex'  = List.fold_left (fun acc ex -> Cache.join ex acc) Cache.bot exs in
      (en', Cache.extend ex' l.LA.label ens')
    | LA.Print (l,_)  ->
      (en, Cache.extend ex l.LA.label (Cache.lookup en l.LA.label)) (* exprs cannot have side effects *)

  (*  eval_case : Last.case -> env * R * R -> Cache * Cache -> (env * R * R) * (Cache * Cache)  *)
  and eval_case (pos,c) (en,ex) = match c with
    | LA.ReadCase (x,ch,s)  ->
      let (sigma,f) = Cache.lookup en pos.LA.label in
      let rng = R.range f in
      let res =
	Chandom.fold_partition
	(fun acc eqcl -> match eqcl with
	  | Chandom.FstEqCl eq1   -> (* write *)
	    let chrep,valrep = WritePair.repr eq1 in
	    let chprj,valprj = WritePair.project eq1 in
	    let d_valrep_f = R.d (writetag ch valrep) f in
	    if Channame.leq (Channame.const ch) chprj (* input concerns 'ch' *)
	      && not (R.leq d_valrep_f R.bot) (* and represents a possible future *)
	    then Proddom.join acc (Astore.extend sigma x valprj, R.close d_valrep_f)
	    else acc
	  | Chandom.SndEqCl _   -> acc)(* read *)
	  Proddom.bot rng in
      let (en',ex') = eval_stmt s (Cache.extend en (LA.first s) res, ex) in
      let labels = LA.last s in
      let join = List.fold_left (fun acc l -> Proddom.join acc (Cache.lookup ex' l)) Proddom.bot labels in
      (en', Cache.extend ex' pos.LA.label join)
    | LA.WriteCase (ch,a,s) ->
      let (sigma,f) = Cache.lookup en pos.LA.label in
      let v' = Astore.eval_aexp a sigma in
      let rng = (R.range f) in
      (*let rng = Chandom.overlay_partitions (R.range f) (Chandom.partition (readtag ch v')) in*)
      let res =
	Chandom.fold_partition
	  (fun acc eqcl ->
	    match eqcl with
	      | Chandom.FstEqCl _   -> acc     (* write *)
	      | Chandom.SndEqCl eq1 ->         (* read *)
		let chrep,valrep = ReadPair.repr eq1 in
		let chprj,valprj = ReadPair.project eq1 in
		let vvmeet = Val.meet valprj v' in
		let d_valrep_f = R.d (readtag ch valrep) f in
		if Channame.leq (Channame.const ch) chprj (* output concerns 'ch' *)
	          && not (Val.leq vvmeet Val.bot)
		  && not (R.leq d_valrep_f R.bot) (* and represents a possible future *)
		then
		  Proddom.join acc
	            (sigma,
	             (*R.concat (h, R.letter (writetag ch (*v'*)vvmeet)),*)
		     R.close d_valrep_f)
		else acc)
	  Proddom.bot rng  in
      let (en',ex') = eval_stmt s (Cache.extend en (LA.first s) res, ex) in
      let labels = LA.last s in
      let join = List.fold_left (fun acc l -> Proddom.join acc (Cache.lookup ex' l)) Proddom.bot labels in
      (en', Cache.extend ex' pos.LA.label join)


(** ppaexp, ppaterm, and ppafactor implements an expression
    pretty printer following the below BNF grammar *)
(*   expr ::= term
            | expr + term
            | expr - term
     term ::= factor
            | term * factor |
   factor ::= Var x
            | Num i
            | Any
            | ( expr )      *)
  let rec ppaexp fmt e = match e with
    | A.Binop (e1,A.Plus,e2) ->
      begin
	ppaexp fmt e1;
	Format.fprintf fmt " + ";
	ppaterm fmt e2;
      end
    | A.Binop (e1,A.Minus,e2) ->
      begin
	ppaexp fmt e1;
	Format.fprintf fmt " - ";
	ppaterm fmt e2;
      end
    | _ -> ppaterm fmt e
  and ppaterm fmt e = match e with
    | A.Binop (e1,A.Mult,e2) ->
      begin
	ppaterm fmt e1;
	Format.fprintf fmt " * ";
	ppafactor fmt e2;
      end
    | _ -> ppafactor fmt e
  and ppafactor fmt e = match e with
    | A.Num n -> Format.fprintf fmt "%i" n
    | A.Var x -> Format.fprintf fmt "%s" x
    | A.Any   -> Format.fprintf fmt "?"
    | A.Binop (_,_,_) ->
      begin
	Format.fprintf fmt "(";
	ppaexp fmt e;
	Format.fprintf fmt ")";
      end

  (*  ppbexp : Format.formatter -> Ast.bexp -> unit  *)
  let rec ppbexp fmt e = match e with
    | A.True   -> Format.fprintf fmt "true"
    | A.False  -> Format.fprintf fmt "false"
    | A.Not e' ->
      begin
	Format.fprintf fmt "not (";
	ppbexp fmt e';
	Format.fprintf fmt ")";
      end
    | A.Relop (e1,op,e2) ->
      let str = match op with
	| A.Eq  -> "=="
	| A.Lt  -> "<"
	| A.Leq -> "<=" in
      begin
	ppaexp fmt e1;
	Format.fprintf fmt " %s " str;
	ppaexp fmt e2;
      end
    | A.Conj (e1,e2) ->
      begin
	Format.fprintf fmt "(";
	ppbexp fmt e1;
	Format.fprintf fmt ") and ("; (* conj's are still fully parenthesized though *)
	ppbexp fmt e2;
	Format.fprintf fmt ")";
      end

  let fmt_str : ('a -> 'b -> 'c, Format.formatter, unit) format = "%s%30s"

  (*  ppstmt : Format.formatter -> Last.stmt -> cache * cache -> unit  *)
  let rec ppstmt fmt s caches =
    let (en,ex) = caches in
    match s with
    | LA.Skip l         ->
      begin
	Format.fprintf fmt "%i:" l.LA.label;
	Format.fprintf fmt fmt_str "skip;" "";
	Proddom.fpprint fmt (Cache.lookup ex l.LA.label);
      end
    | LA.Assign (l,x,e) ->
      begin
	Format.fprintf fmt "%i:" l.LA.label;
	Format.fprintf fmt "%s = " x;
	ppaexp fmt e;
	Format.fprintf fmt fmt_str ";" "";
	Proddom.fpprint fmt (Cache.lookup ex l.LA.label);
      end
    | LA.Seq (s1,s2)  ->
      begin
	Format.fprintf fmt "@[<v 0>";
	ppstmt fmt s1 caches;
	Format.pp_print_space fmt ();
	ppstmt fmt s2 caches;
	Format.fprintf fmt "@]";
      end
    | LA.For (l,e,s')  ->
      begin
	Format.fprintf fmt "%i:" l.LA.label;
	Format.fprintf fmt "@[<v 0>@[<v 2>for ";
	ppbexp fmt e;
	Format.fprintf fmt fmt_str " {" "";
	Proddom.fpprint fmt (Cache.lookup en (LA.first s'));
	Format.pp_print_space fmt ();
	ppstmt fmt s' caches;
	Format.fprintf fmt "@]";
	Format.pp_print_space fmt ();
	Format.fprintf fmt fmt_str "}" "";
	Proddom.fpprint fmt (Cache.lookup ex l.LA.label);
	Format.fprintf fmt "@]";
      end
    | LA.If (l,e,s1,s2)  ->
      begin
	Format.fprintf fmt "%i:" l.LA.label;
	Format.fprintf fmt "@[<v 0>if (";
	ppbexp fmt e;
	Format.fprintf fmt ")";
	Format.pp_print_space fmt ();
	Format.fprintf fmt "@[<v 2>then {";
	Format.pp_print_space fmt ();
	ppstmt fmt s1 caches;
	Format.fprintf fmt "@]";
	Format.pp_print_space fmt ();
	Format.fprintf fmt "@[<v 2>} else {";
	Format.pp_print_space fmt ();
	ppstmt fmt s2 caches;
	Format.fprintf fmt "@]";
	Format.pp_print_space fmt ();
	Format.fprintf fmt fmt_str "}" "";
	Proddom.fpprint fmt (Cache.lookup ex l.LA.label);
	Format.fprintf fmt "@]";
      end
    | LA.Select (l,cs) ->
      begin
	Format.fprintf fmt "%i:" l.LA.label;
	Format.fprintf fmt "@[<v 0>@[<v 2>select {";
	Format.pp_print_space fmt ();
	ppcases fmt cs caches;
	(*Format.fprintf fmt fmt_str " (ppstmt Select not completely implemented yet)" "";*)
	Format.fprintf fmt "@]";
	Format.pp_print_space fmt ();
	Format.fprintf fmt fmt_str "}" "";
	Proddom.fpprint fmt (Cache.lookup ex l.LA.label);
	Format.fprintf fmt "@]";
      end
    | LA.Print (l,e)  ->
      begin
	Format.fprintf fmt "%i:" l.LA.label;
	Format.fprintf fmt "print(";
	ppaexp fmt e;
	Format.fprintf fmt ")";
	Format.fprintf fmt fmt_str ";" "";
	Proddom.fpprint fmt (Cache.lookup ex l.LA.label);
      end

  (*  ppcases : Format.formatter -> Last.case list -> cache * cache -> unit  *)
  and ppcases fmt cs caches =
    begin
      Format.fprintf fmt "@[<v 0>";
      ignore
	(List.fold_left
	   (fun first (l,c) ->
 	      match c with
		| LA.ReadCase ((*l,*)x,ch,s) ->
		  begin
		    if not first then (Format.pp_print_space fmt ());
		    (*Format.fprintf fmt "%i:" l;*)
		    Format.fprintf fmt "%i:" l.LA.label;
		    Format.fprintf fmt "@[<v 2>case %s = <- ch%i:" x ch;
		    Format.pp_print_space fmt ();
		    ppstmt fmt s caches;
		    Format.fprintf fmt "@]";
		    false;
		  end
		| LA.WriteCase ((*l,*)ch,e,s) ->
		  begin
		    if not first then (Format.pp_print_space fmt ());
		    (*Format.fprintf fmt "%i:" l;*)
		    Format.fprintf fmt "%i:" l.LA.label;
		    Format.fprintf fmt "@[<v 2>case ch%i <- " ch;
		    ppaexp fmt e;
		    Format.fprintf fmt fmt_str ":" "";
		    Format.pp_print_space fmt ();
		    ppstmt fmt s caches;
		    Format.fprintf fmt "@]";
		    false
		  end) true cs);
	 Format.fprintf fmt "@]";
    end
      
  (*  ppprog : Format.formatter -> Last.prog -> cache * cache -> unit  *)
  let rec ppprog fmt (chs,ps) caches =
    let _ = List.iter
      (fun c ->
 	 Format.fprintf fmt "ch%i := make(chan int);" (Ast.lookup_chname c Ast.info);
	 Format.pp_print_newline fmt ()) chs in
    List.iter (fun (xs,p) ->
                 begin
		   Format.fprintf fmt "@[<v 0>@[<v 2>go func() {";
		   ignore
		     (List.fold_left
			(fun first x ->
			  begin
			    (if first
			     then
				begin
				  Format.pp_print_space fmt ();
				  Format.fprintf fmt "var %s" x
				end
			     else Format.fprintf fmt ",%s" x);
			    false
			  end) true xs);
		   if xs <> [] then Format.fprintf fmt " int;";
		      
		   Format.pp_print_space fmt ();
                   ppstmt fmt p caches;
		   Format.fprintf fmt "@]";
		   Format.pp_print_space fmt ();
		   Format.fprintf fmt fmt_str "}" "";
		   Format.fprintf fmt "@]";	   
		   Format.pp_print_newline fmt ()
		 end) ps

  (* extract_stmt_hist : Last.stmt -> Cache -> R.elem * R.elem  *)
  let rec extract_stmt_hist s cache = match s with (* returns prefix lang, full lang *)
    | LA.Skip l         -> (Regexpdom.Eps,Regexpdom.Eps)
    | LA.Assign (l,x,e) -> (Regexpdom.Eps,Regexpdom.Eps)
    | LA.Seq (s1,s2)    ->
      let pre1,full1 = extract_stmt_hist s1 cache in
      let pre2,full2 = extract_stmt_hist s2 cache in
      let pre = R.union (pre1, R.concat (full1,pre2)) in
      (pre, R.concat (full1,full2))
    | LA.For (l,e,s')   -> 
      let prebody,fullbody = extract_stmt_hist s' cache in
      let full = R.kleene fullbody in
      let pre  = R.concat (R.kleene fullbody, prebody) in
      (pre,full)
    | LA.If (l,e,s1,s2) ->
      let pre1,full1 = extract_stmt_hist s1 cache in
      let pre2,full2 = extract_stmt_hist s2 cache in
      let pre = R.union (pre1, pre2) in
      (pre, R.union (full1,full2))
    | LA.Select (l,cs)  ->
      let prefulls = List.map (fun c -> extract_case_hist c cache) cs in
      let pres,fulls = List.split prefulls in
      let pre  = List.fold_left (fun a pre -> R.union (a,pre)) Regexpdom.Eps pres in
      let full = List.fold_left (fun a full -> R.union (a,full)) Regexpdom.Empty fulls in
      (pre,full)
    | LA.Print (l,e)    -> (Regexpdom.Eps,Regexpdom.Eps)
  (* extract_case_hist : Last.case -> Cache -> R.elem -> R.elem * R.elem  *)
  and extract_case_hist (pos,c) cache = match c with
    | LA.ReadCase (x,ch,s)  ->
      let sto,_ = Cache.lookup cache (LA.first s) in
      let val1  = Astore.eval_aexp (A.Var x) sto in
      let full1 = R.letter (readtag ch val1) in
      let pre1 = R.union (Regexpdom.Eps, full1) in
      let pre2,full2 = extract_stmt_hist s cache in
      let pre = R.union (pre1, R.concat (full1,pre2)) in
      (pre, R.concat (full1,full2))
    | LA.WriteCase (ch,a,s) ->
      let sigma,f = Cache.lookup cache pos.LA.label in
      let v'  = Astore.eval_aexp a sigma in
      let full1 =
	Chandom.fold_partition
	  (fun acc eqcl ->
	    match eqcl with
	      | Chandom.FstEqCl _   -> acc     (* write *)
	      | Chandom.SndEqCl eq1 ->         (* read *)
		let chrep,valrep = ReadPair.repr eq1 in
		let chprj,valprj = ReadPair.project eq1 in
		let vvmeet = Val.meet valprj v' in
		let d_valrep_f = R.d (readtag ch valrep) f in
		if Channame.leq (Channame.const ch) chprj (* output concerns 'ch' *)
	          && not (Val.leq vvmeet Val.bot)
		  && not (R.leq d_valrep_f R.bot) (* and represents a possible future *)
		then
		  R.union (acc, R.letter (writetag ch (*v'*)vvmeet))
		else acc)
        Regexpdom.Empty (R.range f)  in
      (*let full1 = R.letter (writetag ch v') in *)
      let pre1 = R.union (Regexpdom.Eps, full1) in
      let pre2,full2 = extract_stmt_hist s cache in
      let pre = R.union (pre1, R.concat (full1,pre2)) in
      (pre, R.concat (full1,full2))

(*
  (** Alternative version in which prefix lang does not include full lang.
      A client should R.union the two results
  *)
  
  (* extract_stmt_hist : Last.stmt -> Cache -> R.elem * R.elem  *)
  let rec extract_stmt_hist s cache = match s with (* returns prefix lang, full lang *)
    | LA.Skip l         -> (Regexpdom.Empty,Regexpdom.Eps)
    | LA.Assign (l,x,e) -> (Regexpdom.Empty,Regexpdom.Eps)
    | LA.Seq (s1,s2)    ->
      let pre1,full1 = extract_stmt_hist s1 cache in
      let pre2,full2 = extract_stmt_hist s2 cache in
      let pre = R.union (pre1, R.concat (full1,pre2)) in
      (pre, R.concat (full1,full2))
    | LA.For (l,e,s')   -> 
      let prebody,fullbody = extract_stmt_hist s' cache in
      let full = R.kleene fullbody in
      let pre  = R.concat (R.kleene fullbody, prebody) in
      (pre,full)
    | LA.If (l,e,s1,s2) ->
      let pre1,full1 = extract_stmt_hist s1 cache in
      let pre2,full2 = extract_stmt_hist s2 cache in
      let pre = R.union (pre1, pre2) in
      (pre, R.union (full1,full2))
    | LA.Select (l,cs)  ->
      let prefulls = List.map (fun c -> extract_case_hist c cache) cs in
      let pres,fulls = List.split prefulls in
      let pre  = List.fold_left (fun a pre -> R.union (a,pre)) Regexpdom.Eps pres in
      let full = List.fold_left (fun a full -> R.union (a,full)) Regexpdom.Empty fulls in
      (pre,full)
    | LA.Print (l,e)    -> (Regexpdom.Empty,Regexpdom.Eps)
  (* extract_case_hist : Last.case -> Cache -> R.elem -> R.elem * R.elem  *)
  and extract_case_hist (pos,c) cache = match c with
    | LA.ReadCase ((*l,*)x,ch,s)  ->
      let sto,_ = Cache.lookup cache (LA.first s) in
      let val1  = Astore.eval_aexp (A.Var x) sto in
      let full1 = R.letter (readtag ch val1) in
      let pre1 = R.union (Regexpdom.Eps, full1) in
      let pre2,full2 = extract_stmt_hist s cache in
      let pre = R.union (pre1, R.concat (full1,pre2)) in
      (pre, R.concat (full1,full2))
    | LA.WriteCase ((*l,*)ch,a,s) ->
      let sigma,f = Cache.lookup cache pos.LA.label in
      let v'  = Astore.eval_aexp a sigma in
      let full1 =
	Chandom.fold_partition
	  (fun acc eqcl ->
	    match eqcl with
	      | Chandom.FstEqCl _   -> acc     (* write *)
	      | Chandom.SndEqCl eq1 ->         (* read *)
		let chrep,valrep = ReadPair.repr eq1 in
		let chprj,valprj = ReadPair.project eq1 in
		let vvmeet = Val.meet valprj v' in
		let d_valrep_f = R.d (readtag ch valrep) f in
		if Channame.leq (Channame.const ch) chprj (* output concerns 'ch' *)
	          && not (Val.leq vvmeet Val.bot)
		  && not (R.leq d_valrep_f R.bot) (* and represents a possible future *)
		then
		  R.union (acc, R.letter (writetag ch (*v'*)vvmeet))
		else acc)
        Regexpdom.Empty (R.range f)  in
      (*let full1 = R.letter (writetag ch v') in *)
      let pre1 = R.union (Regexpdom.Eps, full1) in
      let pre2,full2 = extract_stmt_hist s cache in
      let pre = R.union (pre1, R.concat (full1,pre2)) in
      (pre, R.concat (full1,full2))
*)
  
  (*  eval_proc_policy : var list -> Last.stmt -> R.elem -> Format.formatter -> (Cache * Cache) * R.elem  *)
  let eval_proc_policy ?(pp=true) ?(time=false) xs ls pol fmt =
    let apply = if time then time_apply else fun f x -> f x in
    let sigma = List.fold_left (fun sigma x -> Astore.extend sigma x (Val.const 0)) Astore.empty xs in
    let pair = (Cache.extend Cache.empty (LA.first ls) (sigma,pol),
		Cache.empty) in
    let res = apply (eval_stmt ls) pair in
    let en_cache,_ = res in
    (*let coll_h',_  = extract_stmt_hist ls en_cache in*)
    let pre,full = extract_stmt_hist ls en_cache in
    let coll_h'  = if R.leq full pre then pre else R.union (pre,full) in
    (*let _,coll_h',_  = Cache.fold (fun i triple acc -> Proddom.join triple acc) (fst res) in *)
    if pp
    then
      begin
	Format.pp_print_space fmt ();
	Format.fprintf fmt fmt_str "" "";
	Proddom.fpprint fmt (Cache.lookup (fst res) (LA.first ls));
	Format.pp_print_newline fmt ();
	ppstmt fmt ls res;
	Format.pp_print_space fmt ();
	Format.pp_print_space fmt ();
	Format.fprintf fmt "\nCollective prefix': ";
	R.fpprint fmt coll_h';
	Format.pp_print_newline fmt ();
	Format.pp_print_newline fmt ();
	res, coll_h'
      end
    else res, coll_h'

  (*  eval_proclist : (var list * Last.stmt) list -> int -> Format.formatter -> unit  *)
  let rec eval_proclist ?(pp=true) ?(time=false) ss max fmt =
    let rec loop ss i =
      let () = Format.fprintf fmt "Iteration %i\n------------" i in
      let spolhlist =
	List.map (fun (xs,s,f) ->
	            begin
		      Format.fprintf fmt "\nProcess:";
		      let caches,h = eval_proc_policy xs s f fmt in (xs,s,caches,h)
		    end) ss in
      (*  prepend : 'a regexp -> 'a regexp list -> 'a regexp list  *)
      let prepend r ts = List.map (fun t -> R.shuffle (r,t)) ts in
      let fs',_ =
	List.fold_right
	  (fun (_,s,_,h) (prochs,tailh) -> (tailh :: (prepend h prochs), R.shuffle (h,tailh)))
	  spolhlist ([],Regexpdom.Eps) in
      let ss',leq =
	List.fold_right2
	  (fun (xs,s,f) f' (ss,acc) ->
	     (let newf = R.close (R.meet f (R.close f')) in
	      (xs,s,(*f'*)newf)::ss,acc && R.leq (*f'*) f newf)) ss fs' ([],true) in
      let res = List.map (fun (xs,s,caches,h) -> (xs,s,caches)) spolhlist in
      (if leq
       then
	  let () = Format.fprintf fmt "Reached fixed point, bailing early\n" in
	  res
       else
	  if succ i < max
	  then loop ss' (succ i)
	  else
	    let () = Format.fprintf fmt "Reached max iteration count\n" in
	    res)
    in
    loop (List.map (fun (xs,s) -> (xs,s,R.top(*Regexpdom.Eps*))) ss) 0
end

(*
module Parityanalyzer = Make(Paritystore)
module Signanalyzer = Make(Signstore)
module Constanalyzer = Make(Conststore)
module Modstore = Modstore.Make(struct let n = 8 end)
module Modanalyzer  = Make(Modstore)*)
module Intanalyzer  = Make(Intstore)
