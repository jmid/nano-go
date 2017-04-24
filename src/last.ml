module A = Ast

(** Labelled AST for nano-Go *)

type pos = {
  lex_pos : Lexing.position;
  label   : int;
}
  
(*type label = int*)
type stmt =
  | Skip of pos
  | Assign of pos * A.var * A.aexp
  | Seq of stmt * stmt
  | For of pos * A.bexp * stmt
  | If of pos * A.bexp * stmt * stmt
(*| Chread of pos * A.chan * A.var (*var list*)
  | Chwrite of pos * A.chan * A.aexp (*var list*)
  | Choice of pos * stmt * stmt
  | Stop of pos *)
  | Select of pos * (pos * case) list
  | Print of pos * A.aexp
and case =
  | ReadCase of (*pos *) A.var * A.chan * stmt
  | WriteCase of (*pos *) A.chan * A.aexp * stmt

let reset,make_label =
  let count     = ref 1 in
  (fun () -> count := 1),
  (fun () -> let label = !count in
	     begin 
	       incr count;
	       label
	     end)

let make_labelpos pos =
  { lex_pos = pos;
    label   = make_label () }

let rec label s = match s with
  | A.Skip pos          -> Skip (make_labelpos pos)
  | A.Assign (pos,x,e)  -> Assign (make_labelpos pos,x,e)
  | A.Seq (s1,s2)       ->
    let s1' = label s1 in
    let s2' = label s2 in
    Seq (s1',s2')
  | A.For (pos,e,s)     ->
    let l  = make_labelpos pos in
    let s' = label s in
    For (l,e,s')
  | A.If (pos,e,s1,s2)  ->
    let l   = make_labelpos pos in
    let s1' = label s1 in
    let s2' = label s2 in
    If (l,e,s1',s2')
(*
  | A.Chread (ch,x)  -> Chread (make_label (),ch,x)
  | A.Chwrite (ch,e) -> Chwrite (make_label (),ch,e)
  | A.Choice (s1,s2) ->
    let l   = make_label () in
    let s1' = label s1 in
    let s2' = label s2 in
    Choice (l,s1',s2')
  | A.Stop           -> Stop (make_label ())
*)
  | A.Select (pos,cs)  ->
    let l   = make_labelpos pos in
    let cs' = List.fold_left (fun cs c -> (label_case c)::cs) [] cs in
    Select (l,List.rev cs')
  | A.Print (pos,e)    -> Print (make_labelpos pos,e)

and label_case (pos,c) = match c with
  | A.ReadCase (x,ch,s) ->
    let l  = make_labelpos pos in
    let s' = label s in
    (l,ReadCase ((*l,*)x,ch,s'))
  | A.WriteCase (ch,e,s) ->
    let l  = make_labelpos pos in
    let s' = label s in
    (l,WriteCase ((*l,*)ch,e,s'))

(*  label_prog : Ast.prog -> prog  *)
let label_prog (cs,ps) =
  let ps' = List.map (fun (xs,s) -> (xs,label s)) ps in
  (cs,ps')
      
(*  first : stmt -> label  *)
let rec first s = match s with
  | Skip l          -> l.label
  | Assign (l,_,_)  -> l.label
  | Seq (s1,s2)     -> first s1
  | If (l,_,_,_)    -> l.label
  | For (l,_,_)     -> l.label
(*| Choice (l,_,_)  -> l
  | Chread (l,_,_)  -> l
  | Chwrite (l,_,_) -> l
  | Stop l          -> l*)
  | Print (l,_)     -> l.label
  | Select (l,_)    -> l.label

(*  last : stmt -> label list  *)
let rec last s = match s with
  | Skip l           -> [l.label]
  | Assign (l,_,_)   -> [l.label]
  | Seq (s1,s2)      -> last s2
  | If (_,_,s1,s2)   -> (last s1) @ (last s2)
  | For (l,_,_)      -> [l.label]
(*| Choice (_,s1,s2) -> (last s1) @ (last s2)
  | Chread (l,_,_)   -> [l]
  | Chwrite (l,_,_)  -> [l]
  | Stop l           -> [l]*)
  | Print (l,_)      -> [l.label]
  | Select (_,cs)    ->
    List.fold_right (fun (_,c) acc -> match c with
      | ReadCase ((*_,*)_,_,s)  -> (last s)@acc
      | WriteCase ((*_,*)_,_,s) -> (last s)@acc) cs []
