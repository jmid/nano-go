type binop = Plus | Minus | Mult | Mod (* | ... *)
type relop = Eq | Lt | Leq (* | ... *)
type var   = string
type chan  = int
    
type aexp =
  | Num of int
  | Var of var
  | Any
  | Binop of aexp * binop * aexp

type bexp =
  | True
  | False
  | Not of bexp
  | Relop of aexp * relop * aexp
  | Conj of bexp * bexp

type pos = Lexing.position

type stmt =
  | Skip of pos
  | Assign of pos * var * aexp
  | Seq of stmt * stmt
  | For of pos * bexp * stmt
  | If of pos * bexp * stmt * stmt
(*| Chread of var * chan
  | Chwrite of chan * aexp*)
  | Select of pos * (pos * case) list
  | Print of pos * aexp
(*  | Choice of stmt * stmt *)
(*  | Stop *)
and case =
  | ReadCase of var * chan * stmt
  | WriteCase of chan * aexp * stmt
      
type prog = chan list * (var list * stmt) list


(** AST pretty printers *)

(*  ppaexp : Ast.aexp -> string  *)
let rec ppaexp e = match e with
  | Num n            -> string_of_int n
  | Var x            -> x
  | Any              -> "?"
  | Binop (e1,op,e2) ->
    let str = match op with
      | Plus  -> "+"
      | Minus -> "-"
      | Mult  -> "*"
      | Mod   -> "%" in
    "(" ^ (ppaexp e1) ^ ") " ^ str ^ " (" ^ (ppaexp e2) ^ ")"

(** AST for programs in extended syntax *)

module StrSet = Set.Make(String)

(*  fv : aexp -> var set  *)
let rec fv e = match e with
  | Num _          -> StrSet.empty
  | Var x          -> StrSet.singleton x
  | Any            -> StrSet.empty
  | Binop (e,_,e') -> StrSet.union (fv e) (fv e')
    
(** AST for programs in extended syntax *)

type exchan = string

type exstmt =
  | ExAssign of pos * var * aexp
  | ExChread of pos * var option * exchan
  | ExChwrite of pos * exchan * aexp
  | ExFor of pos * bexp * exblock
(*  | ExWhile of bexp * exblock *)
  | ExIf of pos * bexp * exblock * exblock
  | ExSelect of pos * (pos * excase) list
  | ExPrint of pos * aexp
(*  | ExRepeatUntil of exblock * bexp 
  | ExStop *)
and excase =
  | ExReadCase of var option * exchan * exblock
  | ExWriteCase of exchan * aexp * exblock
and exblock = exstmt list

type exproc = var list * exblock
type exprog = exchan list * exproc list * exproc (*(var * exblock) list*)

let print_exproc (vars,exblock) =
  let _ = List.fold_left (fun acc v -> print_string (acc ^ v); ",") "vars: " vars in
  print_endline " ...some block"
  
type info = { mutable next : int;
	      orig_chname  : (int,string) Hashtbl.t;
	      int_chname   : (string,int) Hashtbl.t; }

let insert_chname ch info =
  if Hashtbl.mem info.int_chname ch
  then failwith ("Duplicate channel name " ^ ch)
  else 
    let num = info.next in
    begin
      info.next <- info.next+1;
      Hashtbl.add info.orig_chname num ch;
      Hashtbl.add info.int_chname ch num
    end

let lookup_chname ch info =
  if Hashtbl.mem info.int_chname ch
  then Hashtbl.find info.int_chname ch
  else failwith ("Unknown channel name " ^ ch)

let info = { next = 0;
	     orig_chname = Hashtbl.create 43;
	     int_chname  = Hashtbl.create 43; }

(*  reset_info : unit -> unit  *)
let reset_info () =
  begin
    info.next <- 0;
    Hashtbl.reset info.orig_chname;
    Hashtbl.reset info.int_chname;
  end

(*  print_info : Format.formatter -> unit  *)
let print_info fmt =
  Hashtbl.iter (fun chname i -> Format.fprintf fmt "%s -> %i\n" chname i) info.int_chname
    
(*  desugar_exstmt : exstmt -> info -> stmt  *)
let rec desugar_exstmt exs info = match exs with
  | ExAssign (pos,x,e)        -> Assign (pos,x,e)
  | ExFor (pos,b,exb)         -> For (pos,b,desugar_exblock exb info)
  | ExIf (pos,b,exb,exb')     -> let desb  = desugar_exblock exb info in
				 let desb' = desugar_exblock exb' info in
				 If (pos,b, desb, desb')
  | ExChread (pos,x_opt,ch)   -> let chnum = lookup_chname ch info in
				 let x = match x_opt with Some x -> x | None -> "_" in
				 Select (pos,[pos, ReadCase (x,chnum,Skip pos)])
  | ExChwrite (pos,ch,e)      -> let chnum = lookup_chname ch info in
				 Select (pos,[pos, WriteCase (chnum,e,Skip pos)])
  | ExSelect (pos,excs)       -> let cases = List.map (fun c -> desugar_excase c info) excs in
				 Select (pos,cases)
  | ExPrint (pos,e)           -> Print (pos,e)

and desugar_excase (pos,ec) info = match ec with
  | ExReadCase (x_opt,ch,exb) ->
    let chnum = lookup_chname ch info in
    let desb = desugar_exblock exb info in
    let x = match x_opt with Some x -> x | None -> "_" in
    (pos, ReadCase (x,chnum,desb))
  | ExWriteCase (ch,e,exb) ->
    let chnum = lookup_chname ch info in
    let desb = desugar_exblock exb info in
    (pos, WriteCase (chnum,e,desb))

(*  desugar_exblock : exblock -> info -> stmt  *)
and desugar_exblock exb info = match exb with
  | []       -> Skip Lexing.dummy_pos
  | [exs]    -> desugar_exstmt exs info
  | exs::exb -> let desst = desugar_exstmt exs info in
		let desb = desugar_exblock exb info in
		Seq (desst, desb)

(*  desugar_proc : exblock -> stmt  *)
let desugar_proc (vars,p) =
  let p' = (*Seq ( *) desugar_exblock p info (*, Skip)*) in
  (vars,p')

(*  desugar_prog : exprog -> prog  *)
let desugar_prog (chans,procs,proc) =
  let _ = reset_info () in
  let _ = List.iter (fun ch -> insert_chname ch info) chans in
  let procs' = List.map (fun proc -> desugar_proc proc) procs in
  let proc' = desugar_proc proc in
  (chans,procs'@[proc'])
