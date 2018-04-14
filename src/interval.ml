(** Abstract interval datatype and operations *)

type lowerbound = MInf | LBound of int
type upperbound = PInf | UBound of int

type elem =
  | Bot
  | Interval of lowerbound * upperbound

(** A smart constructor to reduce intervals to bot *)
(* interval : lowerbound * upperbound -> interval *)
let interval (l,u) = match l,u with
  | MInf, _ -> Interval (l,u)
  | _, PInf -> Interval (l,u)
  | LBound l', UBound u' ->
    if l' <= u'
    then Interval (l,u)
    else Bot

(** A shorthand to construct unit intervals *)
(*  const : int -> elem  *)
let const c = Interval (LBound c,UBound c)

(*  lb_ge : lowerbound -> lowerbound -> bool  *)
let lb_ge l1 l2 = match (l1,l2) with
  | _, MInf -> true  (* no lower bound, all included *)
  | MInf, _ -> false (* second is lower bounded, hence not all included *)
  | LBound l1, LBound l2 -> l1 >= l2 (*in*)
(*  ub_le : upperbound -> upperbound -> bool  *)
let ub_le u1 u2 = match (u1,u2) with
  | _, PInf -> true  (* no upper bound, all included *)
  | PInf, _ -> false (* second is upper bounded, hence not all included *)
  | UBound u1, UBound u2 -> u1 <= u2 (*in*)
(*  leq : elem -> elem -> bool  *)
let leq i1 i2 = match i1,i2 with
  | Bot, _ -> true
  | _, Bot -> false (* first is not Bot *)
  | Interval (l1,u1), Interval (l2,u2) -> lb_ge l1 l2 && ub_le u1 u2

(*  eq : elem -> elem -> bool *)
let eq i i' = (i = i')

(*  top : elem  *)
let top = Interval (MInf,PInf)

(*  bot : elem  *)
let bot = Bot

(*  is_bot : elem -> bool  *)
let is_bot i = leq i bot

(*  lb_min : lowerbound -> lowerbound -> lowerbound  *)
let lb_min l1 l2 = match l1,l2 with
  | MInf, _ -> MInf
  | _, MInf -> MInf
  | LBound l1, LBound l2 -> LBound (min l1 l2)
(*  ub_max : upperbound -> upperbound -> upperbound  *)
let ub_max u1 u2 = match u1,u2 with
  | PInf, _ -> PInf
  | _, PInf -> PInf
  | UBound u1, UBound u2 -> UBound (max u1 u2)
(*  join : elem -> elem -> elem  *)
let join i1 i2 = match (i1,i2) with
  | Bot, _ -> i2
  | _, Bot -> i1
  | Interval (l1,u1), Interval (l2,u2) -> interval (lb_min l1 l2, ub_max u1 u2) (*use smartconst*)

(*  lb_max : lowerbound -> lowerbound -> lowerbound  *)
let lb_max l1 l2 = match l1,l2 with
  | MInf, _ -> l2
  | _, MInf -> l1
  | LBound l1, LBound l2 -> LBound (max l1 l2)
(*  ub_min : upperbound -> upperbound -> upperbound  *)
let ub_min u1 u2 = match u1,u2 with
  | PInf, _ -> u2
  | _, PInf -> u1
  | UBound u1, UBound u2 -> UBound (min u1 u2)
(*  meet : elem -> elem -> elem  *)
let meet i1 i2 = match (i1,i2) with
  | Bot, _ -> Bot
  | _, Bot -> Bot
  | Interval (l1,u1), Interval (l2,u2) -> interval (lb_max l1 l2, ub_min u1 u2) (*use smartconst*)

(*  widening : elem -> elem -> elem  *)
let widening i1 i2 =
  let wlb l1 l2 = match l1,l2 with
    | MInf, _ -> MInf
    | _, MInf -> MInf
    | LBound l1, LBound l2 -> if l2 < l1 then MInf else LBound l1 in
  let wub u1 u2 = match u1,u2 with
    | PInf, _ -> PInf
    | _, PInf -> PInf
    | UBound u1, UBound u2 -> if u2 > u1 then PInf else UBound u1 in
  match (i1,i2) with
    | Bot, _ -> i2
    | _, Bot -> i1
    | Interval (l1,u1), Interval (l2,u2) -> interval (wlb l1 l2, wub u1 u2) (*use smartconst*)

(*  narrowing : elem -> elem -> elem  *)
let narrowing i1 i2 =
  let wlb l1 l2 = match l1,l2 with
    | MInf, _ -> l2
    | _,_     -> l1 in
  let wub u1 u2 = match u1,u2 with
    | PInf, _ -> u2
    | _,_     -> u1 in
  match (i1,i2) with
    | Bot, _ -> Bot
    | _, Bot -> Bot
    | Interval (l1,u1), Interval (l2,u2) -> interval (wlb l1 l2, wub u1 u2) (*use smartconst*)
      
(*  lb_add : lowerbound -> lowerbound -> lowerbound  *)
let lb_add l1 l2 = match l1,l2 with
  | MInf, _ -> MInf
  | _, MInf -> MInf
  | LBound l1, LBound l2 -> LBound (l1+l2)
(*  ub_add : upperbound -> upperbound -> upperbound  *)
let ub_add u1 u2 = match u1,u2 with
  | PInf, _ -> PInf
  | _, PInf -> PInf
  | UBound u1, UBound u2 -> UBound (u1+u2)
(*  add : elem -> elem -> elem  *)
let add i1 i2 = match i1,i2 with
  | Bot, _ -> Bot
  | _, Bot -> Bot
  | Interval (l1,u1), Interval (l2,u2) -> interval (lb_add l1 l2, ub_add u1 u2) (*use smartconst*)

(*  lb_sub : lowerbound -> upperbound -> lowerbound  *)
let lb_sub l1 u2 = match l1,u2 with
  | MInf, _ -> MInf
  | _, PInf -> MInf
  | LBound l1, UBound l2 -> LBound (l1-l2)
(*  ub_sub : upperbound -> lowerbound -> upperbound  *)
let ub_sub u1 l2 = match u1,l2 with
  | PInf, _ -> PInf
  | _, MInf -> PInf
  | UBound u1, LBound u2 -> UBound (u1-u2)
(*  sub : elem -> elem -> elem  *)
let sub i1 i2 = match i1,i2 with
  | Bot, _ -> Bot
  | _, Bot -> Bot
  | Interval (l1,u1), Interval (l2,u2) -> interval (lb_sub l1 u2, ub_sub u1 l2) (*use smartconst*)

(* a collapsed lower- and upper-bound type, to locally circumvent type system *)
type mybound = NegInf | PosInf | Bound of int
(*  min_mb : mybound -> mybound -> mybound  *)
let min_mb b1 b2 = match b1,b2 with
  | NegInf, _ -> NegInf
  | _, NegInf -> NegInf
  | PosInf, _ -> b2
  | _, PosInf -> b1
  | Bound b, Bound b' -> if b<b' then b1 else b2
(*  max_mb : mybound -> mybound -> mybound  *)
let max_mb b1 b2 = match b1,b2 with
  | NegInf, _ -> b2
  | _, NegInf -> b1
  | PosInf, _ -> PosInf
  | _, PosInf -> PosInf
  | Bound b, Bound b' -> if b<b' then b2 else b1
(*  mult_mb : mybound -> mybound -> mybound  *)
let mult_mb b1 b2 = match b1,b2 with
  | NegInf, NegInf  -> PosInf
  | NegInf, PosInf  -> NegInf
  | NegInf, Bound b -> if b=0 then Bound 0 else if b>0 then NegInf else PosInf
  | PosInf, NegInf  -> NegInf
  | PosInf, PosInf  -> PosInf
  | PosInf, Bound b -> if b=0 then Bound 0 else if b>0 then PosInf else NegInf
  | Bound b,NegInf  -> if b=0 then Bound 0 else if b>0 then NegInf else PosInf
  | Bound b,PosInf  -> if b=0 then Bound 0 else if b>0 then PosInf else NegInf
  | Bound b,Bound b'-> Bound (b*b')
(*  lb2mb : lowerbound -> mybound  *)
let lb2mb l = match l with MInf -> NegInf | LBound l -> Bound l
(*  ub2mb : upperbound -> mybound  *)
let ub2mb u = match u with PInf -> PosInf | UBound u -> Bound u
(*  mult : elem -> elem -> elem  *)
let mult i1 i2 = match i1,i2 with
  | Bot, _ -> Bot
  | _, Bot -> Bot
  | Interval (l1,u1), Interval (l2,u2) ->
    let l1' = lb2mb l1 in
    let u1' = ub2mb u1 in
    let l2' = lb2mb l2 in
    let u2' = ub2mb u2 in
    let l1_l2 = mult_mb l1' l2' in
    let l1_u2 = mult_mb l1' u2' in
    let u1_l2 = mult_mb u1' l2' in
    let u1_u2 = mult_mb u1' u2' in
    let lb = min_mb (min_mb l1_l2 l1_u2) (min_mb u1_l2 u1_u2) in
    let ub = max_mb (max_mb l1_l2 l1_u2) (max_mb u1_l2 u1_u2) in
    interval ((match lb with
                | NegInf  -> MInf
	        | Bound b -> LBound b
	        | PosInf  -> failwith "Lower bound is +inf"),
	      (match ub with
	        | NegInf  -> failwith "Upper bound is -inf"
	        | Bound b -> UBound b
	        | PosInf  -> PInf)) (*use smartconst*)

(*  rem : elem -> elem -> elem  *)
let rem i1 i2 = match i1,i2 with
  | Bot, _ -> Bot
  | _, Bot -> Bot
  | Interval (l1,u1), Interval (l2,u2) ->
    (* split i1 into a neg. and a pos. interval, handle both separately, join results *)
    let _i1_low  = meet (Interval (MInf,UBound (-1))) i1 in
    let i1_high = meet (Interval (LBound 0,PInf)) i1 in
    (* normalize i2 to a non-neg interval *)
    let i2' = (match l2,u2 with
                | MInf, PInf         -> Interval (LBound 0,PInf)
		| MInf, UBound u     -> Interval (LBound (if u<0 then u else 0), PInf)
		| LBound l, PInf     -> Interval (LBound (if l<0 then 0 else l), PInf)
		| LBound l, UBound u -> if u<0 then Interval (LBound (-u), UBound (-l))
 		                         else if l<0 then Interval (LBound 0, UBound (max (-l) u))
					  else i2) in (* [l2;u2] with u>=0, l>=0 *)
    let _low_sol  = ( (* solve for i1_low i2' ... *) ) in
    let high_sol = (* solve for i1_high i2' ... *)
      (match i1_high,i2' with
	| Bot, _ -> Bot
	| _, Bot -> Bot
	| Interval (l1,u1), Interval (l2,u2) -> (match (l1,u1),(l2,u2) with
	    | (LBound l1,UBound u1), (LBound l2,UBound u2) when l1=u1 && l2=u2 ->
	      if l2 <> 0 then const (l1 mod l2) else Bot
(*	    | (LBound l1,UBound u1), _ when l1=u1 -> (* i1 is const *)
	    | _, (LBound l2,UBound u2) when l2=u2 -> (* i2 is const *) i1 *)
	    | _, (_,UBound u2) -> Interval (LBound 0, ub_min u1 (UBound (u2-1)))
	    | _, (_,PInf)      -> Interval (LBound 0, u1))) in
  (*join low_sol*)
      high_sol
(* min u1,u2-1 is a general upper bound *)
(* l1,if l2 = 0 then l2 else l2-1 is a general lower bound *)
(*  [1;10] mod [4;4] = [0;2]  since 4,8 \in [1;10], 10 mod 4 = 2 *)
(* [23;23] mod [1;5] = [0;3] *)
(*  [1;23] mod [1;5] = *)
(* [10;10] mod [2;9] = [0,1,2,0,4,3,2,1] *)
(*  [0;10] mod [0;10] *)
(* mod by 0: error, hence [0;10] mod [1;10] *)
      
type equiv_class = lowerbound * upperbound
type partition = equiv_class list (* non-empty, spans full range *)

(*  compare : equiv_class -> equiv_class -> int  *)
let compare (l,u) (l',u') =
  let compare_lb l l' = match l,l' with
    | MInf, MInf   -> 0
    | MInf, LBound _ -> -1
    | LBound _, MInf -> 1
    | LBound l, LBound l' ->
      if l<l' then -1 else
	if l>l' then 1 else 0 in
  let compare_ub u u' = match u,u' with
    | PInf, PInf   -> 0
    | PInf, UBound _ -> 1
    | UBound _, PInf -> -1
    | UBound u, UBound u' ->
      if u<u' then -1 else
	if l>l' then 1 else 0 in
  let cmp_lb = compare_lb l l' in
  if cmp_lb = 0 then compare_ub u u' else cmp_lb

(*  repr : equiv_class -> elem (an atom) *)
let repr (l,u) = match (l,u) with
  | MInf,PInf  -> const 0
  | _,UBound u -> const u
  | LBound l,_ -> const l

(*  project : equiv_class -> elem  *)
let project (l,u) = interval (l,u)
  
(*  partition : elem -> partition  *)
let partition i = match i with
  | Bot            -> [(MInf,PInf)] (* failwith "partition applied to Bot in interval"*)
  | Interval (l',u') ->
    (match (l',u') with
      |     MInf, PInf     -> [(l',u')]
      |     MInf, UBound u -> [(MInf,UBound u); (LBound (u+1),PInf)]
      | LBound l, PInf     -> [(MInf,UBound (l-1)); (LBound l,PInf)]
      | LBound l, UBound u -> [(MInf,UBound (l-1)); (l',u'); (LBound (u+1),PInf)])

(*  fold_partition : ('a -> equiv_class -> 'a) -> 'a -> partition -> 'a *)
let fold_partition = List.fold_left
  
(*  overlay_partitions : partition -> partition -> partition  *)
let rec overlay_partitions is1 is2 = match is1,is2 with
  | [],_ -> failwith "overlay_partitions: First argument is [] in interval.ml"
  | _,[] -> failwith "overlay_partitions: Second argument is [] in interval.ml"
  | [(l1,u1)], [(l2,u2)] ->
    if l1=l2
    then (if u1=u2 then is1 else failwith "overlay_partitions: invariant u1=u2 broken in interval.ml")
    else failwith "overlay_partitions: invariant l1=l2 broken in interval.ml"
  | (l1,u1)::is1', (l2,u2)::is2' ->
    if l1 = l2 then
      if ub_le u1 u2 then (* u1 <= u2 *)
	if ub_le u2 u1 then (* u1 = u2 *)
	  (l1,u1)::(overlay_partitions is1' is2')
	else (match u1 with (* u1 < u2 *)
	       | UBound u1' -> (l1,u1)::(overlay_partitions is1' ((LBound (u1'+1),u2)::is2'))
	       | PInf      -> failwith "overlay_partitions: u1 cannot be both lt u2 and PInf")
      else (match u2 with (* u1 > u2 *)
   	     | UBound u2' -> (l2,u2)::(overlay_partitions ((LBound (u2'+1),u1)::is1') is2')
	     | PInf      -> failwith "overlay_partitions: u2 cannot be both lt u1 and PInf")
    else failwith "overlay_partitions: invariant l1=l2 broken in interval.ml"

      
(** {3 Pretty printing routines } *)

(*  fpprint : Format.formatter -> elem -> unit  *)
let fpprint fmt i =
  let pp_lbound fmt l = match l with
    | MInf     -> Format.fprintf fmt "-oo"
    | LBound l -> Format.fprintf fmt "%i" l in
  let pp_ubound fmt u = match u with
    | PInf     -> Format.fprintf fmt "+oo"
    | UBound u -> Format.fprintf fmt "%i" u in
  match i with
  | Bot -> Format.fprintf fmt "Bot"
  | Interval (l,u) ->
    begin
      Format.fprintf fmt "[";
      pp_lbound fmt l;
      Format.fprintf fmt ";";
      pp_ubound fmt u;
      Format.fprintf fmt "]";
    end

(*  pprint : elem -> unit  *)
let pprint e =
  begin
    fpprint Format.std_formatter e;
    Format.print_flush ();
  end

(*  to_string : L.elem -> string  *)
let to_string i =
  let ts_lbound l = match l with
    | MInf     -> "-oo"
    | LBound l -> string_of_int l in
  let ts_ubound u = match u with
    | PInf     -> "+oo"
    | UBound u -> string_of_int u in
  match i with
    | Bot            -> "Bot"
    | Interval (l,u) -> "[" ^ (ts_lbound l) ^ ";" ^ (ts_ubound u) ^ "]"
