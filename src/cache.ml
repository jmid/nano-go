module Make (L : Sigs.LATTICE) =
struct
  module IMap
(* : ( Map.S with type 'a t = L.elem Map.S.t ) *)
    = Map.Make(struct
                 type t = int
		 let compare = Pervasives.compare
               end)

    type elem =
      | Nonbot of L.elem IMap.t
      | Exttop of L.elem IMap.t (* top cache, "downgraded" at selected entries *)

    let empty = Nonbot IMap.empty

    (*  lookup : cache -> int -> L.elem  *)
    let lookup e x = match e with
      | Nonbot e -> (try IMap.find x e with Not_found -> L.bot)
      | Exttop e -> (try IMap.find x e with Not_found -> L.top)
		      
    (*  extend : cache -> int -> L.elem -> cache  *)
    let extend e x v = match e with
      | Nonbot m ->
	Nonbot (IMap.add x v m)
      | Exttop m ->
	if L.leq L.top v
	then
	  if IMap.mem x m
	  then Exttop (IMap.remove x m)
	  else e
	else Exttop (IMap.add x v m)

    (* This is a fold, specialized to accumulators over L.elem *)
    (*   which will either accumulate from bot or top *)
    (*  fold : (int -> L.elem -> L.elem -> L.elem) -> cache -> L.elem  *)
    (*  fold : (int -> L.elem -> 'a -> 'a) -> cache -> 'a -> 'a  *)
    let fold f c = match c with
      | Nonbot c -> IMap.fold f c L.bot
      | Exttop c -> IMap.fold f c L.top

    let bot = empty
    let top = Exttop IMap.empty

    (*  leq : elem -> elem -> bool  *)
    let leq m m' = match m,m' with
      | Nonbot m, _ -> IMap.for_all (fun v e -> L.leq e (lookup m' v)) m
      | Exttop sm, Exttop sm' -> (IMap.for_all (fun v e -> L.leq e (lookup m' v)) sm)
	                      && (IMap.for_all (fun v e -> L.leq (lookup m v) e) sm')
      | Exttop m, _  -> false
	
    (*  join : elem -> elem -> elem  *)
    let join m m' = match m,m' with
      | Nonbot m,Nonbot m' ->
	Nonbot (IMap.merge (fun v e e' -> match (e,e') with
	                      |   None, None    -> None
			      | Some _, None    -> e
			      |   None, Some _  -> e'
			      | Some l, Some l' -> Some (L.join l l')) m m')
      | Nonbot m,Exttop m' ->
	Exttop (IMap.merge (fun v e e' -> match (e,e') with
	                      |   None, None    -> None
			      | Some _, None    -> None
			      |   None, Some _  -> e'
			      | Some l, Some l' -> Some (L.join l l')) m m')
      | Exttop m,Nonbot m' ->
	Exttop (IMap.merge (fun v e e' -> match (e,e') with
	                      |   None, None    -> None
			      | Some _, None    -> e
			      |   None, Some _  -> None
			      | Some l, Some l' -> Some (L.join l l')) m m')
      | Exttop m,Exttop m' ->
	Exttop (IMap.merge (fun v e e' -> match (e,e') with
	                      |   None, None    -> None
			      | Some _, None    -> None
			      |   None, Some _  -> None
			      | Some l, Some l' -> Some (L.join l l')) m m')
	
    (*  meet : elem -> elem -> elem  *)
    let meet m m' = match m,m' with
      | Nonbot m,Nonbot m' ->
	Nonbot (IMap.merge (fun v e e' -> match (e,e') with
	                      |   None, None    -> None
			      | Some _, None    -> None
			      |   None, Some _  -> None
			      | Some l, Some l' -> Some (L.meet l l')) m m')
      | Nonbot m,Exttop m' ->
	Nonbot (IMap.merge (fun v e e' -> match (e,e') with
	                      |   None, None    -> None
			      | Some _, None    -> e
			      |   None, Some _  -> None
			      | Some l, Some l' -> Some (L.meet l l')) m m')
      | Exttop m,Nonbot m' ->
	Nonbot (IMap.merge (fun v e e' -> match (e,e') with
	                      |   None, None    -> None
			      | Some _, None    -> None
			      |   None, Some _  -> e'
			      | Some l, Some l' -> Some (L.meet l l')) m m')
      | Exttop m,Exttop m' ->
	Exttop (IMap.merge (fun v e e' -> match (e,e') with
	                      |   None, None    -> None
			      | Some _, None    -> e
			      |   None, Some _  -> e'
			      | Some l, Some l' -> Some (L.meet l l')) m m')

    (*  widening : elem -> elem -> elem  *)
    let widening m m' = match m,m' with
      | Nonbot m,Nonbot m' ->
	Nonbot (IMap.merge (fun v e e' -> match (e,e') with
	                      |   None, None    -> None
			      | Some _, None    -> e
			      |   None, Some _  -> e'
			      | Some l, Some l' -> Some (L.widening l l')) m m')
      | Nonbot m,Exttop m' ->
	Exttop (IMap.merge (fun v e e' -> match (e,e') with
	                      |   None, None    -> None
			      | Some _, None    -> None
			      |   None, Some _  -> e'
			      | Some l, Some l' -> Some (L.widening l l')) m m')
      | Exttop m,Nonbot m' ->
	Exttop (IMap.merge (fun v e e' -> match (e,e') with
	                      |   None, None    -> None
			      | Some _, None    -> e
			      |   None, Some _  -> None
			      | Some l, Some l' -> Some (L.widening l l')) m m')
      | Exttop m,Exttop m' ->
	Exttop (IMap.merge (fun v e e' -> match (e,e') with
	                      |   None, None    -> None
			      | Some _, None    -> None
			      |   None, Some _  -> None
			      | Some l, Some l' -> Some (L.widening l l')) m m')

    (*  narrowing : elem -> elem -> elem  *)
    let narrowing m m' = match m,m' with
      | Nonbot m,Nonbot m' ->
	Nonbot (IMap.merge (fun v e e' -> match (e,e') with
	                      |   None, None    -> None
			      | Some _, None    -> e
			      |   None, Some _  -> e'
			      | Some l, Some l' -> Some (L.narrowing l l')) m m')
      | Nonbot m,Exttop m' ->
	Exttop (IMap.merge (fun v e e' -> match (e,e') with
	                      |   None, None    -> None
			      | Some _, None    -> None
			      |   None, Some _  -> e'
			      | Some l, Some l' -> Some (L.narrowing l l')) m m')
      | Exttop m,Nonbot m' ->
	Exttop (IMap.merge (fun v e e' -> match (e,e') with
	                      |   None, None    -> None
			      | Some _, None    -> e
			      |   None, Some _  -> None
			      | Some l, Some l' -> Some (L.narrowing l l')) m m')
      | Exttop m,Exttop m' ->
	Exttop (IMap.merge (fun v e e' -> match (e,e') with
	                      |   None, None    -> None
			      | Some _, None    -> None
			      |   None, Some _  -> None
			      | Some l, Some l' -> Some (L.narrowing l l')) m m')
	  
    (*  eq : elem -> elem -> bool  *)
    let eq m m' = leq m m' && leq m' m

    (*  fpprint :  Format.formatter -> elem -> unit  *)
    let fpprint fmt m = match m with
      | Nonbot m ->
	begin
	  Format.fprintf fmt "[";
	  ignore(IMap.fold
		   (fun v e sep ->
		     begin
		       Format.fprintf fmt "%s%i -> " sep v;
		       L.fpprint fmt e;
		       "; "
 	 	     end) m "");
	  Format.fprintf fmt "]";
	end
      | Exttop m ->
	begin
	  Format.fprintf fmt "Top[";
	  ignore(IMap.fold
		   (fun v e sep ->
		     begin
		       Format.fprintf fmt "%s%i -> " sep v;
		       L.fpprint fmt e;
		       "; "
 	 	     end) m "");
	  Format.fprintf fmt "]";
	end

    (*  to_string : elem -> string  *)
    let to_string m = match m with
      | Nonbot m ->
	let acc,_ =
	  IMap.fold
	     (fun v e (acc,sep) ->
	       (acc ^ sep ^ (string_of_int v) ^ " -> " ^ (L.to_string e), "; "))
	     m ("","") in
	"[" ^ acc ^ "]"
      | Exttop m ->
	let acc,_ =
	  IMap.fold
	    (fun v e (acc,sep) ->
	      (acc ^ sep ^ (string_of_int v) ^ " -> " ^ (L.to_string e), "; "))
	    m ("","") in
	  "Top[" ^ acc ^ "]"

    (*  pprint : elem -> unit  *)
    let pprint = fpprint Format.std_formatter
end
