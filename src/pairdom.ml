module Make(C1 : Sigs.LATTICE)(C2 : Sigs.LATTICE) =
struct
  type elem = C1.elem * C2.elem

  let bot = (C1.bot,C2.bot)

  let top = (C1.top,C2.top)

  let pair (c1,c2) = (c1,c2)

  (*  leq : elem -> elem -> bool  *)
  let leq (c1,c2) (d1,d2) = C1.leq c1 d1 && C2.leq c2 d2

  (*  eq : elem -> elem -> bool  *)
  let eq (c1,c2) (d1,d2) = C1.eq c1 d1 && C2.eq c2 d2
    
  (*  join : elem -> elem -> elem  *)
  let join (c1,c2) (d1,d2) = (C1.join c1 d1, C2.join c2 d2)

  (*  meet : elem -> elem -> elem  *)
  let meet (c1,c2) (d1,d2) = (C1.meet c1 d1, C2.meet c2 d2)

  (*  widening : elem -> elem -> elem  *)
  let widening (c1,c2) (d1,d2) = (C1.widening c1 d1, C2.widening c2 d2)

  (*  narrowing : elem -> elem -> elem  *)
  let narrowing (c1,c2) (d1,d2) = (C1.narrowing c1 d1, C2.narrowing c2 d2)
   

  (** {3 Pretty printing routines } *)

  (*  fpprint : Format.formatter -> elem -> unit  *)
  let fpprint fmt (c1,c2) =
    begin
      Format.fprintf fmt "(";
      C1.fpprint fmt c1; 
      Format.fprintf fmt ", ";
      C2.fpprint fmt c2;
      Format.fprintf fmt ")";
    end

  (*  pprint : elem -> unit  *)
  let pprint e =
    begin
      fpprint Format.std_formatter e;
      Format.print_flush ();
    end

  (*  to_string : L.elem -> string  *)
  let to_string (c1,c2) =
    let s1 = C1.to_string c1 in
    let s2 = C2.to_string c2 in
    "(" ^ s1 ^ ", " ^ s2 ^ ")"
end

module MakeExt(C1 : Sigs.EXTLATTICE)(C2 : Sigs.EXTLATTICE) =
struct
  include Make(C1)(C2)
  
  type equiv_class =
    | FstEqCl of C1.equiv_class
    | SndEqCl of C2.equiv_class
  type partition = C1.partition * C2.partition

  (*  compare : equiv_class -> equiv_class -> int  *)
  let compare eq eq' = match eq,eq' with
    | FstEqCl _, SndEqCl _ -> -1
    | SndEqCl _, FstEqCl _ -> 1
    | FstEqCl eq, FstEqCl eq' -> C1.compare eq eq'
    | SndEqCl eq, SndEqCl eq' -> C2.compare eq eq'

  (*  repr : equiv_class -> elem  *)
  let repr eqcl = match eqcl with
    | FstEqCl eqcl1 -> (C1.repr eqcl1, C2.bot)
    | SndEqCl eqcl2 -> (C1.bot, C2.repr eqcl2)

  (*  project : equiv_class -> elem  *)
  let project eqcl = match eqcl with
    | FstEqCl eqcl1 -> (C1.project eqcl1, C2.bot)
    | SndEqCl eqcl2 -> (C1.bot, C2.project eqcl2)

  (*  partition : elem -> partition  *)
  let partition (c,d) = (C1.partition c, C2.partition d)
    
  (*  fold_partition : ('a -> equiv_class -> 'a) -> 'a -> partition -> 'a  *)
  let fold_partition f acc (part1,part2) =
    let acc' = C1.fold_partition (fun acc' eq1 -> f acc' (FstEqCl eq1)) acc part1 in
    C2.fold_partition (fun acc' eq2 -> f acc' (SndEqCl eq2)) acc' part2

  (*  overlay_partitions : partition -> partition -> partition  *)
  let overlay_partitions (p1,p2) (p1',p2') =
    (C1.overlay_partitions p1 p1', C2.overlay_partitions p2 p2')
end
