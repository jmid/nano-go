module Make(C1 : Sigs.LATTICE)(C2 : Sigs.LATTICE)(C3 : Sigs.LATTICE) =
struct
  type elem = C1.elem * C2.elem * C3.elem

  let bot = (C1.bot,C2.bot,C3.bot)

  let top = (C1.top,C2.top,C3.top)

  let prod ((c1,c2,c3) as trip) = trip

  (*  leq : elem -> elem -> bool  *)
  let leq (c1,c2,c3) (d1,d2,d3) = C1.leq c1 d1 && C2.leq c2 d2 && C3.leq c3 d3

  (*  eq : elem -> elem -> bool  *)
  let eq (c1,c2,c3) (d1,d2,d3) = C1.eq c1 d1 && C2.eq c2 d2 && C3.eq c3 d3
    
  (*  join : elem -> elem -> elem  *)
  let join (c1,c2,c3) (d1,d2,d3) = (C1.join c1 d1, C2.join c2 d2, C3.join c3 d3)

  (*  meet : elem -> elem -> elem  *)
  let meet (c1,c2,c3) (d1,d2,d3) = (C1.meet c1 d1, C2.meet c2 d2, C3.meet c3 d3)

  (*  widening : elem -> elem -> elem  *)
  let widening (c1,c2,c3) (d1,d2,d3) = (C1.widening c1 d1, C2.widening c2 d2, C3.widening c3 d3)

  (*  narrowing : elem -> elem -> elem  *)
  let narrowing (c1,c2,c3) (d1,d2,d3) = (C1.narrowing c1 d1, C2.narrowing c2 d2, C3.narrowing c3 d3)


  (** {3 Pretty printing routines } *)

  (*  fpprint : Format.formatter -> elem -> unit  *)
  let fpprint fmt (c1,c2,c3) =
    begin
      Format.fprintf fmt "(";
      C1.fpprint fmt c1;
      Format.fprintf fmt ", ";
      C2.fpprint fmt c2;
      Format.fprintf fmt ", ";
      C3.fpprint fmt c3;
      Format.fprintf fmt ")";
    end

  (*  pprint : elem -> unit  *)
  let pprint (c1,c2,c3) =
    let s1 = C1.to_string c1 in
    let s2 = C2.to_string c2 in
    let s3 = C3.to_string c3 in
    Format.printf "(%s, %s, %s)" s1 s2 s3

  (*  to_string : L.elem -> string  *)
  let to_string e =
    begin
      fpprint Format.str_formatter e;
      Format.flush_str_formatter ();
    end
end

module MakeExt(C1 : Sigs.EXTLATTICE)(C2 : Sigs.EXTLATTICE)(C3 : Sigs.EXTLATTICE) =
struct
  include Make(C1)(C2)(C3)
  
  type equiv_class =
    | FstEqCl of C1.equiv_class
    | SndEqCl of C2.equiv_class
    | TrdEqCl of C3.equiv_class

  type partition = C1.partition * C2.partition * C3.partition

  (*  compare : equiv_class -> equiv_class -> int  *)
  let compare eq eq' = match eq,eq' with
    | FstEqCl eq, FstEqCl eq' -> C1.compare eq eq'
    | SndEqCl eq, SndEqCl eq' -> C2.compare eq eq'
    | TrdEqCl eq, TrdEqCl eq' -> C3.compare eq eq'
    | FstEqCl _, _         -> -1
    | SndEqCl _, FstEqCl _ -> 1
    | SndEqCl _, TrdEqCl _ -> -1
    | TrdEqCl _, _         -> 1

  (*  repr : equiv_class -> elem  *)
  let repr eqcl = match eqcl with
    | FstEqCl eqcl1 -> (C1.repr eqcl1, C2.bot, C3.bot)
    | SndEqCl eqcl2 -> (C1.bot, C2.repr eqcl2, C3.bot)
    | TrdEqCl eqcl3 -> (C1.bot, C2.bot, C3.repr eqcl3)

  (*  project : equiv_class -> elem  *)
  let project eqcl = match eqcl with
    | FstEqCl eqcl1 -> (C1.project eqcl1, C2.bot, C3.bot)
    | SndEqCl eqcl2 -> (C1.bot, C2.project eqcl2, C3.bot)
    | TrdEqCl eqcl3 -> (C1.bot, C2.bot, C3.project eqcl3)

  (*  partition : elem -> partition  *)
  let partition (c,d,e) = (C1.partition c, C2.partition d, C3.partition e)
    
  (*  fold_partition : ('a -> equiv_class -> 'a) -> 'a -> partition -> 'a  *)
  let fold_partition f acc (part1,part2,part3) =
    let acc'  = C1.fold_partition (fun acc' eq1 -> f acc' (FstEqCl eq1)) acc part1 in
    let acc'' = C2.fold_partition (fun acc' eq2 -> f acc' (SndEqCl eq2)) acc' part2 in
                C3.fold_partition (fun acc' eq3 -> f acc' (TrdEqCl eq3)) acc'' part3

  (*  overlay_partitions : partition -> partition -> partition  *)
  let overlay_partitions (p1,p2,p3) (p1',p2',p3') =
    (C1.overlay_partitions p1 p1', C2.overlay_partitions p2 p2', C3.overlay_partitions p3 p3')
end
