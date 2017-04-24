module type LATTICE =
sig
  type elem
  val leq       : elem -> elem -> bool
  val join      : elem -> elem -> elem
  val meet      : elem -> elem -> elem
  val bot       : elem
  val top       : elem 
  val widening  : elem -> elem -> elem
  val narrowing : elem -> elem -> elem
  val eq        : elem -> elem -> bool
  val to_string : elem -> string
  val pprint    : elem -> unit
  val fpprint   : Format.formatter -> elem -> unit
end

module type EXTLATTICE =
sig
  include LATTICE
  type equiv_class
  type partition
  val compare            : equiv_class -> equiv_class -> int
  val repr               : equiv_class -> elem
  val project            : equiv_class -> elem
  val partition          : elem -> partition
  val fold_partition     : ('a -> equiv_class -> 'a) -> 'a -> partition -> 'a
  val overlay_partitions : partition -> partition -> partition
end

  (*
module type MEXTLATTICE =
sig
  include EXTLATTICE
  val repr_pair : equiv_class -> elem * elem
end
  *)
