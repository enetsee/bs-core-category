
module type T1 = sig 
  include TyCon.S1
  module F : Applicative.S1
  val traverse : 'a t -> f:('a -> 'b F.t) -> 'b t F.t 
end

module type S1 = sig 
  include TyCon.S1
  module MakeTraversable(F: Applicative.S1) : T1 with type 'a t := 'a t and module F := F
end

