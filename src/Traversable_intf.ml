
module type T = sig 
  include TyCon.S1
  module F : Applicative.S  
  val traverse : 'a t -> f:('a -> 'b F.t) -> 'b t F.t 
end

module type S = sig 
  include TyCon.S1
  module MakeTraversable(F: Applicative.S) : T with type 'a t := 'a t and module F := F
end

