
module type S = sig 
  module X : TyCon.S0
  include Monoid.S0
  val runEndo : t -> X.t -> X.t
end

module type Endo = sig
  module Make(X: TyCon.S0) : S with module X := X
end

