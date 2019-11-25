
module type S0 = sig 
  module M : Monoid.S0 
  include Monoid.S0
  val abs : t -> M.t 
  val rep : M.t -> t 
end

module type DiffMonoid = sig 
  module type S0 = S0
  module Make0(M:Monoid.S0) : S0 with module M := M
end

