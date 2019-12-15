module type Minimal2 = sig 
  include Cartesian.Minimal2
  include Cocartesian.Minimal2 with type ('a,'b) t := ('a,'b) t 
  val distl : (('a * 'u, 'a * 'v) EitherBase.t, 'a * ('u,'v) Either.t) t
  val distr : ('a * ('u,'v) Either.t, ('a * 'u, 'a * 'v) Either.t) t
end 

module type S2 = sig 
  include Minimal2 
  include Cartesian.S2 with type ('a,'b) t := ('a,'b) t
  include Cocartesian.S2 with type ('a,'b) t := ('a,'b) t and module CategoryInfix := CategoryInfix
end

module type Distributive = sig
  module type Minimal2 = Minimal2 
  module type S2 = S2 
  module Make2(X:Minimal2) : S2 with type ('a,'b) t := ('a,'b) X.t
end
