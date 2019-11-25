module type Minimal0 = sig
  include Semigroup.Minimal0
  val empty : t
end

module type S0 = sig 
  include Minimal0
  include Semigroup.S0 with type t := t
end

module type Minimal1 = sig
  include Semigroup.Minimal1
  val empty : 'a t
end

module type S1 = sig 
  include Minimal1
  include Semigroup.S1 with type 'a t := 'a t
end


module type Minimal2 = sig
  include Semigroup.Minimal2
  val empty : ('a,'b) t
end

module type S2 = sig 
  include Minimal2
  include Semigroup.S2 with type ('a,'b) t := ('a,'b) t
end



module type Monoid = sig 
  module type Minimal0 = Minimal0
  module type S0 = S0 
  module type Minimal1 = Minimal1
  module type S1 = S1
  module type Minimal2 = Minimal2
  module type S2 = S2
  module Make0(X:Minimal0) : S0 with type t := X.t  
  module MakeDual0(X:Minimal0) : S0 with type t := X.t
  module Make(X:Minimal1) : S1 with type 'a t := 'a X.t
  module MakeDual(X:Minimal1) : S1 with type 'a t := 'a X.t
  module Make2(X:Minimal2) : S2 with type ('a,'b) t := ('a,'b) X.t
  module MakeDual2(X:Minimal2) : S2 with type ('a,'b) t := ('a,'b) X.t
end


