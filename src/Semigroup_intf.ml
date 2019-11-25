
module type Minimal0 = sig
  include TyCon.S0
  val combine : t -> t -> t
end

module type Infix0 = sig 
  include TyCon.S0
  val (<>) : t -> t -> t     
end

module type S0 = sig 
  include Minimal0
  module Semigroup_infix : Infix0 with type t := t
  include Infix0 with type t := t
end


module type Minimal1 = sig
  include TyCon.S1
  val combine : 'a t -> 'a t -> 'a t
end

module type Infix1 = sig 
  include TyCon.S1
  val (<>) : 'a t -> 'a t -> 'a t     
end

module type S1 = sig 
  include Minimal1
  module Semigroup_infix : Infix1 with type 'a t := 'a t
  include Infix1 with type 'a t := 'a t
end


module type Minimal2 = sig
  include TyCon.S2
  val combine : ('a,'b) t -> ('a,'b) t -> ('a,'b) t
end

module type Infix2 = sig 
  include TyCon.S2
  val (<>) : ('a,'b) t -> ('a,'b) t -> ('a,'b) t     
end

module type S2 = sig 
  include Minimal2
  module Semigroup_infix : Infix2 with type ('a,'b) t := ('a,'b) t
  include Infix2 with type ('a,'b) t := ('a,'b) t
end

module type Semigroup = sig 
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
