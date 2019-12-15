(*** -- Minimal definitions -- ***)

module type Minimal1 = sig 
  include Monad.Minimal1 
  include Plus.Minimal1 with type 'a t := 'a t
end

module type Minimal2 = sig
  include Monad.Minimal2
  include Plus.Minimal2 with type ('a,'b) t := ('a,'b) t
end

module type Minimal3 = sig
  include Monad.Minimal3
  include Plus.Minimal3 with type ('a,'b,'c) t := ('a,'b,'c) t
end

(*** -- Custom definitions -- ***)

module type Custom1 = sig 
  include Monad.Custom1 
  include Plus.Custom1 with type 'a t := 'a t
end

module type Custom2 = sig
  include Monad.Custom2
  include Plus.Custom2 with type ('a,'b) t := ('a,'b) t
end

module type Custom3 = sig
  include Monad.Custom3
  include Plus.Custom3 with type ('a,'b,'c) t := ('a,'b,'c) t
end

(*** -- Complete definitions -- ***)

module type S1 = sig 
  include Monad.S1 
  include Alternative.S1 with type 'a t := 'a t and module FunctorInfix := FunctorInfix and module ApplyInfix := ApplyInfix
end

module type S2 = sig
  include Monad.S2
  include Alternative.S2 with type ('a,'b) t := ('a,'b) t and module FunctorInfix := FunctorInfix and module ApplyInfix := ApplyInfix
end

module type S3 = sig
  include Monad.S3
  include Alternative.S3 with type ('a,'b,'c) t := ('a,'b,'c) t and module FunctorInfix := FunctorInfix and module ApplyInfix := ApplyInfix
end

(*** -- Module signature -- ***)

module type MonadPlus = sig

 (*** -- Minimal definitions --- ***)
  
  module type Minimal1 = Minimal1
  module type Minimal2 = Minimal2
  module type Minimal3 = Minimal3

  (*** -- Custom definitions --- ***)
  
  module type Custom1 = Custom1
  module type Custom2 = Custom2
  module type Custom3 = Custom3

  (*** -- Complete definitions -- ***)
  
  module type S1 = S1
  module type S2 = S2
  module type S3 = S3

  (*** -- Functors  -- ***)
  
  module S1_to_S2 (X : S1) : S2 with type ('a,'e) t = 'a X.t 
  module S2_to_S1 (X : S2) : S1 with type 'a t = ('a,unit) X.t
  module S2_to_S3 (X : S2) : S3 with type ('a, 'b,_) t = ('a, 'b) X.t
  module S3_to_S2 (X : S3) : S2 with type ('a, 'b) t = ('a, 'b, unit) X.t

  module MakeCustom1(X:Custom1) : S1 with type 'a t := 'a X.t
  module MakeCustom2(X:Custom2) : S2 with type ('a,'b) t := ('a,'b) X.t
  module MakeCustom3(X:Custom3) : S3 with type ('a,'b,'c) t := ('a,'b,'c) X.t

  module Make1(X:Minimal1) : S1 with type 'a t := 'a X.t
  module Make2(X:Minimal2) : S2 with type ('a,'b) t := ('a,'b) X.t
  module Make3(X:Minimal3) : S3 with type ('a,'b,'c) t := ('a,'b,'c) X.t

end