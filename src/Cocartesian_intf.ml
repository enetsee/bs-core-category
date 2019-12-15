module type Minimal2 = sig 
  include Category.Minimal2
  val inl : ('a, ('a,'b) EitherBase.t) t
  val inr : ('b, ('a,'b) EitherBase.t) t
  val sum : ('a,'b) t -> ('a,'c) t -> ('a,('b,'c) Either.t) t
end

module type Infix2 = sig 
  type ('a,'b) t
  val (|+|) : ('a,'b) t -> ('a,'c) t -> ('a,('b,'c) Either.t) t
end

module type S2 = sig 
  include Category.S2
  val inl : ('a, ('a,'b) EitherBase.t) t
  val inr : ('b, ('a,'b) EitherBase.t) t
  val sum : ('a,'b) t -> ('a,'c) t -> ('a,('b,'c) Either.t) t
  module CocartesianInfix : Infix2 with type ('b,'a) t := ('b,'a) t
  include Infix2 with type ('b,'a) t := ('b,'a) t
end

module type Cocartesian = sig 
  module type Minimal2 = Minimal2 
  module type Infix2 = Infix2 
  module type S2 = S2
  module Make2(X:Minimal2) : S2 with type ('b,'a) t := ('b,'a) X.t
end