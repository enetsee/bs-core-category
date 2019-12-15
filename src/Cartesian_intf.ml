module type Minimal2  = sig
  include Category.Minimal2
  val product : ('a,'b) t -> ('a,'c) t -> ('a,'b *'c) t
  val exl : ('a * 'b , 'a) t
  val exr : ('a * 'b , 'b) t
end
  
module type Infix2 = sig 
  include TyCon.S2
  val ( |*| ) : ('a,'b) t -> ('a,'c) t -> ('a , 'b * 'c) t
end
  

module type S2 = sig 
  include Category.S2
  val product : ('a,'b) t -> ('a,'c) t -> ('a,'b *'c) t
  val exl : ('a * 'b , 'a) t
  val exr : ('a * 'b , 'b) t
  module CartesianInfix : Infix2 with type ('a,'b) t := ('a,'b) t
  include Infix2 with type ('a,'b) t := ('a,'b) t
end

module type Cartesian = sig
  module type Minimal2 = Minimal2 
  module type Infix2 = Infix2 
  module type S2 = S2 
  module Make2(X:Minimal2) : S2 with type ('a,'b) t := ('a,'b) X.t
end
  
