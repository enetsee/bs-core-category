module type Minimal2 = sig 
  include Cartesian.Minimal2
  val ap : (('a -> 'b) * 'a , 'b) t
  val curry : ('a * 'b,'c) t -> ('a , ('b -> 'c)) t
  val uncurry : ('a , ('b -> 'c)) t -> ('a * 'b,'c) t 
end  

module type S2 = sig 
  include Minimal2
  include Cartesian.S2 with type ('a,'b) t := ('a,'b) t    
end  

module type Closed = sig 
  module type Minimal2 = Minimal2 
  module type S2 = S2 
  module Make2(X:Minimal2): S2 with type ('a,'b) t := ('a,'b) X.t  
end