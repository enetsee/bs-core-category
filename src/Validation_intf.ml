
module type S = sig 
  module Error : Semigroup.S0
  
  type 'a t = 
    | Success of 'a
    | Failure of Error.t
    
  include Selective.S with type 'a t := 'a t 
  
  val success : 'a -> 'a t 
  val failure : Error.t -> 'a t
end

module type S2 = sig 
  module Error : Semigroup.S1
  
  type ('a,'b) t = 
    | Success of 'a
    | Failure of 'b Error.t
    
  include Selective.S2 with type ('a,'b) t := ('a,'b) t 
  
  val success : 'a -> ('a,_) t 
  val failure : 'b Error.t -> ('a,'b) t
end


module type Validation = sig 
  module type S = S
  module type S2 = S2
  module Make(Error : Semigroup.S0) : S with module Error := Error
  module Make2(Error : Semigroup.S1) : S2 with module Error := Error
end