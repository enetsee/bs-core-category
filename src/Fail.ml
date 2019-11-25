  module type S = sig 
    type 'a t
    type 'a error 
    val fail : _ error -> 'a t
    val catch : 'a t -> f:(_ error -> 'a t) -> 'a t    
  end