  module type S = sig 
    module M : sig type 'a t end
    type 'a t
    val lift : 'a M.t -> 'a t 
  end