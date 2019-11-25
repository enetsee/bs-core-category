  module type S = sig 
    module M : sig type 'a t end 
    module N : sig type 'a t end
    type nat = { apply : 'a. 'a M.t -> 'a N.t }
  end