module type S = sig
  module M : Monoid.S0
  type 'a t 
  val getConst : 'a t -> M.t
  val const : M.t -> 'a t
  include Applicative.S with type 'a t := 'a t 
  module Over : sig
    include Selective.S with type 'a t := 'a t 
    val getConst : 'a t -> M.t
    val const : M.t -> 'a t
  end
  
  module Under : sig
    include Selective.S with type 'a t := 'a t
    val getConst : 'a t -> M.t
    val const : M.t -> 'a t
  end
end

module type Const = sig 
  module type S = S 
  module Make(M : Monoid.S0) : S with module M := M
end