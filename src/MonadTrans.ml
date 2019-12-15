    
module type S1 = sig 
  module M : Monad.S1
  include Monad.S1
  val lift : 'a M.t -> 'a t
end 
    
module type S12 = sig 
  module M : Monad.S1
  include Monad.S2  
  val lift : 'a M.t -> ('a,'e) t
end

module type S21 = sig 
  module M : Monad.S2
  include Monad.S1
  val lift : ('a,'e) M.t -> 'a t
end

module type S2 = sig 
  module M : Monad.S2
  include Monad.S2
  val lift : ('a,'e) M.t -> ('a,'e) t
end

module type S23 = sig 
  module M : Monad.S2
  include Monad.S3
  val lift : ('a,'e) M.t -> ('a,'d,'e) t
end

module type S32 = sig 
  module M : Monad.S3
  include Monad.S2
  val lift : ('a,'d,'e) M.t -> ('a,'e) t
end

module type S3 = sig 
  module M : Monad.S3
  include Monad.S3
  val lift : ('a,'d,'e) M.t -> ('a,'d,'e) t
end
