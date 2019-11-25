  module type S = sig 
    module M : Monad.S    
    include Monad.S
    include Transformer.S with type 'a t := 'a t and module M := M        
  end 
    
  module type S2 = sig 
    module M : Monad.S    
    include Monad.S2
    val lift : 'a M.t -> ('e,'a) t
  end 