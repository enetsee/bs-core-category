  module type S = sig 
    include MonadTransformer.S
    module T : MonadTransformer.S
    include NaturalTransformation.S with module M := T.M and module N := M 
    val hoist : nat -> 'a T.t -> 'a t
  end
  
  module type S2 = sig 
    include MonadTransformer.S2
    module T : MonadTransformer.S2
    include NaturalTransformation.S with module M := T.M and module N := M 
    
    val hoist : nat -> ('a,'b) T.t -> ('a,'b) t
  end