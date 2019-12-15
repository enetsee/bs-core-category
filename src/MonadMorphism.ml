module type S1 = sig 
  module T : MonadTrans.S1
  include MonadTrans.S1  
  include NaturalTrans.S1 with module M := T.M and module N := M 
  val hoist : nat -> 'a T.t -> 'a t
end

module type S2 = sig 
  module T : MonadTrans.S2
  include MonadTrans.S2  
  include NaturalTrans.S2 with module M := T.M and module N := M   
  val hoist : nat -> ('a,'b) T.t -> ('a,'b) t
end

module type S21 = sig 
  module T : MonadTrans.S2
  include MonadTrans.S1  
  include NaturalTrans.S21 with module M := T.M and module N := M   
  val hoist : nat -> ('a,'b) T.t -> 'a t
end

module type S12 = sig 
  module T : MonadTrans.S1
  include MonadTrans.S2  
  include NaturalTrans.S12 with module M := T.M and module N := M   
  val hoist : nat -> 'a T.t -> ('a,'b) t
end

module type S3 = sig
  module T : MonadTrans.S3
  include MonadTrans.S3  
  include NaturalTrans.S3 with module M := T.M and module N := M   
  val hoist : nat -> ('a,'b,'c) T.t -> ('a,'b,'c) t
end

module type S32 = sig
  module T : MonadTrans.S3
  include MonadTrans.S2
  include NaturalTrans.S32 with module M := T.M and module N := M   
  val hoist : nat -> ('a,'b,'c) T.t -> ('a,'b) t
end

module type S23 = sig
  module T : MonadTrans.S2
  include MonadTrans.S3  
  include NaturalTrans.S23 with module M := T.M and module N := M   
  val hoist : nat -> ('a,'b) T.t -> ('a,'b,'c) t
end