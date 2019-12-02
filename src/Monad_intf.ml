(*** -- Minimal definitions -- ***)
module Either = EitherBase

module type Minimal = sig 
  include TyCon.S1
  val return : 'a -> 'a t 
  val bind : 'a t -> f:('a ->'b t) -> 'b t
  val apply : [`Using_bind | `Custom of 'a t -> f:('a -> 'b) t -> 'b t ]
  val map : [`Using_apply | `Custom of 'a t -> f:('a -> 'b) -> 'b t]  
  val liftA2 : [`Using_apply | `Custom of 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t]
  val liftA3 : [`Using_apply | `Custom of 'a t -> 'b t -> 'c t -> f:('a -> 'b -> 'c -> 'd) -> 'd t]
  val discardFirst : [`Using_apply | `Custom of _ t -> 'b t -> 'b t]
  val discardSecond : [`Using_apply | `Custom of 'a t -> _ t -> 'a t]
  val select : [`Using_bind | `Custom of ('a,'b) Either.t t -> f:('a -> 'b) t -> 'b t ]
end

module type Minimal2 = sig 
  include Functor.Minimal2
  val return : 'a -> ('a,_) t 
  val bind : ('a,'e) t -> f:('a -> ('b,'e) t) -> ('b,'e) t
  val apply : [`Using_bind | `Custom of ('a,'e) t -> f:('a -> 'b,'e) t -> ('b,'e) t ]
  val map : [`Using_apply | `Custom of ('a,'e) t -> f:('a -> 'b) -> ('b,'e) t]  
  val liftA2 : [`Using_apply | `Custom of ('a,'e) t -> ('b,'e) t -> f:('a -> 'b -> 'c) -> ('c,'e) t]
  val liftA3 : [`Using_apply | `Custom of ('a,'e) t -> ('b,'e) t -> ('c,'e) t -> f:('a -> 'b -> 'c -> 'd) -> ('d,'e) t]
  val discardFirst : [`Using_apply | `Custom of (_,'e) t -> ('b,'e) t -> ('b,'e) t]
  val discardSecond : [`Using_apply | `Custom of ('a,'e) t -> (_,'e) t -> ('a,'e) t]
  val select : [`Using_bind | `Custom of (('a,'b) Either.t,'e) t -> f:('a -> 'b,'e) t -> ('b,'e) t]
end

module type Minimal3 = sig 
  include Functor.Minimal3
  val return : 'a -> ('a,_,_) t 
  val bind : ('a,'d,'e) t -> f:('a -> ('b,'d,'e) t) -> ('b,'d,'e) t
  val apply : [`Using_bind | `Custom of ('a,'d,'e) t -> f:('a -> 'b,'d,'e) t -> ('b,'d,'e) t ]
  val map : [`Using_apply | `Custom of ('a,'d,'e) t -> f:('a -> 'b) -> ('b,'d,'e) t]  
  val liftA2 : [`Using_apply | `Custom of ('a,'d,'e) t -> ('b,'d,'e) t -> f:('a -> 'b -> 'c) -> ('c,'d,'e) t]
  val liftA3 : [`Using_apply | `Custom of ('a,'e,'f) t -> ('b,'e,'f) t -> ('c,'e,'f) t -> f:('a -> 'b -> 'c -> 'd) -> ('d,'e,'f) t]
  val discardFirst : [`Using_apply | `Custom of (_,'d,'e) t -> ('b,'d,'e) t -> ('b,'d,'e) t]
  val discardSecond : [`Using_apply | `Custom of ('a,'d,'e) t -> (_,'d,'e) t -> ('a,'d,'e) t]
  val select : [`Using_bind | `Custom of (('a,'b) Either.t,'d,'e) t -> f:('a -> 'b,'d,'e) t -> ('b,'d,'e) t]
end

(*** -- Infix functions -- ***)

module type Infix = sig
  include TyCon.S1
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (>>~) : _ t -> 'b t -> 'b t
  val (>=>) : ('a -> 'b t) -> ('b -> 'c t) -> ('a -> 'c t)
end

module type Infix2 = sig
  include TyCon.S2
  val (>>=) : ('a,'e) t -> ('a -> ('b,'e) t) -> ('b,'e) t
  val (>>~) : (_,'e) t -> ('a,'e) t -> ('a,'e) t
  val (>=>) : ('a -> ('b,'e) t) -> ('b -> ('c,'e) t) -> ('a -> ('c,'e) t)
end

module type Infix3 = sig
  include TyCon.S3
  val (>>=) : ('a,'d,'e) t -> ('a -> ('b,'d,'e) t) -> ('b,'d,'e) t
  val (>>~) : (_,'d,'e) t -> ('a,'d,'e) t -> ('a,'d,'e) t
  val (>=>) : ('a -> ('b,'d,'e) t) -> ('b -> ('c,'d,'e) t) -> ('a -> ('c,'d,'e) t)
end


(*** -- Complete definitions --- ***)

module type S = sig   
  include Selective.S
  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  module Monad_infix : Infix with type 'a t := 'a t
  include Infix with type 'a t := 'a t    
  val join : 'a t t -> 'a t
  val forever : (unit -> 'a t ) -> 'b t
  val sequenceM : 'a t list -> 'a list t
  val mapM : 'a list -> f:('a -> 'b t) -> 'b list t
  val mapM_ : 'a list -> f:('a -> 'b t) -> unit t
end

module type S2 = sig 
  include Selective.S2
  val bind : ('a,'e) t -> f:('a -> ('b,'e) t) -> ('b,'e) t
  module Monad_infix : Infix2 with type ('a,'b) t := ('a,'b) t
  include Infix2 with type ('a,'b) t := ('a,'b) t    
  val join : (('a,'e) t,'e) t -> ('a,'e) t
  val forever : (unit -> ('a,'e) t ) -> ('b,'e) t
  val sequenceM : ('a,'e) t list -> ('a list,'e) t
  val mapM : 'a list -> f:('a -> ('b,'e) t) -> ('b list,'e) t
  val mapM_ : 'a list -> f:('a -> ('b,'e) t) -> (unit,'e) t
end

module type S3 = sig   
  include Selective.S3 
  val bind : ('a,'d,'e) t -> f:('a -> ('b,'d,'e) t) -> ('b,'d,'e) t
  module Monad_infix : Infix3 with type ('a,'b,'c) t := ('a,'b,'c) t
  include Infix3 with type ('a,'b,'c) t := ('a,'b,'c) t    
  val join : (('a,'d,'e) t,'d,'e) t -> ('a,'d,'e) t
  val forever : (unit -> ('a,'d,'e) t ) -> ('b,'d,'e) t
  val sequenceM : ('a,'d,'e) t list -> ('a list,'d,'e) t
  val mapM : 'a list -> f:('a -> ('b,'d,'e) t) -> ('b list,'d,'e) t
  val mapM_ : 'a list -> f:('a -> ('b,'d,'e) t) -> (unit,'d,'e) t
end



module type Monad = sig 
    (*** -- Minimal definitions --- ***)
  module type Minimal = Minimal
  module type Minimal2 = Minimal2
  module type Minimal3 = Minimal3

  (*** -- Infix functions --- ***)
  module type Infix = Infix 
  module type Infix2 = Infix2 
  module type Infix3 = Infix3 

  (*** -- Complete definitions -- ***)
  module type S = S
  module type S2 = S2
  module type S3 = S3

  (*** -- Functors  -- ***)
  
  module S_to_S2 (X : S) : S2 with type ('a,'e) t = 'a X.t 
  module S2_to_S (X : S2) : S with type 'a t = ('a,unit) X.t
  module S2_to_S3 (X : S2) : S3 with type ('a, 'b,_) t = ('a, 'b) X.t
  module S3_to_S2 (X : S3) : S2 with type ('a, 'b) t = ('a, 'b, unit) X.t

  module Make(X:Minimal) : S with type 'a t := 'a X.t
  module Make2(X:Minimal2) : S2 with type ('a,'b) t := ('a,'b) X.t
  module Make3(X:Minimal3) : S3 with type ('a,'b,'c) t := ('a,'b,'c) X.t

  module Make_backwards(X:Minimal) : S with type 'a t := 'a X.t
  module Make_backwards2(X:Minimal2) : S2 with type ('a,'b) t := ('a,'b) X.t
  module Make_backwards3(X:Minimal3) : S3 with type ('a,'b,'c) t := ('a,'b,'c) X.t

end