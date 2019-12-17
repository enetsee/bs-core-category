(*** -- Minimal definitions -- ***)

module type Minimal1 = sig 
  include TyCon.S1
  val pure : 'a -> 'a t 
  val bind : 'a t -> f:('a ->'b t) -> 'b t
end 

module type Minimal2 = sig 
  include TyCon.S2
  val pure : 'a -> ('a,_) t 
  val bind : ('a,'e) t -> f:('a -> ('b,'e) t) -> ('b,'e) t
end 

module type Minimal3 = sig 
  include TyCon.S3
  val pure : 'a -> ('a,_,_) t 
  val bind : ('a,'d,'e) t -> f:('a -> ('b,'d,'e) t) -> ('b,'d,'e) t
end

(*** -- Custom definitions -- ***)

module type Custom1 = sig 
  include Minimal1
  val map : [`Derived | `Custom of 'a t -> f:('a -> 'b) -> 'b t]
  val replace : [`Derived | `Custom of 'a t -> const:'b -> 'b t]
  val apply : [`Derived | `Custom of 'a t -> f:('a -> 'b) t -> 'b t ]  
  val liftA2 : [`Derived | `Custom of 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t]
  val applyFirst : [`Derived | `Custom of 'a t -> _ t -> 'a t]
  val applySecond : [`Derived | `Custom of _ t -> 'b t -> 'b t]
  val select : [`Derived | `Custom of ('a,'b) EitherBase.t t -> f:('a -> 'b) t -> 'b t ]
  val join : [`Derived | `Custom of 'a t t -> 'a t]
end

module type Custom2 = sig 
  include Minimal2  
  val map : [`Derived | `Custom of ('a,'e) t -> f:('a -> 'b) -> ('b,'e) t]
  val replace : [`Derived | `Custom of ('a,'e) t -> const:'b -> ('b,'e) t]
  val apply : [`Derived | `Custom of ('a,'e) t -> f:('a -> 'b,'e) t -> ('b,'e) t ]
  val liftA2 : [`Derived | `Custom of ('a,'e) t -> ('b,'e) t -> f:('a -> 'b -> 'c) -> ('c,'e) t]  
  val applyFirst : [`Derived | `Custom of ('a,'e) t -> (_,'e) t -> ('a,'e) t]
  val applySecond : [`Derived | `Custom of (_,'e) t -> ('b,'e) t -> ('b,'e) t]  
  val select : [`Derived | `Custom of (('a,'b) EitherBase.t,'e) t -> f:('a -> 'b,'e) t -> ('b,'e) t]
  val join : [`Derived | `Custom of (('a,'e) t,'e) t -> ('a,'e) t]
end

module type Custom3 = sig 
  include Minimal3  
  val map : [`Derived | `Custom of ('a,'d,'e) t -> f:('a -> 'b) -> ('b,'d,'e) t]
  val replace : [`Derived | `Custom of ('a,'d,'e) t -> const:'b -> ('b,'d,'e) t]
  val apply : [`Derived | `Custom of ('a,'d,'e) t -> f:('a -> 'b,'d,'e) t -> ('b,'d,'e) t ]
  val liftA2 : [`Derived | `Custom of ('a,'d,'e) t -> ('b,'d,'e) t -> f:('a -> 'b -> 'c) -> ('c,'d,'e) t]
  val applyFirst : [`Derived | `Custom of ('a,'d,'e) t -> (_,'d,'e) t -> ('a,'d,'e) t]
  val applySecond : [`Derived | `Custom of (_,'d,'e) t -> ('b,'d,'e) t -> ('b,'d,'e) t]
  val select : [`Derived | `Custom of (('a,'b) EitherBase.t,'d,'e) t -> f:('a -> 'b,'d,'e) t -> ('b,'d,'e) t]
  val join : [`Derived | `Custom of (('a,'d,'e) t,'d,'e) t -> ('a,'d,'e) t]
end

(*** -- Infix functions -- ***)

module type Infix1 = sig
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

module type S1 = sig   
  include Selective.S1
  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  module MonadInfix : Infix1 with type 'a t := 'a t
  include Infix1 with type 'a t := 'a t    
  val join : 'a t t -> 'a t
  val forever : (unit -> 'a t ) -> 'b t
  val sequenceM : 'a t list -> 'a list t
  val mapM : 'a list -> f:('a -> 'b t) -> 'b list t
  val mapM_ : 'a list -> f:('a -> 'b t) -> unit t
end

module type S2 = sig 
  include Selective.S2
  val bind : ('a,'e) t -> f:('a -> ('b,'e) t) -> ('b,'e) t
  module MonadInfix : Infix2 with type ('a,'b) t := ('a,'b) t
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
  module MonadInfix : Infix3 with type ('a,'b,'c) t := ('a,'b,'c) t
  include Infix3 with type ('a,'b,'c) t := ('a,'b,'c) t    
  val join : (('a,'d,'e) t,'d,'e) t -> ('a,'d,'e) t
  val forever : (unit -> ('a,'d,'e) t ) -> ('b,'d,'e) t
  val sequenceM : ('a,'d,'e) t list -> ('a list,'d,'e) t
  val mapM : 'a list -> f:('a -> ('b,'d,'e) t) -> ('b list,'d,'e) t
  val mapM_ : 'a list -> f:('a -> ('b,'d,'e) t) -> (unit,'d,'e) t
end

(*** -- Module signature -- ***)

module type Monad = sig 

  (*** -- Minimal definitions --- ***)
  
  module type Minimal1 = Minimal1
  module type Minimal2 = Minimal2
  module type Minimal3 = Minimal3

  (*** -- Custom definitions --- ***)
  
  module type Custom1 = Custom1
  module type Custom2 = Custom2
  module type Custom3 = Custom3

  (*** -- Infix functions --- ***)
  
  module type Infix1 = Infix1
  module type Infix2 = Infix2 
  module type Infix3 = Infix3 

  (*** -- Complete definitions -- ***)
  
  module type S1 = S1
  module type S2 = S2
  module type S3 = S3

  (*** -- Functors  -- ***)
  
  module S1_to_S2 (X : S1) : S2 with type ('a,'e) t = 'a X.t 
  module S2_to_S1 (X : S2) : S1 with type 'a t = ('a,unit) X.t
  module S2_to_S3 (X : S2) : S3 with type ('a, 'b,_) t = ('a, 'b) X.t
  module S3_to_S2 (X : S3) : S2 with type ('a, 'b) t = ('a, 'b, unit) X.t

  module MakeCustom1(X:Custom1) : S1 with type 'a t := 'a X.t
  module MakeCustom2(X:Custom2) : S2 with type ('a,'b) t := ('a,'b) X.t
  module MakeCustom3(X:Custom3) : S3 with type ('a,'b,'c) t := ('a,'b,'c) X.t

  module Make1(X:Minimal1) : S1 with type 'a t := 'a X.t
  module Make2(X:Minimal2) : S2 with type ('a,'b) t := ('a,'b) X.t
  module Make3(X:Minimal3) : S3 with type ('a,'b,'c) t := ('a,'b,'c) X.t

end