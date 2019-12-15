(*** -- Minimal definitions -- ***)

module type Minimal1 = sig   
  include TyCon.S1
  val pure : 'a -> 'a t 
  val apply : 'a t -> f:('a -> 'b) t -> 'b t
end

module type Minimal2 = sig 
  include TyCon.S2
  val pure : 'a -> ('a,_) t
  val apply : ('a,'e) t -> f:('a -> 'b,'e) t -> ('b,'e) t
end

module type Minimal3 = sig 
  include TyCon.S3
  val pure : 'a -> ('a,_,_) t 
  val apply : ('a,'d,'e) t -> f:('a -> 'b,'d,'e) t -> ('b,'d,'e) t
end

(*** -- Custom definitions -- ***)

module type Custom1 = sig    
  include Minimal1
  val map : [`Derived | `Custom of 'a t -> f:('a -> 'b) -> 'b t]
  val replace :  [`Derived | `Custom of 'a t -> const:'b -> 'b t]
  val liftA2 : [`Derived | `Custom of 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t]  
  val applyFirst : [`Derived | `Custom of 'a t -> _ t -> 'a t]
  val applySecond : [`Derived | `Custom of _ t -> 'b t -> 'b t]  
end

module type Custom2 = sig 
  include Minimal2
  val map : [`Derived | `Custom of ('a,'e) t -> f:('a -> 'b) -> ('b,'e) t]
  val replace :  [`Derived | `Custom of ('a,'e) t -> const:'b -> ('b,'e) t]
  val liftA2 : [`Derived | `Custom of ('a,'e) t -> ('b,'e) t -> f:('a -> 'b -> 'c) -> ('c,'e) t]  
  val applyFirst : [`Derived | `Custom of ('a,'e) t -> (_,'e) t -> ('a,'e) t]
  val applySecond : [`Derived | `Custom of (_,'e) t -> ('b,'e) t -> ('b,'e) t]  
end

module type Custom3 = sig 
  include Minimal3
  val map : [`Derived | `Custom of ('a,'d,'e) t -> f:('a -> 'b) -> ('b,'d,'e) t]
  val replace :  [`Derived | `Custom of ('a,'d,'e) t -> const:'b -> ('b,'d,'e) t]  
  val liftA2 : [`Derived | `Custom of ('a,'d,'e) t -> ('b,'d,'e) t -> f:('a -> 'b -> 'c) -> ('c,'d,'e) t]
  val applyFirst : [`Derived | `Custom of ('a,'d,'e) t -> (_,'d,'e) t -> ('a,'d,'e) t]
  val applySecond : [`Derived | `Custom of (_,'d,'e) t -> ('b,'d,'e) t -> ('b,'d,'e) t]  
end

(*** -- Full signatures -- ***)

module type S1 = sig 
  include Apply.S1
  val pure : 'a -> 'a t 
  val when_ : unit t -> cond:bool -> unit t
  val unless : unit t -> cond:bool -> unit t
end

module type S2 = sig 
  include Apply.S2
  val pure : 'a -> ('a,_) t
  val when_ : (unit,'e) t -> cond:bool -> (unit,'e) t
  val unless : (unit,'e) t -> cond:bool -> (unit,'e) t
end

module type S3 = sig 
  include Apply.S3
  val pure : 'a -> ('a,_,_) t
  val when_ : (unit,'d,'e) t -> cond:bool -> (unit,'d,'e) t
  val unless : (unit,'d,'e) t -> cond:bool -> (unit,'d,'e) t
end

(*** -- Module signatures -- ***)

module type Applicative = sig 
  
  (*** -- Minimal definitions --- ***)
  module type Minimal1 = Minimal1
  module type Minimal2 = Minimal2
  module type Minimal3 = Minimal3

  (*** -- Custom definitions --- ***)
  module type Custom1 = Custom1
  module type Custom2 = Custom2
  module type Custom3 = Custom3

  (*** -- Complete definitions -- ***)
  module type S1 = S1
  module type S2 = S2
  module type S3 = S3

  (*** -- Functors  -- ***)  
  module S1_to_S2 (X : S1) : S2 with type ('a,_) t = 'a X.t 
  module S2_to_S1 (X : S2) : S1 with type 'a t = ('a,unit) X.t
  module S2_to_S3 (X : S2) : S3 with type ('a, 'b, _) t = ('a, 'b) X.t
  module S3_to_S2 (X : S3) : S2 with type ('a, 'b) t = ('a, 'b,unit) X.t

  module MakeCustom1(X:Custom1) : S1 with type 'a t := 'a X.t
  module MakeCustom2(X:Custom2) : S2 with type ('a,'b) t := ('a,'b) X.t
  module MakeCustom3(X:Custom3) : S3 with type ('a,'b,'c) t := ('a,'b,'c) X.t

  module Make1(X:Minimal1) : S1 with type 'a t := 'a X.t
  module Make2(X:Minimal2) : S2 with type ('a,'b) t := ('a,'b) X.t
  module Make3(X:Minimal3) : S3 with type ('a,'b,'c) t := ('a,'b,'c) X.t

  module Backwards1(X:Minimal1) : S1 with type 'a t := 'a X.t
  module Backwards2(X:Minimal2) : S2 with type ('a,'b) t := ('a,'b) X.t
  module Backwards3(X:Minimal3) : S3 with type ('a,'b,'c) t := ('a,'b,'c) X.t

end