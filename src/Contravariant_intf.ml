(*** -- Minimal definitions --- ***)

module type Minimal = sig 
  include TyCon.S1
  val contramap : 'b t -> f:('a -> 'b) -> 'a t
  (** Replace all locations in the output with the same value. *)
  val contramapConst : [ `Custom of 'b t -> 'b -> 'a t  | `Using_contramap ]
end

module type Minimal2 = sig 
  include TyCon.S2
  val contramap : ('b,'e) t -> f:('a -> 'b) -> ('a,'e) t
  val contramapConst : [ `Custom of ('b,'e) t -> 'b -> ('a,'e) t  | `Using_contramap ]
end

module type Minimal3 = sig 
  include TyCon.S3
  val contramap : ('b,'d,'e) t -> f:('a -> 'b) -> ('a,'d,'e) t
  val contramapConst : [ `Custom of ('b,'d,'e) t -> 'b -> ('a,'d,'e) t  | `Using_contramap ]
end

(*** -- Infix functions -- ***)

module type Infix = sig 
  include TyCon.S1
  val (>$<) : ('a -> 'b) -> 'b t -> 'a t 
  val (>$) : 'b -> 'b t -> 'a t
  val (>$$<) : 'b t -> ('a -> 'b)  -> 'a t
  val ($<) : 'b t -> 'b -> 'a t
end 

module type Infix2 = sig 
  include TyCon.S2
  val (>$<) : ('a -> 'b) -> ('b,'e) t -> ('a,'e) t 
  val (>$) : 'b -> ('b,'e) t -> ('a,'e) t
  val (>$$<) : ('b,'e) t  -> ('a -> 'b) -> ('a,'e) t
  val ($<) : ('b,'e) t -> 'b -> ('a,'e) t
end 

module type Infix3 = sig 
  include TyCon.S3
  val (>$<) : ('a -> 'b) -> ('b,'d,'e) t -> ('a,'d,'e) t 
  val (>$) : 'b -> ('b,'d,'e) t -> ('a,'d,'e) t
  val (>$$<) : ('b,'d,'e) t  -> ('a -> 'b) -> ('a,'d,'e) t
  val ($<) : ('b,'d,'e) t -> 'b -> ('a,'d,'e) t
end 


(*** -- Complete signatures -- ***)

module type S = sig  
  include TyCon.S1
  val contramap : 'b t -> f:('a -> 'b) -> 'a t  
  val contramapConst : 'b t -> 'b -> 'a t
  module Contravariant_infix : Infix with type 'a t := 'a t
  include Infix with type 'a t := 'a t
end 

module type S2 = sig
  include TyCon.S2
  val contramap : ('b,'e) t -> f:('a -> 'b) -> ('a,'e) t
  val contramapConst : ('b,'e) t -> 'b -> ('a,'e) t
  module Contravariant_infix : Infix2 with type ('a,'b) t := ('a,'b) t
  include Infix2 with type ('a,'b) t := ('a,'b) t
end 

module type S3 = sig
  include TyCon.S3
  val contramap : ('b,'d,'e) t -> f:('a -> 'b) -> ('a,'d,'e) t
  val contramapConst : ('b,'d,'e) t -> 'b -> ('a,'d,'e) t
  module Contravariant_infix : Infix3 with type ('a,'b,'c) t := ('a,'b,'c) t
  include Infix3 with type ('a,'b,'c) t := ('a,'b,'c) t
end 



module type Contravariant = sig

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
  
  module S_to_S2 (X : S) : S2 with type ('a,_) t = 'a X.t 
  module S2_to_S (X : S2) : S with type 'a t = ('a,unit) X.t
  module S2_to_S3 (X : S2) : S3 with type ('a, 'b, _) t = ('a, 'b) X.t
  module S3_to_S2 (X : S3) : S2 with type ('a, 'b) t = ('a, 'b,unit) X.t

  module Make(X:Minimal) : S with type 'a t := 'a X.t
  module Make2(X:Minimal2) : S2 with type ('a,'b) t := ('a,'b) X.t
  module Make3(X:Minimal3) : S3 with type ('a,'b,'c) t := ('a,'b,'c) X.t
end   
