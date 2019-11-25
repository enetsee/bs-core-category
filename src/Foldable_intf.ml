
(*** -- Minimal definitions --- ***)

module type Minimal = sig 
  include TyCon.S1
  val foldLeft : 'a t -> f:('b -> 'a -> 'b) -> init:'b -> 'b  
end

module type Minimal2 = sig 
  include TyCon.S2
  val foldLeft : ('a,'e) t -> f:('b -> 'a -> 'b) -> init:'b -> 'b  
end

module type Minimal3 = sig 
  include TyCon.S3
  val foldLeft : ('a,'d,'e) t -> f:('b -> 'a -> 'b) -> init:'b -> 'b  
end

(*** -- Minimal definitions using `foldMap` --- ***)
module type MinimalFoldMap = sig 
  include TyCon.S1
  val foldMap :
       (module Monoid.S0 with type t = 'b)    
    -> ?init:'b    
    -> 'a t
    -> f:('a -> 'b)
    -> 'b
end

module type MinimalFoldMap2 = sig 
  include TyCon.S2
  val foldMap :
       (module Monoid.S0 with type t = 'b)    
    -> ?init:'b    
    -> ('a,'e) t
    -> f:('a -> 'b)
    -> 'b
end

module type MinimalFoldMap3 = sig 
  include TyCon.S3
  val foldMap :
       (module Monoid.S0 with type t = 'b)
    -> ?init:'b    
    -> ('a,'d,'e) t
    -> f:('a -> 'b)
    -> 'b
end


(*** -- Complete signatures -- ***)

module type S = sig  
  include TyCon.S1
  val foldLeft : 'a t -> f:('b -> 'a -> 'b) -> init:'b -> 'b 
  val foldRight : 'a t -> f:('a -> 'b -> 'b) -> init:'b -> 'b 
  val foldMap :
       (module Monoid.S0 with type t = 'b)    
    -> ?init:'b   
    -> 'a t 
    -> f:('a -> 'b)
    -> 'b
  val exists : ?init:bool -> 'a t -> pred:('a -> bool) -> bool
  val forall : ?init:bool -> 'a t  -> pred:('a -> bool) -> bool
  val find :  'a t -> pred:('a -> bool) -> 'a option
end 

module type S2 = sig
  include TyCon.S2
  val foldLeft : ('a,'e) t -> f:('b -> 'a -> 'b) -> init:'b -> 'b 
  val foldRight : ('a,'e) t -> f:('a -> 'b -> 'b) -> init:'b -> 'b 
  val foldMap :
       (module Monoid.S0 with type t = 'b)    
    -> ?init:'b    
    -> ('a,'e) t
    -> f:('a -> 'b)
    -> 'b
  val exists : ?init:bool -> ('a,'e) t -> pred:('a -> bool) -> bool
  val forall : ?init:bool -> ('a,'e) t  -> pred:('a -> bool) -> bool
  val find :  ('a,'e) t -> pred:('a -> bool) -> 'a option
end 

module type S3 = sig
  include TyCon.S3
  val foldLeft : ('a,'d,'e) t -> f:('b -> 'a -> 'b) -> init:'b -> 'b 
  val foldRight : ('a,'d,'e) t -> f:('a -> 'b -> 'b) -> init:'b -> 'b 
  val foldMap :
       (module Monoid.S0 with type t = 'b)
    -> ?init:'b    
    -> ('a,'d,'e) t
    -> f:('a -> 'b)
    -> 'b
  val exists : ?init:bool -> ('a,'d,'e) t -> pred:('a -> bool) -> bool
  val forall : ?init:bool -> ('a,'d,'e) t  -> pred:('a -> bool) -> bool
  val find :  ('a,'d,'e) t -> pred:('a -> bool) -> 'a option
end 



module type Foldable = sig

  (*** -- Minimal definitions --- ***)
  module type Minimal = Minimal
  module type Minimal2 = Minimal2
  module type Minimal3 = Minimal3

  (*** -- Minimal definitions using `foldMap` --- ***)
  module type MinimalFoldMap = MinimalFoldMap
  module type MinimalFoldMap2 = MinimalFoldMap2
  module type MinimalFoldMap3 = MinimalFoldMap3

  (*** -- Complete definitions -- ***)
  module type S = S
  module type S2 = S2
  module type S3 = S3

  (*** -- Functors  -- ***)
  
  module S_to_S2 (X : S) : S2 with type ('a,_) t = 'a X.t 
  module S2_to_S (X : S2) : S with type 'a t = ('a,unit) X.t
  module S2_to_S3 (X : S2) : S3 with type ('a, 'b,_) t = ('a, 'b) X.t
  module S3_to_S2 (X : S3) : S2 with type ('a, 'b) t = ('a, 'b,unit) X.t

  module Make(X:Minimal) : S with type 'a t := 'a X.t
  module Make2(X:Minimal2) : S2 with type ('a,'b) t := ('a,'b) X.t
  module Make3(X:Minimal3) : S3 with type ('a,'b,'c) t := ('a,'b,'c) X.t

  (* module MakeFoldMap(X:MinimalFoldMap) : S with type 'a t := 'a X.t
  module MakeFoldMap2(X:MinimalFoldMap2) : S2 with type ('a,'b) t := ('a,'b) X.t
  module MakeFoldMap3(X:MinimalFoldMap3) : S3 with type ('a,'b,'c) t := ('a,'b,'c) X.t *)
end   
