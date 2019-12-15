(*** -- Minimal definitions -- ***)  

module type Minimal2 = sig
  include TyCon.S2 
  val bimap : ('a,'b) t -> first:('a -> 'c) -> second:('b -> 'd) -> ('c,'d) t
end

module type Minimal3 = sig
  include TyCon.S3
  val bimap : ('a,'b,'e) t -> first:('a -> 'c) -> second:('b -> 'd) -> ('c,'d,'e) t
end

(*** -- Custom definitions -- ***)  

module type Custom2 = sig 
  include Minimal2 
  val mapFirst : [ `Derived | `Custom of ('a,'b) t -> f:('a -> 'c) -> ('c,'b) t]
  val mapSecond : [ `Derived | `Custom of ('a,'b) t -> f:('b -> 'c) -> ('a,'c) t]
end

module type Custom3 = sig 
  include Minimal3
  val mapFirst : [ `Derived | `Custom of ('a,'b,'e) t -> f:('a -> 'c) -> ('c,'b,'e) t]
  val mapSecond : [ `Derived | `Custom of ('a,'b,'e) t -> f:('b -> 'c) -> ('a,'c,'e) t]
end

(*** -- Complete definitions -- ***)  

module type S2 = sig 
  include Minimal2 
  val mapFirst : ('a,'b) t -> f:('a -> 'c) -> ('c,'b) t
  val mapSecond : ('a,'b) t -> f:('b -> 'c) -> ('a,'c) t
end

module type S3 = sig 
  include Minimal3
  val mapFirst : ('a,'b,'e) t -> f:('a -> 'c) -> ('c,'b,'e) t
  val mapSecond : ('a,'b,'e) t -> f:('b -> 'c) -> ('a,'c,'e) t
end

(*** -- Module signature -- ***)

module type Bifunctor = sig 

  (*** -- Minimal definitions --- ***)
  module type Minimal2 = Minimal2
  module type Minimal3 = Minimal3

  (*** -- Custom definitions --- ***)
  module type Custom2 = Custom2
  module type Custom3 = Custom3

  (*** -- Complete definitions -- ***)
  module type S2 = S2
  module type S3 = S3

  (*** -- Functors  -- ***)  
  module S2_to_S3 (X : S2) : S3 with type ('a, 'b, _) t = ('a, 'b) X.t
  module S3_to_S2 (X : S3) : S2 with type ('a, 'b) t = ('a, 'b,unit) X.t

  module MakeCustom2(X:Custom2) : S2 with type ('a,'b) t := ('a,'b) X.t
  module MakeCustom3(X:Custom3) : S3 with type ('a,'b,'c) t := ('a,'b,'c) X.t

  module Make2(X:Minimal2) : S2 with type ('a,'b) t := ('a,'b) X.t
  module Make3(X:Minimal3) : S3 with type ('a,'b,'c) t := ('a,'b,'c) X.t

end