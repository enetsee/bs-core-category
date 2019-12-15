(*** -- Minimal definitions -- ***)  

module type Minimal2 = sig
  type ('a,'b) t
  val dimap : ('b,'c) t -> first:('a -> 'b) -> second:('c -> 'd) -> ('a,'d) t
end

module type Minimal3 = sig
  type ('a,'b,'c) t
  val dimap : ('b,'c,'e) t -> first:('a -> 'b) -> second:('c -> 'd) -> ('a,'d,'e) t
end

(*** -- Custom definitions -- ***)  

module type Custom2 = sig
  include Minimal2
  val cmapFirst : [ `Custom of ('b,'c) t -> f:('a -> 'b) -> ('a,'c) t | `Derived ]
  val mapSecond : [ `Custom of ('a,'b) t -> f:('b -> 'c) -> ('a,'c) t | `Derived ]
end

module type Custom3 = sig
  include Minimal3 
  val cmapFirst : [ `Custom of ('b,'c,'e) t -> f:('a -> 'b) -> ('a,'c,'e) t | `Derived ]
  val mapSecond : [ `Custom of ('a,'b,'e) t -> f:('b -> 'c) -> ('a,'c,'e) t | `Derived ]
end

(*** -- Complete definitions -- ***)  

module type S2 = sig
  include Minimal2
  val cmapFirst : ('b,'c) t -> f:('a -> 'b) -> ('a,'c) t
  val mapSecond : ('a,'b) t -> f:('b -> 'c) -> ('a,'c) t
end

module type S3 = sig
  include Minimal3 
  val cmapFirst : ('b,'c,'e) t -> f:('a -> 'b) -> ('a,'c,'e) t
  val mapSecond : ('a,'b,'e) t -> f:('b -> 'c) -> ('a,'c,'e) t
end

(*** -- Module signature -- ***)  

module type Profunctor = sig
  module type Minimal2 = Minimal2
  module type Minimal3 = Minimal3
  module type Custom2 = Custom2 
  module type Custom3 = Custom3 
  module MakeCustom2(X:Custom2) : S2 with type ('a,'b) t := ('a,'b) X.t 
  module MakeCustom3(X:Custom3) : S3 with type ('a,'b,'c) t := ('a,'b,'c) X.t
  module Make2(X:Minimal2) : S2 with type ('a,'b) t := ('a,'b) X.t 
  module Make3(X:Minimal3) : S3 with type ('a,'b,'c) t := ('a,'b,'c) X.t 
end