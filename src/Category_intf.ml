(*** -- Minimal definitions  -- ***)

module type Minimal2 = sig
  type ('a,'b) t
  val id : ('a,'a) t
  val compose : ('b,'c) t -> ('a,'b) t ->  ('a,'c) t
end

(*** -- Infix functions -- ***)

module type Infix2 = sig 
  type ('a,'b) t
  val (<<) : ('b,'c) t -> ('a,'b) t ->  ('a,'c) t
  val (>>) : ('a,'b) t -> ('b,'c) t ->  ('a,'c) t
end
  
(*** -- Complete definitions -- ***)

module type S2 = sig
  include Minimal2
  module CategoryInfix : Infix2 with type ('a,'b) t := ('a,'b) t
  include Infix2 with type ('a,'b) t := ('a,'b) t
end
  
(*** -- Module signature -- ***)

module type Category = sig

  (*** -- Minimal definitions  -- ***)
  module type Minimal2 = Minimal2

  (*** -- Infix functions -- ***) 
  module type Infix2 = Infix2

  (*** -- Complete definitions -- ***)
  module type S2 = S2 

  (*** -- Functors -- ***)
  module Make2(X: Minimal2) : S2 with type ('a,'b) t := ('a,'b) X.t

end
