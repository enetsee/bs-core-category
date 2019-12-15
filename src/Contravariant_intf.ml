(*** -- Minimal definitions --- ***)

module type Minimal1 = sig 
  include TyCon.S1
  val cmap : 'b t -> f:('a -> 'b) -> 'a t
end  

module type Minimal2 = sig 
  include TyCon.S2
  val cmap : ('b,'e) t -> f:('a -> 'b) -> ('a,'e) t
end

module type Minimal3 = sig 
  include TyCon.S3
  val cmap : ('b,'d,'e) t -> f:('a -> 'b) -> ('a,'d,'e) t
end

(*** -- Custom definitions -- ***)

module type Custom1 = sig 
  include Minimal1
  val creplace : [ `Custom of 'b t -> const:'b -> 'a t  | `Derived ]
end

module type Custom2 = sig 
  include Minimal2
  val creplace : [ `Custom of ('b,'e) t -> const:'b -> ('a,'e) t  | `Derived ]
end

module type Custom3 = sig
  include Minimal3
  val creplace : [ `Custom of ('b,'d,'e) t -> const:'b -> ('a,'d,'e) t  | `Derived ]
end

(*** -- Infix functions -- ***)

module type Infix1 = sig 
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

module type S1 = sig  
  include TyCon.S1
  val cmap : 'b t -> f:('a -> 'b) -> 'a t  
  val creplace : 'b t -> const:'b -> 'a t
  module ContravariantInfix : Infix1 with type 'a t := 'a t
  include Infix1 with type 'a t := 'a t
end 

module type S2 = sig
  include TyCon.S2
  val cmap : ('b,'e) t -> f:('a -> 'b) -> ('a,'e) t
  val creplace : ('b,'e) t -> const:'b -> ('a,'e) t
  module ContravariantInfix : Infix2 with type ('a,'b) t := ('a,'b) t
  include Infix2 with type ('a,'b) t := ('a,'b) t
end 

module type S3 = sig
  include TyCon.S3
  val cmap : ('b,'d,'e) t -> f:('a -> 'b) -> ('a,'d,'e) t
  val creplace : ('b,'d,'e) t -> const:'b -> ('a,'d,'e) t
  module ContravariantInfix : Infix3 with type ('a,'b,'c) t := ('a,'b,'c) t
  include Infix3 with type ('a,'b,'c) t := ('a,'b,'c) t
end 

(*** -- Module signature -- ***)

module type Contravariant = sig

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

end   
