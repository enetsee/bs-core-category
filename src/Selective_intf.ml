(*** -- Minimal definitions -- ***)  

module type Minimal1 = sig 
  include Applicative.Minimal1 
  val select : ('a,'b) EitherBase.t t -> f:('a -> 'b) t -> 'b t
end

module type Minimal2 = sig 
  include Applicative.Minimal2
  val select : (('a,'b) EitherBase.t,'e) t -> f:(('a -> 'b),'e) t -> ('b,'e) t
end

module type Minimal3 = sig 
  include Applicative.Minimal3
  val select : (('a,'b) EitherBase.t,'d,'e) t -> f:(('a -> 'b),'d,'e) t -> ('b,'d,'e) t
end

(*** -- Custom definitions -- ***)  

module type Custom1 = sig 
  include Applicative.Custom1 
  val select : ('a,'b) EitherBase.t t -> f:('a -> 'b) t -> 'b t
end

module type Custom2 = sig 
  include Applicative.Custom2
  val select : (('a,'b) EitherBase.t,'e) t -> f:(('a -> 'b),'e) t -> ('b,'e) t
end

module type Custom3 = sig 
  include Applicative.Custom3
  val select : (('a,'b) EitherBase.t,'d,'e) t -> f:(('a -> 'b),'d,'e) t -> ('b,'d,'e) t
end

(*** -- Infix functions -- ***)

module type Infix1 = sig
  include TyCon.S1
  val ( <*? ) : ('a,'b) EitherBase.t t -> ('a -> 'b) t -> 'b t
  val ( <||> ) : bool t -> bool t -> bool t 
  val (<&&>) : bool t -> bool t -> bool t 
end 

module type Infix2 = sig
  include TyCon.S2
  val ( <*? ) :  (('a,'b) EitherBase.t,'e) t -> (('a -> 'b),'e) t -> ('b,'e) t
  val ( <||> ) : (bool,'e) t  -> (bool,'e) t  -> (bool,'e) t 
  val (<&&>) : (bool,'e) t  -> (bool,'e) t  -> (bool,'e) t 
end 

module type Infix3 = sig
  include TyCon.S3
  val ( <*? ) :  (('a,'b) EitherBase.t,'d,'e) t -> (('a -> 'b),'d,'e) t -> ('b,'d,'e) t
  val ( <||> ) : (bool,'d,'e) t -> (bool,'d,'e) t -> (bool,'d,'e) t 
  val (<&&>) : (bool,'d,'e) t -> (bool,'d,'e) t -> (bool,'d,'e) t 
end 

(*** -- Complete definitions --- ***)

module type S1 = sig 
  include Applicative.S1
  val select : ('a,'b) EitherBase.t t -> f:('a -> 'b) t -> 'b t
  
  module SelectiveInfix : Infix1 with type 'a t := 'a t
  include Infix1 with type 'a t := 'a t 

  val orS : bool t -> bool t -> bool t 
  val andS : bool t -> bool t -> bool t 
  val whenS : bool t -> unit t -> unit t 
  val branch : ('a,'b) EitherBase.t t -> first:('a -> 'c) t -> second:('b -> 'c) t -> 'c t
  val ifS : bool t -> t:'a t -> f:'a t -> 'a t 
  val fromOptionS : 'a t -> 'a option t -> 'a t
  val anyS : 'a list -> pred:('a -> bool t) -> bool t 
  val allS : 'a list -> pred:('a -> bool t) -> bool t
  val whileS : bool t -> unit t
end


module type S2 = sig 
  include Applicative.S2
  val select : (('a,'b) EitherBase.t,'e) t -> f:(('a -> 'b),'e) t -> ('b,'e) t
  
  module SelectiveInfix : Infix2 with type ('a,'b) t := ('a,'b) t
  include Infix2 with type ('a,'b) t := ('a,'b) t

  val orS : (bool,'e) t -> (bool,'e) t ->(bool,'e) t 
  val andS : (bool,'e) t -> (bool,'e) t  ->(bool,'e) t 
  val whenS : (bool,'e) t -> (unit,'e) t -> (unit,'e) t 
  val branch : (('a,'b) EitherBase.t,'e) t -> first:('a -> 'c,'e) t -> second:('b -> 'c,'e) t -> ('c,'e) t
  val ifS : (bool,'e) t -> t:('a,'e) t -> f:('a,'e) t -> ('a,'e) t 
  val fromOptionS : ('a,'e) t -> ('a option,'e) t -> ('a,'e) t
  val anyS : 'a list -> pred:('a -> (bool,'e) t) -> (bool,'e) t 
  val allS : 'a list -> pred:('a -> (bool,'e) t) -> (bool,'e) t
  val whileS : (bool,'e) t -> (unit,'e) t
end

module type S3 = sig 
  include Applicative.S3
  val select : (('a,'b) EitherBase.t,'d,'e) t -> f:(('a -> 'b),'d,'e) t -> ('b,'d,'e) t

  module SelectiveInfix : Infix3 with type ('a,'b,'c) t := ('a,'b,'c) t
  include Infix3 with type ('a,'b,'c) t := ('a,'b,'c) t

  val orS :(bool,'d,'e) t -> (bool,'d,'e) t -> (bool,'d,'e) t 
  val andS : (bool,'d,'e) t -> (bool,'d,'e) t -> (bool,'d,'e) t 
  val whenS : (bool,'d,'e) t -> (unit,'d,'e) t -> (unit,'d,'e) t 
  val branch : (('a,'b) EitherBase.t,'d,'e)t -> first:('a -> 'c,'d,'e) t -> second:('b -> 'c,'d,'e) t -> ('c,'d,'e) t
  val ifS : (bool,'d,'e) t -> t:('a,'d,'e) t -> f:('a,'d,'e) t -> ('a,'d,'e) t 
  val fromOptionS : ('a,'d,'e) t -> ('a option,'d,'e) t -> ('a,'d,'e) t
  val anyS : 'a list -> pred:('a -> (bool,'d,'e) t) -> (bool,'d,'e) t 
  val allS : 'a list -> pred:('a -> (bool,'d,'e) t) -> (bool,'d,'e) t
  val whileS : (bool,'d,'e) t -> (unit,'d,'e) t
end

module type Selective = sig 

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

