(*** -- Minimal definitions -- ***)  

module type Minimal1 = sig 
  include Applicative.Minimal1 
  include Plus.Minimal1 with type 'a t := 'a t
end

module type Minimal2 = sig 
  include Applicative.Minimal2
  include Plus.Minimal2 with type ('a,'b) t := ('a,'b) t
end

module type Minimal3 = sig 
  include Applicative.Minimal3
  include Plus.Minimal3 with type ('a,'b,'c) t := ('a,'b,'c) t
end

(*** -- Custom definitions -- ***)  

module type Custom1 = sig    
  include Plus.Minimal1 
  include Applicative.Custom1 with type 'a t := 'a t
end

module type Custom2 = sig   
  include Plus.Minimal2
  include Applicative.Custom2 with type ('a,'b) t := ('a,'b) t
end

module type Custom3 = sig   
  include Plus.Minimal3
  include Applicative.Custom3 with type ('a,'b,'c) t := ('a,'b,'c) t
end

(*** -- Infix functions -- ***)

module type Infix1 = sig
  include TyCon.S1
  val ( <|> ) : 'a t -> 'a t -> 'a t
  val ( </> ) : 'a t -> 'a -> 'a t
end 

module type Infix2 = sig
  include TyCon.S2
  val ( <|> ) :  ('a,'e) t -> ('a,'e) t -> ('a,'e) t
  val ( </> ) : ('a,'e) t -> 'a -> ('a,'e) t
end 

module type Infix3 = sig
  include TyCon.S3
  val ( <|> ) :  ('a,'d,'e) t -> ('a,'d,'e) t -> ('a,'d,'e) t
  val ( </> ) : ('a,'d,'e) t -> 'a -> ('a,'d,'e) t
end 

(*** -- Complete definitions --- ***)

module type S1 = sig 
  include Plus.S1
  include Applicative.S1  with type 'a t := 'a t and module FunctorInfix := FunctorInfix
  module AlternativeInfix : Infix1 with type 'a t := 'a t
  include Infix1 with type 'a t := 'a t  
  val some : 'a t -> 'a list t 
  val many : 'a t -> 'a list t   
  val optional : 'a t -> 'a option t
end


module type S2 = sig 
  include Plus.S2
  include Applicative.S2 with type ('a,'b) t := ('a,'b) t and module FunctorInfix := FunctorInfix
  module AlternativeInfix : Infix2 with type ('a,'b) t := ('a,'b) t
  include Infix2 with type ('a,'b) t := ('a,'b) t  
  val some : ('a,'e) t -> ('a list, 'e) t
  val many : ('a,'e) t -> ('a list, 'e) t
  val optional : ('a,'e) t -> ('a option,'e) t

end

module type S3 = sig 
  include Plus.S3
  include Applicative.S3 with type ('a,'b,'c) t := ('a,'b,'c) t and module FunctorInfix := FunctorInfix
  module AlternativeInfix : Infix3 with type ('a,'b,'c) t := ('a,'b,'c) t
  include Infix3 with type ('a,'b,'c) t := ('a,'b,'c) t
  val some : ('a,'d,'e) t -> ('a list,'d,'e) t
  val many : ('a,'d,'e) t -> ('a list,'d,'e) t  
  val optional : ('a,'d,'e) t -> ('a option,'d,'e) t
end



module type Alternative = sig 

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