(*** -- Minimal definitions -- ***)  
(*** -- Minimal definitions -- ***)  

module type Minimal = sig 
  include Applicative.Minimal 
  val empty : 'a t
  val alt : 'a t -> 'a t -> 'a t
end

module type Minimal2 = sig 
  include Applicative.Minimal2
  val empty: ('a,'e) t
  val alt: ('a,'e) t -> ('a,'e) t -> ('a,'e) t
end

module type Minimal3 = sig 
  include Applicative.Minimal3
  val empty : ('a,'d,'e) t
  val alt : ('a,'d,'e) t -> ('a,'d,'e) t -> ('a,'d,'e) t
end

(*** -- Infix functions -- ***)
module type Infix = sig
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

module type S = sig 
  include Minimal 
  include Applicative.S with type 'a t := 'a t
  module Alternative_infix : Infix with type 'a t := 'a t
  include Infix with type 'a t := 'a t     
  val optional : 'a t -> 'a option t
end


module type S2 = sig 
  include Minimal2
  include Applicative.S2 with type ('a,'b) t := ('a,'b) t
  module Alternative_infix : Infix2 with type ('a,'b) t := ('a,'b) t
  include Infix2 with type ('a,'b) t := ('a,'b) t  
  val optional : ('a,'e) t -> ('a option,'e) t

end

module type S3 = sig 
  include Minimal3
  include Applicative.S3 with type ('a,'b,'c) t := ('a,'b,'c) t
  module Alternative_infix : Infix3 with type ('a,'b,'c) t := ('a,'b,'c) t
  include Infix3 with type ('a,'b,'c) t := ('a,'b,'c) t    
  val optional : ('a,'d,'e) t -> ('a option,'d,'e) t
end



module type Alternative = sig 
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

  module Make_backwards(X:Minimal) : S with type 'a t := 'a X.t
  module Make_backwards2(X:Minimal2) : S2 with type ('a,'b) t := ('a,'b) X.t
  module Make_backwards3(X:Minimal3) : S3 with type ('a,'b,'c) t := ('a,'b,'c) X.t

end