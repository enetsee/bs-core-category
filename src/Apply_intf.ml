(*** -- Minimal definitions -- ***)

module type Minimal1 = sig   
  include Functor.Minimal1
  val apply : 'a t -> f:('a -> 'b) t -> 'b t
end

module type Custom1 = sig 
  include Functor.Custom1
  val apply : 'a t -> f:('a -> 'b) t -> 'b t
  val liftA2 : [`Derived | `Custom of 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t]  
  val applySecond : [`Derived | `Custom of _ t -> 'b t -> 'b t]
  val applyFirst : [`Derived | `Custom of 'a t -> _ t -> 'a t]
end

module type Minimal2 = sig 
  include Functor.Minimal2
  val apply : ('a,'e) t -> f:('a -> 'b,'e) t -> ('b,'e) t  
end

module type Custom2 = sig 
  include Functor.Custom2
  val apply : ('a,'e) t -> f:('a -> 'b,'e) t -> ('b,'e) t  
  val liftA2 : [`Derived | `Custom of ('a,'e) t -> ('b,'e) t -> f:('a -> 'b -> 'c) -> ('c,'e) t]  
  val applySecond : [`Derived | `Custom of (_,'e) t -> ('b,'e) t -> ('b,'e) t]
  val applyFirst : [`Derived | `Custom of ('a,'e) t -> (_,'e) t -> ('a,'e) t]
end

module type Minimal3 = sig 
  include Functor.Minimal3
  val apply : ('a,'d,'e) t -> f:('a -> 'b,'d,'e) t -> ('b,'d,'e) t  
end

module type Custom3 = sig 
  include Functor.Custom3
  val apply : ('a,'d,'e) t -> f:('a -> 'b,'d,'e) t -> ('b,'d,'e) t 
  val liftA2 : [`Derived | `Custom of ('a,'d,'e) t -> ('b,'d,'e) t -> f:('a -> 'b -> 'c) -> ('c,'d,'e) t]  
  val applySecond : [`Derived | `Custom of (_,'d,'e) t -> ('b,'d,'e) t -> ('b,'d,'e) t]
  val applyFirst : [`Derived | `Custom of ('a,'d,'e) t -> (_,'d,'e) t -> ('a,'d,'e) t]
end

(*** -- Infix functions -- ***)

module type Infix1 = sig
  type 'a t 
  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
  val ( *> ) : _ t -> 'a t -> 'a t
  val ( <* ) : 'a t -> _ t -> 'a t
  val ( ** ) : 'a t -> 'b t -> ('a * 'b) t
end 

module type Infix2 = sig
  type ('a,'b) t 
  val ( <*> ) : ('a -> 'b,'e) t -> ('a,'e) t -> ('b,'e) t
  val ( *> ) : (_,'e) t -> ('a,'e) t -> ('a,'e) t
  val ( <* ) : ('a,'e) t -> (_,'e) t -> ('a,'e) t
  val ( ** ) : ('a,'e) t -> ('b,'e) t -> ('a*'b,'e) t
end 

module type Infix3 = sig
  type ('a,'b,'c) t 
  val ( <*> ) : ('a->'b,'d,'e) t -> ('a,'d,'e) t -> ('b,'d,'e) t
  val ( *> ) : (_,'d,'e) t -> ('a,'d,'e) t -> ('a,'d,'e) t
  val ( <* ) : ('a,'d,'e) t -> (_,'d,'e) t -> ('a,'d,'e) t
  val ( ** ) : ('a,'d,'e) t -> ('b,'d,'e) t -> ('a*'b,'d,'e) t
end 


(*** -- Full signatures -- ***)

module type S1 = sig 
  include Functor.S1
  val apply : 'a t -> f:('a -> 'b) t -> 'b t   
  val liftA2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t  
  val applyFirst :  'a t -> _ t -> 'a t
  val applySecond : _ t -> 'b t -> 'b t
  module ApplyInfix : Infix1 with type 'a t := 'a t
  include Infix1 with type 'a t := 'a t
  val liftA3 : 'a t -> 'b t -> 'c t -> f:('a -> 'b -> 'c -> 'd) -> 'd t  
  val liftA4 : 'a t -> 'b t -> 'c t -> 'd t -> f:('a -> 'b -> 'c -> 'd -> 'e) -> 'e t  
  val liftA5 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> f:('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'f t  
  val merge : 'a t -> 'b t -> ('a * 'b) t
end

module type S2 = sig 
  include Functor.S2
  val apply : ('a,'e) t -> f:('a -> 'b,'e) t -> ('b,'e) t
  val applyFirst : ('a,'e) t -> (_,'e) t -> ('a,'e) t
  val applySecond : (_,'e) t -> ('b,'e) t -> ('b,'e) t 
  val liftA2 : ('a,'e) t -> ('b,'e) t -> f:('a -> 'b -> 'c) -> ('c,'e) t  
  module ApplyInfix : Infix2 with type ('a,'b) t := ('a,'b) t
  include Infix2 with type ('a,'b) t := ('a,'b) t
  val liftA3 : ('a,'e) t -> ('b,'e) t -> ('c,'e) t -> f:('a -> 'b -> 'c -> 'd) -> ('d,'e) t
  val liftA4 : ('a,'g) t -> ('b,'g) t -> ('c,'g) t -> ('d,'g) t -> f:('a -> 'b -> 'c -> 'd -> 'e) -> ('e,'g) t
  val liftA5 : ('a,'g) t -> ('b,'g) t -> ('c,'g) t -> ('d,'g) t -> ('e,'g) t -> f:('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> ('f,'g) t
  val merge : ('a,'e) t -> ('b,'e) t -> ('a * 'b, 'e) t
end

module type S3 = sig 
  include Functor.S3
  val apply : ('a,'d,'e) t -> f:('a -> 'b,'d,'e) t -> ('b,'d,'e) t
  val applyFirst : ('a,'d,'e) t -> (_,'d,'e) t -> ('a,'d,'e) t
  val applySecond : (_,'d,'e) t -> ('b,'d,'e) t -> ('b,'d,'e) t  
  val liftA2 : ('a,'d,'e) t -> ('b,'d,'e) t -> f:('a -> 'b -> 'c) -> ('c,'d,'e) t  
  module ApplyInfix : Infix3 with type ('a,'b,'c) t := ('a,'b,'c) t
  include Infix3 with type ('a,'b,'c) t := ('a,'b,'c) t
  val liftA3 : ('a,'e,'f) t -> ('b,'e,'f) t -> ('c,'e,'f) t -> f:('a -> 'b -> 'c -> 'd) -> ('d,'e,'f) t
  val liftA4 : ('a,'f,'g) t -> ('b,'f,'g) t -> ('c,'f,'g) t -> ('d,'f,'g) t -> f:('a -> 'b -> 'c -> 'd -> 'e) -> ('e,'f,'g) t
  val liftA5 : ('a,'g,'h) t -> ('b,'g,'h) t -> ('c,'g,'h) t -> ('d,'g,'h) t -> ('e,'g,'h) t -> f:('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> ('f,'g,'h) t
  val merge : ('a,'d,'e) t -> ('b,'d,'e) t -> ('a * 'b,'d,'e) t
end


module type Apply = sig 

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