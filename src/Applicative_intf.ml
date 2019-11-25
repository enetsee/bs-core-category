(*** -- Minimal definitions -- ***)

module type Minimal = sig 
  include Functor.Minimal
  val return : 'a -> 'a t 
  val apply : 'a t -> f:('a -> 'b) t -> 'b t
end

module type Minimal2 = sig 
  include Functor.Minimal2
  val return : 'a -> ('a,_) t 
  val apply : ('a,'e) t -> f:('a -> 'b ,'e) t -> ('b,'e) t
end

module type Minimal3 = sig 
  include Functor.Minimal3
  val return : 'a -> ('a,_,_) t 
  val apply : ('a,'d,'e) t -> f:('a -> 'b,'d,'e) t -> ('b,'d,'e) t
end

(*** -- Alternate `Monoidal` presentation -- ***)

module type MinimalMonoidal = sig 
  include Functor.Minimal
  val unit : unit -> unit t 
  val merge : 'a t -> 'b t -> ('a * 'b) t
end

module type MinimalMonoidal2 = sig 
  include Functor.Minimal2
  val unit : unit -> (unit,'e) t 
  val merge : ('a,'e) t -> ('b,'e) t -> ('a * 'b,'e) t
end

module type MinimalMonoidal3 = sig 
  include Functor.Minimal3
  val unit : unit -> (unit,'d,'e) t 
  val merge : ('a,'d,'e) t -> ('b,'d,'e) t -> ('a*'b,'d,'e) t
end

(*** -- Infix functions -- ***)

module type Infix = sig
  type 'a t 
  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
  val ( *> ) : _ t -> 'a t -> 'a t
  val ( <* ) : 'a t -> _ t -> 'a t
  val ( ** ) : 'a t -> 'b t -> ('a * 'b) t
end 

module type Infix2 = sig
  type ('a,'e) t 
  val ( <*> ) : ('a ->'b,'e) t -> ('a,'e) t -> ('b,'e) t
  val ( *> ) : (_,'e) t -> ('b,'e) t -> ('b,'e) t
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

module type S = sig 
  include Minimal 
  include MinimalMonoidal with type 'a t := 'a t
  include Functor.S with type 'a t := 'a t
  val liftA : 'a t -> f:('a -> 'b) -> 'b t
  val liftA2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  val liftA3 : 'a t -> 'b t -> 'c t -> f:('a -> 'b -> 'c -> 'd) -> 'd t
  module Applicative_infix : Infix with type 'a t := 'a t
  include Infix with type 'a t := 'a t
end

module type S2 = sig 
  include Minimal2 
  include MinimalMonoidal2 with type ('a,'b) t := ('a,'b) t
  include Functor.S2 with type ('a,'b) t := ('a,'b) t
  val liftA : ('a,'e) t -> f:('a -> 'b) -> ('b,'e) t
  val liftA2 : ('a,'e) t -> ('b,'e) t -> f:('a -> 'b -> 'c) -> ('c,'e) t
  val liftA3 : ('a,'e) t -> ('b,'e) t -> ('c,'e) t -> f:('a -> 'b -> 'c -> 'd) -> ('d,'e) t
  module Applicative_infix : Infix2 with type ('a,'b) t := ('a,'b) t
  include Infix2 with type ('a,'b) t := ('a,'b) t
end

module type S3 = sig 
  include Minimal3
  include MinimalMonoidal3 with type ('a,'b,'c) t := ('a,'b,'c) t
  include Functor.S3 with type ('a,'b,'c) t := ('a,'b,'c) t
  val liftA : ('a,'d,'e) t -> f:('a -> 'b) -> ('b,'d,'e) t
  val liftA2 : ('a,'d,'e) t -> ('b,'d,'e) t -> f:('a -> 'b -> 'c) -> ('c,'d,'e) t
  val liftA3 : ('a,'e,'f) t -> ('b,'e,'f) t -> ('c,'e,'f) t -> f:('a -> 'b -> 'c -> 'd) -> ('d,'e,'f) t
  module Applicative_infix : Infix3 with type ('a,'b,'c) t := ('a,'b,'c) t
  include Infix3 with type ('a,'b,'c) t := ('a,'b,'c) t
end


module type Applicative = sig 
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
  module S2_to_S3 (X : S2) : S3 with type ('a, 'b,_) t = ('a, 'b) X.t
  module S3_to_S2 (X : S3) : S2 with type ('a, 'b) t = ('a, 'b, unit) X.t
  
  module Make(X:Minimal) : S with type 'a t := 'a X.t
  module Make2(X:Minimal2) : S2 with type ('a,'b) t := ('a,'b) X.t
  module Make3(X:Minimal3) : S3 with type ('a,'b,'c) t := ('a,'b,'c) X.t

  module Make_backwards(X:Minimal) : S with type 'a t := 'a X.t
  module Make_backwards2(X:Minimal2) : S2 with type ('a,'b) t := ('a,'b) X.t
  module Make_backwards3(X:Minimal3) : S3 with type ('a,'b,'c) t := ('a,'b,'c) X.t

end