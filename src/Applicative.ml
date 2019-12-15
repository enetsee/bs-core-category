include Applicative_intf

(*** -- Conversion functors -- ***)

module S1_to_S2 (X : S1) : S2 with type ('a,_) t = 'a X.t = struct
  type ('a,_) t = 'a X.t
  include (X : S1 with type 'a t := 'a X.t)
end 

module S2_to_S1 (X : S2) : S1 with type 'a t = ('a,unit) X.t = struct
  type 'a t = ('a,unit) X.t
  include (X : S2 with type ('a, 'b) t := ('a, 'b) X.t)
end

module S2_to_S3 (X : S2) : S3 with type ('a, 'b,_) t = ('a, 'b) X.t = struct
  type ('a, 'b, _) t = ('a, 'b) X.t
  include (X : S2 with type ('a,'b) t := ('a,'b) X.t)
end

module S3_to_S2 (X : S3) : S2 with type ('a, 'b) t = ('a, 'b, unit) X.t = struct
  type ('a, 'b) t = ('a, 'b, unit) X.t
  include (X : S3 with type ('a,'b,'c) t := ('a,'b,'c) X.t)
end

module MakeCustom3(X:Custom3) : S3 with type ('a,'b,'c) t := ('a,'b,'c) X.t = struct 
  
  let pure = X.pure 
  
  module T = struct
    include X 
    let map = 
      match X.map with 
      | `Custom f -> f 
      | _ -> fun x ~f -> apply x ~f:(pure f)
  end
  
  include Functor.MakeCustom3(T)
  include (Apply.MakeCustom3(T) : module type of Apply.MakeCustom3(T) with module FunctorInfix := FunctorInfix)
      
  let when_  t ~cond = if cond then t else pure ()
  let unless t ~cond = if cond then pure () else t
end 

module MakeCustom2(X:Custom2) : S2 with type ('a,'b) t := ('a,'b) X.t = MakeCustom3(struct
  type ('a,'b,_) t = ('a,'b) X.t
  include (X: Custom2 with type ('a,'b) t := ('a,'b) X.t)
end)

module MakeCustom1(X:Custom1) : S1 with type 'a t := 'a X.t = MakeCustom2(struct
  type ('a,_) t = 'a X.t
  include (X: Custom1 with type 'a t := 'a X.t)
end)

module Make3(X:Minimal3): S3 with type ('a,'b,'c) t := ('a,'b,'c) X.t = MakeCustom3(struct 
  include X 
  let map = `Derived 
  let replace = `Derived 
  let liftA2 = `Derived
  let applyFirst = `Derived 
  let applySecond = `Derived
end)

module Make2(X:Minimal2) : S2 with type ('a,'b) t := ('a,'b) X.t = Make3(struct
  type ('a,'b,_) t = ('a,'b) X.t
  include (X: Minimal2 with type ('a,'b) t := ('a,'b) X.t)
end)
  
module Make1(X:Minimal1) : S1 with type 'a t := 'a X.t = Make2(struct
  type ('a,_) t = 'a X.t
  include (X: Minimal1 with type 'a t := 'a X.t)
end)

module Backwards3(X:Minimal3) : S3 with type ('a,'b,'c) t := ('a,'b,'c) X.t = Make3(struct
  include X
  let apply x ~f = X.(apply ~f:(apply ~f:(pure (fun x f -> f x)) x) f)
end)

module Backwards2(X:Minimal2) : S2 with type ('a,'b) t := ('a,'b) X.t = Backwards3(struct
  type ('a,'b,_) t = ('a,'b) X.t
  include (X: Minimal2 with type ('a,'b) t := ('a,'b) X.t)
end)
  
module Backwards1(X:Minimal1) : S1 with type 'a t := 'a X.t = Backwards2(struct
  type ('a,_) t = 'a X.t
  include (X: Minimal1 with type 'a t := 'a X.t)
end)