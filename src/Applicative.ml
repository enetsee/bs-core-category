include Applicative_intf

(*** -- Conversion functors -- ***)

module S_to_S2 (X : S) : S2 with type ('a,_) t = 'a X.t = struct
  type ('a,_) t = 'a X.t
  include (X : S with type 'a t := 'a X.t)
end 

module S2_to_S (X : S2) : S with type 'a t = ('a,unit) X.t = struct
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

(*** -- Make functors --- ***)

module Make3(X:Minimal3) : S3 with type ('a,'b,'c) t := ('a,'b,'c) X.t = struct 
  let return = X.return 
  let apply = X.apply 
  let unit () = return ()

  include Functor.Make3(X)

  let liftA = map 
  let liftA2 x y  ~f = apply ~f:(map x ~f) y
  let liftA3 x y z ~f = apply ~f:(liftA2 x y ~f) z

  let merge mx my = liftA2 ~f:(fun x y -> x,y) mx my

  module Applicative_infix = struct 
    let ( <*> ) f x = apply x ~f 
    let ( *>  ) x y = (Fun.id <$ x) <*> y
    let ( <* )  = liftA2 ~f:(fun x _ -> x)
    let ( ** ) mx my = merge mx my
  end
  include Applicative_infix
end 

module Make2(X:Minimal2) : S2 with type ('a,'b) t := ('a,'b) X.t = Make3(struct
  type ('a,'b,_) t = ('a,'b) X.t
  include (X: Minimal2 with type ('a,'b) t := ('a,'b) X.t)
end)

module Make(X:Minimal) : S with type 'a t := 'a X.t = Make2(struct
  type ('a,_) t = 'a X.t
  include (X: Minimal with type 'a t := 'a X.t)
end)

module Make_backwards3(X:Minimal3) : S3 with type ('a,'b,'c) t := ('a,'b,'c) X.t = Make3(struct
  type ('a,'b,'c) t = ('a,'b,'c) X.t
  let return = X.return   
  let apply x ~f = X.apply ~f:(X.apply ~f:(X.return (fun x f -> f x)) x) f
  let map = X.map
end)

module Make_backwards2(X:Minimal2) : S2 with type ('a,'b) t := ('a,'b) X.t = Make_backwards3(struct
  type ('a,'b,_) t = ('a,'b) X.t
  include (X: Minimal2 with type ('a,'b) t := ('a,'b) X.t)
end)

module Make_backwards(X:Minimal) : S with type 'a t := 'a X.t = Make_backwards2(struct
  type ('a,_) t = 'a X.t
  include (X: Minimal with type 'a t := 'a X.t)
end)