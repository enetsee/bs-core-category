include Apply_intf

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
    
  let apply = X.apply 
    
  include Functor.MakeCustom3(X)  
      
  let liftA2 = 
    match X.liftA2 with
    | `Custom f -> f 
    | _ -> fun x y ~f -> apply ~f:(map x ~f) y
        
  let applySecond = 
    match X.applySecond with 
    | `Custom f -> f
    | _ -> fun x y -> apply ~f:((fun x -> x) <$ x)  y
    
  let applyFirst = 
    match X.applyFirst with 
    | `Custom f -> f 
    | _ -> liftA2 ~f:(fun x _ -> x)

  let merge mx my = liftA2 ~f:(fun x y -> x,y) mx my

  let liftA3 x y z ~f = apply ~f:(liftA2 x y ~f) z

  let liftA4 x y z u ~f = apply ~f:(liftA3 x y z ~f) u

  let liftA5 x y z u v ~f = apply ~f:(liftA4 x y z u ~f) v

  module ApplyInfix = struct 
    let ( <*> ) f x = apply x ~f 
    let ( *>  ) x y = applySecond x y
    let ( <* ) x y = applyFirst x y
    let ( ** ) mx my = merge mx my
  end

  include ApplyInfix
end 

module MakeCustom2(X:Custom2) : S2 with type ('a,'b) t := ('a,'b) X.t = MakeCustom3(struct
  type ('a,'b,_) t = ('a,'b) X.t
  include (X: Custom2 with type ('a,'b) t := ('a,'b) X.t)
end)

module MakeCustom1(X:Custom1) : S1 with type 'a t := 'a X.t = MakeCustom2(struct
  type ('a,_) t = 'a X.t
  include (X: Custom1 with type 'a t := 'a X.t)
end)

module Make3(X:Minimal3) : S3 with type ('a,'b,'c) t := ('a,'b,'c) X.t = MakeCustom3(struct 
  include X
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