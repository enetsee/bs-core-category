include Selective_intf

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


(*** -- Make functors --- ***)


module MakeCustom3(X:Custom3) : S3 with type ('a,'b,'c) t := ('a,'b,'c) X.t = struct 
  
  include Applicative.MakeCustom3(X)

  let select = X.select 
  
  let branch t ~first ~second =
    let x = map t ~f:(EitherBase.bimap ~first:(fun x -> x) ~second:EitherBase.first) in 
    let f = map first ~f:(fun g x ->  EitherBase.second @@ g x) in
    select (select x ~f) ~f:second
    
  let ifS x ~t ~f =
    branch
      (map x ~f:(fun b -> if b then EitherBase.First () else EitherBase.Second ()))
      ~first:(map t ~f:(fun x _ -> x)) 
      ~second:(map f ~f:(fun x _ -> x))

  let whenS x act = ifS x ~t:act ~f:(pure ())
  
  let orS x y =  ifS x ~t:(pure true) ~f:y

  let andS x y = ifS x ~t:y ~f:(pure false)

  let fromOptionS x mx = 
    let y =
      map mx 
        ~f:(function 
        | Some x -> EitherBase.second x 
        | _ -> EitherBase.first ()
        )
    in select y ~f:((fun x _ -> x) <$> x)

  module SelectiveInfix = struct 
    let ( <*? ) x f = select x ~f
    let (<||>) x y = orS x y
    let (<&&>) x y = andS x y
  end

  include SelectiveInfix

  
  let anyS xs ~pred = Belt.List.reduceReverse xs (pure false) (fun accu x -> accu <||> pred x)  

  let allS xs ~pred = Belt.List.reduceReverse xs (pure true) (fun accu x -> accu <&&> pred x)

  let rec whileS x = whenS x (whileS x)
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