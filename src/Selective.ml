include Selective_intf

module Either = EitherBase
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
  let select = X.select 
  include Applicative.Make3(X)

  let branch t ~first ~second =
    let x = map t ~f:(Either.bimap ~f:Fun.id ~g:Either.first) in 
    let f = map first ~f:(Fun.compose Either.second) in
    select (select x ~f) ~f:second
    

  let ifS x ~t ~f =
    branch
      (map x ~f:(fun b -> if b then Either.First () else Either.Second ()))
      ~first:(map t ~f:Fun.const) 
      ~second:(map f ~f:Fun.const)

  let whenS x act = ifS x ~t:act ~f:(return ())
  
  let orS x y =  ifS x ~t:(return true) ~f:y

  let andS x y = ifS x ~t:y ~f:(return false)

  let fromOptionS x mx = 
    let y =
      map mx 
        ~f:(function 
        | Some x -> Either.second x 
        | _ -> Either.first ()
        )
     (* OptionBase.withDefault ~default:(Either.first ()) ~f:Either.second <$> mx in  *)
    in select y ~f:(Fun.const <$> x)

  module Selective_infix = struct 
    let ( <*? ) x f = select x ~f
    let (<||>) x y = orS x y
    let (<&&>) x y = andS x y
  end

  include Selective_infix

  
  let anyS xs ~pred = Belt.List.reduceReverse xs (return false) (fun accu x -> accu <||> pred x)  

  let allS xs ~pred = Belt.List.reduceReverse xs (return true) (fun accu x -> accu <&&> pred x)

  let rec whileS x = whenS x (whileS x)
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
  let select = X.select 
  let map = X.map
  let liftA2 = X.liftA2
  let liftA3 = X.liftA3
  let discardFirst = X.discardFirst
  let discardSecond = X.discardSecond
end)

module Make_backwards2(X:Minimal2) : S2 with type ('a,'b) t := ('a,'b) X.t = Make_backwards3(struct
  type ('a,'b,_) t = ('a,'b) X.t
  include (X: Minimal2 with type ('a,'b) t := ('a,'b) X.t)
end)

module Make_backwards(X:Minimal) : S with type 'a t := 'a X.t = Make_backwards2(struct
  type ('a,_) t = 'a X.t
  include (X: Minimal with type 'a t := 'a X.t)
end)