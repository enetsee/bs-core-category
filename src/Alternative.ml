include Alternative_intf
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
  let emptyA = X.emptyA
  let combineA = X.combineA 

  include Applicative.Make3(X)

  module Alternative_infix = struct 
    let (<|>) x y = combineA x y
  end

  include Alternative_infix

  let optional x = map ~f:(fun x -> Some x) x  <|> return None

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
  let emptyA = X.emptyA 
  let combineA = X.combineA
end)

module Make_backwards2(X:Minimal2) : S2 with type ('a,'b) t := ('a,'b) X.t = Make_backwards3(struct
  type ('a,'b,_) t = ('a,'b) X.t
  include (X: Minimal2 with type ('a,'b) t := ('a,'b) X.t)
end)

module Make_backwards(X:Minimal) : S with type 'a t := 'a X.t = Make_backwards2(struct
  type ('a,_) t = 'a X.t
  include (X: Minimal with type 'a t := 'a X.t)
end)