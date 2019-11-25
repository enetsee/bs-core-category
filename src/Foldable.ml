include Foldable_intf

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
  let foldLeft = X.foldLeft

  let foldRight x ~f ~init =
    let f' k x z = k @@ f x z in
    foldLeft ~f:f' ~init:(fun x -> x) x init

  let foldMap (type a) (module M : Monoid.S0 with type t = a)
      ?init:(empty = M.empty) x  ~f =
    foldRight ~f:(fun x accu -> M.combine accu @@ f x) ~init:empty x
  
  let exists  ?init x ~pred  = foldMap (module Bool.Or) x ~f:pred ?init

  let forall ?init x ~pred = foldMap (module Bool.And) x ~f:pred ?init

  let find x ~pred =
    foldRight
      ~f:(fun x accu ->
          match accu with 
          | Some _ -> accu 
          | _ ->  if pred x then Some x else None 
        )
      ~init:None x
end 

module Make2(X:Minimal2) : S2 with type ('a,'b) t := ('a,'b) X.t = Make3(struct
  type ('a,'b,_) t = ('a,'b) X.t
  include (X: Minimal2 with type ('a,'b) t := ('a,'b) X.t)
end)

module Make(X:Minimal) : S with type 'a t := 'a X.t = Make2(struct
  type ('a,_) t = 'a X.t
  include (X: Minimal with type 'a t := 'a X.t)
end)