include Bifunctor_intf

(*** -- Conversion functors -- ***)

module S2_to_S3 (X : S2) : S3 with type ('a, 'b,_) t = ('a, 'b) X.t = struct
  type ('a, 'b, _) t = ('a, 'b) X.t
  include (X : S2 with type ('a,'b) t := ('a,'b) X.t)
end

module S3_to_S2 (X : S3) : S2 with type ('a, 'b) t = ('a, 'b, unit) X.t = struct
  type ('a, 'b) t = ('a, 'b, unit) X.t
  include (X : S3 with type ('a,'b,'c) t := ('a,'b,'c) X.t)
end

(*** -- Make functors -- ***)

module MakeCustom3(X:Custom3) : S3 with type ('a,'b,'c) t := ('a,'b,'c) X.t = struct

  let bimap = X.bimap

  let mapFirst =
    match X.mapFirst with 
    | `Custom f -> f 
    | _ -> fun t ~f -> bimap t ~first:f ~second:(fun x -> x)

  let mapSecond =
    match X.mapSecond with 
    | `Custom f -> f 
    | _ -> fun t ~f -> bimap t ~second:f ~first:(fun x -> x)
end

module MakeCustom2(X:Custom2) : S2 with type ('a,'b) t := ('a,'b) X.t = MakeCustom3(struct
  type ('a,'b,_) t = ('a,'b) X.t
  include (X: Custom2 with type ('a,'b) t := ('a,'b) X.t)
end)

module Make3(X:Minimal3) : S3 with type ('a,'b,'c) t := ('a,'b,'c) X.t = MakeCustom3(struct 
  include X
  let mapFirst = `Derived
  let mapSecond = `Derived
end)

module Make2(X:Minimal2) : S2 with type ('a,'b) t := ('a,'b) X.t = Make3(struct
  type ('a,'b,_) t = ('a,'b) X.t
  include (X: Minimal2 with type ('a,'b) t := ('a,'b) X.t)
end)
