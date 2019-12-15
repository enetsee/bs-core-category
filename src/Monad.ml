include Monad_intf

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
  type ('a, 'b,_) t = ('a, 'b) X.t
  include (X : S2 with type ('a,'b) t := ('a,'b) X.t)
end

module S3_to_S2 (X : S3) : S2 with type ('a, 'b) t = ('a, 'b,unit) X.t = struct
  type ('a, 'b) t = ('a, 'b,unit) X.t
  include (X : S3 with type ('a,'b,'c) t := ('a,'b,'c) X.t)
end


(*** -- Make functors -- ***)

module MakeCustom3(X:Custom3) : S3 with type ('a,'b,'c) t := ('a,'b,'c) X.t = struct
  
  let bind = X.bind
  
  include Selective.MakeCustom3(struct
    type nonrec ('a,'b,'c) t = ('a,'b,'c) X.t
    
    let pure = X.pure 
    
    let map = X.map 
    
    let replace = X.replace

    let apply = 
      match X.apply with 
      | `Custom f -> f 
      | _ -> 
        fun t ~f -> 
          bind f ~f:(fun g ->
          bind t ~f:(fun x -> 
          pure @@ g x))
    
    let liftA2 = X.liftA2

    let applyFirst = X.applyFirst

    let applySecond = X.applySecond
      
    let select = 
      match X.select with
      | `Custom f -> f 
      | _ -> 
        fun x ~f ->
          bind x ~f:(function
          | EitherBase.First  a -> bind f ~f:(fun g -> pure @@ g a)  (* Execute f *)
          | Second b -> pure b (* Skip f *)
          )

  end)
  
  module MonadInfix = struct
    let (>>=) m f = bind m ~f 
    let (>>~) m n = m >>= fun _ -> n
    let (>=>) f g = fun a -> f a >>= g
  end
  include MonadInfix
  
  let join tt = tt >>= fun x -> x
  
  let sequenceM ts = 
    let op n m = m >>= fun x -> n >>= fun xs -> pure (x::xs) in 
    Belt.List.reduceReverse ts (pure []) op

  exception Undefined 

  let forever f = 
    let z = Obj.magic (fun () -> raise Undefined) in 
    let kcell = ref (fun x -> x) in 
    let rec aux _ = 
      let r = f (kcell := (fun x -> x)) >>= chained 
      in !kcell r
    and chained _ = 
      kcell := aux; z
    in aux z

  let mapM xs ~f = 
    sequenceM @@ Belt.List.map xs f
  
  let mapM_ xs ~f = 
    Belt.List.reduceReverse xs (pure ()) (fun accu x -> bind (f x) ~f:(fun _ -> accu))


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
  let map = `Derived
  let replace = `Derived
  let apply = `Derived
  let liftA2 = `Derived
  let applyFirst = `Derived
  let applySecond = `Derived
  let select = `Derived
end)

module Make2(X:Minimal2) : S2 with type ('a,'b) t := ('a,'b) X.t = Make3(struct
  type ('a,'b,_) t = ('a,'b) X.t
  include (X: Minimal2 with type ('a,'b) t := ('a,'b) X.t)
end)

module Make1(X:Minimal1) : S1 with type 'a t := 'a X.t = Make2(struct
  type ('a,_) t = 'a X.t
  include (X: Minimal1 with type 'a t := 'a X.t)
end)