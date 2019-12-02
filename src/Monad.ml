include Monad_intf

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
  type ('a, 'b,_) t = ('a, 'b) X.t
  include (X : S2 with type ('a,'b) t := ('a,'b) X.t)
end

module S3_to_S2 (X : S3) : S2 with type ('a, 'b) t = ('a, 'b,unit) X.t = struct
  type ('a, 'b) t = ('a, 'b,unit) X.t
  include (X : S3 with type ('a,'b,'c) t := ('a,'b,'c) X.t)
end


(*** -- Make functors -- ***)
module Make3(X:Minimal3) : S3 with type ('a,'b,'c) t := ('a,'b,'c) X.t = struct
let bind = X.bind
  
  
  include Selective.Make3(struct
    type nonrec ('a,'b,'c) t = ('a,'b,'c) X.t
    
    let return = X.return 
    
    let map = X.map 
    
    let apply = 
      match X.apply with 
      | `Custom f -> f 
      | `Using_bind -> 
        fun t ~f -> 
          bind f ~f:(fun g ->
          bind t ~f:(fun x -> 
          return @@ g x))
    
    let liftA2 = X.liftA2
    let liftA3 = X.liftA3
    let discardFirst = X.discardFirst
    let discardSecond = X.discardSecond
      
    let select = 
      match X.select with
      | `Custom f -> f 
      | `Using_bind -> 
        fun x ~f ->
          bind x ~f:(function
          | EitherBase.First  a -> bind f ~f:(fun g -> return @@ g a)  (* Execute f *)
          | EitherBase.Second b -> return b (* Skip f *)
          )

  
  end)
  
  module Monad_infix = struct
    let (>>=) m f = bind m ~f 
    let (>>~) m n = m >>= Fun.const n
    let (>=>) f g = fun a -> f a >>= g
  end
  include Monad_infix
  
  let join tt = tt >>= Fun.id
  
  let sequenceM ts = 
    let op n m = m >>= fun x -> n >>= fun xs -> return (x::xs) in 
    Belt.List.reduceReverse ts (return []) op

  exception Undefined 

  let forever f = 
    let z = Obj.magic (fun () -> raise Undefined) in 
    let kcell = ref Fun.id in 
    let rec aux _ = 
      let r = f (kcell := Fun.id) >>= chained 
      in !kcell r
    and chained _ = 
      kcell := aux; z
    in aux z

  let mapM xs ~f = 
    sequenceM @@ Belt.List.map xs f
  
  let mapM_ xs ~f = 
    Belt.List.reduceReverse xs (return ()) (fun accu x -> bind (f x) ~f:(fun _ -> accu))


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
  let select = X.select
  let map = X.map
  let bind = X.bind
  
  let ap = 
    match X.apply with 
    | `Custom f -> f 
    | `Using_bind -> 
      fun t ~f -> 
        bind f ~f:(fun g ->
        bind t ~f:(fun x -> 
        return @@ g x))
        
  let apply = `Custom (fun t ~f -> 
    ap ~f:(ap ~f:(return (fun x f -> f x)) t) f
    )
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