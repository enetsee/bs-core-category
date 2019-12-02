
include Belt.List


include Monoid.Make(struct
  type nonrec 'a t = 'a t 
  let empty = [] 
  let combine x y = x @ y
end)  

include Foldable.Make(struct
  type nonrec 'a t = 'a t 
  let foldLeft t ~f ~init = reduce t init f 
end)

include Monad.Make(struct
  type nonrec 'a t = 'a t

  let apply = `Custom (fun x ~f ->
    map f (fun g -> map x g)
    |> flatten
  )

  let bind x ~f = 
    map x f
    |> flatten

  let map = `Custom (fun x ~f -> map x f )
  
  let return x = [x]
    
  let select = `Using_bind

  let liftA2 = `Using_apply

  let liftA3 = `Using_apply
    
  let discardFirst = `Using_apply

  let discardSecond = `Using_apply
end)

let isEmpty = function [] -> true | _ -> false

let filter x ~f = Belt.List.keep x f

let filterMap x ~f = Belt.List.keepMap x f

let iter (f: 'a -> unit) xs = 
  let _ = map ~f xs in ()

(** maybe use a more appropriate type when we start using this?? *)
let rec last = function 
  | x::[] -> Some x 
  | _::rest -> last rest
  | [] -> None 
  

let mapi t ~f = 
  Belt.List.mapWithIndex t f

let forAll xs ~f = Belt.List.every xs f

let forAll2 xs ys ~f = Belt.List.every2 xs ys f

let cons x xs = x::xs 

module Traversable = struct 

  module Make3(F: Applicative.S3) = struct     
    let traverse t ~f = 
      let f x ys = F.liftA2 ~f:cons (f x) ys in
      foldRight ~f ~init:(F.return []) t       
  end

  module Make2(F: Applicative.S2) = Make3(Applicative.S2_to_S3(F))

  module Make(F:Applicative.S) = Make2(Applicative.S_to_S2(F))

end