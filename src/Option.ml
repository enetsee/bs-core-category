type 'a t = 'a option

exception NoValue

let isSome = function Some _ -> true | _ -> false 

let isNone = function None -> true | _ -> false 

let orElse t ~else_:u =
  match t with 
  | None -> u 
  | _ -> t

let valueMap t ~f ~default = 
  match t with 
  | Some x -> f x 
  | _ -> default

let value t ~default = valueMap t ~f:(fun x -> x) ~default
    
let valueExn = function
  | Some x -> x  
  | _ -> raise NoValue   

let toList = function 
  | Some x -> [x]
  | _ -> []

let toArray = function 
  | Some x -> [|x|]
  | _ -> [||]

(*** -- Standard interfaces  -- ***)

include MonadPlus.Make1(struct
  type nonrec 'a t = 'a t 
  let pure x = Some x
  let bind x ~f = 
    match x with 
    | Some y -> f y 
    | _ -> None
  let map x ~f = 
    match x with 
    | Some a -> Some (f a)
    | _ -> None 
  let empty = None 
  let alt x y = orElse x ~else_:y      
end)

include Foldable.MakeCustom1(struct
  type nonrec 'a t = 'a t

  let foldLeft t ~f ~init = 
    match t with 
    | Some x -> f init x
    | _ -> init

  let foldRight_ t ~f ~init = 
    match t with 
    | Some x -> f x init 
    | _ -> init 

  let foldRight = `Custom foldRight_

  let foldMap_ (type a) (module M : Monoid.S0 with type t = a) ?(init = M.mempty) t ~f =      
    valueMap t ~default:init ~f 

  let foldMap = `Custom foldMap_

end)

module First = Monoid.Make1(struct
  type nonrec 'a t = 'a t 
  let mempty = None 
  let append = alt 
end)

module Last = Monoid.MakeDual1(struct
  type nonrec 'a t = 'a t 
  let mempty = None 
  let append = alt 
end)


