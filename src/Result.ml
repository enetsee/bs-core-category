type ('a,'err) t = ('a,'err) result

let ok x = Ok x 

let error x = Error x 

let isError = function 
  | Error _ -> true 
  | _ -> false 

let isOk = function 
  | Ok _ -> true 
  | _ -> false 

let result t ~withErr ~withOk = 
  match t with
  | Ok x -> withOk x
  | Error err -> withErr err 

let map x ~f = 
  match x with 
  | Ok x -> Ok (f x)
  | Error err -> Error err 

let mapError x ~f = 
  match x with 
  | Error err -> Error (f err)
  | Ok y -> Ok y

(*** -- Standard interfaces -- ***)

include Monad.MakeCustom2(struct
  type nonrec ('a,'b) t = ('a,'b) t

  let pure = ok
  
  let apply = `Custom (fun x ~f ->
    match f , x with
    | Ok g , Ok y -> Ok (g y)
    | Error err ,  _ 
    | _ , Error err -> Error err
  )
      
  let select  = `Custom (fun x ~f ->
    match x with 
    | Error err  -> Error err
    | Ok Either.(First a)  -> map ~f:(fun g -> g a) f
    | Ok Either.(Second b) -> Ok b
  )

  let map = `Custom map

  let replace = `Derived

  let bind x ~f = 
    match x with 
    | Ok y -> f y 
    | Error err -> Error err 

  let liftA2 = `Derived
  let applyFirst = `Derived 
  let applySecond = `Derived 

  let join = `Derived
end)

include Bifunctor.MakeCustom2(struct
  type nonrec ('a,'b) t  = ('a,'b) t 
  let bimap x ~first ~second = 
    match x with 
    | Ok y -> Ok (first y)
    | Error err -> Error (second err)

  let mapFirst = `Custom map 

  let mapSecond= `Custom mapError

end)

include Foldable.Make2(struct
  type nonrec ('a,'b) t  = ('a,'b) t 
  let foldLeft t ~f ~init = 
    match t with 
    | Ok x -> f init x 
    | _ -> init 
end)