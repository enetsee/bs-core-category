type ('a,'err) t = 
  | Ok of 'a 
  | Error of 'err 

let bimap x ~f ~g = 
  match x with 
  | Ok y -> Ok (f y)
  | Error err -> Error (g err)

let mapError x ~f = 
  match x with 
  | Error err -> Error (f err)
  | Ok y -> Ok y

let ok x = Ok x 

let error x = Error x 

let isError = function 
  | Error _ -> true 
  | _ -> false 

let isOk = function 
  | Ok _ -> true 
  | _ -> false 

let result t ~withError ~withOk = 
  match t with
  | Ok x -> withOk x
  | Error err -> withError err 

include Monad.Make2(struct
  type nonrec ('a,'b) t = ('a,'b) t

  let map_ t ~f =
    match t with 
    | Ok x -> Ok (f x)
    | Error err -> Error err
  
  let map = `Custom map_

  let return x = Ok x 
  
  let apply = `Custom (fun x ~f ->
    match f , x with
    | Ok g , Ok y -> Ok (g y)
    | Error err ,  _ 
    | _ , Error err -> Error err
  )
      
  let select  = `Custom (fun x ~f ->
    match x with 
    | Error err  -> Error err
    | Ok Either.(First a)  -> map_ ~f:(fun g -> g a) f
    | Ok Either.(Second b) -> Ok b
  )

  let bind x ~f = 
    match x with 
    | Ok y -> f y 
    | Error err -> Error err 

  let liftA2 = `Using_apply
  let liftA3 = `Using_apply
  let discardFirst = `Using_apply
  let discardSecond = `Using_apply
end)