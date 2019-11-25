include Belt.Option 
type 'a t = 'a option

let option t ~default ~f = mapWithDefault t default f 

let some t = Some t 

include Monad.Make(struct 
  type nonrec 'a t = 'a t 
  
  let map t ~f = map t f   

  let return x = Some x 

  let apply = `Custom (fun x ~f ->
    match f , x with 
    | Some g, Some y -> Some (g y)
    | _ , _ -> None 
  )

  let bind x ~f = 
    match x with 
    | Some y -> f y 
    | _ ->  None 
  
  let select = `Using_bind
    

end)

let map2 t1 t2 ~f = 
  match t1 , t2 with 
  | Some x, Some y -> Some (f x y)
  | _ -> None

module Traversable = struct

  module Make3(F: Applicative.S3) = struct
    let traverse t ~f = 
      match t with 
      | None -> F.return None 
      | Some x -> F.map ~f:some @@ f x
  end

  module Make2(F: Applicative.S2) = Make3(Applicative.S2_to_S3(F))

  module Make(F:Applicative.S) = Make2(Applicative.S_to_S2(F))
end