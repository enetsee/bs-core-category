include Validation_intf

module Make(Error : Semigroup.S0) : S with module Error := Error = struct
  
  type 'a t = 
    | Success of 'a
    | Failure of Error.t
  
  let success x = Success x 
  let failure x = Failure x
  
  include Selective.Make(struct
    type nonrec 'a t = 'a t
    
    let map_ t ~f = 
      match t with 
      | Success x -> Success (f x)
      | Failure err -> Failure err
      
    let map = `Custom map_
    
    let return x = Success x
    
    let apply x ~f = 
      match f , x with
      | Success g , Success y -> Success (g y)
      | Failure e1 , Failure e2 -> Failure Error.(combine e1 e2)
      | Failure err ,  _ 
      | _ , Failure err -> Failure err
      
    let select x ~f = 
      match x with 
      | Failure err  -> Failure err
      | Success Either.(First a)  -> map_ ~f:((|>) a) f
      | Success Either.(Second b) -> Success b

    let liftA2 = `Using_apply
    let liftA3 = `Using_apply
    let discardFirst = `Using_apply
    let discardSecond = `Using_apply
        
  end)

end