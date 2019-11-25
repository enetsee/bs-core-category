include Validation_intf

module Make(Error : Semigroup.S0) : S with module Error := Error = struct
  
  type 'a t = 
    | Success of 'a
    | Failure of Error.t
  
  let success x = Success x 
  let failure x = Failure x
  
  include Selective.Make(struct
    type nonrec 'a t = 'a t
    
    let map t ~f = 
      match t with 
      | Success x -> Success (f x)
      | Failure err -> Failure err
      
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
      | Success Either.(First a)  -> map ~f:((|>) a) f
      | Success Either.(Second b) -> Success b
        
  end)

end