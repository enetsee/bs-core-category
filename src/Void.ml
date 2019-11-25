
type t = Void of t 

let eq _ _ = true

let compare _ _ = 0

include Semigroup.Make0(struct
  type nonrec t  = t 
  let combine t _ = t
end)