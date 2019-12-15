type ('a,'b) t = 
  | First of 'a 
  | Second of 'b

let first a = First a 

let second a = Second a 

let either ~first ~second = function 
  | First x -> first x 
  | Second x -> second x

let isFirst = function First _ -> true | _ -> false 

let isSecond = function Second _ -> true | _ -> false 

let fromFirst ~default = function 
  | First x -> x 
  | _ -> default 

let fromSecond ~default = function 
  | Second x -> x 
  | _ -> default 

include Bifunctor.Make2(struct
  type nonrec ('a,'b) t = ('a,'b) t
  let bimap t ~first ~second =
    match t with 
    | First x -> First (first x)
    | Second y -> Second (second y)
end)

