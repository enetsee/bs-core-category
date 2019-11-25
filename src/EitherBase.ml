type ('a,'b) t = First of 'a | Second of 'b

let first a = First a 

let second a = Second a 

let either ~first ~second = function 
  | First x -> first x 
  | Second x -> second x

let bimap x ~f ~g = 
  match x with 
  | First a -> First(f a)
  | Second b -> Second(g b)

let map x ~f = bimap x ~f ~g:Fun.id
