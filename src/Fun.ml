
let id x = x 

let const x _ = x 

let compose f g x = f @@ g x

let curry f x y = f (x,y)

let uncurry f (x,y) = f x y

let flip f y x  = f x y

module Fun_infix  = struct
  let (<<) f g  = compose f g
  let (>>) g f = compose f g
end

include Fun_infix

include Functor.Make2(struct
  type ('b,'a) t = ('a -> 'b)
  let map x ~f = f << x
end)