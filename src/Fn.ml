
type (-'a,+'b) t = 'a -> 'b

let const x _ = x

let flip f y x = f x y 

include Closed.Make2(struct
  type nonrec ('a,'b) t = ('a,'b) t
  let id x = x   
  let compose f g x = f @@ g x
  let exl (l,_ )= l
  let exr (_,r) = r
  let product f g x = (f x , g x)
  let ap (f,x)= f x
  let uncurry f (x,y) = f x y 
  let curry f x y = f (x,y)
end)

include Functor.Make2(struct
  type nonrec ('b,'a) t = ('a,'b) t
  let map x ~f = compose f x
end)

include Contravariant.Make2(struct
  type nonrec ('a,'b) t = ('a,'b) t
  let cmap x ~f = compose x f 
end)

include Profunctor.MakeCustom2(struct
  type nonrec ('a,'b) t = ('a,'b) t
  let cmapFirst = `Custom cmap 
  let mapSecond = `Custom map
  let dimap t ~first ~second = 
    compose (cmap ~f:first) (map ~f:second) @@ t
end)