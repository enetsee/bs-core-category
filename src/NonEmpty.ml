type +'a t = NonEmpty of 'a * 'a list 
let head (NonEmpty(x,_)) = x 
let tail (NonEmpty(_,xs)) = 
  match xs with 
  | next::rest -> Some (NonEmpty(next,rest))
  | _ -> None
let cons x (NonEmpty(y,ys)) = NonEmpty(x,y::ys)  
let map (NonEmpty(next,rest)) ~f = NonEmpty(f next, List.map ~f rest)
let singleton x = NonEmpty(x,[])
let append (NonEmpty(nextL,restL)) (NonEmpty(nextR,restR)) = 
  NonEmpty(nextL,restL @ nextR :: restR)
  
include Semigroup.Make(struct
  type nonrec 'a t = 'a t
  let combine = append
end)
