include EitherBase

include Monad.MakeCustom2(struct
  type nonrec ('a,'b) t = ('a,'b) t
  let map = `Custom mapFirst 
  let replace = `Derived 
  let pure x = first x
  let bind x ~f = 
    match x with 
    | First a -> f a 
    | Second b -> Second b
  let apply_ x ~f = 
    match f with 
    | Second e  -> Second e
    | First g -> mapFirst ~f:g x
  let apply = `Custom apply_
  let liftA2 = `Derived 
  let applyFirst = `Derived 
  let applySecond = `Derived
  let select = `Derived 
  let join = `Derived
end)

include Semigroup.Make2(struct
  type nonrec ('a,'b) t = ('a,'b) t
  let append a b = 
    match a with 
    | Second _ -> b 
    | _ -> a 
end)

