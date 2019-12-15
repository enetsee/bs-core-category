type 'a t = 'a Belt.List.t

(*** -- Standard interfaces -- ***)

include MonadPlus.Make1(struct
  type nonrec 'a t = 'a t 
  let pure x = [x]
  let bind x ~f = Belt.List.(map x f  |> flatten)
  let map x ~f = Belt.List.map x f 
  let empty = [] 
  let alt = (@)
end)

include Foldable.MakeCustom1(struct
  type nonrec 'a t = 'a t 
  let foldLeft t ~f ~init = Belt.List.reduce t init f
  let foldRight_ t ~f ~init = Belt.List.reduceReverse t init (fun y x -> f x y)
  let foldRight = `Custom foldRight_
  let foldMap = `Derived
end)

include Monoid.Make1(struct
  type nonrec 'a t = 'a t
  let append = (@)
  let mempty = []
end)
