type 'a t = 'a Js.Promise.t

type error = Js.Promise.error

let oneOf xs = Js.Promise.race @@ Belt.List.toArray xs

let fail x = Js.Promise.reject x

let catch x ~f = 
  Js.Promise.catch 
    (fun y -> Js.Promise.resolve @@ f y) x

include Monad.Make1(struct
  type nonrec 'a t = 'a t
  let pure x = Js.Promise.resolve x
  let bind x ~f = Js.Promise.then_ f x
end)

