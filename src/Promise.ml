
type 'a t = 'a Js.Promise.t

type error = Js.Promise.error

let oneOf xs = Js.Promise.race xs

let fail x = Js.Promise.reject x

let catch x ~f = Js.Promise.catch f x

include Monad.Make(struct
  type nonrec 'a t = 'a t
  let return x = Js.Promise.resolve x

  let bind x ~f = Js.Promise.then_ f x

  let map t ~f = 
    bind t ~f:(Fun.compose return f)

  let apply = `Using_bind
    
  let select = `Using_bind
    
end)

let sequence xs = Js.Promise.all xs

let sequence2 x2 = Js.Promise.all2 x2

let sequence3 x3 = Js.Promise.all3 x3

let sequence4 x4 = Js.Promise.all4 x4

let sequence5 x5 = Js.Promise.all5 x5

let sequence6 x6 = Js.Promise.all6 x6