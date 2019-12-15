type 'a t = 'a Js.Promise.t
type error = Js.Promise.error
val oneOf : 'a t list -> 'a t
val fail : exn -> 'a t 
val catch : 'a t -> f:(error -> 'a) -> 'a t
include Monad.S1 with type 'a t := 'a t 