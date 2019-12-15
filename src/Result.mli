type ('a,'err) t = ('a,'err) result

val ok: 'a -> ('a,_) t
val error: 'err -> (_,'err) t 
val isError: ('a,'err) t -> bool
val isOk: ('a,'err) t -> bool
val result: ('a,'err) t -> withErr:('err -> 'b) -> withOk:('a -> 'b) -> 'b 
val mapError: ('a,'err) t -> f:('err -> 'err1) -> ('a,'err1) t

include Monad.S2 with type ('a,'err) t := ('a,'err) t

include Bifunctor.S2 with type ('a,'err) t := ('a,'err) t

include Foldable.S2 with type ('a,'err) t := ('a,'err) t