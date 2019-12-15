type 'a t = 'a Belt.List.t
include MonadPlus.S1 with type 'a t := 'a t
include Foldable.S1 with type 'a t := 'a t
include Monoid.S1 with type 'a t := 'a t  
