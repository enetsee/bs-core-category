type 'a t = 'a option
exception NoValue
val isSome : 'a t -> bool 
val isNone : 'a t -> bool 
val orElse : 'a t -> else_:'a t -> 'a t 
val value : 'a t -> default:'a -> 'a 
val valueExn : 'a t -> 'a 
val valueMap : 'a t -> f:('a -> 'b) -> default:'b -> 'b
val toList : 'a t -> 'a list
val toArray : 'a t -> 'a array 
include MonadPlus.S1 with type 'a t := 'a t 
include Foldable.S1 with type 'a t := 'a t 
module First : Monoid.S1 with type 'a t := 'a t  
module Last : Monoid.S1 with type 'a t := 'a t  
  


