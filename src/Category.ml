include Category_intf

module Make2(X: Minimal2) : S2 with type ('a,'b) t := ('a,'b) X.t = struct
  let id = X.id 
  let compose = X.compose
  module CategoryInfix = struct
    let (<<) f g = compose f g 
    let (>>) g f = compose f g
  end
  include CategoryInfix
end

