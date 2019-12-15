include Distributive_intf

module Make2(X:Minimal2) : S2 with type ('a,'b) t := ('a,'b) X.t = struct
  include Cartesian.Make2(X)
  include (
    Cocartesian.Make2(X) 
      : module type of Cocartesian.Make2(X) 
        with module CategoryInfix := CategoryInfix
  )
  include X 
end