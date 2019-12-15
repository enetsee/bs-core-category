include Cocartesian_intf

module Make2(X:Minimal2) : S2 with type ('b,'a) t := ('b,'a) X.t = struct
  include Category.Make2(X)
  include X 
  module CocartesianInfix = struct
    let (|+|) = sum
  end
  include CocartesianInfix 
end