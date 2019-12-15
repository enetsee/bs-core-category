include Cartesian_intf

module Make2(X:Minimal2) : S2 with type ('a,'b) t := ('a,'b) X.t = struct
  include Category.Make2(X)
  include X 
  module CartesianInfix = struct
    let (|*|) = product
  end
  include CartesianInfix
end