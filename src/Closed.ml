include Closed_intf

module Make2(X:Minimal2): S2 with type ('a,'b) t := ('a,'b) X.t = struct
  include Cartesian.Make2(X)
  include X
end