
include DiffMonoid_intf

module Make0(M:Monoid.S0) : S0 with module M := M = struct
  type t = M.t -> M.t
  let abs t = t M.empty 
  let rep = M.combine 

  include Monoid.Make0(struct
    type nonrec t = t
    let empty = rep M.empty 
    let combine = Fun.compose
  end)

end

