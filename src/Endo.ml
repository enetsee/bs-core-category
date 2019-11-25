include Endo_intf
module Make(X: TyCon.S0) : S with module X := X = struct
  type t = X.t -> X.t 
  include Monoid.Make0(struct
    type nonrec t = t
    let empty = Fun.id 
    let combine f g = Fun.compose f g
  end)

  let runEndo t x = t x
end
