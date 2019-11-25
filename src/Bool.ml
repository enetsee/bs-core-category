
type t = bool 

let bool p ~f ~t = 
  if p then t else f


module BasicAnd = struct
  type t = bool
  let empty = true
  let combine = (&&)

end

module Or = struct
  type nonrec t = t
  include Monoid.Make0(struct
    type nonrec t = t
    let empty = false
    let combine = (||)
  end)
end

module And = struct
  type nonrec t = t
  include Monoid.Make0(struct
    type nonrec t = t
    let empty = true
    let combine = (&&)
  end)
end