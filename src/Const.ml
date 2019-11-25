include Const_intf

module Make(M : Monoid.S0) : S with module M := M = struct
  type 'a t = M.t
  
  let getConst x = x
  let const x = x
  module MinimalA = struct
    type nonrec 'a t = 'a t
    let map x ~f:_ = x
    let return _ = M.empty
    let apply x ~f = M.combine f x
  end
  
  include Applicative.Make(MinimalA)
  
  module Over = struct
    include Selective.Make(struct
      include MinimalA 
      let select x ~f = M.combine x f
    end)
    let const = const 
    let getConst = getConst
  end
  
  module Under = struct
    include Selective.Make(struct
      include MinimalA 
      let select x ~f:_ = x
    end)
    let const = const 
    let getConst = getConst
  end
end