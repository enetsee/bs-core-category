include Const_intf

module Make(M : Monoid.S0) : S with module M := M = struct
  type 'a t = M.t
  
  let getConst x = x
  let const x = x

  module MinimalA = struct
    type nonrec 'a t = 'a t
    let map = `Custom (fun x ~f:_ -> x)
    let return _ = M.empty

    let apply x ~f = M.combine f x

    let liftA2 = `Using_apply
    let liftA3 = `Using_apply
    let discardFirst = `Using_apply
    let discardSecond = `Using_apply
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