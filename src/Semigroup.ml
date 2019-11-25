include Semigroup_intf

module Make0(X:Minimal0) : S0 with type t := X.t = struct
  let combine = X.combine
  module Semigroup_infix = struct
    let (<>) = combine
  end
  include Semigroup_infix
end

module MakeDual0(X:Minimal0) : S0 with type t := X.t = struct  
  let combine x y = X.combine y x
  module Semigroup_infix = struct
    let (<>) = combine
  end
  include Semigroup_infix
end

module Make(X:Minimal1) : S1 with type 'a t := 'a X.t = struct
  let combine = X.combine
  module Semigroup_infix = struct
    let (<>) = combine
  end
  include Semigroup_infix
end

module MakeDual(X:Minimal1) : S1 with type 'a t := 'a X.t = struct  
  let combine x y = X.combine y x
  module Semigroup_infix = struct
    let (<>) = combine
  end
  include Semigroup_infix
end

module Make2(X:Minimal2) : S2 with type ('a,'b) t := ('a,'b) X.t = struct
  let combine = X.combine
  module Semigroup_infix = struct
    let (<>) = combine
  end
  include Semigroup_infix
end

module MakeDual2(X:Minimal2) : S2 with type ('a,'b) t := ('a,'b) X.t = struct  
  let combine x y = X.combine y x
  module Semigroup_infix = struct
    let (<>) = combine
  end
  include Semigroup_infix
end