include Monoid_intf

module Make0(X:Minimal0) : S0 with type t = X.t = struct
  type t = X.t
  let empty = X.empty 
  include Semigroup.Make0(X)
end

module MakeDual0(X:Minimal0) : S0 with type t = X.t = struct
  type t = X.t
  let empty = X.empty 
  include Semigroup.MakeDual0(X)
end

module Make(X:Minimal1) : S1 with type 'a t = 'a X.t = struct
  type 'a t = 'a X.t
  let empty = X.empty 
  include Semigroup.Make(X)
end

module MakeDual(X:Minimal1) : S1 with type 'a t = 'a X.t = struct
  type 'a t = 'a X.t
  let empty = X.empty 
  include Semigroup.MakeDual(X)
end

module Make2(X:Minimal2) : S2 with type ('a,'b) t = ('a,'b) X.t = struct
  type ('a,'b) t = ('a,'b) X.t
  let empty = X.empty 
  include Semigroup.Make2(X)
end

module MakeDual2(X:Minimal2) : S2 with type ('a,'b) t = ('a,'b) X.t = struct
  type ('a,'b) t = ('a,'b) X.t
  let empty = X.empty 
  include Semigroup.MakeDual2(X)
end