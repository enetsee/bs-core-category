module type S1 = sig 
  module M : TyCon.S1
  module N : TyCon.S1
  type nat = { apply : 'a. 'a M.t -> 'a N.t }
end

module type S12 = sig 
  module M : TyCon.S1
  module N : TyCon.S2
  type nat = { apply : 'a 'e. 'a M.t -> ('a,'e) N.t }
end

module type S21 = sig 
  module M : TyCon.S2
  module N : TyCon.S1
  type nat = { apply : 'a 'e. ('a,'e) M.t -> 'a N.t }
end

module type S2 = sig 
  module M : TyCon.S2
  module N : TyCon.S2
  type nat = { apply : 'a 'e. ('a,'e) M.t -> ('a,'e) N.t }
end

module type S23 = sig 
  module M : TyCon.S2
  module N : TyCon.S3
  type nat = { apply : 'a 'd 'e. ('a,'e) M.t -> ('a,'d,'e) N.t }
end

module type S32 = sig 
  module M : TyCon.S3
  module N : TyCon.S2
  type nat = { apply : 'a 'd 'e. ('a,'d,'e) M.t -> ('a,'e) N.t }
end

module type S3 = sig 
  module M : TyCon.S3
  module N : TyCon.S3
  type nat = { apply : 'a 'd 'e. ('a,'d,'e) M.t -> ('a,'d,'e) N.t }
end

