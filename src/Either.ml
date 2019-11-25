include EitherBase

include Monad.Make2(struct
  type nonrec ('a,'b) t = ('a,'b) t 
  let map = map 
  let return = first 
  let bind t ~f =
    match t with 
    | First x -> f x 
    | Second y -> Second y 
  let apply = `Using_bind 
  let select = `Using_bind
end)

module Traversable = struct

  module Make3(F: Applicative.S3) = struct 
    let traverse t ~f = 
      match t with 
      | First x -> F.map ~f:first @@ f x 
      | Second x -> F.return (Second x)
  end

  module Make2(F: Applicative.S2) = Make3(Applicative.S2_to_S3(F))

  module Make(F:Applicative.S) = Make2(Applicative.S_to_S2(F))

end