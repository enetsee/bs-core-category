module type S0 = sig
  include TyCon.S0 
  val ofJson : t Json.Decode.t 
  val toJson : t Json.Encode.t 
end

module type S1 = sig
  include TyCon.S1 
  val ofJson : 'a Json.Decode.t -> 'a t Json.Decode.t 
  val toJson : 'a Json.Encode.t -> 'a t Json.Encode.t 
end

module type S2 = sig
  include TyCon.S2
  val ofJson : 'a Json.Decode.t -> 'b Json.Decode.t -> ('a,'b) t Json.Decode.t 
  val toJson : 'a Json.Encode.t -> 'b Json.Encode.t -> ('a,'b) t Json.Encode.t 
end

module type S3 = sig
  include TyCon.S3 
  val ofJson : 'a Json.Decode.t -> 'b Json.Decode.t -> 'b Json.Decode.t -> ('a,'b,'c) t Json.Decode.t 
  val toJson : 'a Json.Encode.t -> 'b Json.Encode.t -> 'c Json.Encode.t ->  ('a,'b,'c) t Json.Encode.t 
end