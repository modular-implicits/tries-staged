open Imp.Any

type 't wrapper = Wrapper of 't

(* defined such that Hashtbl.S and Hashtbl.SeededS both implement HashtblS automatically *)
module type HashtblS = sig
  type key
  type 'v t
  val mem : 'v t -> key -> bool
  val find : 'v t -> key -> 'v
  val replace : 'v t -> key -> 'v -> unit
  val create : int -> 'v t
  val iter : (key -> 'v -> unit) -> 'v t -> unit
end

implicit module Hashtbl_HashtblS {K: Any}
  : HashtblS with type key = K.t and type 'v t = (K.t, 'v) Hashtbl.t
= struct
  type key = K.t
  type 'v t = (key, 'v) Hashtbl.t
  let mem = Hashtbl.mem
  let find = Hashtbl.find
  let replace = Hashtbl.replace
  let create size = Hashtbl.create size
  let iter = Hashtbl.iter
end

implicit module Hashtbl_specific {H: HashtblS}
  : S.Mapping with type k = H.key and type 'v t = 'v H.t wrapper
= struct
  type k = H.key
  type 'v t = 'v H.t wrapper
  let lookup k (Wrapper t) =
    if H.mem t k
    then Some (H.find t k)
    else None
  let insert k v (Wrapper t) = H.replace t k v; Wrapper t
  let entries (Wrapper t) =
    let res = ref [] in
    H.iter (fun k v -> res := (k, v) :: !res) t;
    !res
  let empty () = Wrapper (H.create 0)
end
