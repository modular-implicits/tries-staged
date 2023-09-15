open Imp.Data
type ('k, 'v) wrapper = Wrapper of ('k * 'v) list

implicit module Assoc_list {T: Eq}
  : S.Mapping with type k = T.t and type +'v t = (T.t, 'v) wrapper
= struct
  type k = T.t
  type +'v t = (k, 'v) wrapper
  let (=) = T.(=)
  let lookup k (Wrapper t) =
    let rec helper = function
      | [] -> None
      | (k', v) :: es ->
          if k' = k
          then Some v
          else helper es
    in helper t
  let insert k new_v (Wrapper t) =
    let rec helper = function
      | [] -> [(k, new_v)]
      | (k', old_v) :: es ->
          if k' = k
          then (k, new_v) :: es
          else (k, old_v) :: helper es
    in Wrapper (helper t)
  let entries (Wrapper t) = t
  let empty () = Wrapper []
end
