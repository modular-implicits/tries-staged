module OCAML_String = String
module OCAML_List = List
open Imp.Data
type ('k, 'v) assoc_list = Wrapper of ('k * 'v) list

implicit module Assoc_list {T: Eq}
  : S.Mapping with type k = T.t and type +'v t = (T.t, 'v) assoc_list
= struct
  type k = T.t
  type +'v t = (k, 'v) assoc_list
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
          then (k', new_v) :: es
          else (k', old_v) :: helper es
    in Wrapper (helper t)
  let entries (Wrapper t) = t
  let empty () = Wrapper []
end

open Imp.Show

implicit module Show_Assoc_list {K: Show} {V: Show}
  : Show with type t = (K.t, V.t) assoc_list
= struct
  type t = (K.t, V.t) assoc_list
  let show (Wrapper entries) =
    let show_entry (k, v) = K.show k ^ " => " ^ V.show v in
    "Assoc_list (" ^ OCAML_String.concat ", " (OCAML_List.map show_entry entries) ^ ")"
end
