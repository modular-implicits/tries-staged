implicit module Map_Mapping {M: Map.S}
  : S.Mapping with type k = M.key and type +'v t = 'v M.t
= struct
  type k = M.key
  type 'v t = 'v M.t

  let lookup k m =
    if M.mem k m
    then Some (M.find k m)
    else None
  let insert = M.add
  let entries = M.bindings
  let empty () = M.empty
end
