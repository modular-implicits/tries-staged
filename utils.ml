let from_list {M: S.Mapping} xs =
  let m = ref (M.empty ()) in
  List.iter (fun (k, v) -> m := M.insert k v !m) xs; !m

let entries {M: S.Mapping} m = M.entries m
