let from_list {M: S.Mapping} xs =
  List.fold_left (fun m (k, v) -> M.insert k v m) (M.empty ()) xs

let entries {M: S.Mapping} m = M.entries m
