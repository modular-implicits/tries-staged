type myADT = int * unit

let () =
  let open Tries_staged in
  let open [@warning "-33"] Staged_generics.Instances in
  let open [@warning "-33"] Imp.Any in
  let open [@warning "-33"] Imp.Data in
  let open [@warning "-33"] Assoc_list in
  let implicit module My_mapping =
    (val let open Generic in derive (): S.Mapping with type k = myADT) in
  let c x = (x, ()) in
  let l = [
    c 1, "ace";
    c 2, "two";
    c 3, "three";
    c 4, "four"
  ] in
  let m = Utils.from_list l in
  assert (Utils.entries m = l)
