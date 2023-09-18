let () =
  let open Tries_staged in
  let open [@warning "-33"] Staged_generics.Instances in
  let open [@warning "-33"] Imp.Any in
  let open [@warning "-33"] Imp.Data in
  let open [@warning "-33"] Assoc_list in
  let implicit module MyADT : Staged_generics.Classes.Sum
    with type t = Test_support.myADT
    and type a = unit * int
    and type b = string
  = struct
    type t = Test_support.myADT
    type a = unit * int
    type b = string
    let construct_a a = .< let ((), x) = .~a in Test_support.Left ((), x) >.
    let construct_b b = .< Test_support.Right .~b >.
    let match_ fa fb x = .<
      match .~x with
      | Test_support.Left ((), a) -> .~(fa .< ((), a) >.)
      | Test_support.Right b -> .~(fb .< b >.)
    >.
  end in
  let implicit module My_mapping =
    (val let open Generic in derive (): S.Mapping with type k = Test_support.myADT) in
  let c x = Test_support.Left ((), x) in
  let l = [
    c 1, "ace";
    c 2, "two";
    c 3, "three";
    c 4, "four"
  ] in
  let m = Utils.from_list l in
  assert (Utils.entries m = l)
