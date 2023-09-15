open StdLabels

module type ArrayIndex = sig
  type t
  val start_index : int
  val size : int
  val to_int : t -> int
  val from_int : int -> t
end

implicit module Array_map {T: ArrayIndex} : S.Mapping with type k = T.t and type 'v t = 'v option array
= struct
  type k = T.t
  type 'v t = 'v option array
  let lookup i a =
    let i = T.to_int i - T.start_index in
    if i >= T.size || i < 0
    then None
    else a.(i)
  let insert i v a =
    let i = T.to_int i - T.start_index in
    a.(i) <- Some v; a
  let entries a =
    let res = ref [] in
    Array.iteri ~f:(fun i -> function
      | None -> ()
      | Some x -> res := (T.from_int i, x) :: !res
    ) a;
    !res
  let empty () = Array.make T.size None
end
