module type Mapping = sig
  type k
  type 'v t
  val lookup : k -> 'v t -> 'v option
  val insert : k -> 'v -> 'v t -> 'v t
  val entries : 'v t -> (k * 'v) list (* worst-case O(n) *)
  val empty : unit -> 'v t
end
