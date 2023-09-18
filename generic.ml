open Staged_generics.Classes

let unwrap_trie x = .< let Wrapper.T x = .~x in x >.
let wrap_trie x = .< Wrapper.T .~x >.

module type GenericTrie = sig
  type k
  type 'v t
  val lookup : k code -> 'v t code -> 'v option code
  val insert : k code -> 'v code -> 'v t code -> 'v t code
  val entries : 'v t code -> (k * 'v) list code
  val empty : unit -> 'v t code
end

implicit module Trie_Void {T: Void} : GenericTrie
  with type k = T.t
  and type 'v t = unit Wrapper.t
= struct
  type k = T.t
  type 'v t = unit Wrapper.t
  let lookup _ (_: 'v t code) = .< None >.
  let insert k (_: 'v code) (_: 'v t code) = .< T.absurd .~k >.
  let entries _ = .< [] >.
  let empty () = .< Wrapper.T () >.
end

implicit module Trie_Unit {T: Unit} : GenericTrie
  with type k = T.t
  and type 'v t = 'v option Wrapper.t
= struct
  type k = T.t
  type 'v t = 'v option Wrapper.t

  let lookup _ (t: 'v t code) = unwrap_trie t
  let insert _ v _ = .< Wrapper.T (Some .~v) >.
  let entries t = .<
    match .~t with
    | Wrapper.T (Some v) -> [(.~T.unit, v)]
    | Wrapper.T None -> []
  >.
  let empty () = .< Wrapper.T None >.
end

let map_code (type a) (type b) (f: a code -> b code) : a list code -> b list code
  = fun l -> .<
      let rec loop = function
        | [] -> []
        | x :: xs -> .~(f .< x >.) :: loop xs
      in loop .~l
    >.
  
let fstmap_code(f: 'a code -> 'b code) (t: ('a * 'c) code) : ('b * 'c) code
  = .< let (a, c) = .~t in (.~(f .< a >.), c) >.

implicit module Trie_Sum
  {T: Sum}
  {A: GenericTrie with type k = T.a}
  {B: GenericTrie with type k = T.b}
: GenericTrie
  with type k = T.t
  and type 'v t = ('v A.t * 'v B.t) Wrapper.t
= struct
  type k = T.t
  type 'v t = ('v A.t * 'v B.t) Wrapper.t

  let lookup k t = .<
    let Wrapper.T (ta, tb) = .~t in
    .~(T.match_
      (fun a -> A.lookup a .< ta >.)
      (fun b -> B.lookup b .< tb >.)
      k
    )
  >.

  let insert k v t = .<
    let Wrapper.T (ta, tb) = .~t in
    .~(T.match_
      (fun a -> .< Wrapper.T (.~(A.insert a v .< ta >.), tb) >.)
      (fun b -> .< Wrapper.T (ta, .~(B.insert b v .< tb >.)) >.)
      k
    )
  >.

  let entries t = .<
    let Wrapper.T (ta, tb) = .~t in
    .~(map_code (fstmap_code T.construct_a) (A.entries .< ta >.)) @
    .~(map_code (fstmap_code T.construct_b) (B.entries .< tb >.))
  >.

  let empty () = .< Wrapper.T (.~(A.empty ()), .~(B.empty ())) >.
end

implicit module Trie_Product
  {T: Product}
  {A: GenericTrie with type k = T.a}
  {B: GenericTrie with type k = T.b}
: GenericTrie
  with type k = T.t
  and type 'v t = 'v A.t B.t
= struct
  type k = T.t
  type 'v t = 'v A.t B.t

  let lookup k tb =
    let (ka, kb) = T.deconstruct k in .<
    match .~(B.lookup kb tb) with
      | Some ta -> .~(A.lookup ka .< ta >.)
      | None -> None
  >.

  let insert k v tb =
    let (ka, kb) = T.deconstruct k in
    let ta = .<
      match .~(B.lookup kb tb) with
        | Some ta -> ta
        | None -> .~(A.empty ())
    >. in
    B.insert kb (A.insert ka v ta) tb

  let entries tb =
    let helper2 kb entry = .<
      let (ka, v) = .~entry in
      (.~(T.construct .< ka >. kb), v)
    >. in
    let helper entry = .<
      let (kb, ta) = .~entry in
      .~(map_code (helper2 .< kb >.) (A.entries .< ta >.))
    >. in
    .< List.concat .~(map_code helper (B.entries tb)) >.

  let empty () = B.empty ()
end

implicit module Trie_Wrapper
  {T: Wrapper}
  {Inner: GenericTrie with type k = T.inner}
: GenericTrie
  with type k = T.t
  and type 'v t = 'v Inner.t Wrapper.t
= struct
  type k = T.t
  type 'v t = 'v Inner.t Wrapper.t
  let lookup k t = Inner.lookup (T.unwrap k) (unwrap_trie t)
  let insert k v t = wrap_trie (Inner.insert (T.unwrap k) v (unwrap_trie t))
  let entries t = map_code (fstmap_code T.wrap) (Inner.entries (unwrap_trie t))
  let empty () = wrap_trie (Inner.empty ())
end

implicit module Trie_Mapping
  {M: S.Mapping}
: GenericTrie
  with type k = M.k
  and type 'v t = 'v M.t
= struct
  type k = M.k
  type 'v t = 'v M.t
  let lookup k t = .< M.lookup .~k .~t >.
  let insert k v t = .< M.insert .~k .~v .~t >.
  let entries t = .< M.entries .~t >.
  let empty () = .< M.empty () >.
end

(* this can't be marked implicit, because that would cause an infinite loop of instances
   Where you want to use this, use derive, as below.
 *)
module Derive {T: GenericTrie}
  : S.Mapping with type k = T.k and type 'v t = 'v T.t
= struct
  type k = T.k
  type 'v t = 'v T.t
  let lookup k t = Runcode.run @@ T.lookup .< k >. .< t >.
  let insert k v t = Runcode.run @@ T.insert .< k >. .< v >. .< t >.
  let entries t = Runcode.run @@ T.entries .< t >.
  let empty () = Runcode.run @@ T.empty ()
end

(* This function returning a first-class module is a hack to force implicit resolution,
   so that you don't have to explicitly write out the structure of your generic type.

   Ideally we would just be able to write something like
   implicit module My_mapping = Generic.(Derive {_} : S.Mapping with type t = xyz)
   and have the compiler infer what that _ should be.

   However, this {_} feature doesn't exist, and there are also no local opens in module expressions.
   Both of these problems are solved by using this `derive` function using first-class modules.

   All in all, you should write this:
   implicit module My_mapping =
     ((val Generic.(derive ())): S.Mapping with type k = <your key type>)

   Note also that you will need instances of S.Mapping in implicit scope for any of the basic types
   that you want to use non-trie maps for.
   For example, if you just want to use hash tables everywhere, use
   open implicit Tries_staged.Hashtbl'

   Also make sure you only have *one* implicit instance in scope for each of these basic types,
   and that you don't also have any Generic instances in scope for any types
   that you want to store in a basic structure instead of a trie.
 *)
let derive {T: GenericTrie} ()
  : (module S.Mapping with type k = T.k (* and type 'v t = 'v T.t*))
  = (module Derive {T})
