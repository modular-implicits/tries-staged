(* this has to be in a separate file because metaocaml can't deal with unqualified constructors *)
type 'a t_ = T of 'a

open Imp.Show
implicit module Show_Wrapper {S: Show} : Show with type t = S.t t_
= struct
  type t = S.t t_
  let show (T x) = S.show x
end

type 'a t = 'a t_
