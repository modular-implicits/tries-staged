(* this has to be in a separate file because metaocaml can't deal with unqualified constructors *)
type 't t = T of 't
