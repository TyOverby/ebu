module type Datatype_s = sig
  type t

  val t : t Irmin.Type.t
  val merge : t Irmin.Merge.t
end

module Ct : sig
  type 'a t =
    { lca : 'a option
    ; left : 'a
    ; right : 'a
    }
  [@@deriving sexp]
end

module Make (Datatype : Datatype_s) : sig
  module Conflict_tripple : sig
    type t = Datatype.t Ct.t
  end

  module Conflict_set : Stdlib.Set.S with type elt = Conflict_tripple.t
  module Conflict : Irmin.Contents.S with type t = Conflict_set.t
  module Conflict_map : Stdlib.Map.S with type key = Conflict_tripple.t
  module Resolution : Irmin.Contents.S with type t = Datatype.t Conflict_map.t

  val make
    :  (module Irmin.S
          with type contents = Conflict.t
           and type key = string list
           and type t = 'conflicts)
    -> (module Irmin.S
          with type contents = Resolution.t
           and type key = string list
           and type t = 'resolutions)
    -> 'conflicts
    -> 'resolutions
    -> (module Irmin.Contents.S with type t = Datatype.t)
end
