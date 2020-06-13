module type Datatype_s = sig
  type t

  val t : t Irmin.Type.t
  val merge : t Irmin.Merge.t
end

module Make (Datatype : Datatype_s) : sig
  module Conflict_tripple : sig
    type t =
      { lca : Datatype.t option
      ; left : Datatype.t
      ; right : Datatype.t
      }
    [@@deriving compare]
  end

  module Conflict : Irmin.Contents.S with type t = Set.Make(Conflict_tripple).t

  module Resolution :
    Irmin.Contents.S with type t = Datatype.t Map.Make(Conflict_tripple).t

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
