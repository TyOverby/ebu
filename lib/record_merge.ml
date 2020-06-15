open! Core_kernel

module type Datatype_s = sig
  type t

  val t : t Irmin.Type.t
  val merge : t Irmin.Merge.t
end

module Ct = struct
  type 'a t =
    { lca : 'a option
    ; left : 'a
    ; right : 'a
    }
  [@@deriving sexp]
end

module Make (Datatype : Datatype_s) = struct
  module Conflict_tripple = struct
    open Ct

    type nonrec t = Datatype.t t

    let t =
      Irmin.Type.map
        (Irmin.Type.triple (Irmin.Type.option Datatype.t) Datatype.t Datatype.t)
        (fun (lca, left, right) -> { lca; left; right })
        (fun { lca; left; right } -> lca, left, right)
    ;;

    let compare = Irmin.Type.compare t
  end

  module Conflict_map = Stdlib.Map.Make (Conflict_tripple)
  module Conflict_set = Stdlib.Set.Make (Conflict_tripple)

  module Conflict = struct
    include Irmin.Merge.Set (Conflict_tripple)

    type t = Conflict_set.t

    let t =
      Irmin.Type.map
        (Irmin.Type.list Conflict_tripple.t)
        Conflict_set.of_list
        Conflict_set.elements
    ;;

    let merge = Irmin.Merge.option merge
  end

  module Resolution = struct
    include Irmin.Merge.Map (Conflict_tripple)

    type t = Datatype.t Conflict_map.t

    let merge =
      Irmin.Merge.option
      @@ merge Datatype.t (fun _ -> Irmin.Merge.option Datatype.merge)
    ;;

    let t =
      Irmin.Type.map
        (Irmin.Type.list (Irmin.Type.pair Conflict_tripple.t Datatype.t))
        (fun l -> l |> Stdlib.List.to_seq |> Conflict_map.of_seq)
        Conflict_map.bindings
    ;;
  end

  let make
      (type c r)
      (module C : Irmin.S
        with type t = c
         and type contents = Conflict.t
         and type key = string list)
      (module R : Irmin.S
        with type t = r
         and type contents = Resolution.t
         and type key = string list)
      conflicts
      resolutions
    =
    let module T = struct
      include Datatype

      let all_attempts_at_negotiation_have_failed ~error ~path ~tripple =
        let open Lwt.Syntax in
        let return_some_contents a = Lwt.return (Some (`Contents a)) in
        let* (_ : (unit, C.write_error) Result.t) =
          C.with_tree
            ~info:Irmin.Info.none
            ~strategy:`Merge
            conflicts
            path
            (function
              | Some (`Contents (contents, metadata)) ->
                return_some_contents
                @@ (Conflict_set.add tripple contents, metadata)
              | Some _ -> assert false
              | None ->
                return_some_contents
                @@ (Conflict_set.singleton tripple, C.Metadata.default))
        in
        Lwt.return (Error error)
      ;;

      let merge =
        let f =
          let open Lwt.Syntax in
          let old_merge = Irmin.Merge.f merge in
          let new_merge ~old left right =
            let* r = old_merge ~old left right in
            match r with
            | Ok ok -> Lwt.return (Ok ok)
            | Error error ->
              let* old = old () in
              let lca = Result.ok old |> Option.join in
              let path =
                let lca_hashed =
                  Irmin.Type.short_hash (Irmin.Type.option Datatype.t) lca
                in
                let left_hashed = Irmin.Type.short_hash Datatype.t left in
                let right_hashed = Irmin.Type.short_hash Datatype.t right in
                [ sprintf "%d-%d-%d" lca_hashed left_hashed right_hashed ]
              in
              let tripple = { Ct.lca; left; right } in
              let* resolutions = R.find resolutions path in
              (match
                 resolutions |> Option.bind ~f:(Conflict_map.find_opt tripple)
               with
              | Some resolution -> Lwt.return (Ok resolution)
              | None ->
                all_attempts_at_negotiation_have_failed ~error ~path ~tripple)
          in
          new_merge
        in
        Irmin.Merge.option (Irmin.Merge.v Datatype.t f)
      ;;
    end
    in
    (module T : Irmin.Contents.S with type t = Datatype.t)
  ;;
end
