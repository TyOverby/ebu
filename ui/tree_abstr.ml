open! Core_kernel
open! Bonsai_web.Future
open! Bonsai.Let_syntax

module type Tree = sig
  module Cmp : sig
    type t [@@deriving sexp]

    include Comparator.S with type t := t
  end

  type data
  type t
  type extra

  val stuff : extra -> t -> data
  val children : extra -> t -> (Cmp.t, t, Cmp.comparator_witness) Map.t
end

let component
    (type t key data cmp extra)
    (module T : Tree
      with type Cmp.t = key
       and type data = data
       and type Cmp.comparator_witness = cmp
       and type extra = extra
       and type t = t)
    ~(root : T.t Bonsai.Value.t)
    ~(extra : T.extra Bonsai.Value.t)
    ~render
  =
  let key_compare a b = T.Cmp.comparator.Comparator.compare a b in
  let key_equal a b = key_compare a b = 0 in
  let module Path = struct
    module T = struct
      type t = T.Cmp.t list [@@deriving sexp]

      let compare = List.compare key_compare
    end

    include T
    include Comparable.Make (T)
  end
  in
  let rec render_helper ~node ~path ~could_be_selected =
    let%sub selection_value =
      Bonsai.match_option
        could_be_selected
        ~none:
          (let%sub a = Bonsai.const (false, None) in
           return a)
        ~some:(fun selected_path ->
          return
          @@ let%map selected_path = selected_path
             and path = path in
             if List.is_prefix
                  (List.rev selected_path)
                  ~prefix:(List.rev path)
                  ~equal:key_equal
             then (
               let is_equal = List.equal key_equal selected_path path in
               is_equal, Some selected_path)
             else false, None)
    in
    let is_selected = Bonsai.Value.map selection_value ~f:Tuple2.get1 in
    let could_be_selected = Bonsai.Value.map selection_value ~f:Tuple2.get2 in
    let stuff =
      let%map node = node
      and extra = extra in
      T.stuff extra node
    in
    let children =
      let%map node = node
      and extra = extra in
      T.children extra node
    in
    let f key data =
      let new_path = Bonsai.Value.map2 key path ~f:List.cons in
      Bonsai.laze
        (lazy (render_helper ~node:data ~path:new_path ~could_be_selected))
    in
    let%sub rendered_children = Bonsai.assoc (module T.Cmp) children ~f in
    let%sub rendered = render stuff ~is_selected ~children:rendered_children in
    return
    @@ let%map rendered = rendered
       and path = path in
       rendered, path
  in
  let rec paths_helper ~node ~path =
    let f key data =
      let path = Bonsai.Value.map2 key path ~f:List.cons in
      Bonsai.laze (lazy (paths_helper ~node:data ~path))
    in
    let children =
      let%map node = node
      and extra = extra in
      T.children extra node
    in
    let%sub children_paths = Bonsai.assoc (module T.Cmp) children ~f in
    return
    @@ let%map children_paths = children_paths
       and my_path = path in
       Map.fold
         children_paths
         ~init:(Path.Map.singleton my_path ())
         ~f:(fun ~key:_ ~data acc ->
           match Map.append ~lower_part:acc ~upper_part:data with
           | `Ok map -> map
           | `Overlapping_key_ranges -> failwith "no")
  in
  let%sub _paths = paths_helper ~node:root ~path:(Bonsai.Value.return []) in
  let%sub rendered =
    render_helper
      ~node:root
      ~path:(Bonsai.Value.return [])
      ~could_be_selected:(Bonsai.Value.return (Some []))
  in
  return rendered
;;
