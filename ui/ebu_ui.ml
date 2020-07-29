open! Core_kernel
open! Bonsai_web.Future
open Bonsai.Let_syntax
module M2 = Intersection_observer
module T = Tree_abstr

module Tree_t = struct
  module Cmp = Tree.Id

  type data = Tree.leaf
  type t = Tree.t
  type extra = Tree.Store.t

  let rec stuff extra = function
    | Tree.Leaf leaf -> leaf
    | Tree.Branch { hd; _ } -> Map.find_exn extra hd |> stuff extra
  ;;

  let children extra = function
    | Tree.Leaf _ -> Map.empty (module Tree.Id)
    | Tree.Branch { tl; _ } ->
      tl
      |> List.map ~f:(fun k -> k, Map.find_exn extra k)
      |> Tree.Id.Map.of_alist_exn
  ;;
end

module F : Tree_abstr.Tree = Tree_t

let tree, root = Tree.For_testing.demo
let tree = Bonsai.Value.return tree
let root = Bonsai.Value.return root

let textbox ~onkey =
  Vdom.Node.textarea
    [ Vdom.Attr.on_keydown (fun evt ->
          onkey
            (Js_of_ocaml.Js.Optdef.to_option
               (Js_of_ocaml.Js.Optdef.map evt##.key Js_of_ocaml.Js.to_string)))
    ]
    []
;;

let tree =
  let extra = tree in
  let root =
    let%map root = root
    and tree = tree in
    Map.find_exn tree root
  in
  let%sub r =
    Tree_abstr.component
      (module Tree_t)
      ~root
      ~extra
      ~render:(fun data ~is_selected ~children ->
        let%sub state = Bonsai.state [%here] (module Int) ~default_model:0 in
        return
        @@ let%map { kind; content } = data
           and state, set_state = state
           and is_selected = is_selected
           and children = children in
           let kids =
             children
             |> Map.data
             |> List.map ~f:Tuple2.get1
             |> Vdom.Node.div [ Vdom.Attr.class_ "children" ]
           in
           let me =
             Vdom.Node.div
               [ Vdom.Attr.class_ "leaf" ]
               [ Vdom.Node.textf "%s: %s %d" kind content state
               ; Vdom.Node.button
                   [ Vdom.Attr.on_click (fun _ -> set_state (state + 1)) ]
                   [ Vdom.Node.text "+1" ]
               ]
           in
           let leaf_or_branch =
             if Map.is_empty children then "leaf" else "branch"
           in
           let selected = if is_selected then [ "selected" ] else [] in
           let attrs = Vdom.Attr.classes (leaf_or_branch :: selected) in
           Vdom.Node.div [ attrs ] [ me; kids ])
  in
  return
  @@ let%map r, _ = r in
     r
;;

(*
let input =
  let%sub text_state =
    Bonsai.state
      [%here]
      (module struct
        type t = string list [@@deriving sexp, equal]
      end)
      ~default_model:[]
  in
  return
  @@ let%map lines, set_lines = text_state in
     fun on_select ->
       textbox ~onkey:(function
           | None ->
             Vdom.Event.Many
               [ Vdom.Event.Prevent_default; Vdom.Event.Stop_propagation ]
           | Some key ->
             Vdom.Event.Many
               [ Vdom.Event.Prevent_default
               ; Vdom.Event.Stop_propagation
               ; on_select key
               ; set_lines (key :: lines)
               ])
;;

let main =
  let%sub input = input in
  let%sub tree = Tree_ui.main ~tree ~root in
  return
  @@ let%map input = input
     and main, on_key = tree in
     Vdom.Node.div [] [ input on_key; main ]
;; *)

let () =
  let _handle =
    Start.start
      Start.Result_spec.just_the_view
      ~bind_to_element_with_id:"app"
      tree
  in
  ()
;;
