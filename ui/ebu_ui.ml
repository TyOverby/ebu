open! Core_kernel
open! Bonsai_web.Future
open Bonsai.Let_syntax
module M2 = Intersection_observer

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
;;

let () =
  let _handle =
    Start.start
      Start.Result_spec.just_the_view
      ~bind_to_element_with_id:"app"
      main
  in
  ()
;;
