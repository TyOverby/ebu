open! Core_kernel
open! Bonsai_web.Future
open Bonsai.Let_syntax

module Selected = struct
  module Model = struct
    type t =
      { selected : int list
      ; history : int list list
      }
    [@@deriving sexp, equal]

    let default = { selected = [ -1 ]; history = [] }
  end

  module Action = struct
    type t =
      | Set of int list
      | Clear
      | Push of int list
      | Pop of (bool -> Ui_event.t)

    let sexp_of_t = function
      | Set il -> Sexp.List [ Sexp.Atom "set"; [%sexp_of: int list] il ]
      | Push il -> Sexp.List [ Sexp.Atom "push"; [%sexp_of: int list] il ]
      | Pop _ -> Sexp.Atom "pop"
      | Clear -> Sexp.Atom "clear"
    ;;
  end
end

let selected_f =
  Bonsai.state_machine0
    [%here]
    (module Selected.Model)
    (module Selected.Action)
    ~default_model:Selected.Model.default
    ~apply_action:(fun ~inject:_ ~schedule_event model -> function
      | Set selected -> { model with selected }
      | Push il -> { model with history = il :: model.history }
      | Clear -> { model with history = [] }
      | Pop inject_did_pop ->
        let new_model, did_pop =
          match model.history with
          | [] -> model, false
          | hd :: rest -> { selected = hd; history = rest }, true
        in
        schedule_event (inject_did_pop did_pop);
        new_model)
;;

let textbox ~onkey =
  Vdom.Node.textarea
    [ Vdom.Attr.on_keypress (fun evt ->
          onkey
            (Js_of_ocaml.Js.Optdef.to_option
               (Js_of_ocaml.Js.Optdef.map evt##.key Js_of_ocaml.Js.to_string)))
    ]
    []
;;

let debug ~tree =
  return
  @@ let%map tree = tree in
     tree
     |> Tree.Store.sexp_of_t
     |> Sexp.to_string_hum
     |> Vdom.Node.text
     |> List.return
     |> Vdom.Node.pre []
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

module Path = struct
  include Map.Make (struct
    type t = int list [@@deriving sexp, compare]
  end)

  type nonrec t = unit t

  let add t path = add_exn t ~key:path ~data:()
end

module Common = struct
  type t =
    { is_hd : bool
    ; node_id : Tree.Id.t
    ; path : int list
    ; tree : Tree.Store.t
    }
end

let draw_leaf
    { Tree.kind; content }
    { Common.is_hd; node_id; path; _ }
    acc
    selected
  =
  let path = List.rev path in
  let classes = [ "leaf" ] in
  let classes =
    if [%equal: int list] path selected then "current" :: classes else classes
  in
  let classes = if is_hd then "hd" :: classes else classes in
  let view =
    Vdom.Node.div
      [ Vdom.Attr.classes classes ]
      [ Vdom.Node.div
          []
          [ Vdom.Node.textf
              "leaf %s %s %s"
              kind
              (Tree.Id.to_string node_id)
              (Sexp.to_string_hum [%message (path : int list)])
          ]
      ; Vdom.Node.text content
      ]
  in
  view, Path.add acc path
;;

let draw_branch { Tree.hd; tl } render { Common.tree; path; _ } acc selected =
  let hd_view, acc =
    render
      { Common.is_hd = true; tree; node_id = hd; path = -1 :: path }
      acc
      selected
  in
  let tl_view, acc =
    List.foldi tl ~init:([], acc) ~f:(fun i (views, acc) child_id ->
        let child_view, acc =
          render
            { Common.is_hd = false; tree; node_id = child_id; path = i :: path }
            acc
            selected
        in
        child_view :: views, acc)
  in
  let tl_view = List.rev tl_view in
  let classes = [ "branch" ] in
  let classes =
    if List.is_prefix ~equal:Int.equal selected ~prefix:(List.rev path)
    then "selected" :: classes
    else classes
  in
  let view =
    Vdom.Node.div
      [ Vdom.Attr.classes classes ]
      [ hd_view; Vdom.Node.div [ Vdom.Attr.class_ "children" ] tl_view ]
  in
  view, acc
;;

let render ~tree ~root ~selected =
  let rec render common =
    let node = Map.find_exn common.Common.tree common.Common.node_id in
    match node with
    | Tree.Leaf leaf -> draw_leaf leaf common
    | Tree.Branch branch -> draw_branch branch render common
  in
  return
  @@ let%map tree = tree
     and root = root
     and { Selected.Model.selected; _ }, _ = selected in
     render
       { Common.is_hd = false; tree; node_id = root; path = [] }
       Path.empty
       selected
;;

let main ~tree ~root =
  let%sub selected = selected_f in
  let%sub debug = debug ~tree in
  let%sub rendered = render ~tree ~root ~selected in
  return
  @@ let%map _debug = debug
     and rendered = rendered
     and { Selected.Model.selected; _ }, inject_selection = selected in
     let rendered_view, paths = rendered in
     let _paths_view =
       paths
       |> Map.keys
       |> List.map ~f:(fun path ->
              path
              |> [%sexp_of: int list]
              |> Sexp.to_string_hum
              |> Vdom.Node.text
              |> List.return
              |> Vdom.Node.li [])
       |> Vdom.Node.ul []
     in
     let on_key key =
       match key with
       | "h" ->
         let parent =
           match List.rev selected with
           | [] -> [ -1 ]
           | -1 :: _ :: rest | _ :: rest -> -1 :: rest
         in
         let parent = List.rev parent in
         (match Map.closest_key paths `Less_or_equal_to parent with
         | Some (path, ()) ->
           Ui_event.Many
             [ inject_selection (Set path); inject_selection (Push selected) ]
         | None -> Ui_event.Ignore)
       | "l" ->
         inject_selection
           (Pop
              (function
              | true -> Ui_event.Ignore
              | false ->
                let child =
                  match List.rev selected with
                  | -1 :: rest -> 0 :: rest
                  | _ -> []
                  (* This seems like a weird default *)
                in
                let child = List.rev child in
                (match Map.find paths child with
                | Some () -> inject_selection (Set child)
                | None -> Ui_event.Ignore)))
       | "k" ->
         (match Map.closest_key paths `Less_than selected with
         | Some (path, ()) ->
           Ui_event.Many [ inject_selection (Set path); inject_selection Clear ]
         | None -> Vdom.Event.Ignore)
       | "j" ->
         (match Map.closest_key paths `Greater_than selected with
         | Some (path, ()) ->
           Ui_event.Many [ inject_selection (Set path); inject_selection Clear ]
         | None -> Vdom.Event.Ignore)
       | _ -> Vdom.Event.Ignore
     in
     let view = Vdom.Node.div [] [ rendered_view ] in
     view, on_key
;;
