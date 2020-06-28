open! Core_kernel
open! Bonsai_web.Future
open Bonsai.Let_syntax

let tree, root = Tree.For_testing.demo
let tree_var = Bonsai.Var.create tree
let root_var = Bonsai.Var.create root
let tree = Bonsai.Var.value tree_var
let root = Bonsai.Var.value root_var
let history_var = Bonsai.Var.create []
let history = Bonsai.Var.value history_var
let selected_var = Bonsai.Var.create [ -1 ]
let selected = Bonsai.Var.value selected_var
let set_selected = unstage (Effect.of_sync_fun (Bonsai.Var.set selected_var))

let push_history =
  unstage
    (Effect.of_sync_fun (fun h ->
         Bonsai.Var.update history_var ~f:(fun p -> h :: p)))
;;

let pop_history =
  unstage
    (Effect.of_sync_fun (fun () ->
         let did_pop = ref true in
         Bonsai.Var.update history_var ~f:(function
             | [] ->
               did_pop := false;
               []
             | hd :: rest ->
               Bonsai.Var.set selected_var hd;
               rest);
         !did_pop))
;;

let clear_history =
  unstage (Effect.of_sync_fun (fun () -> Bonsai.Var.set history_var []))
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

let debug =
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

let render =
  let rec render common =
    let node = Map.find_exn common.Common.tree common.Common.node_id in
    match node with
    | Tree.Leaf leaf -> draw_leaf leaf common
    | Tree.Branch branch -> draw_branch branch render common
  in
  return
  @@ let%map tree = tree
     and root = root
     and selected = selected in
     render
       { Common.is_hd = false; tree; node_id = root; path = [] }
       Path.empty
       selected
;;

let app =
  let%sub debug = debug in
  let%sub input = input in
  let%sub rendered = render in
  return
  @@ let%map debug = debug
     and input = input
     and rendered = rendered
     and selected = selected in
     let rendered_view, paths = rendered in
     let paths_view =
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
     let input =
       input (fun key ->
           let open Effect.Let_syntax in
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
               Effect.inject_ignoring_response
               @@ let%map () = set_selected path
                  and () = push_history selected in
                  ()
             | None -> Vdom.Event.Ignore)
           | "l" ->
             Effect.inject_ignoring_response
             @@ let%bind did_pop = pop_history () in
                if did_pop
                then return ()
                else (
                  let child =
                    match List.rev selected with
                    | -1 :: rest -> 0 :: rest
                    | _ -> []
                    (* This seems like a weird default *)
                  in
                  let child = List.rev child in
                  match Map.find paths child with
                  | Some () -> set_selected child
                  | None -> return ())
           | "k" ->
             (match Map.closest_key paths `Less_than selected with
             | Some (path, ()) ->
               Effect.inject_ignoring_response
               @@ let%map () = set_selected path
                  and () = clear_history () in
                  ()
             | None -> Vdom.Event.Ignore)
           | "j" ->
             (match Map.closest_key paths `Greater_than selected with
             | Some (path, ()) ->
               Effect.inject_ignoring_response
               @@ let%map () = set_selected path
                  and () = clear_history () in
                  ()
             | None -> Vdom.Event.Ignore)
           | _ -> Vdom.Event.Ignore)
     in
     Vdom.Node.div [] [ debug; rendered_view; paths_view; input ]
;;

let () =
  let _handle =
    Start.start
      Start.Result_spec.just_the_view
      ~bind_to_element_with_id:"app"
      (Tree_ui.main ~tree)
  in
  ()
;;
