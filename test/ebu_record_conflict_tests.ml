open! Core_kernel
open! Import

module Rm = Ebu.Record_merge.Make (struct
  type t = string [@@deriving sexp]

  let t = Irmin.Type.string
  let merge = Irmin.Merge.idempotent Irmin.Type.string
end)

module Conflict_store = Irmin_mem.KV (Rm.Conflict)
module Resolution_store = Irmin_mem.KV (Rm.Resolution)

let prepare () =
  let open Lwt.Syntax in
  let* conflict_repo = Conflict_store.Repo.v (Irmin_mem.config ()) in
  let* resolution_repo = Resolution_store.Repo.v (Irmin_mem.config ()) in
  let* conflict_store = Conflict_store.empty conflict_repo in
  let* resolution_store = Resolution_store.empty resolution_repo in
  Lwt.return
    ( Rm.make
        (module Conflict_store)
        (module Resolution_store)
        conflict_store
        resolution_store
    , conflict_repo
    , resolution_repo
    , conflict_store
    , resolution_store )
;;

let ( value_module
    , conflict_repo
    , resolution_repo
    , conflict_store
    , resolution_store )
  =
  Lwt_main.run (prepare ())
;;

module Value = (val value_module)
module Value_store = Irmin_mem.KV (Value)

let info = Fn.const Irmin.Info.empty

let all_keys () =
  let* conflicts = Conflict_store.list conflict_store [] in
  let conflicts = List.map conflicts ~f:(fun (k, _) -> k) in
  Lwt.return conflicts
;;

let all_conflicts () =
  let* keys = all_keys () in
  let* conflicts =
    List.map keys ~f:(fun k ->
        let+ conflict = Conflict_store.get conflict_store [ k ] in
        k, conflict)
    |> Lwt.all
  in
  conflicts
  |> List.bind ~f:(fun (k, c) ->
         Rm.Conflict_set.elements c |> List.map ~f:(fun e -> k, e))
  |> Lwt.return
;;

let delete_conflicts () =
  let* keys = all_keys () in
  let* _ =
    keys
    |> List.map ~f:List.return
    |> List.map ~f:(Conflict_store.remove_exn ~info conflict_store)
    |> Lwt.all
  in
  Lwt.return_unit
;;

let prep () =
  let* value_repo = Value_store.Repo.v (Irmin_mem.config ()) in
  let* value_store = Value_store.empty value_repo in
  let* () = delete_conflicts () in
  Lwt.return (value_repo, value_store)
;;

let print_conflicts () =
  let* contents = all_conflicts () in
  print_s [%message (contents : (string * string Ebu.Record_merge.Ct.t) list)];
  Lwt.return_unit
;;

let%expect_test "delete_conflicts works" =
  let* _repo, store1 = prep () in
  let* _repo, store2 = prep () in
  let* () = Value_store.set_exn ~info store1 [ "hello" ] "world" in
  let* () = Value_store.set_exn ~info store2 [ "hello" ] "general kenobi" in
  let* _merge_result = Value_store.merge_into ~info ~into:store1 store2 in
  let* () = print_conflicts () in
  let* () =
    [%expect
      {|
        (contents
         ((463254426-168693357-485153258
           ((lca ()) (left "general kenobi") (right world))))) |}]
  in
  let* () = delete_conflicts () in
  let* () = print_conflicts () in
  [%expect {| (contents ()) |}]
;;

let%expect_test "merge two branches that wrote to the same file" =
  let* _repo, store1 = prep () in
  let* _repo, store2 = prep () in
  let* () = Value_store.set_exn ~info store1 [ "hello" ] "world" in
  let* () = Value_store.set_exn ~info store2 [ "hello" ] "general kenobi" in
  let* _merge_result = Value_store.merge_into ~info ~into:store1 store2 in
  let* () = print_conflicts () in
  [%expect
    {|
    (contents
     ((463254426-168693357-485153258
       ((lca ()) (left "general kenobi") (right world))))) |}]
;;

let%expect_test "merge two branches with a user-defined resolution" =
  let* _repo, store1 = prep () in
  let* _repo, store2 = prep () in
  let* () = Value_store.set_exn ~info store1 [ "hello" ] "world" in
  let* () = Value_store.set_exn ~info store2 [ "hello" ] "general kenobi" in
  let* _merge_result = Value_store.merge_into ~info ~into:store1 store2 in
  let* conflicts = all_conflicts () in
  let conflict = List.hd_exn conflicts in
  print_s [%message (conflict : string * string Ebu.Record_merge.Ct.t)];
  let* () =
    [%expect
      {|
      (conflict
       (463254426-168693357-485153258
        ((lca ()) (left "general kenobi") (right world)))) |}]
  in
  let id, tripple = conflict in
  let* () =
    Resolution_store.set_exn
      resolution_store
      ~info
      [ id ]
      (Rm.Conflict_map.singleton tripple "my-resolution")
  in
  let* _merge_result = Value_store.merge_into ~info ~into:store1 store2 in
  let* result = Value_store.get store1 [ "hello" ] in
  print_endline result;
  [%expect {| my-resolution |}]
;;
