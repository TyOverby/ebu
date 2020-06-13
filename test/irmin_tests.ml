open! Core_kernel
open! Import

module Basic = struct
  module Store = Irmin_mem.KV (Irmin.Contents.String)
  module Sync = Irmin.Sync (Store)

  open struct
    let info = Fn.const Irmin.Info.empty

    let prepare () =
      let* repo = Store.Repo.v (Irmin_mem.config ()) in
      let* store = Store.empty repo in
      Lwt.return (repo, store)
    ;;

    let assert_thrown f =
      Lwt.catch
        (fun () ->
          let* () = f () in
          assert false)
        (fun ex ->
          print_endline (Exn.to_string ex);
          Lwt.return ())
    ;;

    let print_sync_status sync_status =
      Sync.pp_status Format.std_formatter sync_status
    ;;
  end

  let%expect_test "set-and-get" =
    let* _repo, store = prepare () in
    let* () = Store.set_exn ~info store [ "hello" ] "world" in
    let* result = Store.get store [ "hello" ] in
    print_endline result;
    [%expect {| world |}]
  ;;

  let%expect_test "write to the same key twice, then read it" =
    let* _repo, store = prepare () in
    let* () = Store.set_exn ~info store [ "hello" ] "world" in
    let* () = Store.set_exn ~info store [ "hello" ] "there general kenobi" in
    let* result = Store.get store [ "hello" ] in
    print_endline result;
    [%expect {| there general kenobi |}]
  ;;

  let%expect_test "read a key that doesn't exist " =
    let* () =
      assert_thrown (fun () ->
          let* _repo, store = prepare () in
          let+ result = Store.get store [ "hello" ] in
          print_endline result)
    in
    [%expect {| (Invalid_argument "Irmin.Tree.get: /hello not found") |}]
  ;;

  let%expect_test "sync two trees with no contents" =
    let* _repo, store1 = prepare () in
    let* _repo, store2 = prepare () in
    let* sync_status =
      Sync.pull_exn
        store1
        (Irmin.remote_store (module Store) store2)
        (`Merge info)
    in
    print_sync_status sync_status;
    [%expect {| empty |}]
  ;;

  let%expect_test "sync two trees with a conflicting file " =
    let* _repo, store1 = prepare () in
    let* _repo, store2 = prepare () in
    let* () = Store.set_exn ~info store1 [ "hello" ] "world" in
    let* () = Store.set_exn ~info store2 [ "hello" ] "general kenobi" in
    let* () =
      assert_thrown (fun () ->
          let* _ =
            Sync.pull_exn
              store1
              (Irmin.remote_store (module Store) store2)
              (`Merge info)
          in
          Lwt.return ())
    in
    [%expect
      {| 
(Invalid_argument
  "Sync.pull_exn: conflict: Recursive merging of common ancestors: default") |}]
  ;;

  let%expect_test "sync two trees that write the same contents into the same \
                   file "
    =
    let* _repo, store1 = prepare () in
    let* _repo, store2 = prepare () in
    let* () = Store.set_exn ~info store1 [ "hello" ] "world" in
    let* () = Store.set_exn ~info store2 [ "hello" ] "world" in
    let* sync_status =
      Sync.pull_exn
        store1
        (Irmin.remote_store (module Store) store2)
        (`Merge info)
    in
    print_sync_status sync_status;
    [%expect
      {| 1741a72a13dea6cd03d7af106a55ed128b5a26319265ab37fd0f9b6cc58e191b3bc72543cfed14cdd56382031bb5f7d0ac4e5a38d971f5a46f20dd9ef910d0ae |}]
  ;;

  let%expect_test "merge two branches that have no contents" =
    let* _repo, store1 = prepare () in
    let* _repo, store2 = prepare () in
    let* merge_result = Store.merge_into ~info ~into:store1 store2 in
    (match merge_result with
    | Ok () -> print_endline "ok"
    | Error (`Conflict s) -> print_endline s);
    [%expect {| ok |}]
  ;;

  let%expect_test "merge two branches that wrote to the same file" =
    let* _repo, store1 = prepare () in
    let* _repo, store2 = prepare () in
    let* () = Store.set_exn ~info store1 [ "hello" ] "world" in
    let* () = Store.set_exn ~info store2 [ "hello" ] "general kenobi" in
    let* merge_result = Store.merge_into ~info ~into:store1 store2 in
    (match merge_result with
    | Ok () -> print_endline "ok"
    | Error (`Conflict s) -> print_endline s);
    [%expect {| Recursive merging of common ancestors: default |}]
  ;;
end

module Custom_data = struct
  module My_data : Irmin.Contents.S = struct
    type t =
      { kind : string
      ; content : string
      }

    let t =
      let open Irmin.Type in
      record "my-data" (fun kind content -> { kind; content })
      |+ field "kind" string (fun { kind; _ } -> kind)
      |+ field "content" string (fun { content; _ } -> content)
      |> sealr
    ;;

    let merge =
      let open Irmin.Merge in
      Irmin.Merge.option @@
      like
        t
        (pair string string)
        (fun { kind; content } -> kind, content)
        (fun (kind, content) -> { kind; content })
    ;;
  end
end
