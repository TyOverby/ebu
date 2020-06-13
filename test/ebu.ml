open! Core_kernel

let%expect_test "hi" =
  print_endline "hi";
  [%expect {| hi |}]
;;
