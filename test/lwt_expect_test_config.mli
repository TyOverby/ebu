open! Core_kernel

include
  Expect_test_config.S
    with type 'a IO_flush.t = 'a Lwt.t
     and type 'a IO_run.t = 'a Lwt.t
