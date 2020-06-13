open! Core_kernel
module IO_run = Lwt

module IO_flush = struct
  include Lwt

  let bind a ~f = Lwt.bind a f
  let to_run t = t
end

let flush () = Lwt.return ()
let run f = Lwt_main.run (f ())
let flushed () = true
let upon_unreleasable_issue = `CR
