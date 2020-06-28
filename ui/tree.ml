open! Core_kernel
module Id = Unique_id.Int63 ()

type leaf =
  { kind : string
  ; content : string
  }
[@@deriving sexp, equal]

type branch =
  { hd : Id.t
  ; tl : Id.t list
  }
[@@deriving sexp, equal]

type t =
  | Leaf of leaf
  | Branch of branch
[@@deriving sexp, equal]

module Store = struct
  type nonrec t = t Id.Map.t [@@deriving sexp, equal]
end

let leaf ~kind ~content = Leaf { kind; content }
let branch ~hd ~tl = Branch { hd; tl }

module For_testing = struct
  let demo =
    let a_id = Id.create () in
    let a_branch_id = Id.create () in
    let b_id = Id.create () in
    let c_id = Id.create () in
    let c_branch_id = Id.create () in
    let d_id = Id.create () in
    let e_branch_id = Id.create () in
    let e_id = Id.create () in
    let string_leaf s = Leaf { kind = "text/plain"; content = s } in
    let many =
      List.init 1000 ~f:(fun i ->
          let id = Id.create () in
          id, string_leaf (sprintf "%d" i))
    in
    let map =
      Id.Map.of_alist_exn
        (List.concat
           [ [ a_id, string_leaf "aaaaa"
             ; b_id, string_leaf "bbcbbbb"
             ; c_id, string_leaf "ccc"
             ; d_id, string_leaf "dddddddddd"
             ; e_id, string_leaf "ee"
             ; ( a_branch_id
               , branch ~hd:a_id ~tl:[ b_id; c_branch_id; e_branch_id ] )
             ; c_branch_id, branch ~hd:c_id ~tl:[ d_id ]
             ; e_branch_id, branch ~hd:e_id ~tl:(List.unzip many |> Tuple2.get1)
             ]
           ; many
           ])
    in
    map, a_branch_id
  ;;
end
