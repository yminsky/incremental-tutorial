open! Core
module Sexp_pp = Sexp_pretty.Pretty_print


let stream = 
  Generator.sequence
    (Random.State.make_self_init ())
    (Time.now ())
    ~num_hosts:1000
    ~pct_initially_active:0.20

let decoded =
  Sequence.fold
    (Sequence.take stream 100000)
    ~init:State.empty
    ~f:State.update

let dump_state () =
  Sexp_pp.pp_out_channel Sexp_pp.Config.default stdout
    [%sexp (decoded : State.t)]

let failed_checks =
  Map.filter_map decoded ~f:(fun (_,checks) ->
    let failures = 
      Map.filter_map checks ~f:(fun (_,outcome) ->
        match outcome with
        | Some (Failed description) -> Some description
        | None | Some Passed -> None
      )
    in
    if Map.is_empty failures then None else Some failures)

let () =
  let module Table = Textutils.Ascii_table in
  let hostname =
    Table.Column.create "hostname" (fun (hi,_) -> hi.Host.Info.name)
  in
  assert false
