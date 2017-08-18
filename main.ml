open! Core
module Sexp_pp = Sexp_pretty.Pretty_print


let stream = 
  Generator.sequence
    (Random.State.make_self_init ())
    (Time.now ())
    ~num_hosts:100
    ~pct_initially_active:0.20

let decoded =
  Sequence.fold
    (Sequence.take stream 100000)
    ~init:State.empty
    ~f:State.update

let () =
  Sexp_pp.pp_out_channel Sexp_pp.Config.default stdout
    [%sexp (decoded : State.t)]
       

    
