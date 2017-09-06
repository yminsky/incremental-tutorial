module Incr = Incremental_lib.Incremental.Make ()
module Incr_map = Incr_map.Make (Incr)
include Protocol

let print_s sexp =
  let module Sexp_pp = Sexp_pretty.Pretty_print in
  Sexp_pp.pp_out_channel Sexp_pp.Config.default stdout sexp
