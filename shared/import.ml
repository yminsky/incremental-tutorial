module Incr = Incremental_lib.Incremental.Make ()
module Incr_map = Incr_map.Make (Incr)
include Protocol
include Command_common
