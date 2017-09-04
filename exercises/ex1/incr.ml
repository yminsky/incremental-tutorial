open! Core
module Incr = Incremental_kernel.Incremental.Make ()
open! Incr.Let_syntax
   
let f x y z ~what : int Incr.t =
  ignore (x,y,z,what);
  failwith "implement me"
     
let () =
  failwith "implement me"
  
