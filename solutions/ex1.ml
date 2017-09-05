open! Core
module Incr = Incremental_kernel.Incremental.Make ()
open Incr.Let_syntax
   
let f x y z ~what : int Incr.t =
  let%bind x = x in
  let%bind y = y in
  let%bind what = what in
  match what with
  | `Add -> return (x + y)
  | `Multiply ->
     let%map z = z in
     x * y * z
            
let main x =
  let x = Incr.Var.create x in
  let y = Incr.Var.create 120 in
  let z = Incr.Var.create 250 in
  let what = Incr.Var.create `Add in
  let w = Incr.Var.watch in
  let result =
    f (w x) (w y) (w z) ~what:(w what)
  in
  let obs = Incr.observe result in
  Incr.Observer.on_update_exn obs
    ~f:(function
        | Initialized v | Changed (_,v) -> printf "%d\n" v
        | Invalidated ->
           (* CR sfunk: Need a comment to explain what's going on here. *)
           assert false);
  Incr.stabilize ();
  Incr.Var.set x 150;
  Incr.Var.set y 90;
  Incr.stabilize ();
  Incr.Var.set z 100;
  Incr.stabilize ();
  (* Note that this prints nothing as z isn't yet used! *)
  
  Incr.Var.set what `Multiply;
  Incr.stabilize ()

let command =
  Command.basic'
    ~summary:"Exercise 1"
    (let open Command.Let_syntax in
     [%map_open
      let x = flag "x" (required int) ~doc:"INT value for x" in
      fun () -> main x
     ])
  
