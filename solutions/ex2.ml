open! Core
open Async
module Incr = Incremental_kernel.Incremental.Make ()
open Incr.Let_syntax
open Tutorial_shared
open Tutorial_shared.Protocol
   
let process_events pipe =
  let print (p,t) = printf "Passed %d / Total %d\n" p t in
  let viewer = Viewer.create ~print ~init:(0,0) in
  let passed_checks = Incr.Var.create 0 in
  let total_checks = Incr.Var.create 0 in
  let result =
    let%map passed = Incr.Var.watch passed_checks
    and total = Incr.Var.watch total_checks
    in
    (passed,total)
  in
  let obs = Incr.observe result in
  Incr.Observer.on_update_exn obs
    ~f:(function
        | Initialized v | Changed (_,v) ->
           Viewer.update viewer v
        | Invalidated -> assert false);
  Pipe.iter
      pipe
      ~f:(fun event ->
        match event with
        | Event.Host_info _
        | Check (Register _) | Check (Unregister _) -> Deferred.unit
        | Check (Report { outcome; _ }) ->
           Incr.Var.set total_checks (Incr.Var.value total_checks + 1);
           begin match outcome with
           | Passed -> Incr.Var.set passed_checks (Incr.Var.value passed_checks + 1)
           | Failed _ -> ()
           end;
           Incr.stabilize ();
           Deferred.unit
      )
    |> Deferred.ignore    

let command =
  Command.async' ~summary:"Exercise 2"
    (let open Command.Let_syntax in
     [%map_open
       let (host, port) = Command_common.host_and_port_param in
       fun () -> Command_common.connect_and_process_events process_events ~host ~port
     ])
