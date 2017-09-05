(* Now we want to look at more interesting queries. For this exercise,
   we'll display a per-host count of the number of checks that are
   currently passed or failed. 
*)

open! Core
open! Async
open! Import

let print_failure_counts c =
  print_s [%sexp (c : int Host.Name.Map.t)]

module Simple = struct

  let count_failures (s:State.t) =
    Map.map s ~f:(fun (_,checks) ->
      Map.count checks ~f:(fun (_,check_opt) ->
        match check_opt with
        | None | Some Passed -> false
        | Some (Failed _) -> true))

  let process_events (events : Event.t Pipe.Reader.t) =
    let viewer = Viewer.create ~print:print_failure_counts in
    let state = ref State.empty in
    Pipe.iter' events ~f:(fun eventq ->
      Queue.iter eventq ~f:(fun event ->
        state := State.update !state event);
      Viewer.update viewer (count_failures !state);
      return ()
    )
end


module Incremental = struct

  let count_failures (s:State.t Incr.t) =
    Incr_map.mapi s ~f:(fun ~key:_ ~data:(_,checks) ->
      Map.count checks ~f:(fun (_,check_opt) ->
        match check_opt with
        | None | Some Passed -> false
        | Some (Failed _) -> true))

  let process_events (events : Event.t Pipe.Reader.t) =
    let viewer = Viewer.create ~print:print_failure_counts in
    let state = Incr.Var.create State.empty in
    let result = Incr.observe (count_failures (Incr.Var.watch state)) in
    Incr.Observer.on_update_exn result ~f:(fun update ->
      match update with
      | Initialized x | Changed (_,x) -> Viewer.update viewer x
      | Invalidated -> assert false
    );
    Pipe.iter' events ~f:(fun eventq ->
      Incr.Var.set state
        (Queue.fold eventq ~init:(Incr.Var.value state) ~f:State.update);
      Incr.stabilize ();
      Deferred.return ()
    )

end



(* Command line setup *)

let build_command ~summary process_events =
  Command.async' ~summary
    (let open Command.Let_syntax in
     [%map_open
       let (host, port) = Command_common.host_and_port_param in
       fun () -> 
         Command_common.connect_and_process_events 
           process_events ~host ~port
     ])

let simple = 
  build_command ~summary:"Simple, all-at-once implementation"
    Simple.process_events

let incremental =
  build_command ~summary:"Incremental implementation"
    Incremental.process_events

let command =
  Command.group ~summary:"Exercise 2"
    [ "simple", simple
    ; "incremental", incremental
    ]
