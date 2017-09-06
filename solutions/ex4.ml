(* In this exercise, we'll compute the set of stale checks for each
   host. Specifically, a check is considered stale if it hasn't been
   updated for the last X seconds, for a configured threshold X.

   To do this, we have to keep track of the last time we've seen an
   update for each (check,host) and whether that time is stale. We
   want to update this whenever we get a new [Event.t] from the server
   and register a time in the future to wake up to consider if the
   check is now stale.
*)

open! Core
open! Async
open! Import

type result = Time.t Check.Name.Map.t Host.Name.Map.t
[@@deriving sexp]

let print_result x =
  print_s [%sexp (x : result)]

module Simple = struct
  let stale_checks (s:State.t) ~(thresh:Time.Span.t) : result =
    Map.filter_map s.hosts ~f:(fun (_,check) ->
      let map = 
        Map.filter_map check ~f:(fun (when_registered,_) ->
          if Time.Span.(<) (Time.diff s.time when_registered) thresh
          then None
          else Some when_registered
        )
      in
      if Map.is_empty map then None else Some map)

  let process_events (events : Event.t Pipe.Reader.t) ~(thresh:Time.Span.t) =
    let viewer = Viewer.create ~print:print_result in
    let state = ref State.empty in
    Pipe.iter' events ~f:(fun eventq ->
      state := Queue.fold eventq ~init:!state ~f:State.update;
      let stale_checks = Viewer.compute viewer (fun () -> stale_checks ~thresh !state) in
      Viewer.update viewer stale_checks;
      return ()
    )
end

module Incremental = struct

  let stale_checks (s:State.t Incr.t) ~(thresh:Time.Span.t) : result Incr.t =
    let open Incr.Let_syntax in
    Incr_map.filter_mapi' (s >>| State.hosts) ~f:(fun ~key:_ ~data ->
      let map = 
        Incr_map.filter_mapi' (data >>| snd) ~f:(fun ~key:_ ~data ->
          let%bind (time,_) = data in
          match%map Incr.at (Time.add time thresh) with
          | Before -> None
          | After -> Some time
        )
      in
      let%map map = map in
      if Map.is_empty map then None else Some map
    )

  let process_events (events : Event.t Pipe.Reader.t) ~(thresh:Time.Span.t) =
    let module Var = Incr.Var in
    let viewer = Viewer.create ~print:print_result in
    let state = Incr.Var.create State.empty in
    let result = Incr.observe (stale_checks ~thresh (Incr.Var.watch state)) in
    Incr.Observer.on_update_exn result ~f:(function
      | Initialized x | Changed (_,x) -> Viewer.update viewer x
      | Invalidated -> assert false
    );
    Pipe.iter' events ~f:(fun eventq ->
      Incr.Var.set state
        (Queue.fold eventq ~init:(Var.value state) ~f:State.update);
      Incr.advance_clock ~to_:(Incr.Var.value state).time;
      Viewer.compute viewer Incr.stabilize;
      return ()
    )
end


let command =
  let cmd summary process_events =
    Command.async' ~summary
      (let open Command.Let_syntax in
       [%map_open
         let (host, port) = Command_common.host_and_port_param 
         and thresh = flag "-thresh" (optional_with_default (Time.Span.of_sec 1.) time_span)
                        ~doc:"Threshold for determing when a host is stale"
         in
         fun () -> 
           Command_common.connect_and_process_events 
             (fun pipe -> process_events pipe ~thresh) ~host ~port
       ])
  in
  Command.group ~summary:"Exercise 4"
    [ "simple"      , cmd "all-at-once implementation" Simple.process_events
    ; "incremental" , cmd "incremental implementation" Incremental.process_events
    ]
