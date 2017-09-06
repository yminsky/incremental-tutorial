(* In this exercise, we'll compute the set of stale checks for each
   host. Specifically, a check is considered stale if it hasn't been
   updated for the last X seconds, for a configured threshold X.

   The all-at-once implementation below should give you a sense of
   what the semantics should be, but implementing this efficiently and
   incrementally is non-trivial.  In particular, if you just use the
   current time in a naive way, then you'll have to do work linear in
   the number of hosts every time you refresh the computation.

   To do this efficiently, you'll want to use Incremental's support
   for time. You'll want to make use of [Incr.advance_clock] and
   [Incr.at].
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
    ignore s; ignore thresh;
    failwith "Implement me!"

  let process_events (events : Event.t Pipe.Reader.t) ~(thresh:Time.Span.t) : unit Deferred.t =
    ignore events; ignore thresh;
    assert false
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
