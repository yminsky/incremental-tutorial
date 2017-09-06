(* Now we want to look at more interesting queries. For this exercise,
   we'll display a per-host count of the number of checks that are
   currently passed or failed, including only failed nodes.
*)

open! Core
open! Async
open! Import

let print_failure_counts c =
  print_s [%sexp (c : int Host.Name.Map.t)]

module Simple = struct

  let count_failures (s:State.t) =
    Map.filter_map s.hosts ~f:(fun (_,checks) ->
      let count = 
        Map.count checks ~f:(fun (_,check_opt) ->
          match check_opt with
          | None | Some Passed -> false
          | Some (Failed _) -> true)
      in
      if count = 0 then None else Some count
    )

  let process_events (events : Event.t Pipe.Reader.t) =
    let viewer = Viewer.create ~print:print_failure_counts in
    let state = ref State.empty in
    Pipe.iter' events ~f:(fun eventq ->
      Queue.iter eventq ~f:(fun event ->
        state := State.update !state event);
      let failures = Viewer.compute viewer (fun () -> count_failures !state) in
      Viewer.update viewer failures;
      return ()
    )
end


module Incremental = struct

  (* In this version, we'll need to replace [Map.filter_map] with
     [Incr.filter_mapi]. (It's [filter_mapi] instead of [filter_map] only
     because [Incr_map] happens not to have a [filter_map] function.  *)

  let count_failures (s:State.t Incr.t) : int Host.Name.Map.t Incr.t =
    ignore s;
    failwith "implement me!"

  (* The structure of process_events will be fairly similar to the
     corresponding function in exercise 2 *)

  let process_events (events : Event.t Pipe.Reader.t) : unit Deferred.t =
    ignore events;
    failwith "implement me!"

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
  Command.group ~summary:"Exercise 3"
    [ "simple", simple
    ; "incremental", incremental
    ]
