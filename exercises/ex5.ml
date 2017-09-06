(* Similarly to exercise 3, we want to display information about failed checks.

   This time we'd like to display the descriptions of all tests that
   are currently failing.

   The tricky bit however comes from how we want to represent this:

   You could imagine having a [string Check.Name.t Host.Name.t Map.t
   Incr.t] to represent this. I.e. the outer map (keyed by host-name)
   contains a map from check-name to description.

   But for the purpose of this exercise, let's represent this as a
   'flat' map of type [string (Check_name.t * Host.Name.t) Map.t
   Incr.t]
*)

open! Core
open! Async
open! Import

let print_failure_descriptions c =
  print_s [%sexp (c : (Host.Name.t * Check.Name.t, string) Map.Poly.t)]

module Simple = struct

  let failed_checks (state : State.t) () =
    Map.fold ~init:Map.Poly.empty state ~f:(fun ~key:host_info ~data:(_,checks) acc ->
        Map.fold checks ~init:acc ~f:(fun ~key:check_name ~data:(_,outcome) acc ->
            match (outcome : Protocol.Check.Outcome.t option) with
            | None | Some Passed -> acc
            | Some (Failed description) ->
              Map.add acc ~key:(host_info, check_name) ~data:description
          ))
        
  let process_events (events : Event.t Pipe.Reader.t) =
    let viewer = Viewer.create ~print:print_failure_descriptions in
    let state = ref State.empty in
    Pipe.iter' events ~f:(fun eventq ->
      Queue.iter eventq ~f:(fun event ->
            state := State.update !state event);
      let update = Viewer.compute viewer (failed_checks !state) in
      Viewer.update viewer update;
      return ()
    )
end


module Incremental = struct
  open! Incr.Let_syntax

  (* First, let's write a helper function that applies [f]
     incrementally to [inc] but keeps track of the input and output of
     the last time [f] ran. You will need to use a [ref] here. *)
  let diff_map (inc : 'a Incr.t) ~(f : old:('a * 'b) option -> 'a -> 'b) : 'b Incr.t =
    ignore (inc, f);
    failwith "Implement me"

  (* Next, let's write the function to flatten [State.t Incr.t] into a
     map keyed by [Host.Name.t * Check.Name.t]. (Incr_map has a
     [flatten] function built in, but we want to ignore that and write
     this from first principals.)

     The basic idea is to use [diff_map] to find all the keys that
     were added, removed or changed between the old and current input,
     and apply those changes to the old output to get the new output.

     Check out [Map.symmetric diff] for an efficient way of
     calculating diffs between maps.  *)
  let flatten_maps
      (mm : State.t Incr.t)
    : (Host.Name.t * Check.Name.t,Time.t * Check.Outcome.t option,_) Map.t Incr.t
    =
    ignore mm;
    failwith "implement me"


  (* Use [flatten_maps] here to compute the final result. *)
  let failed_checks (s:State.t Incr.t) : (Host.Name.t * Check.Name.t, string) Map.Poly.t Incr.t =
    ignore s;
    failwith "implement me"

  (* The structure of process_events will be fairly similar to the
     corresponding function in exercise 3 *)
  let process_events (events : Event.t Pipe.Reader.t) : unit Deferred.t =
    ignore events;
    failwith "implement me"
      
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
  Command.group ~summary:"Exercise 5"
    [ "simple", simple
    ; "incremental", incremental
    ]
