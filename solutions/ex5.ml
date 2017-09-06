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
  let diff_map i ~f =
    let old = ref None in
    let%map a = i in
    let b = f ~old:!old a in
    old := Some (a, b);
    b
    
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
    diff_map mm ~f:(fun ~old input ->
        match old with
        | None -> 
          Map.fold input ~init:Map.Poly.empty  ~f:(fun ~key:key1 ~data:(_,data) acc ->
              Map.fold data ~init:acc ~f:(fun ~key:key2 ~data acc ->
                  Map.add acc ~key:(key1, key2) ~data))
        | Some (old_input, old_output) ->
          let changes = 
            Sequence.bind
              (Map.symmetric_diff ~data_equal:phys_equal old_input input)
              ~f:(function
                  | (key1, `Left (_, m)) -> 
                    Sequence.map (Map.to_sequence m)
                      ~f:(fun (key2,_) -> `Remove (key1,key2))
                  | (key1, `Right (_,m)) ->
                    Sequence.map (Map.to_sequence m)
                      ~f:(fun (key2,data) -> `Add ((key1,key2),data))
                  | (key1, `Unequal ((_,m1),(_,m2))) ->
                    Map.symmetric_diff
                      ~data_equal:(fun (t1,_) (t2,_) -> Time.equal t1 t2)
                      m1 m2
                    |> Sequence.bind ~f:(function
                        | (key2, `Left _)  -> Sequence.singleton (`Remove (key1,key2))
                        | (key2, `Right d) -> Sequence.singleton (`Add ((key1,key2),d))
                        | (key2, `Unequal (_,d2)) ->
                          Sequence.of_list [`Remove (key1,key2); `Add ((key1,key2),d2)])
                )
          in
          Sequence.fold changes ~init:old_output ~f:(fun acc change ->
              match change with
              | `Add (key,data) -> Map.add acc ~key ~data
              | `Remove key -> Map.remove acc key)
      )

  (* Use [flatten_maps] here to compute the final result. *)
  let failed_checks (s:State.t Incr.t) : (Host.Name.t * Check.Name.t, string) Map.Poly.t Incr.t =
    Incr_map.filter_mapi (flatten_maps s) ~f:(fun ~key:_ ~data:(_,check_opt) ->
        match check_opt with
        | None | Some Passed -> None
        | Some (Failed desc) -> Some desc
      )

  (* The structure of process_events will be fairly similar to the
     corresponding function in exercise 3 *)

  let process_events (events : Event.t Pipe.Reader.t) : unit Deferred.t =
    let viewer = Viewer.create ~print:print_failure_descriptions in
    let state = Incr.Var.create State.empty in
    let result = Incr.observe (failed_checks (Incr.Var.watch state)) in
    Incr.Observer.on_update_exn result ~f:(fun update ->
      match update with
      | Initialized x | Changed (_,x) -> Viewer.update viewer x
      | Invalidated -> assert false
    );
    Pipe.iter' events ~f:(fun eventq ->
      Incr.Var.set state (Queue.fold eventq ~init:(Incr.Var.value state) ~f:State.update);
      Viewer.compute viewer Incr.stabilize;
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
  Command.group ~summary:"Exercise 5"
    [ "simple", simple
    ; "incremental", incremental
    ]
