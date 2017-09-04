open! Core
open Async
open Import
open Command_common

module Passed_tests_over_total_tests = struct
  type t = Check.Outcome.t list [@@deriving sexp]

  let print (t:t) =
    let passed_checks =
      List.filter t ~f:(function | Passed -> true | Failed _ -> false)
      |> List.length
    in
    let total_checks = List.length t in      
    printf "%d passes / %d runs" passed_checks total_checks

  let create () =
    Viewer.create
      ~print
      ~init:[]

  (* We don't use state.ml (like the examples below) yet as our query
     is agnostic to hosts. *)
  let process_events pipe =
    let viewer = create () in
    Pipe.fold ~init:[]
      pipe
      ~f:(fun acc event ->
        match event with
        | Event.Host_info _
        | Check (Register _) | Check (Unregister _) -> return acc
        | Check (Report { outcome; _ }) ->
           let acc = outcome :: acc in
           Viewer.update viewer acc;
           return acc)
    |> Deferred.ignore    
end

module Failed_checks = struct
  type t = (Host.Name.t * Check.Name.t, string) Map.Poly.t [@@deriving sexp]

  let print t =
    print_s [%sexp (t : t)] 

  let create () =
    Viewer.create
      ~print
      ~init:Map.Poly.empty
   
  let update viewer (state : State.t) =
    let failed_checks =
      Map.map state ~f:(fun (_host_info, checks) ->
        Map.filter_map checks ~f:(fun (_,outcome) ->
         match outcome with
         | Some (Failed description) -> Some description
         | None | Some Passed -> None
        )
     )
    in
    let t : t =
      Map.fold ~init:Map.Poly.empty failed_checks
       ~f:(fun ~key:host_info ~data:failed_checks acc ->
         Map.fold failed_checks ~init:acc
          ~f:(fun  ~key:check_name ~data:desc acc ->           
            Map.add acc ~key:(host_info, check_name) ~data:desc)
      )
    in
    Viewer.update viewer t
   
end
                     
module Staleness = struct
  let staleness = Time.Span.of_sec 3.
                     
  type t = (Check.Name.t * Host.Name.t, Time.t) Map.Poly.t

  let refresh (t:t) =
    Map.map t ~f:(fun time_last_checked ->
     let diff = Time.diff (Time.now ()) time_last_checked in
     if Time.Span.(>) diff staleness    
     then `Stale_by diff
     else `Not_stale)

  let print t =
    let r = refresh t in
    print_s
      [%sexp (r : (Check.Name.t*Host.Name.t, [ `Stale_by of Time.Span.t | `Not_stale ])
                 Map.Poly.t)]

  let create () =
    let init = Map.Poly.empty in
    Viewer.create ~print ~init

  let update viewer (state : State.t) =
    let t : t =
      Map.fold ~init:Map.Poly.empty state ~f:(fun ~key:host_name ~data:(_hi, checks) acc ->
        Map.fold ~init:acc checks ~f:(fun ~key:check_name ~data:(time, _outcome) acc ->
           Map.add acc ~key:(check_name, host_name) ~data:time
       ))
    in
    Viewer.update viewer t
end

                     
let process_events pipe ~(which_query : Command_common.Query.t) =
  match which_query with
  | `passed_checks ->
     Passed_tests_over_total_tests.process_events pipe
  | `failed_checks_summary | `staleness as query ->
     let update =
       match query with
       | `failed_checks_summary ->
          let query = Failed_checks.create () in
          fun state -> Failed_checks.update query state
       | `staleness ->
          let query = Staleness.create () in
          fun state -> Staleness.update query state
     in
     let state = State.empty in
     Pipe.fold ~init:state pipe
          ~f:(fun state event ->
            let state = State.update state event in
            update state;
            return state)
     |> Deferred.ignore
     
  
let command =
  let open Command.Let_syntax in
  Command.async'
    ~summary:"start client"
    [%map_open
      let (host,port) = Command_common.host_and_port_param
      and which_query = Command_common.Query.flag
      in
      fun () ->      
       connect_and_process_events (process_events ~which_query) ~host ~port
    ]
