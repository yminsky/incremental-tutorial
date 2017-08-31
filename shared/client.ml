open! Core
open Async
open Import
open Command_common

module Number_of_failed_checks = struct
  type t = int [@@deriving sexp]
         
  let failed_checks (checks : State.checks) =
    Map.filter checks
      ~f:(fun (_,outcome) ->
        match outcome with
        | Some (Failed _description) -> true
        | None | Some Passed -> false
      )
    |> Map.length

  let print t =
    print_s [%sexp (t : t)]

  let create () =
    Viewer.create
      ~print
      ~init:0
    
  let update viewer (state : State.t) =
    let failed_checks : t =
      Map.map state
              ~f:(fun (_host_info, checks) -> failed_checks checks)
      |> Map.fold ~init:0 ~f:(fun ~key:_ ~data acc -> data + acc)
    in
    Viewer.update viewer failed_checks
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
  let state = State.empty in
  let update =
    match which_query with
    | `number_of_failed_checks ->
       let query = Number_of_failed_checks.create () in
       fun state -> Number_of_failed_checks.update query state
    | `failed_checks_summary ->
       let query = Failed_checks.create () in
       fun state -> Failed_checks.update query state
    | `staleness ->
       let query = Staleness.create () in
       fun state -> Staleness.update query state
  in
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
      let (host,port) = Command_common.host_and_port
      and which_query = Command_common.Query.flag
      in
      fun () ->      
       connect_and_process_events (process_events ~which_query) ~host ~port
    ]
