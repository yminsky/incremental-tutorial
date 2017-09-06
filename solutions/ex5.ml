(* Compute the nth stalest checks *)

open! Core
open! Async
open! Import

(* Note that this is sorted first by time, then by host name *)
module Time_and_host = struct
  include Tuple.Make       (Time) (Host.Name)
  include Tuple.Comparable (Time) (Host.Name)
  include Tuple.Sexpable   (Time) (Host.Name)
end

type result = Check.Name.t Time_and_host.Map.t
[@@deriving sexp]

let print_result x =
  print_s [%sexp (x : result)]

(** Returns the single stalest check from the map of checks *)
let stalest_check (checks : State.checks) = 
  match Map.to_alist checks with
  | [] -> None
  | (check,(time,_)) :: rest ->
    let (time,check) =
      List.fold rest ~init:(time,check)
        ~f:(fun ((oldest_time,_) as acc) (check,(time,_)) ->
          if time < oldest_time then (time,check) else acc)
    in
    Some (time,check)

module Simple = struct

  let hosts_by_staleness (s:State.t) : result =
    List.filter_map (Map.to_alist s.hosts) ~f:(fun (host,(_,checks)) ->
      match stalest_check checks with
      | None -> None
      | Some (time,check) ->
        Some ((time,host),check))
    |> List.fold ~init:Time_and_host.Map.empty
         ~f:(fun acc (key,data) -> Map.add acc ~key ~data)
         
  let stalest (s:State.t) ~max_count : result =
    let result = hosts_by_staleness s in
    if Map.length result <= max_count then result
    else
      Map.to_sequence result
      |> (fun s -> Sequence.take s max_count)
      |> Sequence.fold ~init:Time_and_host.Map.empty ~f:(fun acc (key,data) ->
        Map.add acc ~key ~data)

  let process_events (events : Event.t Pipe.Reader.t) ~(max_count:int) =
    let viewer = Viewer.create ~print:print_result in
    let state = ref State.empty in
    Pipe.iter' events ~f:(fun eventq ->
      state := Queue.fold eventq ~init:!state ~f:State.update;
      let stalest = Viewer.compute viewer (fun () -> stalest !state ~max_count) in
      Viewer.update viewer stalest;
      return ()
    )
end

module Incremental = struct

  let hosts_by_staleness (s:State.t Incr.t ) : result Incr.t =
    let open Incr.Let_syntax in
    Incr_map.unordered_fold (s >>| State.hosts) ~init:Time_and_host.Map.empty
      ~f:(fun ~key:host ~data:(_,checks) acc -> 
        match stalest_check checks with
        | None -> acc
        | Some (time,check) ->
          Map.add acc ~key:(time,host) ~data:check)
      ~f_inverse:(fun ~key:host ~data:(_,checks) acc ->
        match stalest_check checks with
        | None -> acc
        | Some (time,_) ->
          Map.remove acc (time,host))
      
  let stalest (s:State.t Incr.t) ~(max_count:int) : result Incr.t =
    let open Incr.Let_syntax in
    let%map result = hosts_by_staleness s in
    if Map.length result <= max_count then result
    else
      Map.to_sequence result
      |> (fun s -> Sequence.take s max_count)
      |> Sequence.fold ~init:Time_and_host.Map.empty ~f:(fun acc (key,data) ->
        Map.add acc ~key ~data)

  let process_events (events : Event.t Pipe.Reader.t) ~(max_count:int) =
    let module Var = Incr.Var in
    let viewer = Viewer.create ~print:print_result in
    let state = Incr.Var.create State.empty in
    let result = Incr.observe (stalest ~max_count (Incr.Var.watch state)) in
    Incr.Observer.on_update_exn result ~f:(function
      | Initialized x | Changed (_,x) -> Viewer.update viewer x
      | Invalidated -> assert false
    );
    Pipe.iter' events ~f:(fun eventq ->
      Incr.Var.set state
        (Queue.fold eventq ~init:(Var.value state) ~f:State.update);
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
         and max_count = flag "-max-count" (optional_with_default 10 int)
                        ~doc:"The number of hosts to show"
         in
         fun () -> 
           Command_common.connect_and_process_events 
             (fun pipe -> process_events pipe ~max_count) ~host ~port
       ])
  in
  Command.group ~summary:"Exercise 5"
    [ "simple"      , cmd "all-at-once implementation" Simple.process_events
    ; "incremental" , cmd "incremental implementation" Incremental.process_events
    ]
