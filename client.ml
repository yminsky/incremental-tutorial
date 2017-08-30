open! Core
open Async
open Command_common

(** Display an ASCII table with the number of failed checks for each host. *)
module Simple_query = struct         
  let failed_checks (checks : State.checks) =
    Map.filter_map checks
      ~f:(fun (_,outcome) ->
        match outcome with
        | Some (Failed description) -> Some description
        | None | Some Passed -> None
      )
    |> Map.length

  let print t =
    let oc = Pervasives.stdout in
    Textutils.Ascii_table.simple_list_table
      ~oc
      [ "Host"; "Failed checks" ]
      (Map.to_alist t
       |> List.map ~f:(fun (h, n) -> [Protocol.Host.Name.to_string h; string_of_int n])
      );
    Out_channel.flush oc

  let create () =
    Viewer.create
      ~print
      ~init:Protocol.Host.Name.Map.empty
    
  let update t (state : State.t) =
    let failed_checks =
      Map.map state
        ~f:(fun (_host_info, checks) -> failed_checks checks)
    in
    Viewer.update t failed_checks
end


let process_events pipe =
  let state = State.empty in
  let query = Simple_query.create () in
  Pipe.fold ~init:state pipe
    ~f:(fun state event ->
      let state = State.update state event in
      Simple_query.update query state;
      return state)
  |> Deferred.ignore
    

let command =
  let open Command.Let_syntax in
  Command.async'
    ~summary:"start client"
    [%map_open
       let (host,port) = Command_common.host_and_port in
     fun () ->
       connect_and_process_events process_events ~host ~port
    ]
