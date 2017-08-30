open! Core
open! Async
open! Import

module Failed_checks = struct
  let print x =
    print_s [%sexp (x : (Host.Name.t * Check.Name.t, string) Map.Poly.t)]

  let observe state =
    let failed_checks = Incr_view.failed_checks state in
    let obs = Incr.observe failed_checks in
    Incr.Observer.on_update_exn obs ~f:(fun update ->
      let (_:int) = Core.Sys.command "clear" in
      match update with
      | Initialized v | Changed (_,v) -> print v
      | Invalidated -> printf "------- STATE INVALIDATED -----------\n%!"
    )
end
  
let run ~host ~port which_query =
  Command_common.connect_and_process_events ~host ~port (fun pipe ->
    let state_v = Incr.Var.create State.empty in
    let pipe_finished = 
      Pipe.iter' pipe  ~f:(fun events ->
        Queue.iter events ~f:(fun event ->
          Incr.Var.set state_v (State.update (Incr.Var.value state_v) event));
        Incr.stabilize ();
        return ()
      )
    in
    let state = Incr.Var.watch state_v in
    begin match which_query with
    | `number_of_failed_checks
    | `staleness -> failwith "Not implemented"
    | `failed_checks_summary -> Failed_checks.observe state
    end;
    pipe_finished
  )

let command =
  let open Command.Let_syntax in
  Command.async'
    ~summary:"Start client"
    [%map_open
      let (host, port) = Command_common.host_and_port
      and which_query = Command_common.Query.flag
      in
      (fun () ->
        run ~host ~port which_query)]
