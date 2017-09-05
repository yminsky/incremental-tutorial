open! Core
open! Async
open! Import

let host_and_port_param =
  let open Command.Let_syntax in
  [%map_open
    let port =
      flag "port" (required int) ~doc:"INT port to connect to server"
    and host =
      flag "host" (optional_with_default "127.0.0.1" string)
        ~doc:" host name (default:localhost)"
    in
    (host, port)]

let connect_and_process_events process_events ~host ~port =
  Log.Global.info "Starting client";
  Tcp.connect (Tcp.to_host_and_port host port)
  >>= fun (_socket, reader, writer) ->
  Log.Global.info "Connected to %s:%d" host port;
  Rpc.Connection.with_close reader writer
    ~connection_state:(fun _ -> ())
    ~on_handshake_error:`Raise
    ~dispatch_queries:(fun conn ->
      let%bind (pipe, _metadata) = Rpc.Pipe_rpc.dispatch_exn Protocol.events conn () in
      let%bind () = process_events pipe in
      Log.Global.error "Event pipe closed. Exiting";
      Deferred.unit     
    )
  
let connect_and_view ~host ~port ~view ~print =
  connect_and_process_events ~host ~port (fun pipe ->
    let state_v = Incr.Var.create State.empty in
    let counter = ref 0 in
    let pipe_finished = 
      Pipe.iter' pipe  ~f:(fun events ->
        Queue.iter events ~f:(fun event ->
          Incr.Var.set state_v (State.update (Incr.Var.value state_v) event);
          incr counter;
          Incr.stabilize ());
        return ()
      )
    in
    let state = Incr.Var.watch state_v in
    let view = view state in
    let obs = Incr.observe view in
    Incr.stabilize ();
    let viewer = Viewer.create ~print in
    Incr.Observer.on_update_exn obs ~f:(fun update ->
      match update with
      | Initialized v | Changed (_,v) -> Viewer.update viewer v
      | Invalidated -> ()
    );
    pipe_finished
    >>= fun () ->
    Log.Global.info "Total events processed: %d\n" !counter;
    Deferred.unit
  )

let incr_command ~summary ~view ~print =
  Command.async' ~summary
    (let open Command.Let_syntax in
     [%map_open
       let (host, port) = host_and_port_param in
       (fun () -> 
          connect_and_view ~host ~port ~view ~print)])
                     
let print_s sexp =
  let module Sexp_pp = Sexp_pretty.Pretty_print in
  Sexp_pp.pp_out_channel Sexp_pp.Config.default stdout sexp

module Query = struct
  type t =
    [ `passed_checks
    | `failed_checks_summary
    | `staleness ]
    [@@deriving sexp, enumerate]

  let of_string s =
    String.lowercase s
    |> String.substr_replace_all ~pattern:"-" ~with_:"_"
    |> Sexp.of_string
    |> t_of_sexp

  let to_string = function
    | `passed_checks -> "passed-checks"
    | `failed_checks_summary -> "failed-checks-summary"
    | `staleness -> "staleness"
                              
  let arg_type =
    Command.Arg_type.create of_string

  let doc =
    sprintf
      " query to run: ( %s ) "
      (List.map all ~f:to_string |> String.concat ~sep:" | ")
            
  let flag =
    let open Command.Let_syntax in
    [%map_open
      let flag = flag "which-query" (required arg_type) ~doc
      in
      flag]

end
