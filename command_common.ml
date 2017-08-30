open! Core
open! Async

let host_and_port =
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
  
let print_s sexp =
  let module Sexp_pp = Sexp_pretty.Pretty_print in
  Sexp_pp.pp_out_channel Sexp_pp.Config.default stdout sexp
