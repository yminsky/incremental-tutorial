open! Core
open Async

let go ~port ~host =
  Log.Global.info "Starting client";
  Tcp.connect (Tcp.to_host_and_port host port)
  >>= fun (_socket, reader, writer) ->
  Log.Global.info "Connected to %s:%d" host port;
  Rpc.Connection.with_close reader writer
    ~connection_state:(fun _ -> ())
    ~on_handshake_error:`Raise
    ~dispatch_queries:(fun conn ->
      let%bind (pipe, _metadata) = Rpc.Pipe_rpc.dispatch_exn Protocol.events conn () in
      Pipe.iter pipe
        ~f:(fun event ->
          Log.Global.info !"%{sexp:Protocol.Event.t}\n" event;
          Deferred.unit)
    )

let command =
  let open Command.Let_syntax in
  Command.async'
    ~summary:"start client"
    [%map_open
     let port =
       flag "port" (required int) ~doc:"INT port to connect to server"
     and host =
       flag "host" (optional_with_default "127.0.0.1" string)
            ~doc:" host name (default:localhost)"
     in         
     fun () ->
       go ~host ~port
    ]  
