open! Core
open! Async
open! Import

let events_impl ~(make_stream:unit -> (unit -> (Time.t * Event.t)) Staged.t) =
  Rpc.Pipe_rpc.implement Protocol.events (fun addr () ->
    Log.Global.info !"Client connected on %{sexp:Socket.Address.Inet.t}" addr;
    let next = unstage (make_stream ()) in
    let (r,w) = Pipe.create () in
    let rec write () =
      let (time,event) = next () in
      if Pipe.is_closed w then (
        Log.Global.info !"Client disconnected %{sexp:Socket.Address.Inet.t}" addr;
        Deferred.unit
      ) else (
        let%bind () = at time in
        let%bind () = Pipe.write w event in
        write ()
      )
    in
    don't_wait_for (write ());
    return (Ok r))                                                      

let implementations ~make_stream =
  Rpc.Implementations.create_exn
    ~implementations:[events_impl ~make_stream]
    ~on_unknown_rpc:`Raise

let serve ~make_stream ~port =
  let%bind _tcp_server =
    Tcp.Server.create
     ~on_handler_error:`Ignore
     (Tcp.on_port port)
     (fun addr r w ->
       Rpc.Connection.server_with_close r w
         ~connection_state:(fun _ -> addr)
         ~on_handshake_error:`Raise
         ~implementations:(implementations ~make_stream)
     )
  in
  Log.Global.info "Server started";
  Deferred.unit
  
let go ~port ~num_hosts ~time_scale =
  let make_stream =
    let time = Time.now () in
    let rs = Random.State.make_self_init () in
    (fun () ->
      Generator.stream
        (Random.State.copy rs)
        time
        ~num_hosts
        ~pct_initially_active:0.20
        ~time_scale
    )
  in
  let%bind () = serve ~make_stream ~port in
  Deferred.never ()

let command =
  let open Command.Let_syntax in
  Command.async'
    ~summary:"start server"
    [%map_open
      let port = 
        flag "port" (required int) ~doc:"PORT port to listen for clients" 
      and num_hosts =
        flag "hosts" (optional_with_default 100 int)
          ~doc:"N number of hosts to simulate"
      and time_scale =
        flag "time-scale" (optional_with_default (Time.Span.of_sec 0.01) time_span)
          ~doc:"maximum time to the next event"
      in
     fun () ->
       go ~port ~num_hosts ~time_scale
    ]
