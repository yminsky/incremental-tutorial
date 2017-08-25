open! Core
open! Async

module Config = struct
  type t = { num_hosts: int
           ; pct_initially_active: int
           }
end


let events = 
  Rpc.Pipe_rpc.create ()
    ~name:"events"
    ~version:0 (* Unversioned *)
    ~bin_query:[%bin_type_class: unit]
    ~bin_response:[%bin_type_class: Protocol.Event.t]
    ~bin_error:[%bin_type_class: unit]


let events_impl =
  Rpc.Pipe_rpc.implement


let implementations =
  Rpc.Implementations.create_exn
    ~implementations:[]
    ~on_unknown_rpc:`Raise


let serve stream ~port =
  Tcp.Server.create
    ~on_handler_error:`Ignore
    (Tcp.on_port port)
    (fun _ r w -> 
       Rpc.Connection.server_with_close r w
         ~connection_state:(fun _ -> stream)
         ~on_handshake_error:`Raise
         ~implementations
    )
