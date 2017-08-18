open! Core
open! Async

module Config = struct
  type t = { num_hosts: int
           ; pct_initially_active: int
           }
end


let serve config rs ~port =
  Tcp.Server.create
    ~on_handler_error:`Ignore
    (Tcp.on_port port)
    (fun _ _r _w -> 
       return ())
