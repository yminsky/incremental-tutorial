open! Core
open! Import

let () = 
  Command.group
    ~summary:"Command line API"
    [ "server", Server.command
    ; "client", Client.command
    ; "incr-client", Incr_client.command
    ]
  |> Command.run
