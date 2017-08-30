open! Core
open! Async
open! Import
   
let () =
  Command.group
    ~summary:"main"
    [ "server", Server.command
    ; "client", Client.command
    ; "incr-client", Incr_client.command
    ]
  |> Command.run
  
