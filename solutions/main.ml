open! Core
open! Import

let () = 
  Command.group
    ~summary:"Command line API"
    [ "server", Server.command
    ; "client", Client.command
    ; "ex1", Ex1.command
    ; "ex2", Ex2.command
    ; "ex3", Ex3.command
(*
    ; "ex4", Ex4.command
    ; "ex5", Ex5.command
*)
    ]
  |> Command.run
