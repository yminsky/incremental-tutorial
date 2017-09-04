open! Core
open! Import
open! Incr.Let_syntax

let command =
  Command.basic'
    ~summary:"Exercise 1"
    (let open Command.Let_syntax in
     [%map_open
      let x = flag "x" (required int) ~doc:"INT value for x" in
      fun () -> main x
     ])
