open! Core
open! Async
open! Import

let command =
  Command.group ~summary:"Various incremental views"
    [ "failed", Failed_checks.command
    ;  "ex1", Ex1.command
    ;  "ex2", Ex2.command
    ]
