open! Core
open! Async
open! Import

let command =
  Command.group ~summary:"Various incremental views"
    [ "failed", Failed_checks.command
    ]
