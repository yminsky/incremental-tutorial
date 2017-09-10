(** Utilities for creating simple command-line tools *)

open! Core
open! Async

(** Adds -host and -port command line arguments, with the host
    defaulting to 127.0.0.1. *)
val host_and_port_param : (string * int) Command.Param.t

(** Loop that requests a stream of events from the tutorial server,
    calling [process_events] to handle those events, and shutting the
    connection down when the deferred returned by [process_events]
    becomes determined. *)
val connect_and_process_events
  :  process_events:(Protocol.Event.t Pipe.Reader.t -> unit Deferred.t)
  -> host:string
  -> port:int
  -> unit Deferred.t
