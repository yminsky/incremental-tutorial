open! Core
open! Async

val host_and_port_param : (string * int) Command.Param.t

val connect_and_process_events
  :  (Protocol.Event.t Pipe.Reader.t -> unit Deferred.t)
  -> host:string
  -> port:int
  -> unit Deferred.t

val print_s : Sexp.t -> unit
