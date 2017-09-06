open! Core
open! Protocol

type checks = (Time.t * Check.Outcome.t option) Check.Name.Map.t
[@@deriving sexp]

type t = { time: Time.t
         ; hosts: (Host.Info.t * checks) Host.Name.Map.t
         }
[@@deriving sexp, fields]

val empty : t
val update : t -> Event.t -> t
