open! Core
open Protocol

(** Create a deterministic, infinite sequence of events *)
val stream
  :  Random.State.t
  -> Time.t
  -> num_hosts:int
  -> pct_initially_active:float
  -> time_scale:Time.Span.t
  -> (unit -> Time.t * Event.t) Staged.t
