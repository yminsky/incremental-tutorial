open! Core
open Protocol

(** Create a deterministic, infinite sequence of events *)
val sequence
  :  Random.State.t
  -> Time.t
  -> num_hosts:int
  -> pct_initially_active:float
  -> (Time.t * Event.t) Sequence.t
