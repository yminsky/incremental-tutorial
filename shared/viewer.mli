(** A simple way of viewing results in a terminal. *)
open! Core

type 'display_state t
   
val create : print:('display_state -> unit) -> 'display_state t

(** To register a new value to be printed *)
val update : 'display_state t -> 'display_state -> unit

(** Used to register the computation phase *)
val compute : _ t -> (unit -> 'a) -> 'a
