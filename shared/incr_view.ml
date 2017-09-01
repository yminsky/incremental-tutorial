open! Core
open! Import
open! Incr.Let_syntax

let is_failure (outcome : Protocol.Check.Outcome.t option)  =
  match outcome with
  | Some (Failed _) -> true
  | None | Some Passed -> false

let num_checks_failed (checks : State.checks Incr.t) : int Incr.t =
  Incr_map.unordered_fold checks
    ~init:0
    ~f:(fun ~key:_ ~data:(_,outcome) count ->
      if is_failure outcome then count + 1 else count)
    ~f_inverse:(fun ~key:_ ~data:(_,outcome) count ->
      if is_failure outcome then count - 1 else count)
