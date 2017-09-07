(* Let's start with something very simple. Take a look at the
   functions in [Simple]. Your goal is to write incremental verisons
   of these same functions.

   Note that we don't expect a practical performance improvement here.
   the goal is just to get a sense of how to use the Incremental
   primitives.
*)

open! Core
open! Import
open! Incr.Let_syntax

type what_to_show = Volume | Footprint

module Simple = struct

  let metric what ~w ~h ~d =
    match what with
    | Volume -> w * h * d
    | Footprint -> w * d
  ;;

  let run () =
    let height = ref 50 in
    let width  = ref 120 in
    let depth  = ref 250 in
    let what = ref Footprint in
    (* This is an all-at-once computation *)
    let compute () =
      printf "%d\n" (metric !what ~w:!width ~h:!height ~d:!depth)
    in
    compute ();
    height := 150;
    width := 90;
    compute ();
    what := Volume;
    compute ();
  ;;

end

module Incremental = struct

  let metric (what:what_to_show Incr.t) ~(w:int Incr.t) ~(h: int Incr.t) ~(d:int Incr.t)
    : int Incr.t 
    =
    match%bind what with
    | Volume -> 
      let%map w = w and h = h and d = d in
      w * h * d
    | Footprint ->
      let%map w = w and d = d in
      w * d
  ;;    

  (* The structure of [run] should follow that of [simple_run] above
     closely, except:

     - OCaml references should be replaced with [Incr.Var.t]'s
     - [f] should be called just once
     - An observer should be created based on the result of [f]
     - [Incr.stabilize] needs to be called as part of [compute]
     - [compute] should then get its value using [Incr.Observer.value_exn].
  *)
  let run () : unit =
    let (!) = Incr.Var.watch in
    let (:=) = Incr.Var.set in
    let height = Incr.Var.create 50 in
    let width  = Incr.Var.create 120 in
    let depth  = Incr.Var.create 250 in
    let what = Incr.Var.create Footprint in
    (* This is an all-at-once computation *)
    let result = 
      metric !what ~w:!width ~h:!height ~d:!depth |> Incr.observe
    in
    let compute () =
      Incr.stabilize ();
      printf "%d\n" (Incr.Observer.value_exn result)
    in
    compute ();
    height := 150;
    width := 90;
    compute ();
    what := Volume;
    compute ();
  ;;

end

(* From here on is the declaration of the command-line interface,
   which you can mostly ignore for the purposes of the tutorial. *)
let command =
  let cmd ~summary run = Command.basic' ~summary (Command.Param.return run) in
  Command.group ~summary:"Exercise 1"
    [ "simple"      , cmd ~summary:"all-at-once implementation" Simple.run
    ; "incremental" , cmd ~summary:"incremental implementation" Incremental.run
    ]
