(* Let's start with something very simple. Take a look at [simple_f]
   and [simple_run] below. Your goal is to write incremental verisons
   of these functions, called [f] and [run]. It should have the same
   behavior, but uses incremental to express the computation.

   Note that we don't expect a practical performance improvement here.
   the goal is just to get a sense of how to use the Incremental
   primitives.
*)

open! Core
open! Import
open! Incr.Let_syntax

type what_operation = Multiply | Add

module Simple = struct

  let f what x y z =
    match what with
    | Multiply -> x * y * z
    | Add -> x + y
  ;;

  let run () =
    let x = ref 50 in
    let y = ref 120 in
    let z = ref 250 in
    let what = ref Add in
    (* This is an all-at-once computation *)
    let compute () =
      printf "%d\n" (f !what !x !y !z)
    in
    compute ();
    x := 150;
    y := 90;
    compute ();
    what := Multiply;
    compute ();
  ;;

end

module Incremental = struct

  (* These are the functions you need to implement incrementally. *)


  (* [f] is supposed to take in incrementals and return
     incrementals. Here, we want to use bind on the [what] argument to
     choose which of the two computations to do.*)
  let f (what:what_operation Incr.t) (x:int Incr.t) (y: int Incr.t) (z:int Incr.t)
    : int Incr.t 
    =
    ignore (x,y,z,what);
    failwith "implement me"
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
    failwith "implement me"
  ;;

end


(* From here on is the declaration of the command-line interface,
   which you can mostly ignore for the purposes of the tutorial. *)

let make_command ~summary run =
  Command.basic' ~summary (Command.Param.return run)

let simple_command =
  make_command ~summary:"all-at-once implementation" Simple.run

let incremental_command =
  make_command ~summary:"incremental implementation" Incremental.run

let command =
  Command.group ~summary:"Exercise 1"
    [ "simple"      , simple_command
    ; "incremental" , incremental_command
    ]
