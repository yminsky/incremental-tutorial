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

let simple_f x y z ~what =
  let res =
    match what with
    | `Multiply -> x * y * z
    | `Add -> x + y
  in
  printf "%d\n" res

let simple_run () =
  let x = 50 in
  let y = 120 in
  let z = 250 in
  let what = `Add in
  simple_f x y z ~what; (* Prints 170 *)
  let x = 150 in
  let y = 90 in
  simple_f x y z ~what; (* Prints 240 *)
  let what = `Multiply in
  simple_f x y z ~what; (* Prints 337500 *)
;;

(* These are the functions you need to implement *)
let f x y z ~what : int Incr.t =
  ignore (x,y,z,what);
  failwith "implement me"

let run () =
  failwith "implement me"


(* From here on is the declaration of the command-line interface,
   which you can mostly ignore for the purposes of the tutorial. *)
let simple_command =
  Command.basic' ~summary:"all-at-once implementation"
    (Command.Param.return (fun () -> simple_run ()))

let incr_command =
  Command.basic' ~summary:"incremental implementation"
    (Command.Param.return (fun () -> run ()))

let command =
  Command.group ~summary:"Exercise 1"
    [ "simple"      , simple_command
    ; "incremental" , incr_command
    ]
