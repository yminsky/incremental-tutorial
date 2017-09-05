(*
   Now it's time look at a more real-world example!

   For the rest of this tutorial we are going to work on an
   application that monitors the state and the health of a collection
   of machines, e.g. all boxes in a datacenter.

   To understand the setup, take a look at *shared/protocol.ml* which
   contain the protocol that the client receives from the server.

   You do not need to look at [shared/server.ml] or
   [shared/generator.ml] in any detail.

   As a first simple query, we would like to keep track of the
   following information:

   total number of checks that passed / total number of checks run

   (This is across all hosts.)

   Of course, if you actually had to do this, you would just keep
   track of two integers, rather than using anything in
   Incremental. But for the purpose of this exercise, let's do this by
   building up state that keeps track of all checks and their
   outcome. Every time we update the state we also iterate over it to
   find the updated answer.

   For a non-incremental implementation, take a look at
   shared/client.ml:Passed_tests_over_total_tests. You can run this
   with:

   {v
    ./_build/exercises/main.exe server -port 8080 &
    ./_build/exercises/main.exe ex2 simple -port 8080
   v}

   The goal of this exercise is to write a version of this query that
   uses Incremental, which you can run as follows:

   {v
    ./_build/exercises/main.exe ex2 simple -port 8080
   v}

*)

open! Core
open! Async
open! Import

module Simple = struct
  type t = Check.Outcome.t list [@@deriving sexp]

  let print (t:t) =
    let passed_checks =
      List.filter t ~f:(function Passed -> true | Failed _ -> false)
      |> List.length
    in
    let total_checks = List.length t in      
    printf "%d passes / %d runs" passed_checks total_checks

  (* We don't use state.ml (like the examples below) yet as our query
     is agnostic to hosts. *)
  let process_events (pipe : Event.t Pipe.Reader.t) =
    let viewer = Viewer.create ~print ~init:[] in
    Pipe.fold ~init:[] pipe ~f:(fun acc event ->
      match event with
      | Host_info _ | Check (Register _) | Check (Unregister _) -> return acc
      | Check (Report { outcome; _ }) ->
        let acc = outcome :: acc in
        Viewer.update viewer acc;
        return acc)
    |> Deferred.ignore    
end

module Incremental = struct

  let process_events pipe =
    let open Incr.Let_syntax in
    let print (p,t) = printf "Passed %d / Total %d\n" p t in
    let viewer = Viewer.create ~print ~init:(0,0) in
    let passed_checks = Incr.Var.create 0 in
    let total_checks = Incr.Var.create 0 in
    let result =
      let%map passed = Incr.Var.watch passed_checks
      and total = Incr.Var.watch total_checks
      in
      (passed,total)
    in
    let obs = Incr.observe result in
    Incr.Observer.on_update_exn obs ~f:(function
      | Initialized v | Changed (_,v) ->
        Viewer.update viewer v
      | Invalidated -> assert false
    );
    Pipe.iter pipe ~f:(fun event ->
      match event with
      | Event.Host_info _
      | Check (Register _) | Check (Unregister _) -> Deferred.unit
      | Check (Report { outcome; _ }) ->
        Incr.Var.set total_checks (Incr.Var.value total_checks + 1);
        begin match outcome with
        | Passed -> Incr.Var.set passed_checks (Incr.Var.value passed_checks + 1)
        | Failed _ -> ()
        end;
        Incr.stabilize ();
        Deferred.unit
    )
    |> Deferred.ignore    
end

let build_command ~summary process_events =
  Command.async' ~summary
    (let open Command.Let_syntax in
     [%map_open
       let (host, port) = Command_common.host_and_port_param in
       fun () -> 
         Command_common.connect_and_process_events 
           process_events ~host ~port
     ])

let simple = 
  build_command ~summary:"Simple, all-at-once implementation"
    Simple.process_events

let incremental =
  build_command ~summary:"Incremental implementation"
    Incremental.process_events

let command =
  Command.group ~summary:"Exercise 2"
    [ "simple", simple
    ; "incremental", incremental
    ]
