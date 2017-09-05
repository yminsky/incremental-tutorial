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

let print_passed_ratio passed_ratio = 
  printf "passed_ratio: %.2F\n" passed_ratio

module Simple = struct

  let passed_ratio ~total ~passed =
    passed // total

  let process_events (pipe : Event.t Pipe.Reader.t) =
    let total  = ref 0 in
    let passed = ref 0 in
    let viewer = Viewer.create ~print:print_passed_ratio ~init:Float.nan in
    Pipe.iter pipe ~f:(fun event ->
      match event with
      | Host_info _ | Check (Register _) | Check (Unregister _) -> return ()
      | Check (Report { outcome; _ }) ->
        begin match outcome with
        | Passed -> incr passed; incr total
        | Failed _ -> incr passed; incr total
        end;
        let result = passed_ratio ~total:(!total) ~passed:(!passed) in
        Viewer.update viewer result;
        return ()
    )
end

module Incremental = struct

  let passed_ratio ~total ~passed =
    let open Incr.Let_syntax in
    let%map passed = passed and total = total in
    passed // total
  ;;

  let process_events (pipe : Event.t Pipe.Reader.t) =
    let total = Incr.Var.create 0 in
    let passed = Incr.Var.create 0 in
    let viewer = Viewer.create ~print:print_passed_ratio ~init:Float.nan in
    let result = 
      let (!) = Incr.Var.watch in
      passed_ratio ~total:!total ~passed:!passed
      |> Incr.observe 
    in
    Incr.Observer.on_update_exn result ~f:(function
      | Initialized x | Changed (_,x) -> Viewer.update viewer x
      | Invalidated -> assert false
    );
    Pipe.iter pipe ~f:(fun event ->
      match event with
      | Host_info _ | Check (Register _) | Check (Unregister _) -> return ()
      | Check (Report { outcome; _ }) ->
        let incr i = Incr.Var.set i (1 + Incr.Var.value i) in
        begin match outcome with
        | Passed -> incr passed; incr total
        | Failed _ -> incr passed; incr total
        end;
        return ()
    )
  ;;
end

 (* From here on in is just command-line specification. *)
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
