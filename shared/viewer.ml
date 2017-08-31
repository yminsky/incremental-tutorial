open! Core
open Async

let update_interval = Time.Span.of_sec 0.2
                       
type 'a t = { mutable data : 'a }

let create ~print ~init  =
  let t = { data = init } in
  Clock.every' update_interval (fun () ->
    let%map (_:int) = Sys.command "clear" in
    print t.data
  );
  t

let update t data = t.data <- data  
