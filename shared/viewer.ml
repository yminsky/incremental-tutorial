open! Core
open Async

let update_interval = Time.Span.of_sec 1.0
                       
type 'a t = { mutable data : 'a option
            ; mutable total_events : int
            ; mutable last_refresh : Time_ns.t
            }          
let create ~print  =
  let t =
    { data = None
    ; total_events = 0
    ; last_refresh = Time_ns.now () }
  in
  let started_at = Time_ns.now () in
  Clock.every' update_interval (fun () ->
    match t.data with
    | None -> return ()
    | Some data ->
      let%map (_:int) = Sys.command "clear" in
      let now = Time_ns.now () in
      let diff = Time_ns.diff now started_at |> Time_ns.Span.to_sec in
      Core.printf !"Total events seen by viewer: %d\t\tper sec %.3f\n\n%!"
        t.total_events
        (Float.of_int t.total_events /. diff);
      Core.printf "------------------------------\n\n%!";
      print data;   
      t.last_refresh <- now;
  );
  t

let update t data =
  t.total_events <- t.total_events + 1;
  t.data <- Some data  
