open! Core
open Async

let update_interval = Time.Span.of_sec 0.3
                       
type 'a t = { mutable data : 'a option
            ; mutable total_events : int
            ; mutable last_refresh : Time_ns.t
            ; compute_times : Time.Span.t Queue.t
            }          

let get_and_clear_avg_compute_time t =
  let sum = Queue.sum (module Time.Span) ~f:Fn.id t.compute_times in
  let length = Queue.length t.compute_times in
  let rval = Time.Span.scale sum (1. /. Float.of_int length) in
  Queue.clear t.compute_times;
  rval

let create ~print  =
  let t =
    { data = None
    ; total_events = 0
    ; last_refresh = Time_ns.now ()
    ; compute_times = Queue.create ()
    }
  in
  let started_at = Time_ns.now () in
  Clock.every' update_interval (fun () ->
    match t.data with
    | None -> return ()
    | Some data ->
      let%map (_:int) = Sys.command "clear" in
      let now = Time_ns.now () in
      let diff = Time_ns.diff now started_at |> Time_ns.Span.to_sec in
      let avg_compute_time = get_and_clear_avg_compute_time t in
      Core.printf !"Total events seen by viewer: %d\t\tper sec %.3f\t\tavg time: %s\n\n%!"
        t.total_events
        (Float.of_int t.total_events /. diff)
        (Time.Span.to_string avg_compute_time)
      ;
      printf "------------------------------\n\n%!";
      print data;   
      t.last_refresh <- now;
  );
  t

let update t data =
  t.total_events <- t.total_events + 1;
  t.data <- Some data

let compute t f =
  let start = Time.now () in
  let rval = f () in
  let stop = Time.now () in
  Queue.enqueue t.compute_times (Time.diff stop start);
  rval
  
