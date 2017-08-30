open! Core
open! Async
open! Import

let connect_and_view ~host ~port ~view ~print =
  Command_common.connect_and_process_events ~host ~port (fun pipe ->
    let state_v = Incr.Var.create State.empty in
    let pipe_finished = 
      Pipe.iter' pipe  ~f:(fun events ->
        Queue.iter events ~f:(fun event ->
          Incr.Var.set state_v (State.update (Incr.Var.value state_v) event));
        Incr.stabilize ();
        return ()
      )
    in
    let state = Incr.Var.watch state_v in
    let view = view state in
    let obs = Incr.observe view in
    Incr.stabilize ();
    let viewer = Viewer.create ~print ~init:(Incr.Observer.value_exn obs) in
    Incr.Observer.on_update_exn obs ~f:(fun update ->
      match update with
      | Initialized v | Changed (_,v) -> Viewer.update viewer v
      | Invalidated -> ()
    );
    pipe_finished
  )

let print_sexp sexp_of x =
  print_s (sexp_of x)

let incr_command ~summary ~view ~print =
  Command.async' ~summary
    (let open Command.Let_syntax in
     [%map_open
       let (host, port) = Command_common.host_and_port in
       (fun () -> 
          connect_and_view ~host ~port ~view ~print)])
    
let command =
  Command.group ~summary:"Various incremental views"
    [ "failed", 
      incr_command
        ~summary:"Failed view" 
        ~view:Incr_view.failed_checks
        ~print:(print_sexp [%sexp_of: (Host.Name.t * Check.Name.t, string) Map.Poly.t])
    ]
