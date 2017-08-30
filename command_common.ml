open! Core
open! Async

let host_and_port =
  let open Command.Let_syntax in
  [%map_open
    let port =
      flag "port" (required int) ~doc:"INT port to connect to server"
    and host =
      flag "host" (optional_with_default "127.0.0.1" string)
        ~doc:" host name (default:localhost)"
    in
    (host, port)]


let connect_and_process_events process_events ~host ~port =
  Log.Global.info "Starting client";
  Tcp.connect (Tcp.to_host_and_port host port)
  >>= fun (_socket, reader, writer) ->
  Log.Global.info "Connected to %s:%d" host port;
  Rpc.Connection.with_close reader writer
    ~connection_state:(fun _ -> ())
    ~on_handshake_error:`Raise
    ~dispatch_queries:(fun conn ->
      let%bind (pipe, _metadata) = Rpc.Pipe_rpc.dispatch_exn Protocol.events conn () in
      let%bind () = process_events pipe in
      Log.Global.error "Event pipe closed. Exiting";
      Deferred.unit     
    )
  
let print_s sexp =
  let module Sexp_pp = Sexp_pretty.Pretty_print in
  Sexp_pp.pp_out_channel Sexp_pp.Config.default stdout sexp

                     
module Query = struct
  type t =
    [ `number_of_failed_checks
    | `failed_checks_summary
    | `staleness ]
    [@@deriving sexp, enumerate]

  let of_string s =
    String.lowercase s
    |> String.substr_replace_all ~pattern:"-" ~with_:"_"
    |> Sexp.of_string
    |> t_of_sexp

  let to_string = function
    | `number_of_failed_checks -> "number-of-failed-checks"
    | `failed_checks_summary -> "failed-checks-summary"
    | `staleness -> "staleness"
                              
  let arg_type =
    Command.Arg_type.create of_string

  let doc =
    sprintf
      " query to run: ( %s ) "
      (List.map all ~f:to_string |> String.concat ~sep:" | ")
            
  let flag =
    let open Command.Let_syntax in
    [%map_open
      let flag = flag "which-query" (required arg_type) ~doc
      in
      flag]

end
