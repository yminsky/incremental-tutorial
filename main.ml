open! Core
open Async
   
module Sexp_pp = Sexp_pretty.Pretty_print

module Test = struct
  let stream () =
    Generator.sequence
      (Random.State.make_self_init ())
      (Time.now ())
      ~num_hosts:10
      ~pct_initially_active:0.20
             
  let decoded n =
    Sequence.fold
      (Sequence.take (stream ()) n)
      ~init:State.empty
      ~f:State.update
    
  let dump_state n =
    Sexp_pp.pp_out_channel Sexp_pp.Config.default stdout
      [%sexp (decoded n: State.t)]

  let failed_checks n =
    Map.filter_map (decoded n) ~f:(fun (_,checks) ->
      let failures = 
        Map.filter_map checks ~f:(fun (_,outcome) ->
         match outcome with
         | Some (Failed description) -> Some description
         | None | Some Passed -> None
      )
      in
      if Map.is_empty failures then None else Some failures)

  let command =
    let module Table = Textutils.Ascii_table in
    let open Command.Let_syntax in
    Command.basic'
      ~summary:"dump state to file"
      [%map_open
       let n =
         flag "how-many-events" (required int) ~doc:"INT how many events"
       in
       fun () ->
         dump_state n
    ]
  
end
              
let () =
  Command.group
    ~summary:"main"
    [ "test", Test.command
    ; "server", Server.command
    ; "client", Client.command
    ]
  |> Command.run
  
