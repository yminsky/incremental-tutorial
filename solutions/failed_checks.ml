open! Core
open! Import
open Incr.Let_syntax

let failed_checks (state : State.t Incr.t) =
  Incr_map.mapi' state ~f:(fun ~key:_ ~data ->
    Incr_map.filter_mapi (data >>| snd) ~f:(fun ~key:_ ~data:(_,check) ->
      match check with
      | Some (Failed s) -> Some s
      | Some Passed | None -> None))

let command =
  Command_common.incr_command
    ~summary:"Failed view" 
    ~view:failed_checks
    ~print:(fun x ->
      print_s [%sexp (x : string Check.Name.Map.t Host.Name.Map.t)])
