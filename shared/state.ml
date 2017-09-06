open Core
open Protocol

type checks = (Time.t * Check.Outcome.t option) Check.Name.Map.t
[@@deriving sexp]

type t = { time: Time.t
         ; hosts: (Host.Info.t * checks) Host.Name.Map.t
         }
[@@deriving sexp, fields]

let empty = { time = Time.epoch
            ; hosts = Host.Name.Map.empty
            }

let update (t:t) (ev:Event.t) : t =
  let change_host host ~f : t =
    let hosts = 
      Map.change t.hosts host ~f:(function
        | None -> None
        | Some (hi, checks) -> Some (hi, f hi checks))
    in
    { time = ev.time; hosts }
  in
  match ev.ev with
  | Host_info hi ->
    let hosts = 
      Map.change t.hosts hi.name ~f:(function
        | None -> Some (hi,Check.Name.Map.empty)
        | Some (_,checks) -> Some (hi,checks))
    in
    { time = ev.time; hosts }
  | Check (Register {host;check}) ->
    change_host host ~f:(fun hi checks -> 
      Map.add checks ~key:check ~data:(hi.boot_time,None))
  | Check (Unregister {host; check}) ->
    change_host host ~f:(fun _hi checks -> 
      Map.remove checks check)
  | Check (Report {host; check; when_checked; outcome }) ->
    change_host host ~f:(fun _hi checks ->
      Map.add checks ~key:check ~data:(when_checked, Some outcome))







