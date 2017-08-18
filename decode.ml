open Core
open Protocol

module State = struct
  type checks = (Time.t * Check.Outcome.t option) Check.Name.Map.t
  [@@deriving sexp]
                  
  type t = (Host.Info.t * checks) Host.Name.Map.t
  [@@deriving sexp]

  let empty = Host.Name.Map.empty

  let update (t:t) (ev:Event.t) : t =
    let change_host host ~f : t =
      Map.change t host ~f:(function
        | None -> None
        | Some (hi, checks) -> Some (hi, f hi checks))
    in
    match ev with
    | Host_info hi ->
      Map.change t hi.name ~f:(function
        | None -> Some (hi,Check.Name.Map.empty)
        | Some (_,checks) -> Some (hi,checks))
    | Check (Register {host;check}) ->
      change_host host ~f:(fun hi checks -> 
        Map.add checks ~key:check ~data:(hi.boot_time,None))
    | Check (Unregister {host; check}) ->
      change_host host ~f:(fun _hi checks -> 
        Map.remove checks check)
    | Check (Report {host; check; when_checked; outcome }) ->
      change_host host ~f:(fun _hi checks ->
        Map.add checks ~key:check ~data:(when_checked, Some outcome))

end







