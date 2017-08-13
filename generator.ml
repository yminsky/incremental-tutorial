open! Core
open Protocol

let rselect rs ar =
  Array.get ar (Random.State.int rs (Array.length ar))

module Check_type = struct
  type t =
    | Temp
    | Free_space
    | Packet_drops
  [@@deriving sexp, enumerate]

  let to_check_name t =
    Check.Name.of_string
      (match t with
       | Temp -> "temp"
       | Free_space -> "free-space"
       | Packet_drops -> "packet-drops")
end

let host_kinds = [| "www"; "ws"; "grid"; "lb" |]

let domain = [| "cs.oxford.uk"; "oxford.uk" |]

let random_host rs =
  let id = Random.State.int rs 100 in
  Host.Name.of_string
    (rselect rs host_kinds ^ Int.to_string id ^ "." ^ rselect rs domain)
  
let random_ip rs =
  let byte () = Random.State.int rs 256 in
  Unix.Inet_addr.of_string
    (sprintf "%d.%d.%d.%d" (byte ()) (byte ()) (byte ()) (byte ()))

(** Note that this won't terminate if n is too big! *)
let random_host_set rs n =
  let rec loop set =
    if Set.length set = n then set
    else loop (Set.add set (random_host rs))
  in
  loop Host.Name.Set.empty

module Host_state = struct
  type t = { hosts : Host.Name.Set.t
           ; active : (Host.Info.t * (Time.t * Check.Outcome.t) Check.Name.Map.t)
                        Host.Name.Map.t
           ; time: Time.t
           }

  let activate t rs host =
    let info : Host.Info.t = 
      { name = host
      ; address = random_ip rs
      ; boot_time = t.time 
      }
    in
    let active =
      Map.add t.active ~key:host ~data:(info,Check.Name.Map.empty)
    in
    ({ t with active }, info)

  let activate_pct t rs pct =
    Set.fold t.hosts ~init:t ~f:(fun t host ->
      if Random.State.float rs 1. < pct then fst (activate t rs host) else t)

  let create rs time ~num_hosts ~pct_active =
    let hosts = random_host_set rs num_hosts in
    activate_pct { hosts; active = Host.Name.Map.empty; time } rs pct_active

  let snapshot t : Event.t Sequence.t =
    let open Sequence.Let_syntax in
    let%bind (host,(info,checks)) = Map.to_sequence t.active in
    let%bind (check,(when_checked,outcome)) = Map.to_sequence checks in
    Sequence.of_list [ Event.Host_info info
                     ; Event.Check (Register { host; check })
                     ; Event.Check (Report {host; check; when_checked; outcome})]

  let chooser rs options =
    let total_weight = List.map ~f:fst options |> List.fold ~init:0. ~f:(+.) in
    let choices = Array.of_list options in
    (fun () ->
       let rec find i x_remaining =
         if i = Array.length choices - 1 then snd choices.(i)
         else 
           let current_prob = fst choices.(i) in
           if x_remaining <= current_prob then snd choices.(i) 
           else find (i+1) (x_remaining -. current_prob)
       in
       find 0 (Random.State.float rs total_weight))
    
  let equiprobable l = List.map ~f:(fun x -> (1.,x)) l

  type update_type = Activate_host | Check_success | Check_fail

  let updater t rs =
    let choose_host = chooser rs (equiprobable (Set.to_list t.hosts)) in
    let choose_check = chooser rs (equiprobable (Check_type.all : Check_type.t list)) in
    let update_type =
      chooser rs
        [ 0.02, Activate_host
        ; 0.90, Check_success
        ; 0.08, Check_fail
        ]
    in
    (fun t ->
       let t =
         { t with 
           time = Time.add t.time
                    (Time.Span.of_ms (Float.of_int (Random.State.int rs 1000))) }
       in
       let change_check outcome =
         let host = choose_host () in
         match Map.find t.active host with
         | None -> (t,[])
         | Some (info,checks) ->
           let check_type = choose_check () in
           let outcome = outcome check_type in
           let check = Check_type.to_check_name check_type in
           let when_checked = t.time in
           let register =
             if Map.mem checks check then []
             else [Event.Check (Register { host; check })]
           in
           let checks = Map.add checks ~key:check ~data:(when_checked,outcome) in
           let active = Map.add t.active ~key:host ~data:(info,checks) in
           let report = [ Event.Check (Report { host; check; when_checked; outcome }) ] in
           ({ t with active}, register @ report)
       in
       match update_type () with
       | Activate_host ->
         let host = choose_host () in
         (match Map.find t.active host with
         | Some _ -> (t,[])
         | None ->
           let (t,info) = activate t rs host in
           let events = [ Event.Host_info info ] in
           (t,events)
         )
       | Check_success -> change_check (fun _ -> Passed)
       | Check_fail -> change_check (fun _ -> Failed "Aaaargh!")
    )


end

  
