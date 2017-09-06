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
  let id = Random.State.int rs 1000 in
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

module State = struct
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

  let create rs time ~num_hosts ~pct_initially_active =
    let hosts = random_host_set rs num_hosts in
    activate_pct
      { hosts; active = Host.Name.Map.empty; time } rs
      pct_initially_active

  let snapshot t : Event.t Sequence.t =
    let open Sequence.Let_syntax in
    let%bind (host,(info,checks)) = Map.to_sequence t.active in
    let%bind (check,(when_checked,outcome)) = Map.to_sequence checks in
    Sequence.of_list 
      [ Event.create t.time (Host_info info)
      ; Event.create t.time (Check (Register { host; check }))
      ; Event.create t.time (Check (Report {host; check; when_checked; outcome}))
      ]

  let chooser rs options =
    let options = List.map options ~f:(fun (w,v) -> (Int.to_float w,v)) in
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
    
  let equiprobable l = List.map ~f:(fun x -> (1,x)) l

  type update_type = Activate_host | Check_success | Check_fail

  let next_event t ~time_scale rs =
    let choose_host = chooser rs (equiprobable (Set.to_list t.hosts)) in
    let choose_check = chooser rs (equiprobable (Check_type.all : Check_type.t list)) in
    let update_type =
      chooser rs
        [ 20 , Activate_host
        ; 90 , Check_success
        ; 8  , Check_fail
        ]
    in
    (fun t ->
       let time_delta =
         Random.State.float rs (Time.Span.to_sec time_scale)
         |> Time.Span.of_sec
       in
       let t =
         { t with 
           time = Time.add t.time time_delta
         }
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
             else [Event.create t.time (Check (Register { host; check }))]
           in
           let checks = Map.add checks ~key:check ~data:(when_checked,outcome) in
           let active = Map.add t.active ~key:host ~data:(info,checks) in
           let report = [ Event.create t.time
                            (Check (Report { host; check; when_checked; outcome }))]
           in
           ({ t with active}, register @ report)
       in
       match update_type () with
       | Activate_host ->
         let host = choose_host () in
         (match Map.find t.active host with
         | Some _ -> (t,[])
         | None ->
           let (t,info) = activate t rs host in
           let events = [ Event.create t.time (Host_info info) ] in
           (t,events)
         )
       | Check_success -> change_check (fun _ -> Passed)
       | Check_fail -> change_check (fun _ -> Failed "Aaaargh!")
    )

end

let sequence rs time ~num_hosts ~pct_initially_active ~time_scale =
  let state = State.create rs time ~num_hosts ~pct_initially_active in
  let next_event = State.next_event state ~time_scale rs in
  Sequence.append
    (Sequence.map (State.snapshot state) ~f:(fun ev -> (time,ev)))
    (Sequence.join
       (Sequence.unfold ~init:state ~f:(fun s ->
          let (s',evs) = next_event s in
          let evs = List.map evs ~f:(fun ev -> (s'.time,ev)) in
          Some (Sequence.of_list evs, s'))))
  
  
let stream rs time ~num_hosts ~pct_initially_active ~time_scale =
  let sequence = ref (sequence rs time ~num_hosts ~pct_initially_active ~time_scale) in
  stage (fun () ->
    match Sequence.next !sequence with
    | None -> assert false
    | Some (x,sequence') ->
      sequence := sequence';
      x)
