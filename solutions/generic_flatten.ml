open! Core
open! Import
open Incr.Let_syntax

let diff_map i ~f =
  let old = ref None in
  let%map a = i in
  let b = f ~old:!old a in
  old := Some (a, b);
  b

let flatten_maps
      (type key1) (type key2)
      (mm : (key1, (key2,'data, _) Map.t, _) Map.t Incr.t)
      ~(empty : ((key1 * key2), _,_) Map.t)
      ~(data_equal: 'data -> 'data -> bool)
  : (key1 * key2,'data,_) Map.t Incr.t
  =
  diff_map mm ~f:(fun ~old input ->
    match old with
    | None -> 
      Map.fold input ~init:empty  ~f:(fun ~key:key1 ~data acc ->
        Map.fold data ~init:acc ~f:(fun ~key:key2 ~data acc ->
          Map.add acc ~key:(key1, key2) ~data))
    | Some (old_input, old_output) ->
      let changes = 
        Sequence.bind
          (Map.symmetric_diff ~data_equal:phys_equal old_input input)
          ~f:(function
            | (key1, `Left m) -> 
              Sequence.map (Map.to_sequence m)
                ~f:(fun (key2,_) -> `Remove (key1,key2))
            | (key1, `Right m) ->
              Sequence.map (Map.to_sequence m)
                ~f:(fun (key2,data) -> `Add ((key1,key2),data))
            | (key1, `Unequal (m1,m2)) ->
              Map.symmetric_diff ~data_equal m1 m2
              |> Sequence.bind ~f:(function
                | (key2, `Left _)  -> Sequence.singleton (`Remove (key1,key2))
                | (key2, `Right d) -> Sequence.singleton (`Add ((key1,key2),d))
                | (key2, `Unequal (_,d2)) ->
                  Sequence.of_list [`Remove (key1,key2); `Add ((key1,key2),d2)])
          )
      in
      Sequence.fold changes ~init:old_output ~f:(fun acc change ->
        match change with
        | `Add (key,data) -> Map.add acc ~key ~data
        | `Remove key -> Map.remove acc key)
  )

let failed_checks (state : State.t Incr.t) =
  Incr_map.mapi' state ~f:(fun ~key:_ ~data ->
    Incr_map.filter_mapi (data >>| snd) ~f:(fun ~key:_ ~data:(_,check) ->
      match check with
      | Some (Failed s) -> Some s
      | Some Passed | None -> None))
  |> flatten_maps ~empty:Map.Poly.empty ~data_equal:String.equal      
