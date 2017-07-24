open! Core

let host_kinds = [| "www"; "wkstation"; "grid" |]

let rselect ar =
  Array.get ar (Random.int (Array.length ar))

let random_host () =
  let id = Random.int 100 in
  rselect host_kinds ^ Int.to_string id
  
  
