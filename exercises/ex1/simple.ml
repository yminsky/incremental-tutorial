open! Core

let f x y z ~what =
  let res =
    match what with
    | `Multiply -> x * y * z
    | `Add -> x + y
  in
  printf "%d\n" res

let () =
  let x = 50 in
  let y = 120 in
  let z = 250 in
  let what = `Add in
  f x y z ~what;
  (* Prints 170 *)
  let x = 150 in
  let y = 90 in  
  f x y z ~what;
  (* Prints 240 *)
  let what = `Multiply in
  f x y z ~what
  (* Prints 337500 *)
