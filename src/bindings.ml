(* let bindings for early return when encountering an error *)
(* see https://ocaml.org/releases/4.13/htmlman/bindingops.html *)
let ( let^? ) o f =
  match o with
  | Error e -> Error (Format.sprintf "db error: %s" (Caqti_error.show e))
  | Ok None -> Error "db error"
  | Ok (Some x) -> f x

let ( let^ ) o f =
  match o with
  | Error e -> Error (Format.sprintf "db error: %s" (Caqti_error.show e))
  | Ok x -> f x

let ( let* ) o f =
  match o with
  | Error e -> Error (Format.sprintf "%s" e)
  | Ok x -> f x
