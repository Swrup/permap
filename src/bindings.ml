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

let ( let* ) o f = Result.fold ~ok:f ~error:Result.error o

let unwrap_list f ids =
  let l = List.map f ids in
  let res = List.find_opt Result.is_error l in
  match res with
  | None -> Ok (List.map Result.get_ok l)
  | Some (Ok _) -> assert false
  | Some (Error _e as error) -> error
