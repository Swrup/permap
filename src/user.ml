type t = {
  nick: string;
  password: string;
  email: string option;
}


let login ~nick ~password =
  if nick = nick && password = password then
  Ok ()
  else
    Error "DDD"

let make ~email ~nick ~password =

  let nick_escaped = String.escaped nick in
  (* TODO: remove bad characters (e.g. delthas) *)
  let valid_nick = String.length nick < 64 && String.length nick > 0 && nick_escaped = nick in

  let valid_email = match Emile.of_string email with
  | Ok _ -> true
  | Error _ -> false
  in

  let valid_password = String.length password < 128 && String.length password > 0 in

  let valid = valid_nick && valid_email && valid_password in

  (* TODO: HASH PASSWORD XD *)

  if valid then
    (* TODO: add to db and check uniqueness of id *)
    Ok ()
  else
    Error "Something is wrong."
