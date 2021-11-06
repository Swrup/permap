type t =
  { nick : string
  ; password : string
  ; email : string (* TODO: make email optional ? *)
  }

let () =
  let open Sqlite3_utils in
  let res =
    Db.with_db (fun db ->
        exec0 db
          "CREATE TABLE IF NOT EXISTS user (nick TEXT, password TEXT, email \
           TEXT);" )
  in
  match res with
  | Ok () -> ()
  | Error e ->
    Dream.warning (fun log ->
        log "can't create table user: %s" (Sqlite3.Rc.to_string e) )

let login ~nick ~password =
  if nick = nick && password = password then
    Ok ()
  else
    Error "DDD"

let register ~email ~nick ~password =
  (* TODO: remove bad characters (e.g. delthas) *)
  let valid_nick =
    String.length nick < 64
    && String.length nick > 0
    && String.escaped nick = nick
  in

  let valid_email =
    match Emile.of_string email with
    | Ok _ -> true
    | Error _ -> false
  in

  let valid_password =
    String.length password < 128 && String.length password > 0
  in

  let valid = valid_nick && valid_email && valid_password in

  (* TODO: HASH PASSWORD XD *)
  if not valid then
    Error "Something is wrong"
  else
    (* TODO: add check uniqueness of id *)
    let open Sqlite3_utils in
    let res =
      Db.with_db (fun db ->
          exec_raw_args db "INSERT INTO user VALUES (?, ?, ?);"
            [| Data.TEXT nick; Data.TEXT password; Data.TEXT email |]
            ~f:Cursor.to_list )
    in
    match res with
    | Ok res -> Ok res
    | Error e -> Error (Format.sprintf "db error: %s" (Rc.to_string e))
