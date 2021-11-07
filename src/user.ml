type t =
  { nick : string
  ; password : string
  ; email : string (* TODO: make email optional ? *)
  ; bio : string
  }

let () =
  let open Sqlite3_utils in
  let res =
    Db.with_db (fun db ->
        exec0 db
          "CREATE TABLE IF NOT EXISTS user (nick TEXT, password TEXT, email \
           TEXT, bio TEXT);" )
  in
  match res with
  | Ok () -> ()
  | Error e ->
    Dream.warning (fun log ->
        log "can't create table user: %s" (Sqlite3.Rc.to_string e) )

let login ~nick ~password request =
  let open Sqlite3_utils in
  let good_password =
    Db.with_db (fun db ->
        exec_raw_args db "SELECT password FROM user WHERE nick=?;"
          [| Data.TEXT nick |] ~f:Cursor.to_list )
  in
  match good_password with
  | Ok [ [| Data.TEXT good_password |] ] ->
    if Bcrypt.verify password (Bcrypt.hash_of_string good_password) then
      let _ =
        let%lwt () = Dream.invalidate_session request in
        Dream.put_session "nick" nick request
      in
      Ok ()
    else
      Error "wrong password"
  | Ok _ -> Error "incoherent db answer"
  | Error e -> Error (Format.sprintf "db error: %s" (Rc.to_string e))

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

  let password = Bcrypt.hash password in
  let password = Bcrypt.string_of_hash password in

  if not valid then
    Error "Something is wrong"
  else
    let open Sqlite3_utils in
    let unique =
      Db.with_db (fun db ->
          exec_raw_args db
            "SELECT EXISTS(SELECT 1 FROM user WHERE nick=? OR email=?);"
            [| Data.TEXT nick; Data.TEXT email |]
            ~f:Cursor.to_list )
    in
    match unique with
    | Ok [ [| Data.INT 0L |] ] -> (
      let res =
        Db.with_db (fun db ->
            exec_raw_args db "INSERT INTO user VALUES (?, ?, ?, ?);"
              [| Data.TEXT nick
               ; Data.TEXT password
               ; Data.TEXT email
               ; Data.TEXT ""
              |]
              ~f:Cursor.to_list )
      in
      match res with
      | Ok res -> Ok res
      | Error e -> Error (Format.sprintf "db error: %s" (Rc.to_string e)) )
    | Ok _ -> Error "nick or email already exists"
    | Error e -> Error (Format.sprintf "db error: %s" (Rc.to_string e))

let list () =
  let open Sqlite3_utils in
  let users =
    Db.with_db (fun db ->
        exec_raw_args db "SELECT nick FROM user;" [||] ~f:Cursor.to_list )
  in
  match users with
  | Error e -> Format.sprintf "db error: %s" (Rc.to_string e)
  | Ok users ->
    Format.asprintf "<ul>%a</ul>"
      (Format.pp_print_list (fun fmt -> function
         | [| Data.TEXT s |] ->
           Format.fprintf fmt {|<li><a href="/user/%s">%s</a></li>|} s s
         | _ -> failwith "error" ) )
      users

let public_profile request =
  let nick = Dream.param "user" request in
  let open Sqlite3_utils in
  let user =
    Db.with_db (fun db ->
        exec_raw_args db "SELECT * FROM user WHERE nick=?;" [| Data.TEXT nick |]
          ~f:Cursor.to_list )
  in
  match user with
  | Ok
      [ [| Data.TEXT nick; Data.TEXT password; Data.TEXT email; Data.TEXT bio |]
      ] ->
    Format.sprintf "nick = `%s`; password = `%s`; email = `%s`; bio = '%s'" nick
      password email bio
  | Ok _ -> "incoherent db answer"
  | Error e -> Format.sprintf "db error: %s" (Rc.to_string e)

let profile request =
  match Dream.session "nick" request with
  | None -> "not logged in"
  | Some nick -> Format.sprintf "Hello %s !" nick

let update_bio bio nick =
  let valid = true in
  (* TODO check bio len and FORBIDEN WORDS *)
  if not valid then
    Error "Not biologic"
  else
    let open Sqlite3_utils in
    let res =
      Db.with_db (fun db ->
          exec_raw_args db "UPDATE user SET bio=? WHERE nick=?;"
            [| Data.TEXT bio; Data.TEXT nick |]
            ~f:Cursor.to_list )
    in
    match res with
    | Ok _ -> Ok ()
    | Error e -> Error (Format.sprintf "db error: %s" (Rc.to_string e))

let get_bio nick =
  let open Sqlite3_utils in
  let res =
    Db.with_db (fun db ->
        exec_raw_args db "SELECT bio FROM user WHERE nick=?;"
          [| Data.TEXT nick |] ~f:Cursor.to_list )
  in
  match res with
  | Ok [ [| Data.TEXT bio |] ] -> Ok bio
  | Error e -> Error (Format.sprintf "db error: %s" (Rc.to_string e))
  | Ok _ -> Error "incoherent db result"
