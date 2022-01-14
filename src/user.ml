open Db

type t =
  { nick : string
  ; password : string
  ; email : string
  ; bio : string
  ; avatar : string
  }

module Q = struct
  let create_user_table =
    Caqti_request.exec Caqti_type.unit
      "CREATE TABLE IF NOT EXISTS user (nick TEXT, password TEXT, email TEXT, \
       bio TEXT, avatar BLOB, PRIMARY KEY(nick));"

  let create_plant_user_table =
    Caqti_request.exec Caqti_type.unit
      "CREATE TABLE IF NOT EXISTS plant_user (plant_id TEXT, nick TEXT, \
       PRIMARY KEY(plant_id), FOREIGN KEY(nick) REFERENCES user(nick));"

  let get_password =
    Caqti_request.find_opt Caqti_type.string Caqti_type.string
      "SELECT password FROM user WHERE nick=?;"

  let is_already_user =
    Caqti_request.find_opt
      Caqti_type.(tup2 string string)
      Caqti_type.int
      "SELECT EXISTS(SELECT 1 FROM user WHERE nick=? OR email=?);"

  let inser_new_user =
    Caqti_request.exec
      Caqti_type.(tup4 string string string Caqti_type.(tup2 string string))
      "INSERT INTO user VALUES (?, ?, ?, ?, ?);"

  let list_nicks =
    Caqti_request.collect Caqti_type.unit Caqti_type.string
      "SELECT nick FROM user;"

  let get_user =
    Caqti_request.find_opt Caqti_type.string
      Caqti_type.(tup4 string string string Caqti_type.(tup2 string string))
      "SELECT * FROM user WHERE nick=?;"

  let update_bio =
    Caqti_request.exec
      Caqti_type.(tup2 string string)
      "UPDATE user SET bio=? WHERE nick=?;"

  let get_bio =
    Caqti_request.find_opt Caqti_type.string Caqti_type.string
      "SELECT bio FROM user WHERE nick=?;"

  let get_avatar =
    Caqti_request.find_opt Caqti_type.string Caqti_type.string
      "SELECT avatar FROM user WHERE nick=?;"

  let upload_avatar =
    Caqti_request.exec
      Caqti_type.(tup2 string string)
      "UPDATE user SET avatar=? WHERE nick=?;"
end

let () =
  let tables = [ Q.create_user_table ] in
  if
    List.exists Result.is_error
      (List.map (fun query -> Db.exec query ()) tables)
  then
    Dream.warning (fun log -> log "can't create table")

let login ~nick ~password request =
  let good_password = Db.find_opt Q.get_password nick in
  match good_password with
  | Ok foo -> (
    match foo with
    | Some good_password ->
      if Bcrypt.verify password (Bcrypt.hash_of_string good_password) then
        let _ =
          let%lwt () = Dream.invalidate_session request in
          Dream.put_session "nick" nick request
        in
        Ok ()
      else
        Error "wrong password"
    | None -> Error (Format.sprintf "user `%s` doesn't exist" nick) )
  | Error e -> Error (Format.sprintf "db error: %s" (Caqti_error.show e))

let register ~email ~nick ~password =
  (* TODO: remove bad characters (e.g. delthas) *)
  let valid_nick =
    String.length nick < 64
    && String.length nick > 0
    && Dream.html_escape nick = nick
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
    let unique = Db.find_opt Q.is_already_user (nick, email) in
    match unique with
    | Ok unique -> (
      match unique with
      | Some nb -> (
        match nb with
        | 0 -> (
          let res =
            Db.exec Q.inser_new_user (nick, password, email, ("", ""))
          in
          match res with
          | Ok res -> Ok res
          | Error e ->
            Error (Format.sprintf "db error: %s" (Caqti_error.show e)) )
        | _ -> Error "nick or email already exists" )
      | None -> Error "db error" )
    | Error e -> Error (Format.sprintf "db error: %s" (Caqti_error.show e))

let list () =
  let users = Db.fold Q.list_nicks (fun nick acc -> nick :: acc) () [] in
  match users with
  | Ok users ->
    Format.asprintf "<ul>%a</ul>"
      (Format.pp_print_list (fun fmt -> function
         | s -> Format.fprintf fmt {|<li><a href="/user/%s">%s</a></li>|} s s )
      )
      users
  | Error e -> Format.sprintf "db error: %s" (Caqti_error.show e)

let public_profile request =
  let nick = Dream.param "user" request in
  let user = Db.find_opt Q.get_user nick in
  match user with
  | Ok user -> (
    match user with
    | Some (nick, password, email, (bio, _)) ->
      let user_info =
        Format.sprintf
          {|nick = `%s`; password = `%s`; email = `%s`; bio = '%s';
    <img src="/user/%s/avatar" class="img-thumbnail" alt="Your avatar picture">|}
          nick password email (Dream.html_escape bio) nick
      in
      user_info
    | None -> "incoherent db answer" )
  | Error e -> Format.sprintf "db error: %s" (Caqti_error.show e)

let profile request =
  match Dream.session "nick" request with
  | None -> "not logged in"
  | Some nick -> Format.sprintf "Hello %s !" nick

let update_bio bio nick =
  let bio = Dream.html_escape bio in
  let valid = String.length bio < 10000 in
  if not valid then
    Error "Not biologic"
  else
    let res = Db.exec Q.update_bio (bio, nick) in
    match res with
    | Ok _ -> Ok ()
    | Error e -> Error (Format.sprintf "db error: %s" (Caqti_error.show e))

let get_bio nick =
  let res = Db.find_opt Q.get_bio nick in
  match res with
  | Ok bio -> (
    match bio with
    | Some bio -> Ok bio
    | None -> Error "incoherent db result" )
  | Error e -> Error (Format.sprintf "db error: %s" (Caqti_error.show e))

let get_avatar nick =
  let res = Db.find_opt Q.get_avatar nick in
  match res with
  | Ok avatar -> (
    match avatar with
    | Some avatar ->
      if String.length avatar = 0 then
        Ok None
      else
        Ok (Some avatar)
    | None -> Error "db error:" )
  | Error e -> Error (Format.sprintf "db error: %s" (Caqti_error.show e))

let upload_avatar files nick =
  match files with
  | [] -> Error "No file provided"
  | [ (_, content) ] -> (
    if not (is_valid_image content) then
      Error "Invalid image"
    else
      let res = Db.exec Q.upload_avatar (content, nick) in
      match res with
      | Ok _ -> Ok ()
      | Error e -> Error (Format.sprintf "db error: %s" (Caqti_error.show e)) )
  | _files -> Error "More than one file provided"
