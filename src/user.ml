include Bindings
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
  then Dream.error (fun log -> log "can't create table")

let login ~nick ~password request =
  let^? good_password = Db.find_opt Q.get_password nick in
  if Bcrypt.verify password (Bcrypt.hash_of_string good_password) then
    let _ =
      let%lwt () = Dream.invalidate_session request in
      Dream.put_session "nick" nick request
    in
    Ok ()
  else Error "wrong password"

let register ~email ~nick ~password =
  (* TODO: remove bad characters (e.g. delthas) *)
  let valid_nick =
    String.length nick < 64
    && String.length nick > 0
    && Dream.html_escape nick = nick
  in

  let valid_email =
    match Emile.of_string email with Ok _ -> true | Error _ -> false
  in

  let valid_password =
    String.length password < 128 && String.length password > 0
  in

  let valid = valid_nick && valid_email && valid_password in

  let password = Bcrypt.hash password in
  let password = Bcrypt.string_of_hash password in

  if not valid then Error "Something is wrong"
  else
    let^? nb = Db.find_opt Q.is_already_user (nick, email) in
    if nb = 0 then
      let^ () = Db.exec Q.inser_new_user (nick, password, email, ("", "")) in
      Ok ()
    else Error "nick or email already exists"

let list () =
  let^ users = Db.collect_list Q.list_nicks () in
  Ok
    (Format.asprintf "<ul>%a</ul>"
       (Format.pp_print_list (fun fmt -> function
          | s -> Format.fprintf fmt {|<li><a href="/user/%s">%s</a></li>|} s s )
       )
       users )

let public_profile request =
  let nick = Dream.param request "user" in
  let^? nick, _password, _email, (bio, _) = Db.find_opt Q.get_user nick in
  let user_info =
    Format.sprintf
      {|
    <h1>%s</h1>
    <br />
    <div class="row">
      <div class="col-md-6">
        <blockquote>%s</blockquote>
      </div>
      <div class="col-md-6">
        <img src="/user/%s/avatar" class="img-thumbnail" alt="Your avatar picture">
      </div>
    </div>
|}
      nick bio nick
  in
  Ok user_info

let profile request =
  match Dream.session "nick" request with
  | None -> "not logged in"
  | Some nick -> Format.sprintf "Hello %s !" nick

let update_bio bio nick =
  let bio = Dream.html_escape bio in
  let valid = String.length bio < 10000 in
  if not valid then Error "Not biologic"
  else
    let^ () = Db.exec Q.update_bio (bio, nick) in
    Ok ()

let get_bio nick =
  let^? bio = Db.find_opt Q.get_bio nick in
  Ok bio

let get_avatar nick =
  let^? avatar = Db.find_opt Q.get_avatar nick in
  if String.length avatar = 0 then Ok None else Ok (Some avatar)

let upload_avatar files nick =
  match files with
  | [] -> Error "No file provided"
  | [ (_, content) ] ->
    if not (is_valid_image content) then Error "Invalid image"
    else
      let^ () = Db.exec Q.upload_avatar (content, nick) in
      Ok ()
  | _files -> Error "More than one file provided"
