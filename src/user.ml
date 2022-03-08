include Bindings
open Db

type t =
  { nick : string
  ; display_nick : string
  ; password : string
  ; email : string
  ; bio : string
  ; avatar : string
  ; metadata : (int * string * string) list
  }

module Q = struct
  let create_user_table =
    Caqti_request.exec Caqti_type.unit
      "CREATE TABLE IF NOT EXISTS user (nick TEXT, display_nick TEXT, password \
       TEXT, email TEXT, bio TEXT, avatar BLOB, PRIMARY KEY(nick));"

  let create_banished_table =
    Caqti_request.exec Caqti_type.unit
      "CREATE TABLE IF NOT EXISTS banished (nick TEXT, email TEXT);"

  let create_metadata_table =
    Caqti_request.exec Caqti_type.unit
      "CREATE TABLE IF NOT EXISTS metadata (nick TEXT, count INT, label TEXT, \
       content TEXT, FOREIGN KEY(nick) REFERENCES user(nick) ON DELETE \
       CASCADE);"

  let get_metadata =
    Caqti_request.collect Caqti_type.string
      Caqti_type.(tup3 int string string)
      "SELECT count, label, content FROM metadata WHERE nick=?;"

  let upload_metadata =
    Caqti_request.exec
      Caqti_type.(tup4 string int string string)
      "INSERT INTO metadata VALUES (?, ?, ?, ?);"

  let delete_metadata =
    Caqti_request.exec
      Caqti_type.(tup2 string int)
      "DELETE FROM metadata WHERE nick=? AND count=?;"

  let get_password =
    Caqti_request.find_opt Caqti_type.string Caqti_type.string
      "SELECT password FROM user WHERE nick=?;"

  let is_already_user =
    Caqti_request.find_opt
      Caqti_type.(tup2 string string)
      Caqti_type.int
      "SELECT EXISTS(SELECT 1 FROM user WHERE nick=? OR email=?);"

  let upload_user =
    Caqti_request.exec
      Caqti_type.(
        tup4 string string string Caqti_type.(tup3 string string string))
      "INSERT INTO user VALUES (?, ?, ?, ?, ?, ?);"

  let list_nicks =
    Caqti_request.collect Caqti_type.unit Caqti_type.string
      "SELECT nick FROM user;"

  let get_user =
    Caqti_request.find Caqti_type.string
      (* there is no "tup6" *)
      Caqti_type.(
        tup4 string string string Caqti_type.(tup3 string string string))
      "SELECT * FROM user WHERE nick=?;"

  let update_bio =
    Caqti_request.exec
      Caqti_type.(tup2 string string)
      "UPDATE user SET bio=? WHERE nick=?;"

  let update_display_nick =
    Caqti_request.exec
      Caqti_type.(tup2 string string)
      "UPDATE user SET display_nick=? WHERE nick=?;"

  let update_email =
    Caqti_request.exec
      Caqti_type.(tup2 string string)
      "UPDATE user SET email=? WHERE nick=?;"

  let update_password =
    Caqti_request.exec
      Caqti_type.(tup2 string string)
      "UPDATE user SET password=? WHERE nick=?;"

  let get_display_nick =
    Caqti_request.find Caqti_type.string Caqti_type.string
      "SELECT display_nick FROM user WHERE nick=?;"

  let get_bio =
    Caqti_request.find Caqti_type.string Caqti_type.string
      "SELECT bio FROM user WHERE nick=?;"

  let get_email =
    Caqti_request.find Caqti_type.string Caqti_type.string
      "SELECT email FROM user WHERE nick=?;"

  let get_avatar =
    Caqti_request.find Caqti_type.string Caqti_type.string
      "SELECT avatar FROM user WHERE nick=?;"

  let upload_avatar =
    Caqti_request.exec
      Caqti_type.(tup2 string string)
      "UPDATE user SET avatar=? WHERE nick=?;"

  let delete_user =
    Caqti_request.exec Caqti_type.string "DELETE FROM user WHERE nick=?;"

  let upload_banished =
    Caqti_request.exec
      Caqti_type.(tup2 string string)
      "INSERT INTO banished VALUES (?,?);"

  let get_banished =
    Caqti_request.find Caqti_type.string
      Caqti_type.(tup2 string string)
      "SELECT * FROM banished WHERE nick=?;"
end

let () =
  let tables =
    [| Q.create_user_table; Q.create_banished_table; Q.create_metadata_table |]
  in
  if
    Array.exists Result.is_error
      (Array.map (fun query -> Db.exec query ()) tables)
  then Dream.error (fun log -> log "can't create user tables")

let exist nick = Result.is_ok (Db.find Q.get_user nick)

let get_metadata nick =
  let^ metadata = Db.collect_list Q.get_metadata nick in
  let metadata = List.sort (fun (a, _, _) (b, _, _) -> compare a b) metadata in
  Ok metadata

let get_user nick =
  let^? nick, display_nick, password, (email, bio, avatar) =
    Db.find_opt Q.get_user nick
  in
  let* metadata = get_metadata nick in
  Ok { nick; display_nick; password; email; bio; avatar; metadata }

let is_banished nick = Result.is_ok (Db.find Q.get_banished nick)

let login ~nick ~password request =
  if exist nick then
    let^? good_password = Db.find_opt Q.get_password nick in
    if Bcrypt.verify password (Bcrypt.hash_of_string good_password) then
      let _unit_lwt = Dream.invalidate_session request in
      let _unit_lwt = Dream.put_session "nick" nick request in
      Ok ()
    else Error "wrong password"
  else if is_banished nick then Error "YOU ARE BANISHED"
  else Error "wrong user name"

let valid_nick nick =
  String.length nick < 64
  && String.length nick > 0
  && Dream.html_escape nick = nick

let valid_password password =
  String.length password < 128 && String.length password > 0

let valid_email email =
  match Emile.of_string email with Ok _ -> true | Error _ -> false

let register ~email ~nick ~password =
  let valid = valid_nick nick && valid_email email && valid_password password in

  let password = Bcrypt.hash password in
  let password = Bcrypt.string_of_hash password in

  if not valid then Error "Something is wrong"
  else
    let^? nb = Db.find_opt Q.is_already_user (nick, email) in
    if nb = 0 then
      let^ () = Db.exec Q.upload_user (nick, nick, password, (email, "", "")) in
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

let get_email nick =
  let^? email = Db.find_opt Q.get_email nick in
  Ok email

let get_display_nick nick =
  let^? display_nick = Db.find_opt Q.get_display_nick nick in
  Ok display_nick

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

let is_admin nick = List.mem nick App.admins

let banish nick =
  let* email = get_email nick in
  let^ () = Db.exec Q.delete_user nick in
  let^ () = Db.exec Q.upload_banished (nick, email) in
  Ok ()

let delete_user nick =
  let^ () = Db.exec Q.delete_user nick in
  Ok ()

let update_display_nick display_nick nick =
  if valid_nick display_nick then
    let^ () = Db.exec Q.update_display_nick (display_nick, nick) in
    Ok ()
  else Error "invalid display nick"

let update_email email nick =
  if valid_email email then
    let^ () = Db.exec Q.update_email (email, nick) in
    Ok ()
  else Error "invalid email"

let update_password password nick =
  if valid_password password then
    let password = Bcrypt.hash password in
    let password = Bcrypt.string_of_hash password in
    let^ () = Db.exec Q.update_password (password, nick) in
    Ok ()
  else Error "invalid password"

let update_metadata count label content nick =
  let label = Dream.html_escape label in
  let content = Dream.html_escape content in
  if String.length label > 200 || String.length content > 400 then
    Error "label or content is too long"
  else
    (* rewrite all user's metadata *)
    let* metadata = get_metadata nick in
    let^ _unit_list =
      unwrap_list
        (fun (count, _l, _c) -> Db.exec Q.delete_metadata (nick, count))
        metadata
    in
    let l = List.filter (fun (i, _, _) -> i <> count) metadata in
    let l =
      if not (label = "" && content = "") then (count, label, content) :: l
      else l
    in
    let l = List.sort (fun (a, _, _) (b, _, _) -> compare a b) l in
    let l = List.mapi (fun i (_, label, content) -> (i, label, content)) l in
    let^ _unit_list =
      unwrap_list
        (fun (i, label, content) ->
          Db.exec Q.upload_metadata (nick, i, label, content) )
        l
    in
    Ok ()

let pp_metadata fmt metadata =
  let _count, label, content = metadata in
  Format.fprintf fmt
    {|
<div class="row">
    <div class="col metadata-label">%s</div>
    <div class="col metadata-content">%s</div>
</div>
    |}
    label content

let pp_metadata_form fmt ?is_last metadata request =
  let count, label, content = metadata in
  let form_tag = Dream.form_tag ~action:"/profile" request in
  let button_text = if Option.is_some is_last then "Add" else "Save" in
  Format.fprintf fmt
    {|
<div class="row">
    %s
        <input name="label" class="metadata-label" value="%s"/>
        <input name="content" class="metadata-content" value="%s"/>
    <button name="count" value="%d" type="submit" class="btn btn-primary">%s</button>
    </form>
</div>
    |}
    form_tag label content count button_text

let pp_metadata_table fmt metadata =
  Format.fprintf fmt
    {|
<div class="metadata-table">
    %a
</div>
|}
    (Format.pp_print_list ~pp_sep:Format.pp_print_space pp_metadata)
    metadata

let pp_metadata_table_form fmt metadata request =
  let length = List.length metadata in
  let new_metadata_field fmt () =
    pp_metadata_form fmt ~is_last:() (length, "", "") request
  in
  Format.fprintf fmt
    {|
<div class="metadata-form-table">
    %a
    %a
</div>
|}
    (Format.pp_print_list ~pp_sep:Format.pp_print_space (fun fmt metadata ->
         pp_metadata_form fmt metadata request ) )
    metadata new_metadata_field ()

let public_profile nick =
  let* user = get_user nick in
  let user_info =
    Format.asprintf
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
      <div class="col-md-6">
        %a
      </div>
    </div>
|}
      user.display_nick user.bio user.nick pp_metadata_table user.metadata
  in
  Ok user_info
