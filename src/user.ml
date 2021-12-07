type t =
  { nick : string
  ; password : string
  ; email : string (* TODO: make email optional ? *)
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

  let create_plant_image_table =
    Caqti_request.exec Caqti_type.unit
      "CREATE TABLE IF NOT EXISTS plant_image (id INTEGER PRIMARY KEY, \
       plant_id TEXT, image TEXT, FOREIGN KEY(plant_id) REFERENCES \
       plant_user(plant_id));"

  let create_plant_tag_table =
    Caqti_request.exec Caqti_type.unit
      "CREATE TABLE IF NOT EXISTS plant_tag (plant_id TEXT, tag TEXT, FOREIGN \
       KEY(plant_id) REFERENCES plant_user(plant_id));"

  let create_plant_gps_table =
    Caqti_request.exec Caqti_type.unit
      "CREATE TABLE IF NOT EXISTS plant_gps (plant_id TEXT, gps TEXT, FOREIGN \
       KEY(plant_id) REFERENCES plant_user(plant_id));"

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

  let upload_plant_id =
    Caqti_request.exec
      Caqti_type.(tup2 string string)
      "INSERT INTO plant_user VALUES (?,?);"

  let upload_plant_tag =
    Caqti_request.exec
      Caqti_type.(tup2 string string)
      "INSERT INTO plant_tag VALUES (?,?);"

  let upload_plant_gps =
    Caqti_request.exec
      Caqti_type.(tup2 string string)
      "INSERT INTO plant_gps VALUES (?,?);"

  let upload_plant_image =
    Caqti_request.exec
      Caqti_type.(tup2 string string)
      "INSERT INTO plant_image VALUES (NULL,?,?);"

  let get_user_plants =
    Caqti_request.collect Caqti_type.string Caqti_type.string
      "SELECT plant_id FROM plant_user WHERE nick=?;"

  let count_plant_image =
    Caqti_request.find_opt Caqti_type.string Caqti_type.int
      "SELECT COUNT(*) FROM plant_image WHERE plant_id=?;"

  let get_plant_image =
    Caqti_request.find_opt
      Caqti_type.(tup2 string string)
      Caqti_type.string
      "SELECT image FROM plant_image WHERE plant_id=? AND id=?;"

  let get_tags =
    Caqti_request.collect Caqti_type.string Caqti_type.string
      "SELECT tag FROM plant_tag WHERE plant_id=?;"
end

module Db =
( val Caqti_blocking.connect (Uri.of_string (Filename.concat "sqlite3://" Db.db))
      |> Caqti_blocking.or_fail )

let random_state = Random.State.make_self_init ()

let () =
  let tables =
    [ Q.create_user_table
    ; Q.create_plant_user_table
    ; Q.create_plant_image_table
    ; Q.create_plant_tag_table
    ; Q.create_plant_gps_table
    ]
  in
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

let plant_view plant_id =
  let count = Db.find_opt Q.count_plant_image plant_id in
  match count with
  | Ok count -> (
    match count with
    | Some count -> (
      let images =
        String.concat "\n"
          (List.map
             (Format.sprintf
                {|<li><img src="/plant_pic/%s/%i" class="img-thumbnail"></li>|}
                plant_id )
             (List.init count succ) )
      in
      let tags = Db.fold Q.get_tags (fun tag acc -> tag :: acc) plant_id [] in
      match tags with
      | Error e -> Error (Format.sprintf "db error: %s" (Caqti_error.show e))
      | Ok tags ->
        let tags = String.concat " " tags in
        (* TODO add link to gps/map too *)
        Ok (images ^ tags) )
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
    | Some (nick, password, email, (bio, _)) -> (
      (* TODO show plants *)
      let plant_id_list =
        Db.fold Q.get_user_plants (fun plant_id acc -> plant_id :: acc) nick []
      in
      match plant_id_list with
      | Error e -> Format.sprintf "db error: %s" (Caqti_error.show e)
      | Ok plant_id_list ->
        let plants_result = List.map plant_view plant_id_list in
        if List.exists Result.is_error plants_result then
          Format.sprintf "db error"
        else
          let plants =
            String.concat "\n"
              (List.map
                 (function
                   | Ok s -> s
                   | Error _ -> assert false )
                 plants_result )
          in
          Format.sprintf
            {|nick = `%s`; password = `%s`; email = `%s`; bio = '%s';
    <img src="/user/%s/avatar" class="img-thumbnail" alt="Your avatar picture">
        %s
|}
            nick password email (Dream.html_escape bio) nick plants )
    | None -> "incoherent db answer" )
  | Error e -> Format.sprintf "db error: %s" (Caqti_error.show e)

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
        (* TODO default avatar *)
        Ok None
      else
        Ok (Some avatar)
    | None -> Error "db error:" )
  | Error e -> Error (Format.sprintf "db error: %s" (Caqti_error.show e))

let get_plant_image plant_id nb =
  let res = Db.find_opt Q.get_plant_image (plant_id, nb) in
  match res with
  | Ok content -> (
    match content with
    | Some content -> Ok (Some content)
    | None -> Error "Image not found" )
  | Error e -> Error (Format.sprintf "db error: %s" (Caqti_error.show e))

let upload_avatar files nick =
  match files with
  | [] -> Error "No file provided"
  | [ (_, content) ] -> (
    (* TODO validate image data with konan etc*)
    (* TODO file_name in db??*)
    let valid = true in
    if not valid then
      Error "Invalid image"
    else
      let res = Db.exec Q.upload_avatar (content, nick) in
      match res with
      | Ok _ -> Ok ()
      | Error e -> Error (Format.sprintf "db error: %s" (Caqti_error.show e)) )
  | _files -> Error "More than one file provided"

(* TODO *)
let is_valid_image _content = true

let add_plant tags files nick =
  match files with
  | files -> (
    (* TODO parse tags*)
    (* id for plant*)
    let ok_list = List.map (fun (_, content) -> is_valid_image content) files in
    let valid_files = List.for_all (fun valid -> valid) ok_list in
    if not valid_files then
      Error "Invalid image"
    else
      (* add plant to db *)

      (* TODO make a plant_id *)
      let plant_id = Uuidm.to_string (Uuidm.v4_gen random_state ()) in
      (* add to plant_id <-> user*)
      let res_plant = Db.exec Q.upload_plant_id (plant_id, nick) in
      match res_plant with
      | Error e -> Error (Format.sprintf "db error: %s" (Caqti_error.show e))
      | Ok _ -> (
        (* add to plant_id <-> tag table*)
        (* TODO iter on tags*)
        let res_tags = Db.exec Q.upload_plant_tag (plant_id, tags) in
        match res_tags with
        | Error e -> Error (Format.sprintf "db error: %s" (Caqti_error.show e))
        | Ok _ -> (
          (* add to plant_id <-> image*)
          let res_images =
            List.find_opt Result.is_error
              (List.map
                 (fun (_, content) ->
                   Db.exec Q.upload_plant_image (plant_id, content) )
                 files )
          in
          match res_images with
          | Some (Error e) ->
            Error (Format.sprintf "db error: %s" (Caqti_error.show e))
          | Some (Ok _) -> assert false
          | None -> Ok () ) ) )
