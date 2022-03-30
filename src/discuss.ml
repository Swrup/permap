(** Creating the table of all messages.

    Each message is made of :

    - an id (msg_id)
    - the id of the sender (from_id)
    - the id of the receiver (to_id)
    - some text (msg)

    TODO: add date ? *)
let () =
  let create_msg_table =
    Caqti_request.exec Caqti_type.unit
      "CREATE TABLE IF NOT EXISTS msg ( msg_id TEXT, from_id TEXT, to_id TEXT, \
       msg TEXT, PRIMARY KEY(msg_id), FOREIGN KEY(from_id) REFERENCES \
       user(user_id) ON DELETE CASCADE, FOREIGN KEY(to_id) REFERENCES \
       user(user_id) ON DELETE CASCADE);"
  in
  match Db.Db.exec create_msg_table () with
  | Ok () -> ()
  | Error _e -> Dream.error (fun log -> log "can't create msg table")

(** let's find who the user is talking to so we can know if they're dangerous *)
let find_comrades =
  let find_comrades =
    Caqti_request.collect
      Caqti_type.(tup2 string string)
      Caqti_type.(tup2 string string)
      "SELECT from_id, to_id FROM msg WHERE from_id=? OR to_id=?"
  in
  fun user_id ->
    let open Bindings in
    let^ comrades = Db.Db.collect_list find_comrades (user_id, user_id) in
    let comrades =
      List.map (fun (l, r) -> if l = user_id then r else l) comrades
    in
    Ok (List.sort_uniq String.compare comrades)

(** find all messages between two товарищи *)
let find_messages =
  let find_messages =
    Caqti_request.collect
      Caqti_type.(tup2 (tup2 string string) (tup2 string string))
      Caqti_type.(tup2 string string)
      "SELECT from_id, msg FROM msg WHERE (from_id=? AND to_id=?) OR \
       (from_id=? AND to_id=?)"
  in
  fun k1 k2 ->
    let open Bindings in
    let^ comrades = Db.Db.collect_list find_messages ((k1, k2), (k2, k1)) in
    Ok comrades

(** display the list of discussions *)
let render request =
  match Dream.session "user_id" request with
  | None ->
    let redirect_url =
      Format.sprintf "/login=?redirect=%s" (Dream.to_percent_encoded "/discuss")
    in
    Dream.respond ~status:`See_Other ~headers:[ ("Location", redirect_url) ] ""
  | Some user_id -> (
    match find_comrades user_id with
    | Error e -> Template_utils.render_unsafe e request
    | Ok comrades -> (
      let comrades =
        Bindings.unwrap_list
          (fun id ->
            match User.get_nick id with
            | Error _e as e -> e
            | Ok nick -> Ok (id, nick) )
          comrades
      in
      match comrades with
      | Error e -> Template_utils.render_unsafe e request
      | Ok comrades ->
        let pp_one_discuss fmt (id, nick) =
          Format.fprintf fmt {|<li><a href="/discuss/%s">%s</a></li>|} id nick
        in
        let output =
          Format.asprintf "<ul>%a</ul>"
            (Format.pp_print_list
               ~pp_sep:(fun fmt () -> Format.fprintf fmt "<br />")
               pp_one_discuss )
            comrades
        in
        Template_utils.render_unsafe output request ) )

let pp_discussion (request, user_id, comrade_id) =
  let path = Format.sprintf "/discuss/%s" comrade_id in
  match find_messages user_id comrade_id with
  | Error e -> Template_utils.render_unsafe e request
  | Ok msg -> (
    match User.get_nick user_id with
    | Error e -> Template_utils.render_unsafe e request
    | Ok user_nick -> (
      match User.get_nick comrade_id with
      | Error e -> Template_utils.render_unsafe e request
      | Ok comrade_nick ->
        let pp_one_msg fmt (from_id, msg) =
          Format.fprintf fmt "<li>%s | %s</li>"
            (if from_id = user_id then user_nick else comrade_nick)
            msg
        in
        let pp_all_msg fmt msg =
          Format.fprintf fmt "<ul>%a</ul>"
            (Format.pp_print_list
               ~pp_sep:(fun fmt () -> Format.fprintf fmt "<br />")
               pp_one_msg )
            msg
        in
        Template_utils.render_unsafe
          (Format.asprintf
             {|%a<br />
          %s
          <input value="" name="msg" type="text" />
          <button type="submit" class="btn btn-primary">Send</button>
        </form>|}
             pp_all_msg msg
             (Dream.form_tag ~action:path request) )
          request ) )

(** display one discussion *)
let render_one request =
  let comrade_id = Dream.param request "comrade_id" in

  let path = Format.sprintf "/discuss/%s" comrade_id in

  match Dream.session "user_id" request with
  | None ->
    let redirect_url =
      Format.sprintf "/login=?redirect=%s" (Dream.to_percent_encoded path)
    in
    Dream.respond ~status:`See_Other ~headers:[ ("Location", redirect_url) ] ""
  | Some user_id -> pp_discussion (request, user_id, comrade_id)

let insert_msg =
  let insert_msg =
    Caqti_request.exec
      Caqti_type.(tup3 string string string)
      "INSERT INTO msg VALUES (NULL, ?, ?, ?);"
  in
  fun from_id to_id msg ->
    let open Bindings in
    let^ () = Db.Db.exec insert_msg (from_id, to_id, msg) in
    Ok ()

(** handle posts *)
let post request =
  let comrade_id = Dream.param request "comrade_id" in

  let path = Format.sprintf "/discuss/%s" comrade_id in

  match Dream.session "user_id" request with
  | None ->
    let redirect_url =
      Format.sprintf "/login=?redirect=%s" (Dream.to_percent_encoded path)
    in
    Dream.respond ~status:`See_Other ~headers:[ ("Location", redirect_url) ] ""
  | Some user_id -> (
    match%lwt Dream.form request with
    | `Ok [ ("msg", msg) ] -> begin
      match insert_msg user_id comrade_id msg with
      | Ok () -> pp_discussion (request, user_id, comrade_id)
      | Error e -> Template_utils.render_unsafe e request
    end
    | `Ok _ -> Dream.respond ~status:`Bad_Request "invalid form"
    | `Expired _ | `Many_tokens _ | `Missing_token _ | `Invalid_token _
    | `Wrong_session _ | `Wrong_content_type ->
      Dream.empty `Bad_Request )
