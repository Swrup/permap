open Db
include Bindings

exception Invalid_post of string

type board = Babillard

let int_of_board = function
  | Babillard -> 1

let pp_board fmt = function
  | Babillard -> Format.fprintf fmt "babillard"

let board_of_int = function
  | 1 -> Babillard
  | _ -> raise (Invalid_argument "board_of_int")

type thread_data =
  { board : board
  ; subject : string
  ; lng : float
  ; lat : float
  }

type reply =
  { id : string
  ; parent_id : string
  ; date : int
  ; nick : string
  ; comment : string
  ; image : (string * string * string) option
  ; tags : string list
  ; replies : string list
  ; citations : string list
  }

type post =
  | Op of thread_data * reply
  | Reply of reply

module Q = struct
  let create_post_user_table =
    Caqti_request.exec Caqti_type.unit
      "CREATE TABLE IF NOT EXISTS post_user (post_id TEXT, nick TEXT, PRIMARY \
       KEY(post_id), FOREIGN KEY(nick) REFERENCES user(nick));"

  (* post_id -> OP's post_id *)
  let create_post_parent_table =
    Caqti_request.exec Caqti_type.unit
      "CREATE TABLE IF NOT EXISTS post_parent (post_id TEXT, parent_id TEXT, \
       FOREIGN KEY(post_id) REFERENCES post_user(post_id),\n\
      \       FOREIGN KEY(parent_id) REFERENCES post_user(post_id));"

  let create_thread_board_table =
    Caqti_request.exec Caqti_type.unit
      "CREATE TABLE IF NOT EXISTS thread_board (thread_id TEXT, board INT, \
       FOREIGN KEY(thread_id) REFERENCES post_user(post_id));"

  (* TODO useless? *)
  let create_thread_table =
    Caqti_request.exec Caqti_type.unit
      "CREATE TABLE IF NOT EXISTS threads (thread_id TEXT, post_id TEXT,\n\
      \         FOREIGN KEY(thread_id) REFERENCES post_user(post_id),\n\
      \         FOREIGN KEY(post_id) REFERENCES post_user(post_id));"

  let create_post_replies_table =
    Caqti_request.exec Caqti_type.unit
      "CREATE TABLE IF NOT EXISTS post_replies (post_id TEXT, reply_id TEXT, \
       FOREIGN KEY(post_id) REFERENCES post_user(post_id),\n\
      \       FOREIGN KEY(reply_id) REFERENCES post_user(post_id));"

  let create_post_citations_table =
    Caqti_request.exec Caqti_type.unit
      "CREATE TABLE IF NOT EXISTS post_citations (post_id TEXT, cited_id TEXT, \
       FOREIGN KEY(post_id) REFERENCES post_user(post_id),\n\
      \       FOREIGN KEY(cited_id) REFERENCES post_user(post_id));"

  let create_post_date_table =
    Caqti_request.exec Caqti_type.unit
      "CREATE TABLE IF NOT EXISTS post_date (post_id TEXT, date INT, FOREIGN \
       KEY(post_id) REFERENCES post_user(post_id));"

  let create_post_comment_table =
    Caqti_request.exec Caqti_type.unit
      "CREATE TABLE IF NOT EXISTS post_comment (post_id TEXT, comment TEXT, \
       FOREIGN KEY(post_id) REFERENCES post_user(post_id));"

  let create_post_image_table =
    Caqti_request.exec Caqti_type.unit
      "CREATE TABLE IF NOT EXISTS post_image (post_id TEXT, image_name TEXT, \
       image_content TEXT, image_alt TEXT, FOREIGN KEY(post_id) REFERENCES \
       post_user(post_id));"

  let create_post_gps_table =
    Caqti_request.exec Caqti_type.unit
      "CREATE TABLE IF NOT EXISTS post_gps (post_id TEXT, lat FLOAT, lng FLOAT ,\n\
      \       FOREIGN KEY(post_id) REFERENCES post_user(post_id));"

  let create_post_subject_table =
    Caqti_request.exec Caqti_type.unit
      "CREATE TABLE IF NOT EXISTS post_subject (post_id TEXT, subject TEXT, \
       FOREIGN KEY(post_id) REFERENCES post_user(post_id));"

  let create_post_tags_table =
    Caqti_request.exec Caqti_type.unit
      "CREATE TABLE IF NOT EXISTS post_tags (post_id TEXT, tag TEXT, FOREIGN \
       KEY(post_id) REFERENCES post_user(post_id));"

  let upload_post_id =
    Caqti_request.exec
      Caqti_type.(tup2 string string)
      "INSERT INTO post_user VALUES (?,?);"

  let upload_post_gps =
    Caqti_request.exec
      Caqti_type.(tup3 string float float)
      "INSERT INTO post_gps VALUES (?,?,?);"

  let upload_post_image =
    Caqti_request.exec
      Caqti_type.(tup4 string string string string)
      "INSERT INTO post_image VALUES (?,?,?,?);"

  let upload_post_reply =
    Caqti_request.exec
      Caqti_type.(tup2 string string)
      "INSERT INTO post_replies VALUES (?,?);"

  let upload_post_comment =
    Caqti_request.exec
      Caqti_type.(tup2 string string)
      "INSERT INTO post_comment VALUES (?,?);"

  let upload_post_subject =
    Caqti_request.exec
      Caqti_type.(tup2 string string)
      "INSERT INTO post_subject VALUES (?,?);"

  let upload_post_tag =
    Caqti_request.exec
      Caqti_type.(tup2 string string)
      "INSERT INTO post_tags VALUES (?,?);"

  let upload_post_date =
    Caqti_request.exec
      Caqti_type.(tup2 string int)
      "INSERT INTO post_date VALUES (?,?);"

  let upload_to_thread =
    Caqti_request.exec
      Caqti_type.(tup2 string string)
      "INSERT INTO threads VALUES (?,?);"

  let upload_thread_board =
    Caqti_request.exec
      Caqti_type.(tup2 string int)
      "INSERT INTO thread_board VALUES (?,?);"

  let upload_post_parent =
    Caqti_request.exec
      Caqti_type.(tup2 string string)
      "INSERT INTO post_parent VALUES (?,?);"

  let get_post_nick =
    Caqti_request.find Caqti_type.string Caqti_type.string
      "SELECT nick FROM post_user WHERE post_id=?;"

  let get_post_comment =
    Caqti_request.find Caqti_type.string Caqti_type.string
      "SELECT comment FROM post_comment WHERE post_id=?;"

  let get_post_image_content =
    Caqti_request.find_opt Caqti_type.string Caqti_type.string
      "SELECT image_content FROM post_image WHERE post_id=?;"

  let get_post_image_info =
    Caqti_request.find_opt Caqti_type.string
      Caqti_type.(tup2 string string)
      "SELECT image_name,image_alt FROM post_image WHERE post_id=?;"

  let get_post_tags =
    Caqti_request.collect Caqti_type.string Caqti_type.string
      "SELECT tag FROM post_tags WHERE post_id=?;"

  let get_post_date =
    Caqti_request.find Caqti_type.string Caqti_type.int
      "SELECT date FROM post_date WHERE post_id=?;"

  let get_post_citations =
    Caqti_request.collect Caqti_type.string Caqti_type.string
      "SELECT post_id FROM post_citations WHERE reply_id=?;"

  let get_post_replies =
    Caqti_request.collect Caqti_type.string Caqti_type.string
      "SELECT reply_id FROM post_replies WHERE post_id=?;"

  let get_thread_posts =
    Caqti_request.collect Caqti_type.string Caqti_type.string
      "SELECT post_id FROM threads WHERE thread_id=?;"

  let count_thread_posts =
    Caqti_request.find Caqti_type.string Caqti_type.int
      "SELECT COUNT(post_id) FROM threads WHERE thread_id=?;"

  (* TODO return bool *)
  let is_thread =
    Caqti_request.find_opt Caqti_type.string Caqti_type.string
      "SELECT thread_id FROM threads WHERE thread_id=? LIMIT 1;"

  let get_thread_board =
    Caqti_request.find Caqti_type.string Caqti_type.int
      "SELECT board FROM thread_board WHERE thread_id=? LIMIT 1;"

  let get_post_subject =
    Caqti_request.find_opt Caqti_type.string Caqti_type.string
      "SELECT subject FROM post_subject WHERE post_id=?;"

  let get_post_gps =
    Caqti_request.find_opt Caqti_type.string
      Caqti_type.(tup2 float float)
      "SELECT lat, lng FROM post_gps WHERE post_id=?;"

  let list_threads =
    Caqti_request.collect Caqti_type.int Caqti_type.string
      "SELECT thread_id FROM thread_board WHERE board=?;"
end

let () =
  let tables =
    [ Q.create_post_user_table
    ; Q.create_post_parent_table
    ; Q.create_thread_table
    ; Q.create_thread_board_table
    ; Q.create_post_replies_table
    ; Q.create_post_citations_table
    ; Q.create_post_date_table
    ; Q.create_post_comment_table
    ; Q.create_post_image_table
    ; Q.create_post_gps_table
    ; Q.create_post_subject_table
    ; Q.create_post_tags_table
    ]
  in
  if
    List.exists Result.is_error
      (List.map (fun query -> Db.exec query ()) tables)
  then
    Dream.warning (fun log -> log "can't create table")

let parse_image image =
  match image with
  | None -> Ok None
  | Some image -> (
    let image =
      match image with
      | Some image_name, image_content, alt ->
        (Dream.html_escape image_name, image_content, Dream.html_escape alt)
      | None, image_content, alt ->
        (* make up random name if no name was given *)
        let image_name = Uuidm.to_string (Uuidm.v4_gen random_state ()) in
        (image_name, image_content, Dream.html_escape alt)
    in
    match image with
    | _, image_content, alt ->
      if not (is_valid_image image_content) then
        Error "invalid image"
      else if String.length alt > 1000 then
        Error "Image description too long"
      else
        Ok (Some image) )

(* TODO: Is this safe? *)
(*TODO fix bad link if post in other thread*)
let parse_comment comment =
  let handle_word w =
    let trim_w = String.trim w in
    (* '>' is '&gt;' after html_escape *)
    match String.starts_with ~prefix:{|&gt;&gt;|} trim_w with
    | false -> (w, None)
    | true -> (
      let sub_w = String.sub trim_w 8 (String.length trim_w - 8) in
      match Uuidm.of_string sub_w with
      | None -> (w, None)
      | Some _ ->
        let new_w = Format.sprintf {|<a href="#%s">%s</a>|} sub_w w in
        (new_w, Some sub_w) )
  in
  let handle_line l =
    let trim_w = String.trim l in
    (*insert quote*)
    let line =
      match
        String.starts_with ~prefix:{|&gt;|} trim_w
        && not (String.starts_with ~prefix:{|&gt;&gt;|} trim_w)
      with
      | false -> l
      | true -> {|<span class="quote">|} ^ l ^ {|</span>|}
    in
    let words = String.split_on_char ' ' line in
    let words, cited_posts =
      List.fold_left
        (fun (acc_words, acc_cited_posts) w ->
          match handle_word w with
          | w, Some cited_id -> (w :: acc_words, cited_id :: acc_cited_posts)
          | w, None -> (w :: acc_words, acc_cited_posts) )
        ([], []) words
    in
    let words = List.rev words in
    let line =
      Format.asprintf "%a"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt " ")
           Format.pp_print_string )
        words
    in
    (line, cited_posts)
  in

  let comment = String.trim comment in
  let lines = String.split_on_char '\n' comment in
  let lines, cited_posts =
    List.fold_left
      (fun (acc_lines, acc_cited_posts) l ->
        let line, cited_posts = handle_line l in
        (line :: acc_lines, cited_posts @ acc_cited_posts) )
      ([], []) lines
  in
  let lines = List.rev lines in
  (*insert <br>*)
  let comment =
    Format.asprintf "%a"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@.<br>")
         Format.pp_print_string )
      lines
  in
  (* remove duplicate cited_id *)
  let cited_posts = List.sort_uniq String.compare cited_posts in
  (comment, cited_posts)

let upload_post post =
  let thread_data, reply =
    match post with
    | Op (thread_data, reply) -> (Some thread_data, reply)
    | Reply reply -> (None, reply)
  in
  let post_id, parent_id, date, nick, comment, image, tags, citations =
    match reply with
    | { id
      ; parent_id
      ; date
      ; nick
      ; comment
      ; image
      ; tags
      ; replies = _replies
      ; citations
      } ->
      (id, parent_id, date, nick, comment, image, tags, citations)
  in

  let^ _res_post_id = Db.exec Q.upload_post_id (post_id, nick) in
  let^ _res_comment = Db.exec Q.upload_post_comment (post_id, comment) in
  let^ _res_date = Db.exec Q.upload_post_date (post_id, date) in
  let^ _res_thread = Db.exec Q.upload_to_thread (parent_id, post_id) in
  let^ _res_image =
    match image with
    | None -> Ok ()
    | Some (image_name, image_content, alt) ->
      Db.exec Q.upload_post_image (post_id, image_name, image_content, alt)
  in
  let^ _res_parent = Db.exec Q.upload_post_parent (post_id, parent_id) in
  let^ _res_tags =
    match
      List.find_opt Result.is_error
        (List.map (fun tag -> Db.exec Q.upload_post_tag (post_id, tag)) tags)
    with
    | Some (Error e) -> Error e
    | Some _ -> assert false
    | None -> Ok ()
  in
  let^ _res_citations =
    match
      List.find_opt Result.is_error
        (List.map
           (fun cited_id -> Db.exec Q.upload_post_reply (cited_id, post_id))
           citations )
    with
    | Some (Error e) -> Error e
    | Some _ -> assert false
    | None -> Ok ()
  in
  match thread_data with
  | None -> Ok post_id
  | Some thread_data -> (
    match thread_data with
    | { board; subject; lng; lat } ->
      let^ _res_board =
        Db.exec Q.upload_thread_board (post_id, int_of_board board)
      in
      let^ _res_gps = Db.exec Q.upload_post_gps (post_id, lat, lng) in
      let^ _res_subject = Db.exec Q.upload_post_subject (post_id, subject) in
      Ok post_id )

let build_reply ~comment ?image ~tags ?parent_id nick =
  let comment = Dream.html_escape comment in
  let tags = Dream.html_escape tags in
  let id = Uuidm.to_string (Uuidm.v4_gen random_state ()) in
  (* parent_id is None if this reply is supposed to be a new thread *)
  let parent_id =
    match parent_id with
    | Some parent_id -> parent_id
    | None -> id
  in
  if Option.is_none (Uuidm.of_string parent_id) then
    Error "invalid thread id"
  else if String.length comment > 10000 then
    Error "invalid comment"
  else
    match parse_image image with
    | Error e -> Error e
    | Ok image ->
      if String.length tags > 1000 then
        Error "invalid tags"
      else
        (* TODO latlng validation? *)
        let tag_list = Str.split (Str.regexp " +") tags in
        let date = int_of_float (Unix.time ()) in
        let comment, citations = parse_comment comment in
        let reply =
          { id
          ; parent_id
          ; date
          ; nick
          ; comment
          ; image
          ; tags = tag_list
          ; replies = []
          ; citations
          }
        in
        Ok reply

let build_op ~comment ?image ~tags ~subject ~lat ~lng ~board nick =
  let subject = Dream.html_escape subject in
  (* TODO latlng validation? *)
  let is_valid_latlng = true in
  if not is_valid_latlng then
    Error "Invalid coordinate"
  else if String.length subject > 600 then
    Error "Invalid subject"
  else
    let thread_data = { board; subject; lng; lat } in
    let* reply =
      match image with
      | Some image -> build_reply ~comment ~image ~tags nick
      | None -> build_reply ~comment ~tags nick
    in
    let op = Op (thread_data, reply) in
    Ok op

let make_reply ~comment ?image ~tags ~parent_id nick =
  let* reply = build_reply ~comment ?image ~tags ~parent_id nick in
  let post = Reply reply in
  upload_post post

let make_op ~comment ?image ~tags ~subject ~lat ~lng ~board nick =
  let* op = build_op ~comment ?image ~tags ~subject ~lat ~lng ~board nick in
  upload_post op

let get_post_image_content post_id =
  let^? content = Db.find_opt Q.get_post_image_content post_id in
  Ok content
