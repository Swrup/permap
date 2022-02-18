open Db
include Bindings

type thread_data =
  { subject : string
  ; lng : float
  ; lat : float
  }

type reply =
  { id : string
  ; parent_id : string
  ; date : int
  ; nick : string
  ; comment : string
  ; image_info : (string * string) option
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

  let create_image_info_table =
    Caqti_request.exec Caqti_type.unit
      "CREATE TABLE IF NOT EXISTS image_info (post_id TEXT, image_name TEXT, \
       image_alt TEXT, FOREIGN KEY(post_id) REFERENCES post_user(post_id));"

  let create_image_content_table =
    Caqti_request.exec Caqti_type.unit
      "CREATE TABLE IF NOT EXISTS image_content (post_id TEXT,image_content \
       TEXT, FOREIGN KEY(post_id) REFERENCES post_user(post_id));"

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

  let upload_image_info =
    Caqti_request.exec
      Caqti_type.(tup3 string string string)
      "INSERT INTO image_info VALUES (?,?,?);"

  let upload_image_content =
    Caqti_request.exec
      Caqti_type.(tup2 string string)
      "INSERT INTO image_content VALUES (?,?);"

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
      "SELECT image_content FROM image_content WHERE post_id=?;"

  let get_post_image_info =
    Caqti_request.find_opt Caqti_type.string
      Caqti_type.(tup2 string string)
      "SELECT image_name,image_alt FROM image_info WHERE post_id=?;"

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

  let get_thread =
    Caqti_request.find Caqti_type.string Caqti_type.string
      "SELECT thread_id FROM threads WHERE thread_id=? LIMIT 1;"

  let get_post_subject =
    Caqti_request.find_opt Caqti_type.string Caqti_type.string
      "SELECT subject FROM post_subject WHERE post_id=?;"

  let get_post_gps =
    Caqti_request.find_opt Caqti_type.string
      Caqti_type.(tup2 float float)
      "SELECT lat, lng FROM post_gps WHERE post_id=?;"

  let get_threads =
    Caqti_request.collect Caqti_type.unit Caqti_type.string
      "SELECT thread_id FROM threads;"
end

let () =
  let tables =
    [ Q.create_post_user_table
    ; Q.create_post_parent_table
    ; Q.create_thread_table
    ; Q.create_post_replies_table
    ; Q.create_post_citations_table
    ; Q.create_post_date_table
    ; Q.create_post_comment_table
    ; Q.create_image_info_table
    ; Q.create_image_content_table
    ; Q.create_post_gps_table
    ; Q.create_post_subject_table
    ; Q.create_post_tags_table
    ]
  in
  if
    List.exists Result.is_error
      (List.map (fun query -> Db.exec query ()) tables)
  then Dream.error (fun log -> log "can't create table")

let parse_image image =
  match image with
  | None -> Ok None
  | Some ((name, alt), content) ->
    let name =
      match name with
      | Some name -> Dream.html_escape name
      | None ->
        (* make up random name if no name was given *)
        Uuidm.to_string (Uuidm.v4_gen random_state ())
    in
    if not (is_valid_image content) then Error "invalid image"
    else if String.length alt > 1000 then Error "Image description too long"
    else Ok (Some ((name, alt), content))

(*TODO switch to markdown !*)
(* insert html into the comment, and keep tracks of citations :
       -wraps lines starting with ">" with a <span class="quote">
       -make raw posts uuid into links
   (*TODO fix bad link if post is in other thread*)
       -keeps tracks of every post cited in this comment
      - add <br> at each line *)
let parse_comment comment =
  let handle_word w =
    let trim_w = String.trim w in
    (* '>' is '&gt;' after html_escape *)
    if String.starts_with ~prefix:{|&gt;&gt;|} trim_w then
      let sub_w = String.sub trim_w 8 (String.length trim_w - 8) in
      match Uuidm.of_string sub_w with
      | None -> (w, None)
      | Some _ ->
        let new_w = Format.sprintf {|<a href="#%s">%s</a>|} sub_w w in
        (new_w, Some sub_w)
    else (w, None)
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

let upload_post ?image_content post =
  let thread_data, reply =
    match post with
    | Op (thread_data, reply) -> (Some thread_data, reply)
    | Reply reply -> (None, reply)
  in
  let { id; parent_id; date; nick; comment; image_info; tags; citations; _ } =
    reply
  in

  let^ () = Db.exec Q.upload_post_id (id, nick) in
  let^ () = Db.exec Q.upload_post_comment (id, comment) in
  let^ () = Db.exec Q.upload_post_date (id, date) in
  let^ () = Db.exec Q.upload_to_thread (parent_id, id) in
  let _res_image_info, _res_image_content =
    match image_info with
    | None -> (Ok (), Ok ())
    | Some (name, alt) -> (
      match image_content with
      | None -> failwith "No image_content for a post with image"
      | Some content ->
        ( Db.exec Q.upload_image_info (id, name, alt)
        , Db.exec Q.upload_image_content (id, content) ) )
  in
  (* what is parent and why do i need it again? TODO TODO *)
  let^ () = Db.exec Q.upload_post_parent (id, parent_id) in
  let^ () =
    match
      List.find_opt Result.is_error
        (List.map (fun tag -> Db.exec Q.upload_post_tag (id, tag)) tags)
    with
    | Some (Error e) -> Error e
    | Some _ -> assert false
    | None -> Ok ()
  in
  let^ () =
    match
      List.find_opt Result.is_error
        (List.map
           (fun cited_id -> Db.exec Q.upload_post_reply (cited_id, id))
           citations )
    with
    | Some (Error e) -> Error e
    | Some _ -> assert false
    | None -> Ok ()
  in
  match thread_data with
  | None -> Ok id
  | Some { subject; lng; lat } ->
    let^ () = Db.exec Q.upload_post_gps (id, lat, lng) in
    let^ () = Db.exec Q.upload_post_subject (id, subject) in
    Ok id

let build_reply ~comment ?image ~tags ?parent_id nick =
  let comment = Dream.html_escape comment in
  let tags = Dream.html_escape tags in
  let id = Uuidm.to_string (Uuidm.v4_gen random_state ()) in
  (* parent_id is None if this reply is supposed to be a new thread *)
  let parent_id = Option.value parent_id ~default:id in
  if Option.is_none (Uuidm.of_string parent_id) then Error "invalid thread id"
  else if String.length comment > 10000 then Error "invalid comment"
  else
    match parse_image image with
    | Error e -> Error e
    | Ok image ->
      if String.length tags > 1000 then Error "invalid tags"
      else
        let image_info =
          match image with
          | None -> None
          | Some (image_info, _image_content) -> Some image_info
        in
        let tag_list = Str.split (Str.regexp " +") tags in
        let date = int_of_float (Unix.time ()) in
        let comment, citations = parse_comment comment in
        let reply =
          { id
          ; parent_id
          ; date
          ; nick
          ; comment
          ; image_info
          ; tags = tag_list
          ; replies = []
          ; citations
          }
        in
        Ok reply

let build_op ~comment ?image ~tags ~subject ~lat ~lng nick =
  let subject = Dream.html_escape subject in
  (* TODO latlng validation? *)
  let is_valid_latlng = true in
  if not is_valid_latlng then Error "Invalid coordinate"
  else if String.length subject > 600 then Error "Invalid subject"
  else
    let thread_data = { subject; lng; lat } in
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
  match image with
  | None -> upload_post post
  | Some (_image_info, image_content) -> upload_post ~image_content post

let make_op ~comment ?image ~tags ~subject ~lat ~lng nick =
  let* op = build_op ~comment ?image ~tags ~subject ~lat ~lng nick in
  match image with
  | None -> upload_post op
  | Some (_image_info, image_content) -> upload_post ~image_content op

let get_post_image_content post_id =
  let^? content = Db.find_opt Q.get_post_image_content post_id in
  Ok content
