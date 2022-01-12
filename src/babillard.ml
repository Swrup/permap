open Db

exception Invalid_post of string

type op =
  { id : string
  ; date : int
  ; nick : string
  ; subject : string
  ; comment : string
  ; image : string * string
  ; tags : string list
  ; longitude : float
  ; latitude : float
  ; replies : string list
  ; citations : string list
  }

type reply =
  { id : string
  ; parent_id : string
  ; date : int
  ; nick : string
  ; comment : string
  ; image : (string * string) option
  ; tags : string list
  ; replies : string list
  ; citations : string list
  }

type post =
  | Op of op
  | Reply of reply

(* ('a option, string) result *)
let ( let** ) o f =
  match o with
  | Error e -> Error (Format.sprintf "db error: %s" (Caqti_error.show e))
  | Ok None -> Error (Format.sprintf "db error: value not found")
  | Ok (Some x) -> f x

(* ('a, string) result *)
let ( let* ) o f =
  match o with
  | Error e -> Error (Format.sprintf "db error: %s" (Caqti_error.show e))
  | Ok x -> f x

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

  let create_post_image_table =
    Caqti_request.exec Caqti_type.unit
      "CREATE TABLE IF NOT EXISTS post_image (post_id TEXT, image_name TEXT, \
       image_content, FOREIGN KEY(post_id) REFERENCES post_user(post_id));"

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
      Caqti_type.(tup3 string string string)
      "INSERT INTO post_image VALUES (?,?,?);"

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
      "SELECT image_content FROM post_image WHERE post_id=?;"

  let get_post_image_name =
    Caqti_request.find_opt Caqti_type.string Caqti_type.string
      "SELECT image_name FROM post_image WHERE post_id=?;"

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

  (* TODO return bool *)
  let is_thread =
    Caqti_request.find_opt Caqti_type.string Caqti_type.string
      "SELECT thread_id FROM threads WHERE thread_id=? LIMIT 1;"

  let get_post_subject =
    Caqti_request.find_opt Caqti_type.string Caqti_type.string
      "SELECT subject FROM post_subject WHERE post_id=?;"

  let get_post_gps =
    Caqti_request.find_opt Caqti_type.string
      Caqti_type.(tup2 float float)
      "SELECT lat, lng FROM post_gps WHERE post_id=?;"

  let list_thread_ids =
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

(* TODO should I escape html or smthing ?*)
let parse_comment comment =
  let words = String.split_on_char ' ' comment in
  let cited_posts, words =
    List.fold_left
      (fun (acc_cited, acc_posts) w ->
        match String.starts_with ~prefix:">>" w with
        | false -> (acc_cited, acc_posts @ [ w ])
        | true -> (
          let sub_w = String.sub w 2 (String.length w - 2) in
          match Uuidm.of_string sub_w with
          | None -> (acc_cited, acc_posts @ [ w ])
          | Some _ ->
            let new_w = Format.sprintf {|<a href="#%s">%s</a>|} sub_w w in
            (acc_cited @ [ sub_w ], acc_posts @ [ new_w ]) ) )
      ([], []) words
  in
  let comment = String.concat (String.make 1 ' ') words in
  (* remove duplicate *)
  let cited_posts = List.sort_uniq (fun _ _ -> 0) cited_posts in
  (comment, cited_posts)

let view_post post_id =
  let* nick = Db.find Q.get_post_nick post_id in
  let* comment = Db.find Q.get_post_comment post_id in
  let* date = Db.find Q.get_post_date post_id in
  let* image_name = Db.find_opt Q.get_post_image_name post_id in

  let* _tags = Db.fold Q.get_post_tags (fun tag acc -> tag :: acc) post_id [] in
  let* replies =
    Db.fold Q.get_post_replies (fun reply_id acc -> reply_id :: acc) post_id []
  in

  (* TODO special stuff for OP
     let* _subject = Db.find_opt Q.get_post_subject post_id in
     let* _latlng = Db.find_opt Q.get_post_gps post_id in
  *)
  let image_view =
    match image_name with
    | Some image_name ->
      (*TODO thumbnails *)
      Format.sprintf
        {|
    <div class="postImage">
        <a title="%s" href="/post_pic/%s">
        <img src="/post_pic/%s" style="width:200px;height:200px;" loading="lazy">
        </a>
    </div> 
|}
        image_name post_id post_id
    | None -> ""
  in
  let replies_view =
    {|<div class="repliesLink">|}
    ^ String.concat " "
        (List.map
           (fun reply_id ->
             Format.sprintf {|<a class="replyLink" href="#%s">>>%s</a>|}
               reply_id reply_id )
           replies )
    ^ {|</div> |}
  in
  (* TODO how to display date, I should probably render everything on the client*)
  let post_info_view =
    Format.sprintf
      {|
    <div class="postInfo"
        <span class=nick>%s</span>
        <span class=date unix-time="%d"></span>
        <span class=postNo>
            <a href="#%s" title="Link to this post">No.</a>
            <a href="javascript:insert_quote('%s')" "class=quoteLink title="Reply to this post">%s</a>
        </span>
        %s
    </div>|}
      nick date post_id post_id post_id replies_view
  in
  let post_view =
    Format.sprintf
      {|
<div class="container">
        <div class="post" id="%s">
            %s
            %s
            <blockquote class="postComment">%s</blockquote> 
        </div> 
</div> 
|}
      post_id post_info_view image_view comment
  in
  Ok post_view

let view_thread thread_id =
  let** _ = Db.find_opt Q.is_thread thread_id in
  let* thread_posts =
    Db.fold Q.get_thread_posts (fun post_id acc -> post_id :: acc) thread_id []
  in
  (*order by date *)
  (*TODO do this more clean *)
  let dates =
    List.map (fun post_id -> Db.find Q.get_post_date post_id) thread_posts
  in
  match List.find_opt Result.is_error dates with
  | Some (Error e) -> Error (Format.sprintf "db error: %s" (Caqti_error.show e))
  | Some (Ok _) -> assert false
  | None ->
    let dates =
      List.map
        (function
          | Ok date -> date
          | Error _ -> assert false )
        dates
    in
    let posts_dates = List.combine thread_posts dates in
    let sorted_posts_dates =
      List.sort (fun (_, a) (_, b) -> compare a b) posts_dates
    in

    let posts, _ = List.split sorted_posts_dates in
    let view_posts = List.map view_post posts in
    let view_posts =
      List.map
        (function
          | Ok view -> view
          | Error _ -> assert false )
        (List.filter Result.is_ok view_posts)
    in
    Ok (String.concat "\n\r" view_posts)

let upload_post post =
  let post_id, parent_id, date, nick, comment, image, tags, citations, op_data =
    match post with
    | Op
        { id
        ; date
        ; nick
        ; subject
        ; comment
        ; image
        ; tags
        ; longitude
        ; latitude
        ; replies = _replies
        ; citations
        } ->
      let op_data = Some (subject, longitude, latitude) in
      (id, id, date, nick, comment, Some image, tags, citations, op_data)
    | Reply
        { id
        ; parent_id
        ; date
        ; nick
        ; comment
        ; image
        ; tags
        ; replies = _replies
        ; citations
        } ->
      (id, parent_id, date, nick, comment, image, tags, citations, None)
  in
  let* _res_post_id = Db.exec Q.upload_post_id (post_id, nick) in
  let* _res_comment = Db.exec Q.upload_post_comment (post_id, comment) in
  let* _res_date = Db.exec Q.upload_post_date (post_id, date) in
  let* _res_thread = Db.exec Q.upload_to_thread (parent_id, post_id) in
  let* _res_image =
    match image with
    | None -> Ok ()
    | Some (image_name, image_content) ->
      Db.exec Q.upload_post_image (post_id, image_name, image_content)
  in
  let* _res_parent = Db.exec Q.upload_post_parent (post_id, parent_id) in
  let* _res_tags =
    match
      List.find_opt Result.is_error
        (List.map (fun tag -> Db.exec Q.upload_post_tag (post_id, tag)) tags)
    with
    | Some (Error e) -> Error e
    | Some _ -> assert false
    | None -> Ok ()
  in
  let* _res_citations =
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
  match op_data with
  | None -> Ok post_id
  | Some (subject, lng, lat) ->
    let* _res_gps = Db.exec Q.upload_post_gps (post_id, lat, lng) in
    let* _res_subject = Db.exec Q.upload_post_subject (post_id, subject) in
    Ok post_id

let make_reply ~comment ?image ~tags ~parent_id nick =
  if String.length comment > 10000 then
    Error "invalid comment"
  else
    let image =
      match image with
      | Some (Some image_name, image_content) -> Some (image_name, image_content)
      | Some (None, image_content) ->
        (* make up random name if no name was given *)
        let image_name = Uuidm.to_string (Uuidm.v4_gen random_state ()) in
        Some (image_name, image_content)
      | None -> None
    in
    let is_valid =
      match image with
      | None -> true
      | Some (_, image_content) -> is_valid_image image_content
    in
    if not is_valid then
      Error "invalid image"
    else if String.length tags > 1000 then
      Error "invalid tags"
    else
      (* TODO latlng validation? *)
      let tag_list = Str.split (Str.regexp " +") tags in
      let id = Uuidm.to_string (Uuidm.v4_gen random_state ()) in
      let date = int_of_float (Unix.time ()) in
      let comment, citations = parse_comment comment in
      let reply =
        Reply
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
      upload_post reply

let make_op ~comment ~image ~tags ~subject ~lat ~lng nick =
  if String.length comment > 10000 then
    Error "invalid comment"
  else
    let image =
      match image with
      | Some image_name, image_content -> (image_name, image_content)
      | None, image_content ->
        (* make up random name if no name was given *)
        let image_name = Uuidm.to_string (Uuidm.v4_gen random_state ()) in
        (image_name, image_content)
    in
    let is_valid =
      match image with
      | _, image_content -> is_valid_image image_content
    in
    if not is_valid then
      Error "invalid image"
    else if String.length tags > 1000 then
      Error "invalid tags"
    else
      (* TODO latlng validation? *)
      let is_valid_latlng = true in
      if not is_valid_latlng then
        Error "Invalid coordinate"
      else if String.length subject > 600 then
        Error "Invalid subject"
      else
        let tag_list = Str.split (Str.regexp " +") tags in
        let id = Uuidm.to_string (Uuidm.v4_gen random_state ()) in
        let date = int_of_float (Unix.time ()) in
        let comment, citations = parse_comment comment in
        let op =
          Op
            { id
            ; date
            ; nick
            ; subject
            ; comment
            ; image
            ; tags = tag_list
            ; longitude = lng
            ; latitude = lat
            ; replies = []
            ; citations
            }
        in
        upload_post op

(* TODO make this return geojson directly *)
let marker_list () =
  let* thread_id_list =
    Db.fold Q.list_thread_ids (fun thread_id acc -> thread_id :: acc) () []
  in
  let markers_res =
    List.map
      (fun thread_id ->
        match Db.find_opt Q.get_post_gps thread_id with
        | Ok (Some (lat, lng)) ->
          let content =
            match view_post thread_id with
            | Ok s -> s
            | Error e -> e
          in
          Ok (lat, lng, content, thread_id)
        | Ok None -> Error "latlng not found"
        | Error e -> Error (Format.sprintf "db error: %s" (Caqti_error.show e))
        )
      thread_id_list
  in
  let markers =
    List.map
      (function
        | Ok res -> res
        | Error _ -> assert false )
      (List.filter Result.is_ok markers_res)
  in
  Ok markers

let marker_to_geojson marker =
  match marker with
  | lat, lng, content, thread_id ->
    Format.sprintf
      {|
{
  "type": "Feature",
  "geometry": {
    "type": "Point",
    "coordinates": [%s,%s]
  },
  "properties": {
    "content": "%s",
    "thread_id": "%s"
  }
} 
|}
      (* geojson use lng lat, and not lat lng*)
      (Float.to_string lng)
      (Float.to_string lat) (String.escaped content) thread_id

let get_post_image post_id =
  let** content = Db.find_opt Q.get_post_image_content post_id in
  Ok content
