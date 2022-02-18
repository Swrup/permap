include Bindings
include Babillard
open Db

let view_post ?is_thread_preview post_id =
  let^ nick = Db.find Q.get_post_nick post_id in
  let^ comment = Db.find Q.get_post_comment post_id in
  let^ date = Db.find Q.get_post_date post_id in
  let^ image_info = Db.find_opt Q.get_post_image_info post_id in

  let^ tags = Db.collect_list Q.get_post_tags post_id in
  let^ replies = Db.collect_list Q.get_post_replies post_id in

  let image_view fmt () =
    match image_info with
    | Some (_image_name, image_alt) ->
      (*TODO thumbnails *)
      (*TODO image info like file name and size on top of image*)
      Format.fprintf fmt
        {|
    <div class="postImageContainer">
        <a href="/post_pic/%s">
        <img class="postImage" src="/post_pic/%s" alt="%s" title="%s" loading="lazy">
        </a>
    </div> 
|}
        post_id post_id image_alt image_alt
    | None -> Format.fprintf fmt ""
  in

  let pp_print_reply fmt reply =
    Format.fprintf fmt {|<a class="replyLink" href="#%s">&gt;&gt;%s</a>|} reply
      reply
  in
  let pp_print_replies fmt replies =
    Format.fprintf fmt {|<div class="replies">%a</div>|}
      (Format.pp_print_list ~pp_sep:Format.pp_print_space pp_print_reply)
      replies
  in

  let replies_view fmt () =
    match is_thread_preview with
    | None -> pp_print_replies fmt replies
    | Some () -> (
      let res_nb = Db.find Q.count_thread_posts post_id in
      match res_nb with
      | Error _ -> Format.fprintf fmt ""
      | Ok ((1 | 2) as nb) ->
        Format.fprintf fmt {|<div class="replies">%d reply</div>|} (nb - 1)
      | Ok nb ->
        Format.fprintf fmt {|<div class="replies">%d replies</div>|} (nb - 1) )
  in

  let post_links_view fmt () =
    match is_thread_preview with
    | None ->
      Format.fprintf fmt
        {|
        <span class=postNo>
            <a href="#%s" title="Link to this post">#</a>
            <a href="javascript:insert_quote('%s')" class="quoteLink" title="Reply to this post">%s</a>
        </span>
        %a
        |}
        post_id post_id post_id replies_view ()
    | Some () -> Format.fprintf fmt {|
        %a
        |} replies_view ()
  in

  let post_info_view fmt () =
    Format.fprintf fmt
      {|
    <div class="postInfo">
        <span class="nick">%s</span>
        <span class="date" data-time="%d"></span>
        %a
    </div>|}
      nick date post_links_view ()
  in

  let pp_print_tag fmt tag =
    Format.fprintf fmt {|<span class="tag">%s</span>|} tag
  in
  let pp_print_tags fmt tags =
    Format.fprintf fmt {|<div class="tags">%a</div>|}
      (Format.pp_print_list ~pp_sep:Format.pp_print_space pp_print_tag)
      tags
  in
  let tags = List.sort String.compare tags in
  let tags_view fmt () = pp_print_tags fmt tags in

  let post_view =
    Format.asprintf
      {|
<div class="container">
        <div class="post" id="%s">
            %a
            %a
            <blockquote class="postComment">%s</blockquote> 
            %a
        </div> 
</div> 
|}
      post_id post_info_view () image_view () comment tags_view ()
  in
  Ok post_view

let preview_thread thread_id =
  let* post = view_post ~is_thread_preview:() thread_id in
  let^? subject = Db.find_opt Q.get_post_subject thread_id in
  let thread_preview =
    Format.sprintf
      {|
<div class="threadPreview">
    <div class="threadSubject">
        %s
    </div>
    %s
</div>
|}
      subject post
  in
  Ok thread_preview

let view_thread thread_id =
  let^ _is_thread = Db.find Q.get_thread thread_id in
  let^? subject = Db.find_opt Q.get_post_subject thread_id in
  let^ thread_posts = Db.collect_list Q.get_thread_posts thread_id in
  (*order by date *)
  let dates = List.map (Db.find Q.get_post_date) thread_posts in
  match List.find_opt Result.is_error dates with
  | Some (Error e) -> Error (Format.sprintf "db error: %s" (Caqti_error.show e))
  | Some (Ok _) -> assert false
  | None -> (
    let dates = List.map Result.get_ok dates in
    let posts_dates = List.combine thread_posts dates in
    let sorted_posts_dates =
      List.sort (fun (_, a) (_, b) -> compare a b) posts_dates
    in

    let posts, _ = List.split sorted_posts_dates in
    let view_posts = List.map view_post posts in
    match List.find_opt Result.is_error view_posts with
    | Some (Error e) -> Error e
    | Some (Ok _) -> assert false
    | None ->
      let posts =
        List.map Result.get_ok (List.filter Result.is_ok view_posts)
      in
      let posts =
        Format.asprintf "%a"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "\r\n")
             Format.pp_print_string )
          posts
      in
      let thread_view =
        Format.sprintf
          {|
<div class="thread">
    <div class="threadSubject">
        %s
    </div>
    <div class="threadPosts">
        %s
    </div>
</div>
|}
          subject posts
      in
      Ok thread_view )

let get_markers () =
  let^ thread_id_list = Db.collect_list Q.get_threads () in
  let markers_res =
    List.map
      (fun thread_id ->
        let^? lat, lng = Db.find_opt Q.get_post_gps thread_id in
        match preview_thread thread_id with
        | Ok content -> Ok (lat, lng, content, thread_id)
        | Error e -> Error e )
      thread_id_list
  in
  let markers = List.map Result.get_ok (List.filter Result.is_ok markers_res) in

  let pp_marker fmt (lat, lng, content, thread_id) =
    Format.fprintf fmt
      {|{
  "type": "Feature",
  "geometry": {
    "type": "Point",
    "coordinates": [%s,%s]
  },
  "properties": {
    "content": "%s",
    "thread_id": "%s"
  }}|}
      (* geojson use lng lat, and not lat lng*)
      (Float.to_string lng)
      (Float.to_string lat) (String.escaped content) thread_id
  in
  let markers =
    Format.asprintf "[%a]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
         pp_marker )
      markers
  in
  Ok markers
