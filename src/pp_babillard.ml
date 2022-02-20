include Bindings
include Babillard
open Db

let pp_post fmt ~hide_replies post =
  let { id
      ; parent_id = _parent_id
      ; date
      ; nick
      ; comment
      ; image_info
      ; tags
      ; replies
      ; citations = _citations
      } =
    post
  in

  let image_view fmt () =
    match image_info with
    | Some (_image_name, image_alt) ->
      Format.fprintf fmt
        {|
    <div class="post-image-container">
        <a href="/img/%s">
        <img class="post-image" src="/img/%s" alt="%s" title="%s" loading="lazy">
        </a>
    </div> 
|}
        id id image_alt image_alt
    | None -> Format.fprintf fmt ""
  in

  let pp_print_reply fmt reply =
    Format.fprintf fmt {|<a class="reply-link" href="#%s">&gt;&gt;%s</a>|} reply
      reply
  in
  let pp_print_replies fmt replies =
    Format.fprintf fmt {|<div class="replies">%a</div>|}
      (Format.pp_print_list ~pp_sep:Format.pp_print_space pp_print_reply)
      replies
  in

  let replies_view fmt () =
    if hide_replies then
      (* TODO put thread_posts count in thread_info ? *)
      let res_nb = Db.find Q.count_thread_posts id in
      match res_nb with
      | Error _ -> Format.fprintf fmt ""
      | Ok ((1 | 2) as nb) ->
        Format.fprintf fmt {|<div class="replies">%d reply</div>|} (nb - 1)
      | Ok nb ->
        Format.fprintf fmt {|<div class="replies">%d replies</div>|} (nb - 1)
    else pp_print_replies fmt replies
  in

  let post_links_view fmt () =
    if hide_replies then
      Format.fprintf fmt {|
        %a
        |} replies_view ()
    else
      Format.fprintf fmt
        {|
        <span class=postNo>
            <a href="#%s" title="Link to this post" class="quote">#</a>
            <button data-id="%s" class="quote-link" title="Reply to this post">%s</button>
        </span>
        %a
        |}
        id id id replies_view ()
  in

  let post_info_view fmt () =
    Format.fprintf fmt
      {|
    <div class="post-info">
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

  let pp =
    Format.fprintf fmt
      {|
<div class="container">
        <div class="post" id="%s">
            %a
            %a
            <blockquote class="post-comment">%s</blockquote> 
            %a
        </div> 
</div> 
|}
      id post_info_view () image_view () comment tags_view ()
  in
  pp

let preview_thread thread_id =
  let* post_data = get_post thread_id in
  let^? subject, _lat, _lng = Db.find_opt Q.get_thread_info thread_id in
  let post =
    (Format.asprintf "%a" (fun fmt data -> pp_post fmt ~hide_replies:true data))
      post_data
  in
  let thread_preview =
    Format.sprintf
      {|
<div class="thread-preview">
    <div class="thread-subject">
        %s
    </div>
    %s
</div>
|}
      subject post
  in
  Ok thread_preview

let catalog_content () =
  Format.printf "catalog_content@.";
  let^ threads = Db.collect_list Q.get_threads () in
  let res_previews = List.map preview_thread threads in
  let res_opt = List.find_opt Result.is_error res_previews in
  if Option.is_some res_opt then Option.get res_opt
  else
    let previews = List.map Result.get_ok res_previews in
    Ok
      (Format.asprintf "%a"
         (Format.pp_print_list ~pp_sep:Format.pp_print_space
            Format.pp_print_string )
         previews )

let view_thread thread_id =
  let^ _is_thread = Db.find Q.get_is_thread thread_id in
  let^? subject, _lat, _lng = Db.find_opt Q.get_thread_info thread_id in
  let^ thread_posts = Db.collect_list Q.get_thread_posts thread_id in
  (*order by date *)
  let dates = List.map (Db.find Q.get_post_date) thread_posts in
  match List.find_opt Result.is_error dates with
  | Some (Error e) -> Error (Format.sprintf "db error: %s" (Caqti_error.show e))
  | Some (Ok _) -> assert false
  | None ->
    let dates = List.map Result.get_ok dates in
    let posts_dates = List.combine thread_posts dates in
    let sorted_posts_dates =
      List.sort (fun (_, a) (_, b) -> compare a b) posts_dates
    in

    let posts, _ = List.split sorted_posts_dates in
    let posts_data = List.map get_post posts in
    let res_opt = List.find_opt Result.is_error posts_data in
    if Option.is_some res_opt then
      let res = Result.get_error (Option.get res_opt) in
      Error res
    else
      let posts_data = List.map Result.get_ok posts_data in
      let posts =
        Format.asprintf "%a"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "\r\n")
             (fun fmt data -> pp_post fmt ~hide_replies:false data) )
          posts_data
      in
      let thread_view =
        Format.sprintf
          {|
<div class="thread">
    <div class="thread-subject">
        %s
    </div>
    <div class="thread-posts">
        %s
    </div>
</div>
|}
          subject posts
      in
      Ok thread_view

let get_markers () =
  let^ threads = Db.collect_list Q.get_threads () in
  let res_previews = List.map preview_thread threads in
  let res_infos = List.map (Db.find Q.get_thread_info) threads in

  let res_previews_opt = List.find_opt Result.is_error res_previews in
  let res_info_opt = List.find_opt Result.is_error res_infos in
  if Option.is_some res_previews_opt then Option.get res_previews_opt
  else if Option.is_some res_info_opt then
    Error
      (Result.fold
         ~ok:(assert false)
         ~error:Caqti_error.show (Option.get res_info_opt) )
  else
    let previews = List.map Result.get_ok res_previews in
    let infos = List.map Result.get_ok res_infos in
    let previews_infos = List.combine previews infos in
    let previews_infos_ids = List.combine previews_infos threads in

    let pp_marker fmt lat lng content thread_id =
      (* geojson use lng lat, and not lat lng*)
      let json =
        `Assoc
          [ ("type", `String "Feature")
          ; ( "geometry"
            , `Assoc
                [ ("type", `String "Point")
                ; ("coordinates", `List [ `Float lng; `Float lat ])
                ] )
          ; ( "properties"
            , `Assoc
                [ ("content", `String content)
                ; ("thread_id", `String thread_id)
                ] )
          ]
      in
      Yojson.pretty_print fmt json
    in

    let markers =
      Format.asprintf "[%a]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
           (fun fmt ((preview, (_sub, lat, lng)), id) ->
             pp_marker fmt lat lng preview id ) )
        previews_infos_ids
    in
    Ok markers
