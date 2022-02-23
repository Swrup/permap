include Bindings
include Babillard
open Db

let pp_post fmt t =
  let thread_data_opt, post =
    match t with
    | Op (data, post) -> (Some data, post)
    | Post post -> (None, post)
  in
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
    if Option.is_some thread_data_opt then
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
    if Option.is_some thread_data_opt then
      Format.fprintf fmt {|
        %a
        |} replies_view ()
    else
      Format.fprintf fmt
        {|
        <span class=postNo>
            <button data-id="%s" class="quote-link" title="Reply to this post">%s</button>
        </span>
        %a
        |}
        id id replies_view ()
  in

  let post_info_view fmt () =
    Format.fprintf fmt
      {|
    <div class="post-info">
        <span class="nick">%s</span>
        <span class="date" data-time="%f"></span>
        <div class="dropend post-menu-div">
          <a class="dropdown-toggle post-menu-link" href="#" role="button" id="dropdownMenuLink" data-bs-toggle="dropdown" aria-expanded="false">
          </a>
          <ul class="dropdown-menu post-menu-content" aria-labelledby="dropdownMenuLink">
            <li><a class="dropdown-item" href="#%s">Link to this post</a></li>
            <li><a class="dropdown-item" href="/delete/%s">Delete</a></li>
            <li><a class="dropdown-item" href="/report/%s">Report</a></li>
          </ul>
        </div>
        %a
    </div>|}
      nick date id id id post_links_view ()
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

  let subject =
    Option.fold ~none:""
      ~some:(fun thread_data -> thread_data.subject)
      thread_data_opt
  in
  (* put a link in if its a preview *)
  let link fmt () =
    if Option.is_some thread_data_opt then
      Format.fprintf fmt
        {|<a class="stretched-link preview-link" href="/thread/%s"></a>|} id
  in

  Format.fprintf fmt
    {|
    <div class="position-relative post" id="%s">
        <div class="thread-subject">
            %s
        </div>
        %a
        %a
        %a
        <blockquote class="post-comment">%s</blockquote> 
        %a
    </div> 
|}
    id subject link () post_info_view () image_view () comment tags_view ()

let view_post id =
  let* post = get_post id in
  Ok (Format.asprintf "%a" pp_post (Post post))

let pp_thread_preview fmt op =
  let thread_data, post = op in
  let thread_preview =
    Format.fprintf fmt
      {|
    <div class="thread-preview">
        %a
    </div>
|}
      pp_post
      (Op (thread_data, post))
  in
  thread_preview

let catalog_content () =
  let^ ids = Db.collect_list Q.get_threads () in
  let* ops = get_ops ids in
  Ok
    (Format.asprintf "%a"
       (Format.pp_print_list ~pp_sep:Format.pp_print_space pp_thread_preview)
       ops )

let pp_thread fmt op posts =
  let thread_data, _post = op in
  (*order by date *)
  let posts = List.sort (fun a b -> compare a.date b.date) posts in
  let posts_view fmt () =
    Format.pp_print_list ~pp_sep:Format.pp_print_space
      (fun fmt post -> pp_post fmt (Post post))
      fmt posts
  in
  Format.fprintf fmt
    {|
<div class="thread">
    <div class="thread-subject">
        %s
    </div>
    <div class="thread-posts">
        %a
    </div>
</div>
|}
    thread_data.subject posts_view ()

let view_thread thread_id =
  let* op = get_op thread_id in
  let^ ids = Db.collect_list Q.get_thread_posts thread_id in
  let* posts = get_posts ids in
  let s =
    (Format.asprintf "%a" (fun fmt (op, posts) -> pp_thread fmt op posts))
      (op, posts)
  in
  Ok s

let pp_marker fmt op =
  let thread_data, post = op in
  let content = Format.asprintf "%a" pp_thread_preview op in
  (* geojson use lng lat, and not lat lng*)
  let json =
    `Assoc
      [ ("type", `String "Feature")
      ; ( "geometry"
        , `Assoc
            [ ("type", `String "Point")
            ; ( "coordinates"
              , `List [ `Float thread_data.lng; `Float thread_data.lat ] )
            ] )
      ; ( "properties"
        , `Assoc
            [ ("content", `String content); ("thread_id", `String post.id) ] )
      ]
  in
  Yojson.pretty_print fmt json

let get_markers () =
  let^ ids = Db.collect_list Q.get_threads () in
  let* ops = get_ops ids in
  let markers =
    Format.asprintf "[%a]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
         pp_marker )
      ops
  in
  Ok markers
