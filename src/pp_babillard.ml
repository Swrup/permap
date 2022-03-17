include Bindings
include Babillard
open Db

type moderation_action =
  | Ignore
  | Delete
  | Banish

let moderation_action_to_string = function
  | Ignore -> "ignore"
  | Delete -> "delete"
  | Banish -> "banish"

let moderation_action_from_string = function
  | "ignore" -> Some Ignore
  | "delete" -> Some Delete
  | "banish" -> Some Banish
  | _ -> None

let pp_post fmt t =
  let thread_data_opt, post =
    match t with
    | Op (data, post) -> (Some data, post)
    | Post post -> (None, post)
  in
  let { id
      ; parent_id = _parent_id
      ; date
      ; user_id
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
        <span class="nick" data-user-id="%s">%s</span>
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
      user_id nick date id id id post_links_view ()
  in

  let pp_print_category fmt category =
    Format.fprintf fmt {|<span class="category tag">%s</span>|} category
  in
  let pp_print_tag fmt tag =
    Format.fprintf fmt {|<span class="tag">%s</span>|} tag
  in
  let pp_print_tags fmt tags =
    let categories, tags =
      List.partition (fun tag -> List.mem tag App.categories) tags
    in
    let categories = List.sort String.compare categories in
    let tags = List.sort String.compare tags in
    Format.fprintf fmt {|<div class="tags">%a%a</div>|}
      (Format.pp_print_list ~pp_sep:Format.pp_print_space pp_print_category)
      categories
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

let pp_report fmt post report request =
  let url = "/admin" in
  let _reporter_id, reporter_nick, reason, _date, id = report in
  let input_post_id fmt id =
    Format.fprintf fmt
      {|<input value="%s" name="post_id" type="hidden"></input>|} id
  in
  let button fmt action =
    let s = moderation_action_to_string action in
    Format.fprintf fmt
      {|<button value="%s" name="action" type="submit" class="btn btn-primary">%s</button>|}
      s (String.uppercase_ascii s)
  in
  let form fmt action =
    Format.fprintf fmt {|%s %a %a </form>|}
      (Dream.form_tag ~action:url request)
      input_post_id id button action
  in

  Format.fprintf fmt
    {|
<div class="report">
    <div class="row mb-3">
        <div class="col-md-6">
            %a
        </div>
        <div class="col-md-6">
            <span> From: %s Reason: %s</span>
            <div>
                %a
                </form><br>
                %a
                </form><br>
                %a
                </form><br>
            </div>
        </div>
    </div>
</div><br>
|}
    pp_post (Post post) reporter_nick reason form Ignore form Delete form Banish

let admin_page_content posts reports request =
  let posts_reports = List.combine posts reports in
  Format.asprintf "%a"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space
       (fun fmt (post, report) -> pp_report fmt post report request) )
    posts_reports

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
        <h1>%s</h1>
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

let pp_checkboxes fmt () =
  let pp_checkbox fmt category =
    Format.fprintf fmt
      {|
<div class="form-check col">
    <input name="category" id="category-%s" type="checkbox" class"form-check-input" value="%s">
    <label class="form-check-label" for="category-%s">%s</label>
</div>
|}
      category category category category
  in
  Format.fprintf fmt
    {| 
<div class="row">
   %a
</div>|}
    (Format.pp_print_list ~pp_sep:Format.pp_print_space pp_checkbox)
    App.categories

(* RFC-3339 date-time *)
let pp_date fmt date =
  let date = Unix.gmtime date in
  Format.fprintf fmt "%04d-%02d-%02dT%02d:%02d:%02dZ" (1900 + date.tm_year)
    (1 + date.tm_mon) date.tm_mday date.tm_hour date.tm_min date.tm_sec

let pp_feed_entry fmt post =
  Format.fprintf fmt
    {|
  <entry>
    <title></title>
    <id>urn:uuid:%s</id>
    <updated>%a</updated>
  <author>
    <name>%s</name>
  </author>
    <content type="html">%s</content>
    <link rel="alternate" href="%s/thread/%s#%s"/>
  </entry>
    |}
    post.id pp_date post.date post.nick
    (Dream.html_escape post.comment)
    App.hostname post.parent_id post.id

let feed thread_id =
  let* thread_data, op_post = get_op thread_id in
  let^ ids = Db.collect_list Q.get_thread_posts thread_id in
  let* posts = get_posts ids in
  let posts = List.sort (fun a b -> compare b.date a.date) posts in
  let* last_update =
    match posts with [] -> Error "empty thread" | op :: _l -> Ok op.date
  in

  let entries fmt () =
    (Format.pp_print_list ~pp_sep:Format.pp_print_space pp_feed_entry) fmt posts
  in
  let feed =
    Format.asprintf
      {|<?xml version="1.0" encoding="utf-8"?>
<feed xmlns="http://www.w3.org/2005/Atom">
  <title>%s</title>
  <link rel="self" href="%s/thread/%s"/>
  <updated>%a</updated>
  <author>
    <name>%s</name>
  </author>
  <id>urn:uuid:%s</id>
  %a
</feed>|}
      thread_data.subject App.hostname thread_id pp_date last_update
      op_post.nick op_post.id entries ()
  in
  Ok feed
