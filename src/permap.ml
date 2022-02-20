let get_title content =
  let open Soup in
  try
    let soup = content |> parse in
    soup $ "h1" |> R.leaf_text
  with Failure _e -> "Permap"

let render ?title content request =
  let title =
    match title with None -> get_title content | Some title -> title
  in
  Dream.html
  @@ Template.render_unsafe ~title:(Dream.html_escape title)
       ~content:(Dream.html_escape content)
       request

let render_unsafe ?title content request =
  let title =
    match title with None -> get_title content | Some title -> title
  in
  Dream.html @@ Template.render_unsafe ~title ~content request

let asset_loader _root path _request =
  match Content.read ("assets/" ^ path) with
  | None -> Dream.empty `Not_Found
  | Some asset -> Dream.respond asset

let page name request =
  match Content.read (name ^ ".md") with
  | None -> Dream.empty `Not_Found
  | Some page ->
    let content = Omd.of_string page |> Omd.to_html in
    render_unsafe content request

let about request = page "about" request

let register_get request = render_unsafe (Register.f request) request

let register_post request =
  match%lwt Dream.form request with
  | `Ok [ ("email", email); ("nick", nick); ("password", password) ] ->
    render_unsafe (Register.f ~nick ~email ~password request) request
  | `Ok _ | `Many_tokens _ | `Missing_token _ | `Invalid_token _
  | `Wrong_session _ | `Expired _ | `Wrong_content_type ->
    assert false

let login_get request = render_unsafe (Login.f request) request

let login_post request =
  match%lwt Dream.form request with
  | `Ok [ ("nick", nick); ("password", password) ] ->
    render_unsafe (Login.f ~nick ~password request) request
  | _ -> assert false

let users request =
  render_unsafe (Result.fold ~ok:Fun.id ~error:Fun.id (User.list ())) request

let user_profile request =
  let nick = Dream.param request "user" in
  if User.exists nick then
    render_unsafe
      (Result.fold ~ok:Fun.id ~error:Fun.id (User.public_profile nick))
      request
  else Dream.respond ~status:`Not_Found "404: User does not exists"

let logout request =
  let _ = Dream.invalidate_session request in
  let content = "Logged out !" in
  render_unsafe content request

let profile_get request =
  match Dream.session "nick" request with
  | None -> render_unsafe "Not logged in" request
  | Some nick ->
    let bio = match User.get_bio nick with Ok bio -> bio | Error e -> e in
    render_unsafe (User_profile.f nick bio request) request

let profile_post request =
  match Dream.session "nick" request with
  | None -> render_unsafe "Not logged in" request
  | Some nick -> (
    match%lwt Dream.form request with
    | `Ok [ ("bio", bio) ] -> (
      match User.update_bio bio nick with
      | Ok () ->
        Dream.respond ~status:`See_Other
          ~headers:[ ("Location", "/profile") ]
          "Your bio was updated!"
      | Error e -> render_unsafe e request )
    | `Ok _ | `Many_tokens _ | `Missing_token _ | `Invalid_token _
    | `Wrong_session _ | `Expired _ | `Wrong_content_type -> (
      match%lwt Dream.multipart request with
      | `Ok [ ("file", file) ] -> (
        match User.upload_avatar file nick with
        | Ok () ->
          Dream.respond ~status:`See_Other
            ~headers:[ ("Location", "/profile") ]
            "Your avatar was updated!"
        | Error e -> render_unsafe e request )
      | `Ok _ -> Dream.empty `Bad_Request
      | `Expired _ | `Many_tokens _ | `Missing_token _ | `Invalid_token _
      | `Wrong_session _ | `Wrong_content_type ->
        Dream.empty `Bad_Request ) )

let avatar_image request =
  let nick = Dream.param request "user" in
  if User.exists nick then
    let avatar = User.get_avatar nick in
    match avatar with
    | Ok (Some avatar) ->
      Dream.respond ~headers:[ ("Content-Type", "image") ] avatar
    | Ok None | Error _ -> (
      match Content.read "/assets/img/default_avatar.png" with
      | None -> failwith "can't find default avatar"
      | Some avatar ->
        Dream.respond ~headers:[ ("Content-Type", "image") ] avatar )
  else Dream.respond ~status:`Not_Found "404: User does not exists"

let post_image request =
  let post_id = Dream.param request "post_id" in
  if Babillard.post_exists post_id then
    let image = Babillard.get_post_image_content post_id in
    match image with
    | Ok image -> Dream.respond ~headers:[ ("Content-Type", "image") ] image
    | Error _ -> Dream.empty `Not_Found
  else Dream.respond ~status:`Not_Found "404: Image does not exists"

let markers request =
  let markers = Pp_babillard.get_markers () in
  match markers with
  | Ok markers ->
    Dream.respond ~headers:[ ("Content-Type", "application/json") ] markers
  | Error e -> render_unsafe e request

let babillard_get request = render_unsafe (Babillard_page.f request) request

let newthread_get request = render_unsafe (Newthread_page.f request) request

let newthread_post request =
  match Dream.session "nick" request with
  | None -> render_unsafe "Not logged in" request
  | Some nick -> (
    match%lwt Dream.multipart request with
    | `Ok
        [ ("alt", [ (_, alt) ])
        ; ("file", file)
        ; ("lat-input", [ (_, lat) ])
        ; ("lng-input", [ (_, lng) ])
        ; ("subject", [ (_, subject) ])
        ; ("tags", [ (_, tags) ])
        ; ("thread-comment", [ (_, comment) ])
        ] -> (
      match (Float.of_string_opt lat, Float.of_string_opt lng) with
      | None, _ -> render_unsafe "Invalide coordinate" request
      | _, None -> render_unsafe "Invalide coordinate" request
      | Some lat, Some lng -> (
        let res =
          match file with
          | [] -> Babillard.make_op ~comment ~lat ~lng ~subject ~tags nick
          | _ :: _ :: _ -> Error "More than one image"
          | [ (image_name, image_content) ] ->
            let image = ((image_name, alt), image_content) in
            Babillard.make_op ~comment ~image ~lat ~lng ~subject ~tags nick
        in
        match res with
        | Ok thread_id ->
          let adress = Format.asprintf "/thread/%s" thread_id in
          Dream.respond ~status:`See_Other
            ~headers:[ ("Location", adress) ]
            "Your thread was posted!"
        | Error e -> render_unsafe e request ) )
    | `Ok _ -> Dream.empty `Bad_Request
    | `Expired _ | `Many_tokens _ | `Missing_token _ | `Invalid_token _
    | `Wrong_session _ | `Wrong_content_type ->
      Dream.empty `Bad_Request )

let thread_get request =
  let thread_id = Dream.param request "thread_id" in
  if Babillard.thread_exists thread_id then
    let thread_view = Pp_babillard.view_thread thread_id in
    let res =
      match thread_view with
      | Error e -> e
      | Ok thread_view -> Thread_page.f thread_view thread_id request
    in
    render_unsafe res request
  else Dream.respond ~status:`Not_Found "404: Thread not found"

(*form to reply to a thread *)
let reply_post request =
  match Dream.session "nick" request with
  | None -> render_unsafe "Not logged in" request
  | Some nick -> (
    match%lwt Dream.multipart request with
    | `Ok
        [ ("alt", [ (_, alt) ])
        ; ("file", file)
        ; ("reply-comment", [ (_, comment) ])
        ; ("tags", [ (_, tags) ])
        ] -> (
      let parent_id = Dream.param request "thread_id" in
      let res =
        match file with
        | [] -> Babillard.make_reply ~comment ~tags ~parent_id nick
        | [ (image_name, image_content) ] ->
          let image = ((image_name, alt), image_content) in
          Babillard.make_reply ~comment ~image ~tags ~parent_id nick
        | _ :: _ :: _ -> Error "More than one image"
      in
      match res with
      | Ok post_id ->
        let adress = Format.sprintf "/thread/%s#%s" parent_id post_id in
        Dream.respond ~status:`See_Other
          ~headers:[ ("Location", adress) ]
          "Your reply was posted!"
      | Error e -> render_unsafe e request )
    | `Ok _ -> Dream.empty `Bad_Request
    | `Expired _ | `Many_tokens _ | `Missing_token _ | `Invalid_token _
    | `Wrong_session _ | `Wrong_content_type ->
      Dream.empty `Bad_Request )

let routes =
  (* this is just so that they're visually aligned *)
  let get_ = Dream.get in
  let post = Dream.post in

  [ get_ "/" babillard_get
  ; get_ "/about" about
  ; get_ "/assets/**" (Dream.static ~loader:asset_loader "")
  ; get_ "/img/:post_id" post_image
  ; get_ "/login" login_get
  ; post "/login" login_post
  ; get_ "/logout" logout
  ; get_ "/markers" markers
  ; get_ "/new_thread" newthread_get
  ; post "/new_thread" newthread_post
  ; get_ "/post_pic/:post_id" post_image
  ; get_ "/profile" profile_get
  ; post "/profile" profile_post
  ; get_ "/users" users
  ; get_ "/user/:user" user_profile
  ; get_ "/user/:user/avatar" avatar_image
  ; get_ "/thread/:thread_id" thread_get
  ; post "/thread/:thread_id" reply_post
  ]
  @
  if App.open_registration then
    [ get_ "/register" register_get; post "/register" register_post ]
  else []

let () =
  let logger = if App.log then Dream.logger else Fun.id in
  Dream.run ~port:App.port @@ logger @@ Dream.cookie_sessions
  (* this should replace memory/cookie sessions but it doesn't work :-(
     @@ Dream.sql_pool Db.db_uri
     @@ Dream.sql_sessions
  *)
  @@ Dream.router routes
