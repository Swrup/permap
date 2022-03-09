include Bindings

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

let not_logged_in redirect request =
  let content =
    Format.sprintf
      {|Not logged in, please <a href="/login?redirect=%s">login<a> to access this page|}
      (Dream.to_percent_encoded redirect)
  in
  render_unsafe content request

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
  | `Ok [ ("email", email); ("nick", nick); ("password", password) ] -> (
    match User.register ~email ~nick ~password with
    | Error e -> render_unsafe e request
    | Ok () ->
      let res =
        Result.fold ~error:Fun.id
          ~ok:(fun _ -> "User created ! Welcome !")
          (User.login ~login:nick ~password request)
      in
      render_unsafe res request )
  | `Ok _ | `Many_tokens _ | `Missing_token _ | `Invalid_token _
  | `Wrong_session _ | `Expired _ | `Wrong_content_type ->
    Dream.empty `Bad_Request

let login_get request = render_unsafe (Login.f request) request

let login_post request =
  match%lwt Dream.form request with
  | `Ok [ ("login", login); ("password", password) ] -> (
    match User.login ~login ~password request with
    | Error e -> render_unsafe e request
    | Ok () ->
      let url =
        match Dream.query request "redirect" with
        | None -> "/"
        | Some redirect -> Dream.from_percent_encoded redirect
      in
      Dream.respond ~status:`See_Other
        ~headers:[ ("Location", url) ]
        "Logged in: Happy geo-posting!" )
  | `Ok _ | `Many_tokens _ | `Missing_token _ | `Invalid_token _
  | `Wrong_session _ | `Expired _ | `Wrong_content_type ->
    Dream.empty `Bad_Request

let admin_get request =
  match Dream.session "user_id" request with
  | None ->
    let redirect_url =
      Format.sprintf "/login?redirect=%s" (Dream.to_percent_encoded "/admin")
    in
    Dream.respond ~status:`See_Other ~headers:[ ("Location", redirect_url) ] ""
  | Some user_id ->
    if not (User.is_admin user_id) then Dream.respond ~status:`Forbidden ""
    else
      let res =
        match Babillard.get_reports () with
        | Error e -> e
        | Ok (posts, reports) ->
          Pp_babillard.admin_page_content posts reports request
      in
      render_unsafe res request

let admin_post request =
  match Dream.session "user_id" request with
  | None -> not_logged_in "/admin" request
  | Some user_id -> (
    if not (User.is_admin user_id) then Dream.respond ~status:`Forbidden ""
    else
      match%lwt Dream.form request with
      | `Ok [ ("action", action); ("post_id", id) ] -> (
        let res =
          match Babillard.get_post id with
          | Error _e as e -> e
          | Ok post -> (
            let evil_user_id = post.user_id in
            match action with
            | "delete" -> Babillard.try_delete_post ~user_id:evil_user_id id
            | "banish" -> User.banish evil_user_id
            | "ignore" -> Babillard.ignore_report id
            | a -> Error (Format.sprintf "invalid action: `%s`" a) )
        in
        match res with
        | Error e -> render_unsafe e request
        | Ok () ->
          Dream.respond ~status:`See_Other
            ~headers:[ ("Location", "/admin") ]
            "" )
      | `Ok _ | `Expired _ | `Many_tokens _ | `Missing_token _
      | `Invalid_token _ | `Wrong_session _ | `Wrong_content_type ->
        Dream.empty `Bad_Request )

let catalog request =
  let catalog_content =
    Result.fold ~ok:Fun.id ~error:Fun.id (Pp_babillard.catalog_content ())
  in
  render_unsafe (Catalog_page.f catalog_content) request

let delete_get request =
  let post_id = Dream.param request "post_id" in
  let post_preview =
    Result.fold ~ok:Fun.id ~error:Fun.id (Pp_babillard.view_post post_id)
  in
  render_unsafe (Delete_page.f post_preview post_id request) request

let delete_post request =
  let post_id = Dream.param request "post_id" in
  match Dream.session "user_id" request with
  | None -> not_logged_in (Format.sprintf "/delete/%s" post_id) request
  | Some user_id -> (
    (* match on Dream.form needed for hidden csrf field *)
    match%lwt Dream.form request with
    | `Ok [] -> (
      match Babillard.try_delete_post ~user_id post_id with
      | Error e -> render_unsafe e request
      | Ok () ->
        Dream.respond ~status:`See_Other
          ~headers:[ ("Location", "/") ]
          "Your post was deleted!" )
    | `Ok _ | `Expired _ | `Many_tokens _ | `Missing_token _ | `Invalid_token _
    | `Wrong_session _ | `Wrong_content_type ->
      Dream.empty `Bad_Request )

let report_get request =
  let post_id = Dream.param request "post_id" in
  let post_preview =
    Result.fold ~ok:Fun.id ~error:Fun.id (Pp_babillard.view_post post_id)
  in
  render_unsafe (Report_page.f post_preview post_id request) request

let report_post request =
  let post_id = Dream.param request "post_id" in
  match Dream.session "user_id" request with
  | None -> not_logged_in (Format.sprintf "/report/%s" post_id) request
  | Some user_id -> (
    match%lwt Dream.form request with
    | `Ok [ ("reason", reason) ] ->
      let res =
        match Babillard.report ~user_id ~reason post_id with
        | Error e -> e
        | Ok () -> "The post was reported!"
      in
      render_unsafe res request
    | `Ok _ | `Expired _ | `Many_tokens _ | `Missing_token _ | `Invalid_token _
    | `Wrong_session _ | `Wrong_content_type ->
      Dream.empty `Bad_Request )

let user request =
  render_unsafe (Result.fold ~ok:Fun.id ~error:Fun.id (User.list ())) request

let user_profile request =
  let nick = Dream.param request "user" in
  match User.get_user_id_from_nick nick with
  | Error _e -> Dream.respond ~status:`Not_Found "User does not exists"
  | Ok user_id ->
    render_unsafe
      (Result.fold ~ok:Fun.id ~error:Fun.id (User.public_profile user_id))
      request

let logout request =
  let _ = Dream.invalidate_session request in
  let content = "Logged out !" in
  render_unsafe content request

let account_get request =
  match Dream.session "user_id" request with
  | None -> not_logged_in "/account" request
  | Some user_id ->
    let res =
      match User.get_user user_id with
      | Error e -> e
      | Ok user -> User_account.f user request
    in
    render_unsafe res request

(*TODO ask for password *)
let account_post request =
  match Dream.session "user_id" request with
  | None -> not_logged_in "/account" request
  | Some user_id -> (
    match%lwt Dream.form request with
    | `Ok [ ("delete", _) ] ->
      (*TODO ask for confirmation *)
      let res =
        Result.fold ~error:Fun.id
          ~ok:(fun () -> "Your account was deleted")
          (User.delete_user user_id)
      in
      render_unsafe res request
    | `Ok [ ("email", email) ] ->
      let res =
        Result.fold ~error:Fun.id
          ~ok:(fun () -> "Your email was updated!")
          (User.update_email email user_id)
      in
      render_unsafe res request
    | `Ok
        [ ("confirm-new-password", confirm_password)
        ; ("new-password", password)
        ] ->
      let res =
        if password = confirm_password then
          Result.fold ~error:Fun.id
            ~ok:(fun _ -> "Your password was updated!")
            (User.update_password password user_id)
        else "Password confimation does not match"
      in
      render_unsafe res request
    | `Ok _ | `Many_tokens _ | `Missing_token _ | `Invalid_token _
    | `Wrong_session _ | `Expired _ | `Wrong_content_type ->
      Dream.empty `Bad_Request )

let profile_get request =
  match Dream.session "user_id" request with
  | None -> not_logged_in "/profile" request
  | Some user_id ->
    let res =
      match User.get_user user_id with
      | Error e -> e
      | Ok user -> User_profile.f user request
    in
    render_unsafe res request

let profile_post request =
  match Dream.session "user_id" request with
  | None -> not_logged_in "/profile" request
  | Some user_id -> (
    match%lwt Dream.form request with
    | `Ok [ ("bio", bio) ] -> (
      match User.update_bio bio user_id with
      | Ok () ->
        Dream.respond ~status:`See_Other
          ~headers:[ ("Location", "/profile") ]
          "Your bio was updated!"
      | Error e -> render_unsafe e request )
    | `Ok [ ("nick", nick) ] -> (
      match User.update_nick nick user_id with
      | Ok () ->
        Dream.respond ~status:`See_Other
          ~headers:[ ("Location", "/profile") ]
          "Your display nick was updated!"
      | Error e -> render_unsafe e request )
    | `Ok [ ("content", content); ("count", count); ("label", label) ] -> (
      match int_of_string_opt count with
      | None -> render_unsafe "Error: invalid count" request
      | Some count -> (
        match User.update_metadata count label content user_id with
        | Ok () ->
          Dream.respond ~status:`See_Other
            ~headers:[ ("Location", "/profile") ]
            "Your display nick was updated!"
        | Error e -> render_unsafe e request ) )
    | `Ok _ | `Many_tokens _ | `Missing_token _ | `Invalid_token _
    | `Wrong_session _ | `Expired _ | `Wrong_content_type -> (
      match%lwt Dream.multipart request with
      | `Ok [ ("file", file) ] -> (
        match User.upload_avatar file user_id with
        | Ok () ->
          Dream.respond ~status:`See_Other
            ~headers:[ ("Location", "/profile") ]
            "Your avatar was updated!"
        | Error e -> render_unsafe e request )
      | `Ok _ | `Expired _ | `Many_tokens _ | `Missing_token _
      | `Invalid_token _ | `Wrong_session _ | `Wrong_content_type ->
        Dream.empty `Bad_Request ) )

let avatar_image request =
  let nick = Dream.param request "user" in
  match User.get_user_id_from_nick nick with
  | Error _e -> Dream.respond ~status:`Not_Found "User does not exists"
  | Ok user_id -> (
    let avatar = User.get_avatar user_id in
    match avatar with
    | Ok (Some avatar) ->
      Dream.respond ~headers:[ ("Content-Type", "image") ] avatar
    | Ok None | Error _ -> (
      match Content.read "/assets/img/default_avatar.png" with
      | None -> failwith "can't find default avatar"
      | Some avatar ->
        Dream.respond ~headers:[ ("Content-Type", "image") ] avatar ) )

let post_image request =
  let post_id = Dream.param request "post_id" in
  if Babillard.post_exist post_id then
    let image = Babillard.get_post_image_content post_id in
    match image with
    | Ok image -> Dream.respond ~headers:[ ("Content-Type", "image") ] image
    | Error _ -> Dream.empty `Not_Found
  else Dream.respond ~status:`Not_Found "Image does not exists"

let markers request =
  let markers = Pp_babillard.get_markers () in
  match markers with
  | Ok markers ->
    Dream.respond ~headers:[ ("Content-Type", "application/json") ] markers
  | Error e -> render_unsafe e request

let babillard_get request = render_unsafe (Babillard_page.f request) request

let babillard_post request =
  match Dream.session "user_id" request with
  | None -> not_logged_in "/" request
  | Some user_id -> (
    match%lwt Dream.multipart request with
    | `Ok
        [ ("alt", [ (_, alt) ])
        ; ("category", categories)
        ; ("file", file)
        ; ("lat-input", [ (_, lat) ])
        ; ("lng-input", [ (_, lng) ])
        ; ("subject", [ (_, subject) ])
        ; ("tags", [ (_, tags) ])
        ; ("thread-comment", [ (_, comment) ])
        ] -> (
      let categories =
        List.map (fun (_name, category) -> category) categories
      in
      match (Float.of_string_opt lat, Float.of_string_opt lng) with
      | None, _ -> render_unsafe "Invalide coordinate" request
      | _, None -> render_unsafe "Invalide coordinate" request
      | Some lat, Some lng -> (
        let res =
          match file with
          | [] ->
            Babillard.make_op ~comment ~lat ~lng ~subject ~tags ~categories
              user_id
          | _ :: _ :: _ -> Error "More than one image"
          | [ (image_name, image_content) ] ->
            let image = ((image_name, alt), image_content) in
            Babillard.make_op ~comment ~image ~lat ~lng ~subject ~tags
              ~categories user_id
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

let thread_feed_get request =
  let thread_id = Dream.param request "thread_id" in
  if Babillard.thread_exist thread_id then
    match Pp_babillard.feed thread_id with
    | Error e -> render_unsafe e request
    | Ok feed ->
      Dream.respond ~headers:[ ("Content-Type", "application/atom+xml") ] feed
  else Dream.respond ~status:`Not_Found "Thread not found"

let thread_get request =
  let thread_id = Dream.param request "thread_id" in
  if Babillard.thread_exist thread_id then
    let thread_view = Pp_babillard.view_thread thread_id in
    let res =
      match thread_view with
      | Error e -> e
      | Ok thread_view -> Thread_page.f thread_view thread_id request
    in
    render_unsafe res request
  else Dream.respond ~status:`Not_Found "Thread not found"

(*form to reply to a thread *)
let reply_post request =
  let parent_id = Dream.param request "thread_id" in
  match Dream.session "user_id" request with
  | None -> not_logged_in (Format.sprintf "/thread/%s" parent_id) request
  | Some user_id -> (
    match%lwt Dream.multipart request with
    | `Ok
        [ ("alt", [ (_, alt) ])
        ; ("file", file)
        ; ("reply-comment", [ (_, comment) ])
        ; ("tags", [ (_, tags) ])
        ] -> (
      let res =
        match file with
        | [] -> Babillard.make_reply ~comment ~tags ~parent_id user_id
        | [ (image_name, image_content) ] ->
          let image = ((image_name, alt), image_content) in
          Babillard.make_reply ~comment ~image ~tags ~parent_id user_id
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

let error_template _error _debug_info response =
  let status = Dream.status response in
  let code = Dream.status_to_int status in

  (*TODO improve: can't use template.elm.html because it needs "request" *)
  let%lwt body = Dream.body response in
  let reason =
    if String.equal "" body then Dream.status_to_string status else body
  in
  Dream.set_body response (Format.sprintf "%d: %s" code reason);
  Lwt.return response

let routes =
  (* this is just so that they're visually aligned *)
  let get_ = Dream.get in
  let post = Dream.post in

  [ get_ "/" babillard_get
  ; post "/" babillard_post
  ; get_ "/about" about
  ; get_ "/account" account_get
  ; post "/account" account_post
  ; get_ "/admin" admin_get
  ; post "/admin" admin_post
  ; get_ "/assets/**" (Dream.static ~loader:asset_loader "")
  ; get_ "/catalog" catalog
  ; get_ "/delete/:post_id" delete_get
  ; post "/delete/:post_id" delete_post
  ; get_ "/img/:post_id" post_image
  ; get_ "/login" login_get
  ; post "/login" login_post
  ; get_ "/logout" logout
  ; get_ "/markers" markers
  ; get_ "/post_pic/:post_id" post_image
  ; get_ "/profile" profile_get
  ; post "/profile" profile_post
  ; get_ "/report/:post_id" report_get
  ; post "/report/:post_id" report_post
  ; get_ "/thread/:thread_id" thread_get
  ; post "/thread/:thread_id" reply_post
  ; get_ "/thread/:thread_id/feed" thread_feed_get
  ; get_ "/user" user
  ; get_ "/user/:user" user_profile
  ; get_ "/user/:user/avatar" avatar_image
  ]
  @
  if App.open_registration then
    [ get_ "/register" register_get; post "/register" register_post ]
  else []

let () =
  let logger = if App.log then Dream.logger else Fun.id in
  Dream.run ~port:App.port ~error_handler:(Dream.error_template error_template)
  @@ logger @@ Dream.cookie_sessions
  (* this should replace memory/cookie sessions but it doesn't work :-(
     @@ Dream.sql_pool Db.db_uri
     @@ Dream.sql_sessions
  *)
  @@ Dream.router routes
