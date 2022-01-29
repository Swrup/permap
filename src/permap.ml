let get_title content =
  let open Soup in
  try
    let soup = content |> parse in
    soup $ "h1" |> R.leaf_text
  with
  | Failure _e -> "Permap"

let render ?title content request =
  let title =
    match title with
    | None -> get_title content
    | Some title -> title
  in
  Dream.html
  @@ Template.render_unsafe ~title:(Dream.html_escape title)
       ~content:(Dream.html_escape content)
       request

let render_unsafe ?title content request =
  let title =
    match title with
    | None -> get_title content
    | Some title -> title
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

let homepage request = page "index" request

let register_get request = render_unsafe (Register.f request) request

let register_post request =
  match%lwt Dream.form request with
  | `Ok [ ("email", email); ("nick", nick); ("password", password) ] ->
    render_unsafe (Register.f ~nick ~email ~password request) request
  | `Ok _
  | `Many_tokens _
  | `Missing_token _
  | `Invalid_token _
  | `Wrong_session _
  | `Expired _
  | `Wrong_content_type ->
    assert false

let login_get request = render_unsafe (Login.f request) request

let login_post request =
  match%lwt Dream.form request with
  | `Ok [ ("nick", nick); ("password", password) ] ->
    render_unsafe (Login.f ~nick ~password request) request
  | _ -> assert false

let user request =
  render_unsafe
    ( match User.list () with
    | Ok s -> s
    | Error _ -> "" )
    request

let user_profile request =
  render_unsafe
    ( match User.public_profile request with
    | Ok s -> s
    | Error e -> e )
    request

let logout request =
  let _ = Dream.invalidate_session request in
  let content = "Logged out !" in
  render_unsafe content request

let profile_get request =
  match Dream.session "nick" request with
  | None -> render_unsafe "Not logged in" request
  | Some nick ->
    let bio =
      match User.get_bio nick with
      | Ok bio -> bio
      | Error e -> e
    in
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
    | `Ok _
    | `Many_tokens _
    | `Missing_token _
    | `Invalid_token _
    | `Wrong_session _
    | `Expired _
    | `Wrong_content_type -> (
      match%lwt Dream.multipart request with
      | `Ok [ ("file", file) ] -> (
        match User.upload_avatar file nick with
        | Ok () ->
          Dream.respond ~status:`See_Other
            ~headers:[ ("Location", "/profile") ]
            "Your avatar was updated!"
        | Error e -> render_unsafe e request )
      | `Ok _ -> Dream.empty `Bad_Request
      | `Expired _
      | `Many_tokens _
      | `Missing_token _
      | `Invalid_token _
      | `Wrong_session _
      | `Wrong_content_type ->
        Dream.empty `Bad_Request ) )

let avatar_image request =
  let nick = Dream.param "user" request in
  let avatar = User.get_avatar nick in
  match avatar with
  | Ok (Some avatar) ->
    Dream.respond ~headers:[ ("Content-Type", "image") ] avatar
  | Ok None
  | Error _ -> (
    match Content.read "/assets/img/default_avatar.png" with
    | None -> Dream.empty `Not_Found
    | Some avatar -> Dream.respond ~headers:[ ("Content-Type", "image") ] avatar
    )

let post_image request =
  let post_id = Dream.param "post_id" request in
  let image = Babillard.get_post_image_content post_id in
  match image with
  | Ok image -> Dream.respond ~headers:[ ("Content-Type", "image") ] image
  | Error _ -> Dream.empty `Not_Found

let markers ~board request =
  let markers = Pp_babillard.get_markers board in
  match markers with
  | Ok markers ->
    Dream.respond ~headers:[ ("Content-Type", "application/json") ] markers
  | Error e -> render_unsafe e request

let babillard_get ~board request =
  render_unsafe (Babillard_page.f ~board request) request

let newthread_get ~board request =
  render_unsafe (Newthread_page.f ~board request) request

let newthread_post ~board request =
  match Dream.session "nick" request with
  | None -> render_unsafe "Not logged in" request
  | Some nick -> (
    match%lwt Dream.multipart request with
    | `Ok
        [ ("alt", [ (_, alt) ])
        ; ("file", file)
        ; ("lat_input", [ (_, lat) ])
        ; ("lng_input", [ (_, lng) ])
        ; ("subject", [ (_, subject) ])
        ; ("tags", [ (_, tags) ])
        ; ("threadComment", [ (_, comment) ])
        ] -> (
      match (Float.of_string_opt lat, Float.of_string_opt lng) with
      | None, _ -> render_unsafe "Invalide coordinate" request
      | _, None -> render_unsafe "Invalide coordinate" request
      | Some lat, Some lng -> (
        let res =
          match file with
          | [] ->
            Babillard.make_op ~comment ~lat ~lng ~subject ~tags ~board nick
          | _ :: _ :: _ -> Error "More than one image"
          | [ (image_name, image_content) ] ->
            let image = (image_name, image_content, alt) in
            Babillard.make_op ~comment ~image ~lat ~lng ~subject ~tags ~board
              nick
        in
        match res with
        | Ok thread_id ->
          let adress =
            Format.asprintf "/%a/%s" Babillard.pp_board board thread_id
          in
          Dream.respond ~status:`See_Other
            ~headers:[ ("Location", adress) ]
            "Your thread was posted!"
        | Error e -> render_unsafe e request ) )
    | `Ok _ -> Dream.empty `Bad_Request
    | `Expired _
    | `Many_tokens _
    | `Missing_token _
    | `Invalid_token _
    | `Wrong_session _
    | `Wrong_content_type ->
      Dream.empty `Bad_Request )

let thread_get request =
  let thread_id = Dream.param "thread_id" request in
  let thread_view = Pp_babillard.view_thread thread_id in
  match thread_view with
  | Error e -> render_unsafe e request
  | Ok thread_view ->
    render_unsafe (Thread_page.f thread_view thread_id request) request

(* get thread view but not wrapped in template, so we can display it on /babillard*)
let thread_view request =
  let thread_id = Dream.param "thread_id" request in
  let thread_view = Pp_babillard.view_thread thread_id in
  match thread_view with
  | Error e -> render_unsafe e request
  | Ok thread_view -> Dream.html (Thread_page.f thread_view thread_id request)

(*form to reply to a thread *)
let reply_post request =
  match Dream.session "nick" request with
  | None -> render_unsafe "Not logged in" request
  | Some nick -> (
    match%lwt Dream.multipart request with
    | `Ok
        [ ("alt", [ (_, alt) ])
        ; ("file", file)
        ; ("replyComment", [ (_, comment) ])
        ; ("tags", [ (_, tags) ])
        ] -> (
      let parent_id = Dream.param "thread_id" request in
      let res =
        match file with
        | [] -> Babillard.make_reply ~comment ~tags ~parent_id nick
        | [ (image_name, image_content) ] ->
          let image = (image_name, image_content, alt) in
          Babillard.make_reply ~comment ~image ~tags ~parent_id nick
        | _ :: _ :: _ -> Error "More than one image"
      in
      match res with
      | Ok post_id ->
        let adress = Format.sprintf "/babillard/%s#%s" parent_id post_id in
        Dream.respond ~status:`See_Other
          ~headers:[ ("Location", adress) ]
          "Your reply was posted!"
      | Error e -> render_unsafe e request )
    | `Ok _ -> Dream.empty `Bad_Request
    | `Expired _
    | `Many_tokens _
    | `Missing_token _
    | `Invalid_token _
    | `Wrong_session _
    | `Wrong_content_type ->
      Dream.empty `Bad_Request )

let () =
  Dream.run @@ Dream.logger @@ Dream.memory_sessions
  @@ Dream.router
       [ Dream.get "/assets/**" (Dream.static ~loader:asset_loader "")
       ; Dream.get "/" homepage
       ; Dream.get "/register" register_get
       ; Dream.post "/register" register_post
       ; Dream.get "/login" login_get
       ; Dream.post "/login" login_post
       ; Dream.get "/user" user
       ; Dream.get "/user/:user" user_profile
       ; Dream.get "/user/:user/avatar" avatar_image
       ; Dream.get "/logout" logout
       ; Dream.get "/profile" profile_get
       ; Dream.post "/profile" profile_post
       ; Dream.get "/thread_view/:thread_id" thread_view
       ; Dream.get "/babillard/markers" (markers ~board:Babillard)
       ; Dream.get "/post_pic/:post_id" post_image
       ; Dream.get "/babillard" (babillard_get ~board:Babillard)
       ; Dream.get "/babillard/new_thread" (newthread_get ~board:Babillard)
       ; Dream.post "/babillard/new_thread" (newthread_post ~board:Babillard)
       ; Dream.get "/babillard/:thread_id" thread_get
       ; Dream.post "/reply/:thread_id" reply_post
       ; Dream.get "/post_pic/:post_id" post_image
       ]
  @@ Dream.not_found
