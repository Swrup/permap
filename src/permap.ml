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

let user request = render_unsafe (User.list ()) request

let user_profile request = render_unsafe (User.public_profile request) request

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
    | `Ok [ ("bio", bio) ] ->
      let res =
        match User.update_bio bio nick with
        | Ok () -> "Bio updated!"
        | Error e -> e
      in
      render_unsafe res request
    | `Ok _
    | `Many_tokens _
    | `Missing_token _
    | `Invalid_token _
    | `Wrong_session _
    | `Expired _
    | `Wrong_content_type -> (
      match%lwt Dream.multipart request with
      | `Ok [ ("file", file) ] ->
        let res =
          match User.upload_avatar file nick with
          | Ok () -> "Avatar was uploaded!"
          | Error e -> e
        in
        render_unsafe res request
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

let plant_image request =
  let plant_id = Dream.param "plant_id" request in
  let nb = int_of_string (Dream.param "nb" request) in
  let image = Plant.get_plant_image plant_id nb in
  match image with
  | Ok (Some image) ->
    Dream.respond ~headers:[ ("Content-Type", "image") ] image
  | Ok None
  | Error _ ->
    Dream.empty `Not_Found

let post_image request =
  let post_id = Dream.param "post_id" request in
  let image = Babillard.get_post_image post_id in
  match image with
  | Ok (Some image) ->
    Dream.respond ~headers:[ ("Content-Type", "image") ] image
  | Ok None
  | Error _ ->
    Dream.empty `Not_Found

(* TODO fix *)
let map request = page "map" request

let add_plant_get request =
  match Dream.session "nick" request with
  | None -> render_unsafe "Not logged in" request
  | Some nick -> render_unsafe (Add_plant.f nick request) request

let add_plant_post request =
  match Dream.session "nick" request with
  | None -> render_unsafe "Not logged in" request
  | Some nick -> (
    match%lwt Dream.multipart request with
    | `Ok
        [ ("files", files)
        ; ("lat_input", [ (_, lat) ])
        ; ("lng_input", [ (_, lng) ])
        ; ("tags", [ (_, tags) ])
        ]
    | `Ok
        (("files", files)
        :: ("lat_input", [ (_, lat) ])
           :: ("lng_input", [ (_, lng) ]) :: ("tags", [ (_, tags) ]) :: _ :: _
        ) -> (
      match (Float.of_string_opt lat, Float.of_string_opt lng) with
      | None, _ -> render_unsafe "Invalide coordinate" request
      | _, None -> render_unsafe "Invalide coordinate" request
      | Some lat, Some lng ->
        let res =
          match Plant.add_plant (lat, lng) tags files nick with
          | Ok () -> "Your plant was uploaded!"
          | Error e -> e
        in
        render_unsafe res request )
    | `Ok _ -> Dream.empty `Bad_Request
    | `Expired _
    | `Many_tokens _
    | `Missing_token _
    | `Invalid_token _
    | `Wrong_session _
    | `Wrong_content_type ->
      Dream.empty `Bad_Request )

let plant_markers request =
  (*TODO should be in plant *)
  let marker_list = Plant.marker_list () in
  match marker_list with
  | Ok marker_list ->
    let json =
      {| [ |}
      ^ String.concat "," (List.map Plant.marker_to_geojson marker_list)
      ^ "]"
    in
    Dream.respond ~headers:[ ("Content-Type", "application/json") ] json
  | Error e -> render_unsafe e request

let thread_markers request =
  (*TODO should be in babillard*)
  let marker_list = Babillard.marker_list () in
  match marker_list with
  | Ok marker_list ->
    let json =
      {| [ |}
      ^ String.concat "," (List.map Babillard.marker_to_geojson marker_list)
      ^ "]"
    in
    Dream.respond ~headers:[ ("Content-Type", "application/json") ] json
  | Error e -> render_unsafe e request

let babillard_get request = render_unsafe (Babillard_page.f request) request

let newthread_get request = render_unsafe (Newthread_page.f request) request

let newthread_post request =
  match Dream.session "nick" request with
  | None -> render_unsafe "Not logged in" request
  | Some nick -> (
    match%lwt Dream.multipart request with
    (*TODO jpp du duplicat la *)
    | `Ok
        [ ("file", file)
        ; ("lat_input", [ (_, lat) ])
        ; ("lng_input", [ (_, lng) ])
        ; ("subject", [ (_, subject) ])
        ; ("tags", [ (_, tags) ])
        ; ("threadComment", [ (_, comment) ])
        ]
    | `Ok
        (("file", file)
        :: ("lat_input", [ (_, lat) ])
           :: ("lng_input", [ (_, lng) ])
              :: ("subject", [ (_, subject) ])
                 :: ("tags", [ (_, tags) ])
                    :: ("threadComment", [ (_, comment) ]) :: _ :: _ ) -> (
      match (Float.of_string_opt lat, Float.of_string_opt lng) with
      | None, _ -> render_unsafe "Invalide coordinate" request
      | _, None -> render_unsafe "Invalide coordinate" request
      | Some lat, Some lng -> (
        match file with
        | [] -> render_unsafe "No image" request
        | _ :: _ :: _ -> render_unsafe "More than one image" request
        | [ file ] ->
          let res =
            match
              Babillard.make_thread comment file (lat, lng) subject tags nick
            with
            | Ok _post_id -> "Your thread was posted on the babillard!"
            | Error e -> e
          in
          render_unsafe res request ) )
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
  let thread_view = Babillard.view_thread thread_id in
  match thread_view with
  | Error e -> render_unsafe e request
  | Ok thread_view ->
    render_unsafe (Thread_page.f thread_view thread_id request) request

(* get thread view but not wrapped in template, so we can display it on /babillard*)
let thread_view request =
  let thread_id = Dream.param "thread_id" request in
  let thread_view = Babillard.view_thread thread_id in
  match thread_view with
  | Error e -> render_unsafe e request
  | Ok thread_view ->
    Dream.respond
      ~headers:[ ("Content-Type", "html") ]
      (Thread_page.f thread_view thread_id request)

(*form to reply to a thread *)
let thread_post request =
  match Dream.session "nick" request with
  | None -> render_unsafe "Not logged in" request
  | Some nick -> (
    match%lwt Dream.multipart request with
    | `Ok
        [ ("file", file)
        ; ("replyComment", [ (_, comment) ])
        ; ("tags", [ (_, tags) ])
        ]
    | `Ok
        (("file", file)
        :: ("tags", [ (_, tags) ])
           :: ("replyComment", [ (_, comment) ]) :: _ :: _ ) ->
      let parent_id = Dream.param "thread_id" request in
      let res =
        match file with
        | [] -> Babillard.make_post ~comment ~tags ~parent_id nick
        | [ file ] -> Babillard.make_post ~comment ~file ~tags ~parent_id nick
        | _ :: _ :: _ -> Error "More than one image"
      in
      let msg =
        match res with
        | Ok _post_id -> "Your reply was posted"
        | Error e -> e
      in
      render_unsafe msg request
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
       ; Dream.get "/map" map
       ; Dream.get "/user" user
       ; Dream.get "/user/:user" user_profile
       ; Dream.get "/user/:user/avatar" avatar_image
       ; Dream.get "/logout" logout
       ; Dream.get "/profile" profile_get
       ; Dream.post "/profile" profile_post
       ; Dream.get "/add_plant" add_plant_get
       ; Dream.post "/add_plant" add_plant_post
       ; Dream.get "/plant_pic/:plant_id/:nb" plant_image
       ; Dream.get "/plant_markers" plant_markers
       ; Dream.get "/thread_markers" thread_markers
       ; Dream.get "/thread_view/:thread_id" thread_view
       ; Dream.get "/babillard" babillard_get
       ; Dream.get "/babillard/new_thread" newthread_get
       ; Dream.post "/babillard/new_thread" newthread_post
       ; Dream.get "/babillard/:thread_id" thread_get (*todo, bad names ^^*)
       ; Dream.post "/babillard/:thread_id" thread_post
       ; Dream.get "/post_pic/:post_id" post_image
       ]
  @@ Dream.not_found
