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
  | _ -> assert false

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

let profile_get request = render_unsafe (User_profile.f request) request

let profile_post request =
  match%lwt Dream.form request with
  | `Ok [ ("bio", bio) ] -> render_unsafe (User_profile.f ~bio request) request
  | _ -> assert false

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
       ; Dream.get "/logout" logout
       ; Dream.get "/profile" profile_get
       ; Dream.post "/profile" profile_post
       ]
  @@ Dream.not_found
