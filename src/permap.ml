let get_title content =
  let open Soup in
  try
    let soup = content |> parse in
    soup $ "h1" |> R.leaf_text
  with
  | Failure _e -> "Permap"

let render ?title content =
  let title =
    match title with
    | None -> get_title content
    | Some title -> title
  in
  Dream.html
  @@ Template.render_unsafe ~title:(Dream.html_escape title)
       ~content:(Dream.html_escape content)

let render_unsafe ?title content =
  let title =
    match title with
    | None -> get_title content
    | Some title -> title
  in
  Dream.html @@ Template.render_unsafe ~title ~content

let asset_loader _root path _request =
  match Content.read ("assets/" ^ path) with
  | None -> Dream.empty `Not_Found
  | Some asset -> Dream.respond asset

let page path =
  match Content.read (path ^ ".md") with
  | None -> None
  | Some page -> Some (Omd.of_string page |> Omd.to_html)

let page_of_name name =
  match page name with
  | None -> Dream.empty `Not_Found
  | Some content -> render_unsafe content

let homepage _request = page_of_name "index"

let register _request = page_of_name "register"

let () =
  Dream.run ~interface:"0.0.0.0"
  @@ Dream.logger
  @@ Dream.memory_sessions
  @@ Dream.router
       [ Dream.get "/assets/**" (Dream.static ~loader:asset_loader "")
       ; Dream.get "/" homepage
       ; Dream.get "/register" (fun request -> render_unsafe (Register.f request))
       ; Dream.post "/register" (fun request ->
           match%lwt Dream.form request with
           | `Ok ["email", email; "nick", nick; "password", password] ->
             render_unsafe (Register.f ~nick ~email ~password request)
           | _ -> assert false)
       ; Dream.get "/login" (fun request -> render_unsafe (Login.f request))
       ; Dream.post "/login" (fun request ->
           match%lwt Dream.form request with
           | `Ok ["nick", nick; "password", password] ->
             render_unsafe (Login.f ~nick ~password request)
           | _ -> assert false)
       ]
  @@ Dream.not_found