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
