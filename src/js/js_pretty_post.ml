let log = Format.printf

type image_size =
  | Big
  | Small

let of_string = function
  | "post-image" -> Some Small
  | "post-image-big" -> Some Big
  | _ -> None

let to_string = function Small -> "post-image" | Big -> "post-image-big"

(*change postImage class to make it bigger/smaller on click*)
let image_click post_image event =
  log "image_click@\n";
  let current_class =
    Jv.to_string @@ Jv.call post_image "getAttribute" [| Jv.of_string "class" |]
  in
  let new_class =
    match of_string current_class with
    | Some image_size ->
      to_string (match image_size with Big -> Small | Small -> Big)
    | None -> failwith "invalid image class name"
  in
  ignore
  @@ Jv.call post_image "setAttribute"
       [| Jv.of_string "class"; Jv.of_string new_class |];
  (*prevent redirect to /img/:img*)
  ignore @@ Jv.call event "preventDefault" [||];
  ignore @@ Jv.call event "stopPropagation" [||]

let render_time date_span =
  log "render time@\n";
  let t =
    float_of_int
      (Jv.to_int
         (Jv.call date_span "getAttribute" [| Jv.of_string "data-time" |]) )
  in
  let t = Unix.localtime t in
  let date =
    Format.sprintf "%02d-%02d-%02d %02d:%02d" (1900 + t.tm_year) (1 + t.tm_mon)
      t.tm_mday t.tm_hour t.tm_min
  in
  ignore @@ Jv.set date_span "innerHTML" (Jv.of_string date)

(* make threads preview clickable to get to thread *)
let preview_click thread_id _event =
  log "preview_click@\n";
  let url = Format.sprintf "/thread/%s" thread_id in
  let window = Jv.get Jv.global "window" in
  let location = Jv.get window "location" in
  ignore @@ Jv.set location "href" (Jv.of_string url)

let make_pretty _event =
  log "make pretty@\n";
  let document = Jv.get Jv.global "document" in

  let times =
    Jv.to_jv_list
    @@ Jv.call document "getElementsByClassName" [| Jv.of_string "date" |]
  in
  List.iter render_time times;

  (*add event image_click to all postImage*)
  let post_images =
    Jv.to_jv_list
    @@ Jv.call document "getElementsByClassName" [| Jv.of_string "post-image" |]
  in
  let add_click el =
    ignore
    @@ Jv.call el "addEventListener"
         [| Jv.of_string "click"; Jv.repr (image_click el) |]
  in
  List.iter add_click post_images;

  log "make_pretty_catalog@\n";
  let previews =
    Jv.to_jv_list
    @@ Jv.call document "getElementsByClassName"
         [| Jv.of_string "thread-preview" |]
  in
  let add_preview_click el =
    let thread_id =
      Jv.to_string @@ Jv.call el "getAttribute" [| Jv.of_string "data-id" |]
    in
    ignore
    @@ Jv.call el "addEventListener"
         [| Jv.of_string "click"; Jv.repr (preview_click thread_id) |]
  in
  List.iter add_preview_click previews

(*make pretty after page load*)
let () =
  log "add load eventlistener to make pretty@\n";
  let window = Jv.get Jv.global "window" in
  ignore
  @@ Jv.call window "addEventListener"
       [| Jv.of_string "load"; Jv.repr make_pretty |]
