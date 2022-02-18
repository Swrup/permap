let log = Format.printf

type image_size =
  | Big
  | Small

let of_string = function
  | "postImage" -> Some Small
  | "postImageBig" -> Some Big
  | _ -> None

let to_string = function
  | Small -> "postImage"
  | Big -> "postImageBig"

(*change postImage class to make it bigger/smaller on click*)
let image_click post_image event =
  log "image_click@.";
  let current_class =
    Jv.to_string @@ Jv.call post_image "getAttribute" [| Jv.of_string "class" |]
  in
  let new_class =
    match of_string current_class with
    | Some image_size ->
      to_string
        ( match image_size with
        | Big -> Small
        | Small -> Big )
    | None -> failwith "invalid image class name"
  in
  ignore
  @@ Jv.call post_image "setAttribute"
       [| Jv.of_string "class"; Jv.of_string new_class |];
  (*prevent opening image in new tab*)
  ignore @@ Jv.call event "preventDefault" [||]

let render_time date_span =
  log "render time@.";
  let unix_time =
    float_of_int
      (Jv.to_int
         (Jv.call date_span "getAttribute" [| Jv.of_string "data-time" |]) )
  in
  (*use float because int overflow*)
  let time_millisecs = 1000.0 *. unix_time in
  let date_constructor = Jv.get Jv.global "Date" in
  let date = Jv.new' date_constructor [| Jv.of_float time_millisecs |] in
  let year = Jv.to_int @@ Jv.call date "getFullYear" [||] in
  (*the month is 0-indexed*)
  let month = (Jv.to_int @@ Jv.call date "getMonth" [||]) + 1 in
  let day = Jv.to_int @@ Jv.call date "getDate" [||] in
  let hour = Jv.to_int @@ Jv.call date "getHours" [||] in
  let min = Jv.to_int @@ Jv.call date "getMinutes" [||] in
  let date_string =
    Format.sprintf "%02d-%02d-%02d %02d:%02d" year month day hour min
  in
  ignore @@ Jv.set date_span "innerHTML" (Jv.of_string date_string)

let make_pretty _ =
  log "make pretty@.";
  let document = Jv.get Jv.global "document" in

  let times =
    Jv.to_jv_list
    @@ Jv.call document "getElementsByClassName" [| Jv.of_string "date" |]
  in
  List.iter render_time times;

  (*add event image_click to all postImage*)
  let post_images =
    Jv.to_jv_list
    @@ Jv.call document "getElementsByClassName" [| Jv.of_string "postImage" |]
  in
  let add_click el =
    ignore
    @@ Jv.call el "addEventListener"
         [| Jv.of_string "click"; Jv.repr (image_click el) |]
  in
  List.iter add_click post_images;
  ()

(*make pretty after page load*)
let () =
  log "add load eventlistener to make pretty@.";
  let window = Jv.get Jv.global "window" in
  ignore
  @@ Jv.call window "addEventListener"
       [| Jv.of_string "load"; Jv.repr make_pretty |]
