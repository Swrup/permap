let log = Format.printf

(* called by clicking post_id *)
(* insert id into reply form *)
let insert_quote post_id =
  log "quote@.";
  match Jv.(find global "replyComment") with
  | None -> Jv.undefined
  | Some comment_textarea ->
    let content = Jv.get comment_textarea "value" in
    let new_content =
      Jv.call content "concat"
        [| Jv.of_string " >>"; post_id; Jv.of_string " " |]
    in
    ignore @@ Jv.set comment_textarea "value" new_content;
    Jv.undefined

let () = Jv.set Jv.global "insert_quote" (Jv.repr insert_quote)

(*change postImage class to make it bigger/smaller on click*)
let image_click post_image event =
  log "image_click@.";
  let current_class =
    Jv.to_string @@ Jv.call post_image "getAttribute" [| Jv.of_string "class" |]
  in
  let new_class =
    match current_class with
    | "postImage" -> "postImageBig"
    | "postImageBig" -> "postImage"
    | _ -> failwith "invalid image class name"
  in
  ignore
  @@ Jv.call post_image "setAttribute"
       [| Jv.of_string "class"; Jv.of_string new_class |];
  (*prevent opening image in new tab*)
  ignore @@ Jv.call event "preventDefault" [||];
  ()

(*add event image_click to all postImage*)
let () =
  let document = Jv.get Jv.global "document" in
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

(*!Duplicate*)
(* make image description field visible when a file is selected*)
let make_visible alt_input alt_label _event =
  let alt_style = Jv.get alt_input "style" in
  let alt_label_style = Jv.get alt_label "style" in
  ignore @@ Jv.set alt_style "display" (Jv.of_string "block");
  ignore @@ Jv.set alt_label_style "display" (Jv.of_string "block");
  ()

let () =
  let file_input = Jv.find Jv.global "file" in
  match file_input with
  | None -> () (*not post form on the page, not logged in*)
  | Some file_input ->
    let alt_input = Jv.get Jv.global "alt" in
    let alt_label = Jv.get Jv.global "altLabel" in
    ignore
    @@ Jv.call file_input "addEventListener"
         [| Jv.of_string "change"; Jv.repr (make_visible alt_input alt_label) |];
    ()
