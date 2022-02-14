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
        [| Jv.of_string "\n>>"; post_id; Jv.of_string " " |]
    in
    ignore @@ Jv.set comment_textarea "value" new_content;
    Jv.undefined

let () = Jv.set Jv.global "insert_quote" (Jv.repr insert_quote)

(* make image description field visible when a file is selected*)
let make_visible el _event =
  let el_style = Jv.get el "style" in
  ignore @@ Jv.set el_style "display" (Jv.of_string "block");
  ()

let () =
  log "change image description visibility@.";
  let file_input = Jv.find Jv.global "file" in
  match file_input with
  | None -> () (*not post form on the page, not logged in*)
  | Some file_input ->
    let alt_input = Jv.get Jv.global "alt" in
    let alt_label = Jv.get Jv.global "altLabel" in
    ignore
    @@ Jv.call file_input "addEventListener"
         [| Jv.of_string "change"; Jv.repr (make_visible alt_input) |];
    ignore
    @@ Jv.call file_input "addEventListener"
         [| Jv.of_string "change"; Jv.repr (make_visible alt_label) |];
    ()
