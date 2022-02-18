let log = Format.printf

(* called by clicking post_id *)
(* insert id into reply form *)
let insert_quote post_id _event =
  log "quote@.";
  Option.iter
    (fun comment_textarea ->
      let content = Jv.get comment_textarea "value" in
      let new_content =
        Jv.call content "concat"
          [| Jv.of_string "\n>>"; post_id; Jv.of_string " " |]
      in
      ignore @@ Jv.set comment_textarea "value" new_content )
    Jv.(find global "reply-comment");
  Jv.undefined

let () =
  log "add inser_quote event on post links@.";
  let document = Jv.get Jv.global "document" in
  let quote_links =
    Jv.to_jv_list
    @@ Jv.call document "getElementsByClassName" [| Jv.of_string "quote-link" |]
  in
  log "quote_links leng %d@." (List.length quote_links);
  let add_click quote_link =
    let post_id =
      Jv.call quote_link "getAttribute" [| Jv.of_string "data-id" |]
    in
    ignore
    @@ Jv.call quote_link "addEventListener"
         [| Jv.of_string "click"; Jv.repr (insert_quote post_id) |]
  in
  List.iter add_click quote_links

(* make image description field visible when a file is selected*)
let make_visible el _event =
  let el_style = Jv.get el "style" in
  ignore @@ Jv.set el_style "display" (Jv.of_string "block")

let () =
  log "change image description visibility@.";
  Option.iter
    (fun file_input ->
      let alt_input = Jv.get Jv.global "alt" in
      let alt_label = Jv.get Jv.global "alt-label" in
      ignore
      @@ Jv.call file_input "addEventListener"
           [| Jv.of_string "change"; Jv.repr (make_visible alt_input) |];
      ignore
      @@ Jv.call file_input "addEventListener"
           [| Jv.of_string "change"; Jv.repr (make_visible alt_label) |] )
    Jv.(find global "file")
