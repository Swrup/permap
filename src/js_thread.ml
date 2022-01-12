let log = Format.printf

(* called by clicking post_id to reply *)
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
