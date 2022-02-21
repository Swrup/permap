let db_root = App.data_dir

let () =
  match Bos.OS.Dir.create (Fpath.v db_root) with
  | Ok true -> Dream.log "created %s" db_root
  | Ok false -> Dream.log "%s already exists" db_root
  | Error (`Msg _) ->
    Dream.warning (fun log -> log "error when creating %s" db_root)

let db = Filename.concat db_root "permap.db"

let db_uri = Format.sprintf "sqlite3://%s" db

let random_state = Random.State.make_self_init ()

module Db =
(val Caqti_blocking.connect (Uri.of_string db_uri) |> Caqti_blocking.or_fail)

let () =
  let set_foreign_keys_on =
    Caqti_request.exec Caqti_type.unit "PRAGMA foreign_keys = ON;"
  in
  if Result.is_error (Db.exec set_foreign_keys_on ()) then
    Dream.error (fun log -> log "can't et foreign_keys on")

(* TODO do image validation: length and MIME types with conan*)
(* TODO do the same for text input: check length, forbidden chars and have a forbidden words filter*)
let is_valid_image _content = true

let () =
  let query =
    Caqti_request.exec Caqti_type.unit
      "CREATE TABLE IF NOT EXISTS dream_session (id TEXT PRIMARY KEY, label \
       TEXT NOT NULL, expires_at REAL NOT NULL, payload TEXT NOT NULL);"
  in
  match Db.exec query () with
  | Ok () -> ()
  | Error _e ->
    Format.eprintf "db error@\n";
    exit 1
