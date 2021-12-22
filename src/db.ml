let db_root = App.data_dir

let () =
  match Bos.OS.Dir.create (Fpath.v db_root) with
  | Ok true -> Dream.log "created %s" db_root
  | Ok false -> Dream.log "%s already exists" db_root
  | Error (`Msg _) ->
    Dream.warning (fun log -> log "error when creating %s" db_root)

let db = Filename.concat db_root "permap.db"

let random_state = Random.State.make_self_init ()

let with_db ?mode ?mutex ?cache ?vfs ?timeout f =
  Sqlite3_utils.with_db ?mode ?mutex ?cache ?vfs ?timeout db f

module Db =
( val Caqti_blocking.connect (Uri.of_string ("sqlite3://" ^ db))
      |> Caqti_blocking.or_fail )

(* TODO do image validation: length and MIME types with conan*)
(* TODO do the same for text input: check length, forbidden chars and have a forbidden words filter*)
let is_valid_image _content = true
