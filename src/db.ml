let db_root = App.data_dir

let () =
  match Bos.OS.Dir.create (Fpath.v db_root) with
  | Ok true -> Dream.log "created %s" db_root
  | Ok false -> Dream.log "%s already exists" db_root
  | Error (`Msg _) ->
    Dream.warning (fun log -> log "error when creating %s" db_root)

let db = Filename.concat db_root "permap.db"

let with_db ?mode ?mutex ?cache ?vfs ?timeout f =
  Sqlite3_utils.with_db ?mode ?mutex ?cache ?vfs ?timeout db f
