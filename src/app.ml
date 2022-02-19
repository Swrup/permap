module App_id = struct
  let qualifier = "org"

  let organization = "Permap"

  let application = "permap"
end

module Project_dirs = Directories.Project_dirs (App_id)

let data_dir =
  match Project_dirs.data_dir with
  | None -> failwith "can't compute data directory"
  | Some data_dir -> data_dir

let config_dir =
  match Project_dirs.config_dir with
  | None -> failwith "can't compute configuration directory"
  | Some config_dir -> config_dir

let config =
  let filename = Filename.concat config_dir "config.scfg" in
  if not @@ Sys.file_exists filename then
    failwith
    @@ Format.sprintf "configuration file `%s` does not exist, please create it"
         filename;
  Dream.log "config file: %s" filename;
  match Scfg.Parse.from_file filename with
  | Error e -> failwith e
  | Ok config -> config

let open_registration =
  match Scfg.Query.get_dir "open_registration" config with
  | None -> true
  | Some open_registration -> (
    match Scfg.Query.get_param 0 open_registration with
    | Error e -> failwith e
    | Ok "true" -> true
    | Ok "false" -> false
    | Ok unknown ->
      failwith
      @@ Format.sprintf
           "invalid value for `open_registration` in configuration file, \
            expected `true` or `false` but got `%s`"
           unknown )

let () = Dream.log "open_registration: %s" (Bool.to_string open_registration)
