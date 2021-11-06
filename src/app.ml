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
