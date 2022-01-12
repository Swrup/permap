(*TODO implement plants as special posts? *)
open Db

type t =
  { id : string
  ; date : int
  ; nick : string (*TODO ? ; comment : string *)
  ; images : (string * string) list
  ; tags : string list
  ; longitude : float
  ; latitude : float
  ; replies : string list
  ; citations : string list
  }

(* ('a option, string) result *)
let ( let** ) o f =
  match o with
  | Error e -> Error (Format.sprintf "db error: %s" (Caqti_error.show e))
  | Ok None -> Error (Format.sprintf "db error: value not found")
  | Ok (Some x) -> f x

(* ('a, string) result *)
let ( let* ) o f =
  match o with
  | Error e -> Error (Format.sprintf "db error: %s" (Caqti_error.show e))
  | Ok x -> f x

module Q = struct
  let create_plant_user_table =
    Caqti_request.exec Caqti_type.unit
      "CREATE TABLE IF NOT EXISTS plant_user (plant_id TEXT, nick TEXT, \
       PRIMARY KEY(plant_id), FOREIGN KEY(nick) REFERENCES user(nick));"

  let create_plant_image_table =
    Caqti_request.exec Caqti_type.unit
      "CREATE TABLE IF NOT EXISTS plant_image ( plant_id TEXT, image TEXT,id \
       INTEGER, FOREIGN KEY(plant_id) REFERENCES plant_user(plant_id));"

  let create_plant_tag_table =
    Caqti_request.exec Caqti_type.unit
      "CREATE TABLE IF NOT EXISTS plant_tag (plant_id TEXT, tag TEXT, FOREIGN \
       KEY(plant_id) REFERENCES plant_user(plant_id));"

  let create_plant_gps_table =
    Caqti_request.exec Caqti_type.unit
      "CREATE TABLE IF NOT EXISTS plant_gps (plant_id TEXT, lat FLOAT,lng \
       FLOAT, FOREIGN KEY(plant_id) REFERENCES plant_user(plant_id));"

  let upload_plant_id =
    Caqti_request.exec
      Caqti_type.(tup2 string string)
      "INSERT INTO plant_user VALUES (?,?);"

  let upload_plant_tag =
    Caqti_request.exec
      Caqti_type.(tup2 string string)
      "INSERT INTO plant_tag VALUES (?,?);"

  let upload_plant_gps =
    Caqti_request.exec
      Caqti_type.(tup3 string float float)
      "INSERT INTO plant_gps VALUES (?,?,?);"

  let upload_plant_image =
    Caqti_request.exec
      Caqti_type.(tup3 string string int)
      "INSERT INTO plant_image VALUES (?,?,?);"

  let get_user_plants =
    Caqti_request.collect Caqti_type.string Caqti_type.string
      "SELECT plant_id FROM plant_user WHERE nick=?;"

  let list_plant_ids =
    Caqti_request.collect Caqti_type.unit Caqti_type.string
      "SELECT plant_id FROM plant_user;"

  let count_plant_image =
    Caqti_request.find_opt Caqti_type.string Caqti_type.int
      "SELECT COUNT(*) FROM plant_image WHERE plant_id=?;"

  let get_plant_image =
    Caqti_request.find_opt
      Caqti_type.(tup2 string int)
      Caqti_type.string
      "SELECT image FROM plant_image WHERE plant_id=? AND id=?;"

  let get_plant_tags =
    Caqti_request.collect Caqti_type.string Caqti_type.string
      "SELECT tag FROM plant_tag WHERE plant_id=?;"

  let get_plant_gps =
    Caqti_request.find_opt Caqti_type.string
      Caqti_type.(tup2 float float)
      "SELECT lat, lng FROM plant_gps WHERE plant_id=?;"
end

let () =
  let tables =
    [ Q.create_plant_user_table
    ; Q.create_plant_image_table
    ; Q.create_plant_tag_table
    ; Q.create_plant_gps_table
    ]
  in
  if
    List.exists Result.is_error
      (List.map (fun query -> Db.exec query ()) tables)
  then
    Dream.warning (fun log -> log "can't create table")

let view_plant plant_id =
  let** count = Db.find_opt Q.count_plant_image plant_id in
  let gps =
    match Db.find_opt Q.get_plant_gps plant_id with
    | Ok (Some (lat, lng)) -> Float.to_string lat ^ " " ^ Float.to_string lng
    | Ok None -> ""
    | Error e -> Format.sprintf "db error: %s" (Caqti_error.show e)
  in
  let images =
    String.concat "\n"
      (List.map
         (Format.sprintf
            {|<li><img src="/plant_pic/%s/%i" class="img-thumbnail"></li>|}
            plant_id )
         (List.init count (fun i -> i)) )
  in
  let tags = Db.fold Q.get_plant_tags (fun tag acc -> tag :: acc) plant_id [] in
  match tags with
  | Error e -> Error (Format.sprintf "db error: %s" (Caqti_error.show e))
  | Ok tags ->
    let tags = String.concat " " tags in
    (* TODO add link to gps/map too *)
    Ok (images ^ tags ^ gps)

let marker_list () =
  let* plant_id_list =
    Db.fold Q.list_plant_ids (fun plant_id acc -> plant_id :: acc) () []
  in
  let markers_res =
    List.map
      (fun plant_id ->
        match Db.find_opt Q.get_plant_gps plant_id with
        | Ok (Some (lat, lng)) -> (
          let content = view_plant plant_id in
          match content with
          | Error e -> Error e
          | Ok content -> Ok (lat, lng, content) )
        | Ok None -> Error "latlng not found"
        | Error e -> Error (Format.sprintf "db error: %s" (Caqti_error.show e))
        )
      plant_id_list
  in
  let markers =
    List.map
      (fun res ->
        match res with
        | Ok res -> res
        | Error _ -> assert false )
      (List.filter Result.is_ok markers_res)
  in
  Ok markers

let marker_to_geojson marker =
  match marker with
  | lat, lng, content ->
    Format.sprintf
      {|
{
  "type": "Feature",
  "geometry": {
    "type": "Point",
    "coordinates": [%s,%s]
  },
  "properties": {
    "content": "%s"
  }
} 
|}
      (* geojson use lng lat, and not lat lng*)
      (Float.to_string lng)
      (Float.to_string lat) (String.escaped content)

let view_user_plant_list nick =
  let plant_id_list =
    Db.fold Q.get_user_plants (fun plant_id acc -> plant_id :: acc) nick []
  in
  match plant_id_list with
  | Error e -> Format.sprintf "db error: %s" (Caqti_error.show e)
  | Ok plant_id_list ->
    let plants =
      List.map
        (fun p ->
          match view_plant p with
          | Ok p -> p
          | Error _ -> "" )
        plant_id_list
    in
    String.concat "\n" plants

let get_plant_image plant_id nb =
  let** content = Db.find_opt Q.get_plant_image (plant_id, nb) in
  Ok content

(*TODO split validation and uploading to db like for babillard *)
let add_plant (lat, lng) tags files nick =
  if String.length tags > 1000 then
    Error "tags too long"
  else
    let tag_list = Str.split (Str.regexp " +") tags in
    let is_valid_list =
      List.map (fun (_, content) -> is_valid_image content) files
    in
    let is_valid_files = List.for_all (fun valid -> valid) is_valid_list in
    if not is_valid_files then
      Error "Invalid image"
    else
      (* add plant to db *)
      (* make id for plant*)
      let plant_id = Uuidm.to_string (Uuidm.v4_gen random_state ()) in
      (* add to plant_id <-> user*)
      let res_plant = Db.exec Q.upload_plant_id (plant_id, nick) in
      match res_plant with
      | Error e -> Error (Format.sprintf "db error: %s" (Caqti_error.show e))
      | Ok _ -> (
        (* add to plant_id <-> gps table*)
        (*TODO check if valid latlng *)
        let res_gps = Db.exec Q.upload_plant_gps (plant_id, lat, lng) in
        match res_gps with
        | Error e -> Error (Format.sprintf "db error: %s" (Caqti_error.show e))
        | Ok _ -> (
          (* add to plant_id <-> tag table*)
          let res_tags =
            List.map
              (fun tag -> Db.exec Q.upload_plant_tag (plant_id, tag))
              tag_list
          in
          match List.find_opt Result.is_error res_tags with
          | Some (Error e) ->
            Error (Format.sprintf "db error: %s" (Caqti_error.show e))
          | Some _ -> assert false
          | None -> (
            (* add to plant_id <-> image*)
            let res_images =
              List.find_opt Result.is_error
                (List.mapi
                   (fun nb (_, content) ->
                     Db.exec Q.upload_plant_image (plant_id, content, nb) )
                   files )
            in
            match res_images with
            | Some (Error e) ->
              Error (Format.sprintf "db error: %s" (Caqti_error.show e))
            | Some (Ok _) -> assert false
            | None -> Ok () ) ) )
