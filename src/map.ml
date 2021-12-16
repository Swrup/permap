(* TODO only run this on /add_plant and /map *)
let log = Format.printf

(* get the leaflet object *)
let leaflet =
  match Jv.(find global "L") with
  | Some l -> l
  | None -> failwith "can't load leaflet"

(* get popup object *)
let popup = Jv.call leaflet "popup" [||]

(* create a map *)
let map =
  log "creating map@.";
  let open Brr in
  let _container = El.div ~at:At.[ id (Jstr.v "map") ] [] in
  Jv.call leaflet "map" [| Jv.of_string "map" |]

(* create map's pos *)
let lat_lng =
  log "making latlng@.";
  Jv.call leaflet "latLng" [| Jv.of_float 51.505; Jv.of_float (-0.09) |]

(* set map's pos *)
let () =
  log "setting view@.";
  let _m : Jv.t = Jv.call map "setView" [| lat_lng; Jv.of_int 13 |] in
  ()

(* create map tile layer *)
let tile_layer =
  log "creating tile layer@.";
  Jv.call leaflet "tileLayer"
    [| Jv.of_string "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
     ; Jv.obj
         [| ( "attribution"
            , Jv.of_string
                {|&copy; <a href="https://.www.openstreetmap.org/copyright">OpenStreetMap</a> contributors|}
            )
         |]
    |]

(* add tile layer *)
let () =
  log "adding tile layer@.";
  let _map : Jv.t = Jv.call tile_layer "addTo" [| map |] in
  ()

let on_click e =
  log "on_click@.";

  (*TODO use Jv.find *)
  let lat_lng = Jv.get e "latlng" in
  ignore @@ Jv.call popup "setLatLng" [| lat_lng |];
  ignore @@ Jv.call popup "setContent" [| Jv.of_string "euujjj" |];
  ignore @@ Jv.call popup "openOn" [| map |];

  (* TODO only on /add_plant *)
  let lat = Jv.get lat_lng "lat" in
  let lng = Jv.get lat_lng "lng" in
  let lat_input = Jv.get Jv.global "lat_input" in
  let lng_input = Jv.get Jv.global "lng_input" in
  ignore @@ Jv.call lat_input "setAttribute" [| Jv.of_string "value"; lat |];
  ignore @@ Jv.call lng_input "setAttribute" [| Jv.of_string "value"; lng |]

(* let add_marker lat lng content =
   log "add_marker@.";

   let marker =
     Jv.call leaflet "marker" [| Jv.of_array Jv.of_float [| lat; lng |] |]
   in
   ignore @@ Jv.call marker "bindPopup" [| Jv.of_string content |];
   ignore @@ Jv.call marker "addTo" [| map |]
*)

let handle_geojson geojson =
  log "handle_geojson@.";
  log "feed geojson to leaflet@.";
  let layer = Jv.call leaflet "geoJSON" [| geojson |] in
  ignore @@ Jv.call layer "addTo" [| map |];
  (* TODO this doesnt work :^) *)
  ()

let handle_response response =
  log "handle_response@.";
  let geo_json_list_futur = Jv.call response "json" [||] in
  ignore @@ Jv.call geo_json_list_futur "then" [| Jv.repr handle_geojson |];
  ()

let () =
  (* TODO only on /map *)
  (* TODO add marker for plants on the map *)
  (* TODO GET /markers -> geojson *)
  (* TODO make popup *)
  log "fetch geojson@.";
  let window = Jv.get Jv.global "window" in
  let fetchfutur = Jv.call window "fetch" [| Jv.of_string "/markers" |] in
  ignore @@ Jv.call fetchfutur "then" [| Jv.repr handle_response |];
  ()

let () =
  (*add on_click callback to map*)
  ignore @@ Jv.call map "on" [| Jv.of_string "click"; Jv.repr on_click |]
