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

  let lat = Jv.get lat_lng "lat" in
  let lng = Jv.get lat_lng "lng" in
  let lat_input = Jv.get Jv.global "lat_input" in
  let lng_input = Jv.get Jv.global "lng_input" in
  ignore @@ Jv.call lat_input "setAttribute" [| Jv.of_string "value"; lat |];
  ignore @@ Jv.call lng_input "setAttribute" [| Jv.of_string "value"; lng |]

let () =
  (*add on_click callback to map*)
  ignore @@ Jv.call map "on" [| Jv.of_string "click"; Jv.repr on_click |]
