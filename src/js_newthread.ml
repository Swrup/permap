let log = Format.printf

module Leaflet = struct
  (* get the leaflet object *)
  let leaflet =
    match Jv.(find global "L") with
    | Some l -> l
    | None -> failwith "can't load leaflet"

  (* get popup object, with no close button*)
  let popup =
    Jv.call leaflet "popup" [| Jv.obj [| ("closeButton", Jv.false') |] |]

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
end

module Marker = struct
  let board =
    let board_div = Jv.get Jv.global "board" in
    Jv.to_string
      (Jv.call board_div "getAttribute" [| Jv.of_string "data-board" |])

  let handle_geojson geojson =
    log "handle_geojson@.";
    log "feed geojson to leaflet@.";
    let layer = Jv.call Leaflet.leaflet "geoJSON" [| geojson |] in
    let _marker_layer = Jv.call layer "addTo" [| Leaflet.map |] in
    ()

  let markers_handle_response response =
    log "markers_handle_response@.";
    let geo_json_list_futur = Jv.call response "json" [||] in
    ignore @@ Jv.call geo_json_list_futur "then" [| Jv.repr handle_geojson |];
    ()

  let () =
    log "fetch thread geojson@.";
    let window = Jv.get Jv.global "window" in
    let link = Jv.of_string (Format.sprintf "/%s/markers" board) in
    let fetchfutur = Jv.call window "fetch" [| link |] in
    ignore @@ Jv.call fetchfutur "then" [| Jv.repr markers_handle_response |];
    ()
end

let on_click e =
  log "on_click@.";

  let lat_lng = Jv.get e "latlng" in
  ignore @@ Jv.call Leaflet.popup "setLatLng" [| lat_lng |];
  ignore
  @@ Jv.call Leaflet.popup "setContent" [| Jv.of_string "create thread here" |];
  ignore @@ Jv.call Leaflet.popup "openOn" [| Leaflet.map |];

  let lat = Jv.get lat_lng "lat" in
  let lng = Jv.get lat_lng "lng" in
  let lat_input = Jv.get Jv.global "lat_input" in
  let lng_input = Jv.get Jv.global "lng_input" in
  ignore @@ Jv.call lat_input "setAttribute" [| Jv.of_string "value"; lat |];
  ignore @@ Jv.call lng_input "setAttribute" [| Jv.of_string "value"; lng |];

  let form_div = Jv.get Jv.global "newthread-form" in
  ignore
  @@ Jv.call form_div "setAttribute"
       [| Jv.of_string "style"; Jv.of_string "visibility:visible" |];
  let board_div = Jv.get Jv.global "board" in
  ignore
  @@ Jv.call board_div "setAttribute"
       [| Jv.of_string "style"; Jv.of_string "visibility:hidden" |];
  ()

(*add on_click callback to map*)
let () =
  ignore
  @@ Jv.call Leaflet.map "on" [| Jv.of_string "click"; Jv.repr on_click |]

(*!Duplicate*)
(* make image description field visible when a file is selected*)
let make_visible alt_input alt_label _event =
  let alt_style = Jv.get alt_input "style" in
  let alt_label_style = Jv.get alt_label "style" in
  ignore @@ Jv.set alt_style "display" (Jv.of_string "block");
  ignore @@ Jv.set alt_label_style "display" (Jv.of_string "block");
  ()

let () =
  log "change image description visibility@.";
  let file_input = Jv.find Jv.global "file" in
  match file_input with
  | None -> () (*not post form on the page, not logged in*)
  | Some file_input ->
    let alt_input = Jv.get Jv.global "alt" in
    let alt_label = Jv.get Jv.global "altLabel" in
    ignore
    @@ Jv.call file_input "addEventListener"
         [| Jv.of_string "change"; Jv.repr (make_visible alt_input alt_label) |];
    ()
