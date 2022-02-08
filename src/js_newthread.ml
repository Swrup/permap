let log = Format.printf

(*TODO fix duplicate modules *)
module Leaflet = struct
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

  let storage = Brr_io.Storage.local Brr.G.window

  (* set map's view *)
  (* try to set map's view to last position viewed by using web storage *)
  let () =
    log "setting view@.";
    let lat = Brr_io.Storage.get_item storage (Jstr.of_string "lat") in
    let lng = Brr_io.Storage.get_item storage (Jstr.of_string "lng") in
    let zoom = Brr_io.Storage.get_item storage (Jstr.of_string "zoom") in
    match (lat, lng, zoom) with
    | Some lat, Some lng, Some zoom ->
      let latlng =
        Jv.call leaflet "latLng" [| Jv.of_jstr lat; Jv.of_jstr lng |]
      in
      ignore @@ Jv.call map "setView" [| latlng; Jv.of_jstr zoom |]
    | _ ->
      let latlng =
        Jv.call leaflet "latLng" [| Jv.of_float 51.505; Jv.of_float (-0.09) |]
      in
      ignore @@ Jv.call map "setView" [| latlng; Jv.of_int 13 |]

  let on_moveend _event =
    log "on zoomend event@.";
    let latlng = Jv.call map "getCenter" [||] in
    let lat = Jv.get latlng "lat" in
    let lng = Jv.get latlng "lng" in
    match
      Brr_io.Storage.set_item storage (Jstr.of_string "lat") (Jv.to_jstr lat)
    with
    | (exception Jv.Error _)
    | Error _ ->
      failwith "can't set latlng storage"
    | Ok () -> (
      match
        Brr_io.Storage.set_item storage (Jstr.of_string "lng") (Jv.to_jstr lng)
      with
      | (exception Jv.Error _)
      | Error _ ->
        failwith "can't set latlng storage"
      | Ok () -> () )

  let on_zoomend _event =
    log "on zoomend event@.";
    let zoom = Jv.call map "getZoom" [||] in
    match
      Brr_io.Storage.set_item storage (Jstr.of_string "zoom") (Jv.to_jstr zoom)
    with
    | (exception Jv.Error _)
    | Error _ ->
      failwith "can't set latlng storage"
    | Ok () -> ()

  let () =
    log "add on (move/zoom)end event@.";
    ignore @@ Jv.call map "on" [| Jv.of_string "moveend"; Jv.repr on_moveend |];
    ignore @@ Jv.call map "on" [| Jv.of_string "zoomend"; Jv.repr on_zoomend |];
    ()
end

module Geolocalize = struct
  let update_location geo =
    log "update_location@.";
    match geo with
    | Error _ -> failwith "error in geolocation"
    | Ok geo ->
      let lat = Brr_io.Geolocation.Pos.latitude geo in
      let lng = Brr_io.Geolocation.Pos.longitude geo in
      let latlng =
        Jv.call Leaflet.leaflet "latLng" [| Jv.of_float lat; Jv.of_float lng |]
      in
      ignore @@ Jv.call Leaflet.map "setView" [| latlng; Jv.of_int 13 |];
      ()

  let geolocalize () =
    log "geolocalize@.";
    let l = Brr_io.Geolocation.of_navigator Brr.G.navigator in
    ignore @@ Fut.await (Brr_io.Geolocation.get l) update_location;
    ()

  let () = Jv.set Jv.global "geolocalize" (Jv.repr geolocalize)
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
