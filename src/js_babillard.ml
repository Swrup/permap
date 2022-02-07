let log = Format.printf

(*TODO fix duplicate module *)
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

module Marker = struct
  let board =
    let board_div = Jv.get Jv.global "board" in
    Jv.to_string
      (Jv.call board_div "getAttribute" [| Jv.of_string "data-board" |])

  let marker_on_click thread_preview thread_id _e =
    log "marker_on_click@.";
    let thread_id = Jv.to_string thread_id in
    let thread_preview_div = Jv.get Jv.global "thread_preview_div" in
    ignore @@ Jv.set thread_preview_div "innerHTML" thread_preview;
    let thread_link = Jv.get Jv.global "thread_link" in
    let link = Format.sprintf "/%s/%s" board thread_id in
    ignore @@ Jv.set thread_link "href" (Jv.of_string link);
    ignore @@ Jv.set thread_link "innerText" (Jv.of_string "[View Thread]");
    ()

  let on_each_feature feature layer =
    log "on_each_feature@.";
    let feature_properties = Jv.get feature "properties" in
    let thread_preview = Jv.get feature_properties "content" in
    let thread_id = Jv.get feature_properties "thread_id" in
    ignore
    @@ Jv.call layer "on"
         [| Jv.of_string "click"
          ; Jv.repr (marker_on_click thread_preview thread_id)
         |];
    ()

  let handle_geojson geojson =
    log "handle_geojson@.";
    log "feed geojson to leaflet@.";
    let dict = Jv.obj [| ("onEachFeature", Jv.repr on_each_feature) |] in
    let layer = Jv.call Leaflet.leaflet "geoJSON" [| geojson; dict |] in
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
