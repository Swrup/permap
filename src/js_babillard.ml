let log = Format.printf

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

  (*add on_click callback to map*)
  let () =
    ignore @@ Jv.call map "on" [| Jv.of_string "click"; Jv.repr on_click |]
end

module Marker = struct
  let marker_on_click thread_preview thread_id _e =
    log "marker_on_click@.";
    let thread_id = Jv.to_string thread_id in
    let thread_preview_div = Jv.get Jv.global "thread_preview_div" in
    ignore @@ Jv.set thread_preview_div "innerHTML" thread_preview;
    let thread_link = Jv.get Jv.global "thread_link" in
    let board_div = Jv.get Jv.global "board" in
    let board =
      Jv.to_string
        (Jv.call board_div "getAttribute" [| Jv.of_string "boardvalue" |])
    in
    let link = "/" ^ board ^ "/" ^ thread_id in
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
    let board_div = Jv.get Jv.global "board" in
    let board =
      Jv.to_string
        (Jv.call board_div "getAttribute" [| Jv.of_string "boardvalue" |])
    in
    let window = Jv.get Jv.global "window" in
    let fetchfutur =
      Jv.call window "fetch" [| Jv.of_string ("/" ^ board ^ "/markers") |]
    in
    ignore @@ Jv.call fetchfutur "then" [| Jv.repr markers_handle_response |];
    ()
end
