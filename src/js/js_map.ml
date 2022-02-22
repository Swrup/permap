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
    log "creating map@\n";
    let open Brr in
    let _container = El.div ~at:At.[ id (Jstr.v "map") ] [] in
    Jv.call leaflet "map" [| Jv.of_string "map" |]

  (* create map tile layer *)
  let tile_layer =
    log "creating tile layer@\n";
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
    log "adding tile layer@\n";
    let _map : Jv.t = Jv.call tile_layer "addTo" [| map |] in
    ()

  let storage = Brr_io.Storage.local Brr.G.window

  (* set map's view *)
  (* try to set map's view to last position viewed by using web storage *)
  let () =
    log "setting view@\n";
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
    log "on moveend event@\n";
    let latlng = Jv.call map "getCenter" [||] in
    (*we need to wrap coordinates so we don't drift into a parralel universe and lose track of markers :^) *)
    let wrapped_latlng = Jv.call map "wrapLatLng" [| latlng |] in
    let lat = Jv.get wrapped_latlng "lat" in
    let lng = Jv.get wrapped_latlng "lng" in
    match
      Brr_io.Storage.set_item storage (Jstr.of_string "lat") (Jv.to_jstr lat)
    with
    | (exception Jv.Error _) | Error _ -> failwith "can't set latlng storage"
    | Ok () -> (
      match
        Brr_io.Storage.set_item storage (Jstr.of_string "lng") (Jv.to_jstr lng)
      with
      | (exception Jv.Error _) | Error _ -> failwith "can't set latlng storage"
      | Ok () ->
        let is_wrapped =
          not @@ Jv.to_bool @@ Jv.call latlng "equals" [| wrapped_latlng |]
        in
        if is_wrapped then (
          log "setView to wrapped coordinate@\n";
          (* warning: calling setView in on_moveend can cause recursion *)
          ignore @@ Jv.call map "setView" [| wrapped_latlng |] ) )

  let on_zoomend _event =
    log "on zoomend event@\n";
    let zoom = Jv.call map "getZoom" [||] in
    match
      Brr_io.Storage.set_item storage (Jstr.of_string "zoom") (Jv.to_jstr zoom)
    with
    | (exception Jv.Error _) | Error _ -> failwith "can't set latlng storage"
    | Ok () -> ()

  let () =
    log "add on (move/zoom)end event@\n";
    ignore @@ Jv.call map "on" [| Jv.of_string "moveend"; Jv.repr on_moveend |];
    ignore @@ Jv.call map "on" [| Jv.of_string "zoomend"; Jv.repr on_zoomend |]
end

module Geolocalize = struct
  let update_location geo =
    log "update_location@\n";
    match geo with
    | Error _ -> failwith "error in geolocation"
    | Ok geo ->
      let lat = Brr_io.Geolocation.Pos.latitude geo in
      let lng = Brr_io.Geolocation.Pos.longitude geo in
      let latlng =
        Jv.call Leaflet.leaflet "latLng" [| Jv.of_float lat; Jv.of_float lng |]
      in
      ignore @@ Jv.call Leaflet.map "setView" [| latlng; Jv.of_int 13 |]

  let geolocalize _ =
    log "geolocalize@\n";
    let l = Brr_io.Geolocation.of_navigator Brr.G.navigator in
    ignore @@ Fut.await (Brr_io.Geolocation.get l) update_location

  let () =
    let button = Jv.get Jv.global "geolocalize" in
    ignore
    @@ Jv.call button "addEventListener"
         [| Jv.of_string "click"; Jv.repr geolocalize |]
end
