open Js_map
include Js_post_form

module Visibility = struct
  let new_thread_div = Jv.get Jv.global "new-thread"

  let thread_preview_div = Jv.get Jv.global "thread-preview"

  let return_button = Jv.get Jv.global "return-button"

  (* new-thread-button is new-thread-button-redirect if not logged in *)
  let new_thread_button = Jv.find Jv.global "new-thread-button"

  let is_in_new_thread_mode = ref false

  let set_visible el =
    log "set_visible@\n";
    let class_list = Jv.get el "classList" in
    ignore
    @@ Jv.call class_list "replace" [| Jv.of_string "off"; Jv.of_string "on" |]

  let set_invisible el =
    log "set_invisible@\n";
    let class_list = Jv.get el "classList" in
    ignore
    @@ Jv.call class_list "replace" [| Jv.of_string "on"; Jv.of_string "off" |]

  let to_new_thread_mode _event =
    log "change_page_mode@\n";
    is_in_new_thread_mode := true;
    set_visible new_thread_div;
    set_visible return_button;
    set_invisible thread_preview_div;
    Option.iter set_invisible new_thread_button;
    ignore @@ Jv.call Leaflet.map "closePopup" [||]

  let to_babillard_mode _event =
    log "change_page_mode@\n";
    is_in_new_thread_mode := false;
    set_invisible new_thread_div;
    set_invisible return_button;
    set_visible thread_preview_div;
    Option.iter set_visible new_thread_button;
    ignore @@ Jv.call Leaflet.map "closePopup" [||]

  let () =
    log "add events on return/new thread button@\n";
    ignore
    @@ Jv.call return_button "addEventListener"
         [| Jv.of_string "click"; Jv.repr to_babillard_mode |];
    Option.iter
      (fun button ->
        ignore
        @@ Jv.call button "addEventListener"
             [| Jv.of_string "click"; Jv.repr to_new_thread_mode |] )
      new_thread_button
end

module Marker = struct
  let window = Jv.get Jv.global "window"

  let thread_preview_div = Jv.get Jv.global "thread-preview"

  let marker_on_click thread_preview _e =
    log "marker_on_click@\n";
    if not !Visibility.is_in_new_thread_mode then
      ignore @@ Jv.set thread_preview_div "innerHTML" thread_preview;
    Js_pretty_post.make_pretty ()

  let on_each_feature feature layer =
    log "on_each_feature@\n";
    let feature_properties = Jv.get feature "properties" in
    let thread_preview = Jv.get feature_properties "content" in
    ignore
    @@ Jv.call layer "on"
         [| Jv.of_string "click"; Jv.repr (marker_on_click thread_preview) |]

  let handle_geojson geojson =
    log "handle_geojson@\n";
    let dict = Jv.obj [| ("onEachFeature", Jv.repr on_each_feature) |] in
    let layer = Jv.call Leaflet.leaflet "geoJSON" [| geojson; dict |] in
    let _marker_layer = Jv.call layer "addTo" [| Leaflet.map |] in
    ()

  let markers_handle_response response =
    log "markers_handle_response@\n";
    let geo_json_list_futur = Jv.call response "json" [||] in
    ignore @@ Jv.call geo_json_list_futur "then" [| Jv.repr handle_geojson |]

  let () =
    log "fetch thread geojson@\n";
    let link = Jv.of_string "/markers" in
    let fetchfutur = Jv.call window "fetch" [| link |] in
    ignore @@ Jv.call fetchfutur "then" [| Jv.repr markers_handle_response |]
end

let lat_input = Jv.get Jv.global "lat-input"

let lng_input = Jv.get Jv.global "lng-input"

let button = Jv.get Jv.global "submit-new-thread-button"

(* set input lat/lng when clicked*)
let on_click_set_latlng e =
  log "on_click_set_latlng@\n";
  if !Visibility.is_in_new_thread_mode then (
    let lat_lng = Jv.get e "latlng" in
    ignore @@ Jv.call Leaflet.popup "setLatLng" [| lat_lng |];
    ignore
    @@ Jv.call Leaflet.popup "setContent"
         [| Jv.of_string "create thread here" |];
    ignore @@ Jv.call Leaflet.popup "openOn" [| Leaflet.map |];

    let lat = Jv.get lat_lng "lat" in
    let lng = Jv.get lat_lng "lng" in
    ignore @@ Jv.call lat_input "setAttribute" [| Jv.of_string "value"; lat |];
    ignore @@ Jv.call lng_input "setAttribute" [| Jv.of_string "value"; lng |];

    ignore @@ Jv.call button "removeAttribute" [| Jv.of_string "disabled" |] )

(*add on_click callback to map*)
let () =
  ignore
  @@ Jv.call Leaflet.map "on"
       [| Jv.of_string "click"; Jv.repr on_click_set_latlng |]
