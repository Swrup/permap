open Js_map
include Js_post_form

module Visibility = struct
  let is_in_new_thread_mode () =
    log "is_in_new_thread_mode?@\n";
    let new_thread_div = Jv.get Jv.global "new-thread" in
    let class_list = Jv.get new_thread_div "classList" in
    Jv.to_bool @@ Jv.call class_list "contains" [| Jv.of_string "on" |]

  let set visible el =
    log "set (un)visible@\n";
    let class_list = Jv.get el "classList" in
    if visible then
      ignore
      @@ Jv.call class_list "replace"
           [| Jv.of_string "off"; Jv.of_string "on" |]
    else
      ignore
      @@ Jv.call class_list "replace"
           [| Jv.of_string "on"; Jv.of_string "off" |]

  let change_page_mode ~form_visibility _event =
    log "change_page_mode@\n";
    let new_thread_div = Jv.get Jv.global "new-thread" in
    let thread_preview_div = Jv.get Jv.global "thread-preview" in
    let new_thread_button = Jv.get Jv.global "new-thread-button" in
    let return_button = Jv.get Jv.global "return-button" in
    let () = set form_visibility new_thread_div in
    let () = set form_visibility return_button in
    let () = set (not form_visibility) thread_preview_div in
    let () = set (not form_visibility) new_thread_button in
    ignore @@ Jv.call Leaflet.map "closePopup" [||]

  let () =
    log "add events on return/new thread button@\n";
    let return_button = Jv.get Jv.global "return-button" in
    ignore
    @@ Jv.call return_button "addEventListener"
         [| Jv.of_string "click"
          ; Jv.repr (change_page_mode ~form_visibility:false)
         |];
    (* new-thread-button is new-thread-button-redirect if not logged in *)
    let opt = Jv.find Jv.global "new-thread-button" in
    if Option.is_some opt then
      let new_thread_button = Option.get opt in
      ignore
      @@ Jv.call new_thread_button "addEventListener"
           [| Jv.of_string "click"
            ; Jv.repr (change_page_mode ~form_visibility:true)
           |]
end

module Marker = struct
  let marker_on_click thread_preview _e =
    log "marker_on_click@\n";
    if not (Visibility.is_in_new_thread_mode ()) then (
      let thread_preview_div = Jv.get Jv.global "thread-preview" in
      ignore @@ Jv.set thread_preview_div "innerHTML" thread_preview;
      let _ = Js_pretty_post.make_pretty () in
      () )

  let on_each_feature feature layer =
    log "on_each_feature@\n";
    let feature_properties = Jv.get feature "properties" in
    let thread_preview = Jv.get feature_properties "content" in
    ignore
    @@ Jv.call layer "on"
         [| Jv.of_string "click"; Jv.repr (marker_on_click thread_preview) |]

  let handle_geojson geojson =
    log "handle_geojson@\n";
    log "feed geojson to leaflet@\n";
    (* make markers unresponsive on*)
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
    let window = Jv.get Jv.global "window" in
    let link = Jv.of_string "/markers" in
    let fetchfutur = Jv.call window "fetch" [| link |] in
    ignore @@ Jv.call fetchfutur "then" [| Jv.repr markers_handle_response |]
end

(* set input lat/lng when clicked*)
let on_click_set_latlng e =
  log "on_click_set_latlng@\n";
  if Visibility.is_in_new_thread_mode () then (
    let lat_lng = Jv.get e "latlng" in
    ignore @@ Jv.call Leaflet.popup "setLatLng" [| lat_lng |];
    ignore
    @@ Jv.call Leaflet.popup "setContent"
         [| Jv.of_string "create thread here" |];
    ignore @@ Jv.call Leaflet.popup "openOn" [| Leaflet.map |];

    let lat = Jv.get lat_lng "lat" in
    let lng = Jv.get lat_lng "lng" in
    let lat_input = Jv.get Jv.global "lat-input" in
    let lng_input = Jv.get Jv.global "lng-input" in
    ignore @@ Jv.call lat_input "setAttribute" [| Jv.of_string "value"; lat |];
    ignore @@ Jv.call lng_input "setAttribute" [| Jv.of_string "value"; lng |];

    log "on_click_set_latlng: disableb disabled@\n";
    let button = Jv.get Jv.global "submit-new-thread-button" in
    ignore @@ Jv.call button "removeAttribute" [| Jv.of_string "disabled" |] )

(*add on_click callback to map*)
let () =
  ignore
  @@ Jv.call Leaflet.map "on"
       [| Jv.of_string "click"; Jv.repr on_click_set_latlng |]
