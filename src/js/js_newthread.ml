open Js_map
include Js_post_form

let log = Format.printf

(* set input lat/lng when clicked and make new thread form visible *)
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
  let newthread_div = Jv.get Jv.global "newthread" in
  ignore
  @@ Jv.call newthread_div "setAttribute"
       [| Jv.of_string "style"; Jv.of_string "visibility:hidden" |];
  ()

(*add on_click callback to map*)
let () =
  ignore
  @@ Jv.call Leaflet.map "on" [| Jv.of_string "click"; Jv.repr on_click |]
