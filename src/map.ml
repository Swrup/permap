let log = Format.printf

(* get the leaflet object *)
let leaflet =
  match Jv.(find global "L") with
  | Some l -> l
  | None -> failwith "can't load leaflet"

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

(* TODO :-)
   (* binding callbacks@ *)
     let () =
       Format.eprintf "binding callbacks@.";

       Ev.listen Leaflet.Map.click (fun e ->
           Console.log [ e |> Ev.as_type |> Leaflet.Ev.MouseEvent.latlng ] )
       @@ Leaflet.Map.as_target map
*)
