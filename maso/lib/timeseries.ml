[@@@ocaml.warning "-69-27-32-37-34-50"]

let get_timestamp () = Unix.localtime (Unix.time ())

module Timestep = struct
  type ('a, 'b) t =
    { label : 'a
    ; timestamp : Unix.tm
    ; entry : int
    ; data : 'b
    }

  let new_timestep label entry data =
    let timestamp = get_timestamp () in
    { label; entry; timestamp; data }
  ;;

  let label t = t.label
  let data t = t.data
end

type ('a, 'b) t = { series : ('a, 'b) Timestep.t list }

let create () = { series = [] }
let add_timestep t step = { series = step :: t.series }

let add t label data =
  let entry =
    match t.series with
    | [] -> 0
    | hd :: _ -> hd.entry + 1
  in
  let timestep = Timestep.new_timestep label entry data in
  add_timestep t timestep
;;

let length t = List.length t.series

let get_by_label t label =
  let rec aux (series : ('a, 'b) Timestep.t list) =
    match series with
    | [] -> None
    | hd :: tl -> if hd.label = label then Some hd else aux tl
  in
  aux t.series
;;

let get_by_entry t entry =
  let rec aux (series : ('a, 'b) Timestep.t list) =
    match series with
    | [] -> None
    | hd :: tl -> if hd.entry = entry then Some hd else aux tl
  in
  aux t.series
;;

let timestamp_to_string t =
  let day = t.Unix.tm_mday in
  let month = t.Unix.tm_mon + 1 in
  let year = t.Unix.tm_year + 1990 in
  let hour = t.Unix.tm_hour in
  let min = t.Unix.tm_min in
  let sec = t.Unix.tm_sec in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d" year month day hour min sec
;;

let iter t ~f = List.iter f t.series
let map t ~f = List.map f t.series
