[@@@ocaml.warning "-69-27-32-37-34-50"]

open Lib

let flip (weight : float) : bool =
  let result = Random.float 1.0 in
  if result > weight then false else true
;;

module Utility = struct
  let timestep_to_string (timestep : (int, int) Timeseries.Timestep.t) =
    Printf.sprintf
      "%s | %d | Label: %d | Data: %d"
      timestep.timestamp
      timestep.entry
      timestep.label
      timestep.data
  ;;

  let print_timestep (timestep : (int, int) Timeseries.Timestep.t) =
    print_string (timestep_to_string timestep ^ "\n")
  ;;

  let print_timeseries (timeseries : (int, int) Timeseries.t) =
    let series = List.rev timeseries.series in
    List.iter print_timestep series
  ;;
end

module Setup = struct
  let init () = Random.self_init ()
end

module State = struct
  type t =
    { olin : int
    ; wellesly : int
    }

  let to_string t = Printf.sprintf "olin:      %d\nwellesley: %d" t.olin t.wellesly
  let print t = print_string (to_string t)
end

module Test = struct
  let flip_test (weight : float) (times : int) =
    let rec aux times' true_count false_count =
      match times' < 0 with
      | true ->
        Printf.printf
          "true: %d\nfalse: %d\ntrue ratio: %f\nfalse ratio: %f"
          true_count
          false_count
          (float_of_int true_count /. float_of_int times)
          (float_of_int false_count /. float_of_int times)
      | false ->
        if flip weight
        then aux (times' - 1) (true_count + 1) false_count
        else aux (times' - 1) true_count (false_count + 1)
    in
    aux times 0 0
  ;;
end

let bike_to_wellesly (t : State.t) : State.t = { olin = t.olin - 1; wellesly = t.wellesly + 1 }
let bike_to_olin (t : State.t) : State.t = { olin = t.olin + 1; wellesly = t.wellesly - 1 }

let step state wellelsly_probability olin_probability =
  let state' = if flip wellelsly_probability then bike_to_wellesly state else state in
  let state'' = if flip olin_probability then bike_to_olin state' else state' in
  state''
;;

(* let rec loop (grid : Tile.t Grid.t) pool = *)
(*   match Raylib.window_should_close () with *)
(*   | true -> Raylib.close_window () *)
(*   | false -> *)
(*     let new_grid = Domainslib.Task.run pool (fun _ -> parrallel_rule_apply pool grid) in *)
(*     Raylib.begin_drawing (); *)
(*     Raylib.clear_background Window.bg_color; *)
(*     Grid.iter Tile.draw new_grid; *)
(*     Raylib.end_drawing (); *)
(*     loop new_grid pool *)
(* ;; *)

let rec loop state olin_series count =
  match count < 0 with
  | true -> Utility.print_timeseries olin_series
  | false ->
    let new_state = step state 0.5 0.33 in
    let new_olin_series = Timeseries.add olin_series count new_state.olin in
    loop new_state new_olin_series (count - 1)
;;

let () =
  print_endline "Chapter 2";
  Setup.init ();
  let olin_timeseries = Timeseries.create () in
  let initial_state : State.t = { olin = 10; wellesly = 2 } in
  loop initial_state olin_timeseries 10
;;
