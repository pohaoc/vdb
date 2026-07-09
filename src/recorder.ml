open! Core
open Bonsai_term

module State = struct
  type t =
    | Idle
    | Recording of
        { pane : Pane_tree.Id.t
        ; plugin_name : string
        ; x_axis : string
        ; y_axis : string
        ; points : (float * float) list
        }
    | Prompting of
        { plugin_name : string
        ; x_axis : string
        ; y_axis : string
        ; points : (float * float) list
        ; filename : string
        }
  [@@deriving sexp_of, equal]
end

let default_name ~plugin_name =
  let now = Time_ns.now () in
  let date, ofday = Time_ns.to_date_ofday now ~zone:(force Timezone.local) in
  let time_part =
    String.prefix (Time_ns.Ofday.to_string ofday) 8
    |> String.substr_replace_all ~pattern:":" ~with_:""
  in
  let date_part = Date.to_string date |> String.substr_replace_all ~pattern:"-" ~with_:"" in
  sprintf "vdb-%s-%s-%s.csv" plugin_name date_part time_part
;;

let resolve_path ~input ~plugin_name =
  let input = String.strip input in
  if String.is_empty input
  then default_name ~plugin_name
  else if String.is_suffix input ~suffix:"/"
  then input ^/ default_name ~plugin_name
  else if (match Stdlib.Sys.is_directory input with
           | is_dir -> is_dir
           | exception _ -> false)
  then input ^/ default_name ~plugin_name
  else input
;;

(* Integer-valued floats (e.g. byte counts) print without a spurious ".", everything
   else round-trips. *)
let format_float value =
  if Float.is_integer value && Float.(abs value < 1e15)
  then sprintf "%.0f" value
  else Float.to_string value
;;

let save ~input ~plugin_name ~x_axis ~y_axis ~points =
  (* [Async] shadows [input] (the blocking Stdlib reader), so rebind it first. *)
  let user_input = input in
  Effect.of_deferred_thunk (fun () ->
    let open Async in
    let%map result =
      Monitor.try_with_or_error (fun () ->
        let path = resolve_path ~input:user_input ~plugin_name in
        let contents =
          (sprintf "%s,%s" x_axis y_axis
           :: List.map points ~f:(fun (x, y) ->
             sprintf "%s,%s" (format_float x) (format_float y)))
          |> String.concat ~sep:"\n"
        in
        let%map () = Writer.save path ~contents:(contents ^ "\n") in
        path)
    in
    Result.map_error result ~f:Error.to_string_hum)
;;
